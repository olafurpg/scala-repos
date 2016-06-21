/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */

/**
  * Some elements of this were copied from:
  *
  * https://gist.github.com/casualjim/1819496
  */
package play.it.http.websocket

import java.util.concurrent.atomic.AtomicBoolean

import akka.stream.FlowShape
import akka.stream.scaladsl._
import akka.stream.stage.{Context, PushStage}
import akka.util.ByteString
import com.typesafe.netty.{HandlerPublisher, HandlerSubscriber}
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.{ByteBufHolder, Unpooled}
import io.netty.channel.socket.SocketChannel
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http._
import io.netty.handler.codec.http.websocketx._

import java.net.URI
import io.netty.util.ReferenceCountUtil
import play.api.http.websocket._
import play.it.http.websocket.WebSocketClient.ExtendedMessage

import scala.concurrent.{Promise, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * A basic WebSocketClient.  Basically wraps Netty's WebSocket support into something that's much easier to use and much
  * more Scala friendly.
  */
trait WebSocketClient {

  /**
    * Connect to the given URI.
    *
    * @return A future that will be redeemed when the connection is closed.
    */
  def connect(url: URI, version: WebSocketVersion = WebSocketVersion.V13)(
      onConnect: Flow[ExtendedMessage, ExtendedMessage, _] => Unit): Future[_]

  /**
    * Shutdown the client and release all associated resources.
    */
  def shutdown()
}

object WebSocketClient {

  trait ExtendedMessage {
    def finalFragment: Boolean
  }
  object ExtendedMessage {
    implicit def messageToExtendedMessage(message: Message): ExtendedMessage =
      SimpleMessage(message, finalFragment = true)
  }
  case class SimpleMessage(message: Message, finalFragment: Boolean)
      extends ExtendedMessage
  case class ContinuationMessage(data: ByteString, finalFragment: Boolean)
      extends ExtendedMessage

  def create(): WebSocketClient = new DefaultWebSocketClient

  def apply[T](block: WebSocketClient => T) = {
    val client = WebSocketClient.create()
    try {
      block(client)
    } finally {
      client.shutdown()
    }
  }

  private implicit class ToFuture(chf: ChannelFuture) {
    def toScala: Future[Channel] = {
      val promise = Promise[Channel]()
      chf.addListener(new ChannelFutureListener {
        def operationComplete(future: ChannelFuture) = {
          if (future.isSuccess) {
            promise.success(future.channel())
          } else if (future.isCancelled) {
            promise.failure(new RuntimeException("Future cancelled"))
          } else {
            promise.failure(future.cause())
          }
        }
      })
      promise.future
    }
  }

  private class DefaultWebSocketClient extends WebSocketClient {

    val eventLoop = new NioEventLoopGroup()
    val client = new Bootstrap()
      .group(eventLoop)
      .channel(classOf[NioSocketChannel])
      .option(ChannelOption.AUTO_READ, java.lang.Boolean.FALSE)
      .handler(new ChannelInitializer[SocketChannel] {
        def initChannel(ch: SocketChannel) = {
          ch.pipeline()
            .addLast(new HttpClientCodec, new HttpObjectAggregator(8192))
        }
      })

    /**
      * Connect to the given URI
      */
    def connect(url: URI, version: WebSocketVersion)(
        onConnected: (Flow[ExtendedMessage, ExtendedMessage, _]) => Unit) = {

      val normalized = url.normalize()
      val tgt =
        if (normalized.getPath == null || normalized.getPath.trim().isEmpty) {
          new URI(normalized.getScheme,
                  normalized.getAuthority,
                  "/",
                  normalized.getQuery,
                  normalized.getFragment)
        } else normalized

      val disconnected = Promise[Unit]()

      client
        .connect(tgt.getHost, tgt.getPort)
        .toScala
        .map { channel =>
          val handshaker = WebSocketClientHandshakerFactory
            .newHandshaker(tgt, version, null, false, new DefaultHttpHeaders())
          channel
            .pipeline()
            .addLast(
                "supervisor",
                new WebSocketSupervisor(disconnected, handshaker, onConnected))
          handshaker.handshake(channel)
          channel.read()
        }
        .onFailure {
          case t => disconnected.tryFailure(t)
        }

      disconnected.future
    }

    def shutdown() = eventLoop.shutdownGracefully()
  }

  private class WebSocketSupervisor(
      disconnected: Promise[Unit],
      handshaker: WebSocketClientHandshaker,
      onConnected: Flow[ExtendedMessage, ExtendedMessage, _] => Unit)
      extends ChannelInboundHandlerAdapter {
    override def channelRead(ctx: ChannelHandlerContext, msg: Object) {
      msg match {
        case resp: HttpResponse if handshaker.isHandshakeComplete =>
          throw new WebSocketException(
              "Unexpected HttpResponse (status=" + resp.getStatus + ")")
        case resp: FullHttpResponse =>
          // Setup the pipeline
          val publisher =
            new HandlerPublisher(ctx.executor, classOf[WebSocketFrame])
          val subscriber = new HandlerSubscriber[WebSocketFrame](ctx.executor)
          ctx.pipeline.addAfter(ctx.executor,
                                ctx.name,
                                "websocket-subscriber",
                                subscriber)
          ctx.pipeline
            .addAfter(ctx.executor, ctx.name, "websocket-publisher", publisher)

          // Now remove ourselves from the chain
          ctx.pipeline.remove(ctx.name)

          handshaker.finishHandshake(ctx.channel(), resp)

          val clientConnection = Flow.fromSinkAndSource(
              Sink.fromSubscriber(subscriber),
              Source.fromPublisher(publisher))

          onConnected(webSocketProtocol(clientConnection))

        case _ => throw new WebSocketException("Unexpected message: " + msg)
      }
    }

    val serverInitiatedClose = new AtomicBoolean

    def webSocketProtocol(
        clientConnection: Flow[WebSocketFrame, WebSocketFrame, _])
      : Flow[ExtendedMessage, ExtendedMessage, _] = {
      val clientInitiatedClose = new AtomicBoolean

      val captureClientClose = Flow[WebSocketFrame].transform(() =>
            new PushStage[WebSocketFrame, WebSocketFrame] {
          def onPush(elem: WebSocketFrame, ctx: Context[WebSocketFrame]) =
            elem match {
              case close: CloseWebSocketFrame =>
                clientInitiatedClose.set(true)
                ctx.push(close)
              case other =>
                ctx.push(other)
            }
      })

      val messagesToFrames = Flow[ExtendedMessage].map {
        case SimpleMessage(TextMessage(data), finalFragment) =>
          new TextWebSocketFrame(finalFragment, 0, data)
        case SimpleMessage(BinaryMessage(data), finalFragment) =>
          new BinaryWebSocketFrame(finalFragment,
                                   0,
                                   Unpooled.wrappedBuffer(data.asByteBuffer))
        case SimpleMessage(PingMessage(data), finalFragment) =>
          new PingWebSocketFrame(finalFragment,
                                 0,
                                 Unpooled.wrappedBuffer(data.asByteBuffer))
        case SimpleMessage(PongMessage(data), finalFragment) =>
          new PongWebSocketFrame(finalFragment,
                                 0,
                                 Unpooled.wrappedBuffer(data.asByteBuffer))
        case SimpleMessage(CloseMessage(statusCode, reason), finalFragment) =>
          new CloseWebSocketFrame(finalFragment,
                                  0,
                                  statusCode.getOrElse(CloseCodes.NoStatus),
                                  reason)
        case ContinuationMessage(data, finalFragment) =>
          new ContinuationWebSocketFrame(
              finalFragment,
              0,
              Unpooled.wrappedBuffer(data.asByteBuffer))
      }

      val framesToMessages = Flow[WebSocketFrame].map { frame =>
        val message = frame match {
          case text: TextWebSocketFrame =>
            SimpleMessage(TextMessage(text.text()), text.isFinalFragment)
          case binary: BinaryWebSocketFrame =>
            SimpleMessage(BinaryMessage(toByteString(binary)),
                          binary.isFinalFragment)
          case ping: PingWebSocketFrame =>
            SimpleMessage(PingMessage(toByteString(ping)),
                          ping.isFinalFragment)
          case pong: PongWebSocketFrame =>
            SimpleMessage(PongMessage(toByteString(pong)),
                          pong.isFinalFragment)
          case close: CloseWebSocketFrame =>
            SimpleMessage(
                CloseMessage(Some(close.statusCode()), close.reasonText()),
                close.isFinalFragment)
          case continuation: ContinuationWebSocketFrame =>
            ContinuationMessage(toByteString(continuation),
                                continuation.isFinalFragment)
        }
        ReferenceCountUtil.release(frame)
        message
      }

      messagesToFrames via captureClientClose via Flow.fromGraph(
          GraphDSL.create[FlowShape[WebSocketFrame, WebSocketFrame]]() {
        implicit b =>
          import GraphDSL.Implicits._

          val broadcast = b.add(Broadcast[WebSocketFrame](2))
          val merge = b.add(Merge[WebSocketFrame](2, eagerComplete = true))

          val handleServerClose = Flow[WebSocketFrame].filter {
            frame =>
              if (frame.isInstanceOf[CloseWebSocketFrame] &&
                  !clientInitiatedClose.get()) {
                serverInitiatedClose.set(true)
                true
              } else {
                // If we're going to drop it, we need to release it first
                ReferenceCountUtil.release(frame)
                false
              }
          }

          val handleConnectionTerminated = Flow[WebSocketFrame].transform(() =>
                new PushStage[WebSocketFrame, WebSocketFrame] {
              def onPush(elem: WebSocketFrame, ctx: Context[WebSocketFrame]) =
                ctx.push(elem)
              override def onUpstreamFinish(ctx: Context[WebSocketFrame]) = {
                disconnected.trySuccess(())
                super.onUpstreamFinish(ctx)
              }
              override def onUpstreamFailure(cause: Throwable,
                                             ctx: Context[WebSocketFrame]) = {
                if (serverInitiatedClose.get()) {
                  disconnected.trySuccess(())
                  ctx.finish()
                } else {
                  disconnected.tryFailure(cause)
                  ctx.fail(cause)
                }
              }
          })

          /**
            * Since we've got two consumers of the messages when we broadcast, we need to ensure that they get retained for each.
            */
          val retainForBroadcast = Flow[WebSocketFrame].map { frame =>
            ReferenceCountUtil.retain(frame)
            frame
          }

          merge.out ~> clientConnection ~> handleConnectionTerminated ~> retainForBroadcast ~> broadcast.in
          merge.in(0) <~ handleServerClose <~ broadcast.out(0)

          FlowShape(merge.in(1), broadcast.out(1))
      }) via framesToMessages
    }

    def toByteString(data: ByteBufHolder) = {
      val builder = ByteString.newBuilder
      data
        .content()
        .readBytes(builder.asOutputStream, data.content().readableBytes())
      val bytes = builder.result()
      bytes
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, e: Throwable) {
      if (serverInitiatedClose.get()) {
        disconnected.trySuccess(())
      } else {
        disconnected.tryFailure(e)
      }
      ctx.channel.close()
      ctx.fireExceptionCaught(e)
    }

    override def channelInactive(ctx: ChannelHandlerContext) = {
      disconnected.trySuccess(())
    }
  }

  class WebSocketException(s: String, th: Throwable)
      extends java.io.IOException(s, th) {
    def this(s: String) = this(s, null)
  }
}
