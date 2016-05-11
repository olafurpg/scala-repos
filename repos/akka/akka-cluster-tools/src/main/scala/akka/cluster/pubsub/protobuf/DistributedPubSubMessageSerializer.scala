/**
  * Copyright (C) 2009-2016 Lightbend Inc. <http://www.lightbend.com>
  */
package akka.cluster.pubsub.protobuf

import akka.serialization.BaseSerializer
import scala.collection.breakOut
import akka.actor.{ExtendedActorSystem, Address}
import scala.Some
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import akka.protobuf.{ByteString, MessageLite}
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import akka.cluster.pubsub.protobuf.msg.{DistributedPubSubMessages ⇒ dm}
import scala.collection.JavaConverters._
import akka.cluster.pubsub.DistributedPubSubMediator._
import akka.cluster.pubsub.DistributedPubSubMediator.Internal._
import akka.serialization.Serialization
import akka.actor.ActorRef
import akka.serialization.SerializationExtension
import scala.collection.immutable.TreeMap
import akka.serialization.SerializerWithStringManifest

/**
  * INTERNAL API: Protobuf serializer of DistributedPubSubMediator messages.
  */
private[akka] class DistributedPubSubMessageSerializer(
    val system: ExtendedActorSystem)
    extends SerializerWithStringManifest with BaseSerializer {

  private lazy val serialization = SerializationExtension(system)

  private final val BufferSize = 1024 * 4

  private val StatusManifest = "A"
  private val DeltaManifest = "B"
  private val SendManifest = "C"
  private val SendToAllManifest = "D"
  private val PublishManifest = "E"

  private val fromBinaryMap =
    collection.immutable.HashMap[String, Array[Byte] ⇒ AnyRef](
        StatusManifest -> statusFromBinary,
        DeltaManifest -> deltaFromBinary,
        SendManifest -> sendFromBinary,
        SendToAllManifest -> sendToAllFromBinary,
        PublishManifest -> publishFromBinary)

  override def manifest(obj: AnyRef): String = obj match {
    case _: Status ⇒ StatusManifest
    case _: Delta ⇒ DeltaManifest
    case _: Send ⇒ SendManifest
    case _: SendToAll ⇒ SendToAllManifest
    case _: Publish ⇒ PublishManifest
    case _ ⇒
      throw new IllegalArgumentException(
          s"Can't serialize object of type ${obj.getClass} in [${getClass.getName}]")
  }

  override def toBinary(obj: AnyRef): Array[Byte] = obj match {
    case m: Status ⇒ compress(statusToProto(m))
    case m: Delta ⇒ compress(deltaToProto(m))
    case m: Send ⇒ sendToProto(m).toByteArray
    case m: SendToAll ⇒ sendToAllToProto(m).toByteArray
    case m: Publish ⇒ publishToProto(m).toByteArray
    case _ ⇒
      throw new IllegalArgumentException(
          s"Can't serialize object of type ${obj.getClass} in [${getClass.getName}]")
  }

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef =
    fromBinaryMap.get(manifest) match {
      case Some(f) ⇒ f(bytes)
      case None ⇒
        throw new IllegalArgumentException(
            s"Unimplemented deserialization of message with manifest [$manifest] in [${getClass.getName}]")
    }

  private def compress(msg: MessageLite): Array[Byte] = {
    val bos = new ByteArrayOutputStream(BufferSize)
    val zip = new GZIPOutputStream(bos)
    try msg.writeTo(zip) finally zip.close()
    bos.toByteArray
  }

  private def decompress(bytes: Array[Byte]): Array[Byte] = {
    val in = new GZIPInputStream(new ByteArrayInputStream(bytes))
    val out = new ByteArrayOutputStream()
    val buffer = new Array[Byte](BufferSize)

    @tailrec def readChunk(): Unit = in.read(buffer) match {
      case -1 ⇒ ()
      case n ⇒
        out.write(buffer, 0, n)
        readChunk()
    }

    try readChunk() finally in.close()
    out.toByteArray
  }

  private def addressToProto(address: Address): dm.Address.Builder =
    address match {
      case Address(protocol, system, Some(host), Some(port)) ⇒
        dm.Address
          .newBuilder()
          .setSystem(system)
          .setHostname(host)
          .setPort(port)
          .setProtocol(protocol)
      case _ ⇒
        throw new IllegalArgumentException(
            s"Address [${address}] could not be serialized: host or port missing.")
    }

  private def addressFromProto(address: dm.Address): Address =
    Address(address.getProtocol,
            address.getSystem,
            address.getHostname,
            address.getPort)

  private def statusToProto(status: Status): dm.Status = {
    val versions = status.versions.map {
      case (a, v) ⇒
        dm.Status.Version
          .newBuilder()
          .setAddress(addressToProto(a))
          .setTimestamp(v)
          .build()
    }.toVector.asJava
    dm.Status.newBuilder().addAllVersions(versions).build()
  }

  private def statusFromBinary(bytes: Array[Byte]): Status =
    statusFromProto(dm.Status.parseFrom(decompress(bytes)))

  private def statusFromProto(status: dm.Status): Status =
    Status(
        status.getVersionsList.asScala
          .map(v ⇒ addressFromProto(v.getAddress) -> v.getTimestamp)(breakOut))

  private def deltaToProto(delta: Delta): dm.Delta = {
    val buckets = delta.buckets.map { b ⇒
      val entries = b.content.map {
        case (key, value) ⇒
          val b =
            dm.Delta.Entry.newBuilder().setKey(key).setVersion(value.version)
          value.ref.foreach(r ⇒ b.setRef(Serialization.serializedActorPath(r)))
          b.build()
      }.toVector.asJava

      dm.Delta.Bucket
        .newBuilder()
        .setOwner(addressToProto(b.owner))
        .setVersion(b.version)
        .addAllContent(entries)
        .build()
    }.toVector.asJava
    dm.Delta.newBuilder().addAllBuckets(buckets).build()
  }

  private def deltaFromBinary(bytes: Array[Byte]): Delta =
    deltaFromProto(dm.Delta.parseFrom(decompress(bytes)))

  private def deltaFromProto(delta: dm.Delta): Delta =
    Delta(
        delta.getBucketsList.asScala.toVector.map { b ⇒
      val content: TreeMap[String, ValueHolder] =
        b.getContentList.asScala.map { entry ⇒
          entry.getKey -> ValueHolder(entry.getVersion,
                                      if (entry.hasRef)
                                        Some(resolveActorRef(entry.getRef))
                                      else None)
        }(breakOut)
      Bucket(addressFromProto(b.getOwner), b.getVersion, content)
    })

  private def resolveActorRef(path: String): ActorRef = {
    system.provider.resolveActorRef(path)
  }

  private def sendToProto(send: Send): dm.Send = {
    dm.Send
      .newBuilder()
      .setPath(send.path)
      .setLocalAffinity(send.localAffinity)
      .setPayload(payloadToProto(send.msg))
      .build()
  }

  private def sendFromBinary(bytes: Array[Byte]): Send =
    sendFromProto(dm.Send.parseFrom(bytes))

  private def sendFromProto(send: dm.Send): Send =
    Send(
        send.getPath, payloadFromProto(send.getPayload), send.getLocalAffinity)

  private def sendToAllToProto(sendToAll: SendToAll): dm.SendToAll = {
    dm.SendToAll
      .newBuilder()
      .setPath(sendToAll.path)
      .setAllButSelf(sendToAll.allButSelf)
      .setPayload(payloadToProto(sendToAll.msg))
      .build()
  }

  private def sendToAllFromBinary(bytes: Array[Byte]): SendToAll =
    sendToAllFromProto(dm.SendToAll.parseFrom(bytes))

  private def sendToAllFromProto(sendToAll: dm.SendToAll): SendToAll =
    SendToAll(sendToAll.getPath,
              payloadFromProto(sendToAll.getPayload),
              sendToAll.getAllButSelf)

  private def publishToProto(publish: Publish): dm.Publish = {
    dm.Publish
      .newBuilder()
      .setTopic(publish.topic)
      .setPayload(payloadToProto(publish.msg))
      .build()
  }

  private def publishFromBinary(bytes: Array[Byte]): Publish =
    publishFromProto(dm.Publish.parseFrom(bytes))

  private def publishFromProto(publish: dm.Publish): Publish =
    Publish(publish.getTopic, payloadFromProto(publish.getPayload))

  private def payloadToProto(msg: Any): dm.Payload = {
    val m = msg.asInstanceOf[AnyRef]
    val msgSerializer = serialization.findSerializerFor(m)
    val builder = dm.Payload
      .newBuilder()
      .setEnclosedMessage(ByteString.copyFrom(msgSerializer.toBinary(m)))
      .setSerializerId(msgSerializer.identifier)

    msgSerializer match {
      case ser2: SerializerWithStringManifest ⇒
        val manifest = ser2.manifest(m)
        if (manifest != "")
          builder.setMessageManifest(ByteString.copyFromUtf8(manifest))
      case _ ⇒
        if (msgSerializer.includeManifest)
          builder.setMessageManifest(
              ByteString.copyFromUtf8(m.getClass.getName))
    }

    builder.build()
  }

  private def payloadFromProto(payload: dm.Payload): AnyRef = {
    val manifest =
      if (payload.hasMessageManifest) payload.getMessageManifest.toStringUtf8
      else ""
    serialization
      .deserialize(payload.getEnclosedMessage.toByteArray,
                   payload.getSerializerId,
                   manifest)
      .get
  }
}
