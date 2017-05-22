package mesosphere.marathon

import java.io.{IOException, FileInputStream}

import com.google.protobuf.ByteString
import mesosphere.chaos.http.HttpConf
import org.apache.mesos.Protos.{Credential, FrameworkInfo, FrameworkID}
import org.apache.mesos.{MesosSchedulerDriver, SchedulerDriver}
import org.slf4j.LoggerFactory

object MarathonSchedulerDriver
  private[this] val log = LoggerFactory.getLogger(getClass)

  //TODO: fix style issue and enable this scalastyle check
  //scalastyle:off method.length
  def newDriver(config: MarathonConf,
                httpConfig: HttpConf,
                newScheduler: MarathonScheduler,
                frameworkId: Option[FrameworkID]): SchedulerDriver =

    log.info(s"Create new Scheduler Driver with frameworkId: $frameworkId")

    val frameworkInfoBuilder = FrameworkInfo
      .newBuilder()
      .setName(config.frameworkName())
      .setFailoverTimeout(config.mesosFailoverTimeout().toDouble)
      .setUser(config.mesosUser())
      .setCheckpoint(config.checkpoint())
      .setHostname(config.hostname())

    // Set the role, if provided.
    config.mesosRole.get.foreach(frameworkInfoBuilder.setRole)

    // Set the ID, if provided
    frameworkId.foreach(frameworkInfoBuilder.setId)

    if (config.webuiUrl.isSupplied)
      frameworkInfoBuilder.setWebuiUrl(config.webuiUrl())
    else if (httpConfig.sslKeystorePath.isDefined)
      // ssl enabled, use https
      frameworkInfoBuilder.setWebuiUrl(
          s"https://${config.hostname()}:${httpConfig.httpsPort()}")
    else
      // ssl disabled, use http
      frameworkInfoBuilder.setWebuiUrl(
          s"http://${config.hostname()}:${httpConfig.httpPort()}")

    // set the authentication principal, if provided
    config.mesosAuthenticationPrincipal.get
      .foreach(frameworkInfoBuilder.setPrincipal)

    val credential: Option[Credential] =
      config.mesosAuthenticationPrincipal.get.map  principal =>
        val credentialBuilder = Credential.newBuilder().setPrincipal(principal)

        config.mesosAuthenticationSecretFile.get.foreach  secretFile =>
          try
            val secretBytes =
              ByteString.readFrom(new FileInputStream(secretFile))
            credentialBuilder.setSecret(secretBytes.toStringUtf8)
          catch
            case cause: Throwable =>
              throw new IOException(
                  s"Error reading authentication secret from file [$secretFile]",
                  cause)

        credentialBuilder.build()

    val frameworkInfo = frameworkInfoBuilder.build()

    log.debug("Start creating new driver")

    val implicitAcknowledgements = false
    val newDriver: MesosSchedulerDriver = credential match
      case Some(cred) =>
        new MesosSchedulerDriver(newScheduler,
                                 frameworkInfo,
                                 config.mesosMaster(),
                                 implicitAcknowledgements,
                                 cred)

      case None =>
        new MesosSchedulerDriver(newScheduler,
                                 frameworkInfo,
                                 config.mesosMaster(),
                                 implicitAcknowledgements)

    log.debug("Finished creating new driver", newDriver)

    newDriver
