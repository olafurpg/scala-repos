/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.sbt

import java.security.SecureRandom
import com.typesafe.config.{ConfigValue, ConfigOrigin, Config, ConfigFactory}
import sbt._

/**
  * Provides tasks for generating and updating application secrets
  */
object ApplicationSecretGenerator {

  def generateSecret = {
    val random = new SecureRandom()

    (1 to 64)
      .map { _ =>
        (random.nextInt(75) + 48).toChar
      }
      .mkString
      .replaceAll("\\\\+", "/")
  }

  def generateSecretTask = Def.task[String] {
    val secret = generateSecret
    Keys.streams.value.log.info("Generated new secret: " + secret)
    secret
  }

  private val ApplicationSecret =
    """\s*(?:(?:application\.secret)|(?:play\.crypto\.secret))\s*[=:].*""".r

  def updateSecretTask = Def.task[File] {
    val secret: String = play.sbt.PlayImport.PlayKeys.generateSecret.value
    val baseDir: File = Keys.baseDirectory.value
    val log = Keys.streams.value.log

    val appConfFile = Option(System.getProperty("config.file")) match {
      case Some(applicationConf) => new File(baseDir, applicationConf)
      case None =>
        (Keys.resourceDirectory in Compile).value / "application.conf"
    }

    if (appConfFile.exists()) {
      log.info(
        "Updating application secret in " + appConfFile.getCanonicalPath)

      val lines = IO.readLines(appConfFile)
      val config: Config = ConfigFactory.parseString(lines.mkString("\n"))

      val newLines =
        if (config.hasPath("play.crypto.secret")) {
          log.info(
            "Replacing old application secret: " +
              config.getString("play.crypto.secret"))
          getUpdatedSecretLines(secret, lines, config)
        } else {
          log.warn(
            "Did not find application secret in " +
              appConfFile.getCanonicalPath)
          log.warn("Adding application secret to start of file")
          val secretConfig = s"""play.crypto.secret="$secret""""
          secretConfig :: lines
        }

      IO.writeLines(appConfFile, newLines)

      appConfFile
    } else {
      log.error(
        "Could not find configuration file at " +
          appConfFile.getCanonicalPath)
      throw new FeedbackProvidedException {}
    }
  }

  def getUpdatedSecretLines(newSecret: String,
                            lines: List[String],
                            config: Config): List[String] = {

    val secretConfigValue: ConfigValue = config.getValue("play.crypto.secret")
    val secretConfigOrigin: ConfigOrigin = secretConfigValue.origin()

    if (secretConfigOrigin.lineNumber == -1) {
      throw new MessageOnlyException("Could not change play.crypto.secret")
    } else {
      val lineNumber: Int = secretConfigOrigin.lineNumber - 1

      val newLines: List[String] = lines.updated(
        lineNumber,
        lines(lineNumber).replace(
          secretConfigValue.unwrapped().asInstanceOf[String],
          newSecret))

      // removes existing application.secret key
      if (config.hasPath("application.secret")) {
        val applicationSecretValue = config.getValue("application.secret")
        val applicationSecretOrigin = applicationSecretValue.origin()

        if (applicationSecretOrigin.lineNumber == -1) {
          newLines
        } else {
          newLines.patch(applicationSecretOrigin.lineNumber() - 1, Nil, 1)
        }
      } else {
        newLines
      }
    }
  }
}
