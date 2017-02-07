/*
 * Copyright 2011-2015 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._
import Keys._
import net.liftweb.sbt.LiftBuildPlugin.{crossMapped, defaultOrMapped}

object Dependencies {

  type ModuleMap = String => ModuleID

  lazy val CVMappingAll = crossMapped("2.11.7" -> "2.11")

  lazy val slf4jVersion = "1.7.2"

  // Compile scope:
  // Scope available in all classpath, transitive by default.
  lazy val commons_codec = "commons-codec" % "commons-codec" % "1.10"
  lazy val commons_fileupload =
    "commons-fileupload" % "commons-fileupload" % "1.3.1"
  lazy val commons_httpclient =
    "commons-httpclient" % "commons-httpclient" % "3.1"
  lazy val javamail = "javax.mail" % "mail" % "1.4.7"
  lazy val joda_time = "joda-time" % "joda-time" % "2.9.2"
  lazy val joda_convert = "org.joda" % "joda-convert" % "1.8.1"
  lazy val htmlparser = "nu.validator.htmlparser" % "htmlparser" % "1.4"
  lazy val mongo_java_driver = "org.mongodb" % "mongo-java-driver" % "2.14.0"
  lazy val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.8"
  lazy val scalajpa =
    ("org.scala-libs" % "scalajpa" % "1.5").cross(CVMappingAll)
  lazy val scalap: ModuleMap = "org.scala-lang" % "scalap" % _
  lazy val scala_compiler: ModuleMap = "org.scala-lang" % "scala-compiler" % _
  lazy val scalaz7_core =
    ("org.scalaz" % "scalaz-core" % "7.2.0").cross(CVMappingAll)
  lazy val squeryl =
    ("org.squeryl" % "squeryl" % "0.9.5-7").cross(CVMappingAll)
  lazy val slf4j_api = "org.slf4j" % "slf4j-api" % slf4jVersion
  lazy val scala_xml = "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
  lazy val rhino = "org.mozilla" % "rhino" % "1.7.7.1"
  lazy val scala_parser =
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  lazy val xerces = "xerces" % "xercesImpl" % "2.11.0"

  // Aliases
  lazy val mongo_driver = mongo_java_driver
  lazy val scalaz7 = scalaz7_core

  // Provided scope:
  // Scope provided by container, available only in compile and test classpath, non-transitive by default.
  lazy val logback =
    "ch.qos.logback" % "logback-classic" % "1.1.5" % "provided"
  lazy val log4j = "log4j" % "log4j" % "1.2.17" % "provided"
  lazy val slf4j_log4j12 =
    "org.slf4j" % "slf4j-log4j12" % slf4jVersion % "provided"
  lazy val persistence_api =
    "javax.persistence" % "persistence-api" % "1.0.2" % "provided"
  lazy val servlet_api =
    "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided"

  // Runtime scope:
  // Scope provided in runtime, available only in runtime and test classpath, not compile classpath, non-transitive by default.
  lazy val derby =
    "org.apache.derby" % "derby" % "10.7.1.1" % "test" //% "optional"
  lazy val h2database =
    "com.h2database" % "h2" % "1.2.147" % "test" //% "optional"

  // Aliases
  lazy val h2 = h2database

  // Test scope:
  // Scope available only in test classpath, non-transitive by default.
  // TODO: See if something alternative with lesser footprint can be used instead of mega heavy apacheds
  lazy val apacheds =
    "org.apache.directory.server" % "apacheds-server-integ" % "1.5.5" % "test" // TODO: 1.5.7
  lazy val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.26" % "test"
  lazy val jwebunit =
    "net.sourceforge.jwebunit" % "jwebunit-htmlunit-plugin" % "2.5" % "test"
  lazy val mockito_all = "org.mockito" % "mockito-all" % "1.9.0" % "test"
  lazy val scalacheck = "org.specs2" %% "specs2-scalacheck" % "3.7" % "test"
  lazy val specs2 = "org.specs2" %% "specs2" % "3.7" % "test"
  lazy val scalatest = "org.scalatest" %% "scalatest" % "2.1.3" % "test"
  lazy val junit = "junit" % "junit" % "4.8.2" % "test"

  lazy val jquery = "org.webjars.bower" % "jquery" % "1.11.3" % "provided"
  lazy val jasmineCore =
    "org.webjars.bower" % "jasmine-core" % "2.4.1" % "provided"
  lazy val jasmineAjax =
    "org.webjars.bower" % "jasmine-ajax" % "3.2.0" % "provided"
}
