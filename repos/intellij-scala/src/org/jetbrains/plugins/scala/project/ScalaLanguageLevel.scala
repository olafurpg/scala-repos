package org.jetbrains.plugins.scala
package project

/**
  * @author Pavel Fatin
  */
sealed case class ScalaLanguageLevel(ordinal: Int, version: String)
    extends Named {
  def name: String = "Scala " + version

  override def getName = name

  private[project] def proxy: ScalaLanguageLevelProxy =
    ScalaLanguageLevel.LevelToProxy(this)

  def >(level: ScalaLanguageLevel): Boolean = ordinal > level.ordinal

  def >=(level: ScalaLanguageLevel): Boolean = ordinal >= level.ordinal

  def <(level: ScalaLanguageLevel): Boolean = ordinal < level.ordinal

  def <=(level: ScalaLanguageLevel): Boolean = ordinal <= level.ordinal

  def isDotty: Boolean = name.startsWith("Dotty")
}

object ScalaLanguageLevel {
  val Values = Array(Scala_2_7,
                     Scala_2_8,
                     Scala_2_9,
                     Scala_2_10,
                     Scala_2_11,
                     Scala_2_12,
                     Dotty)

  val Default = Scala_2_11

  // We have to rely on the Java's enumeration for library property serialization
  private val LevelToProxy = Map(
      (null, null),
      Scala_2_7 -> ScalaLanguageLevelProxy.Scala_2_7,
      Scala_2_8 -> ScalaLanguageLevelProxy.Scala_2_8,
      Scala_2_9 -> ScalaLanguageLevelProxy.Scala_2_9,
      Scala_2_10 -> ScalaLanguageLevelProxy.Scala_2_10,
      Scala_2_11 -> ScalaLanguageLevelProxy.Scala_2_11,
      Scala_2_12 -> ScalaLanguageLevelProxy.Scala_2_12,
      Dotty -> ScalaLanguageLevelProxy.Dotty)

  private val ProxyToLevel = LevelToProxy.map(_.swap)

  def from(proxy: ScalaLanguageLevelProxy): ScalaLanguageLevel =
    ProxyToLevel(proxy)

  def from(version: Version): Option[ScalaLanguageLevel] =
    ScalaLanguageLevel.Values.find(it => version.number.startsWith(it.version))

  object Scala_2_7 extends ScalaLanguageLevel(0, "2.7")

  object Scala_2_8 extends ScalaLanguageLevel(1, "2.8")

  object Scala_2_9 extends ScalaLanguageLevel(2, "2.9")

  object Scala_2_10 extends ScalaLanguageLevel(3, "2.10")

  object Scala_2_11 extends ScalaLanguageLevel(4, "2.11")

  object Scala_2_12 extends ScalaLanguageLevel(5, "2.12")

  object Dotty extends ScalaLanguageLevel(5, "0.1-SNAPSHOT") {
    override def name: String = "Dotty " + version
  }
}
