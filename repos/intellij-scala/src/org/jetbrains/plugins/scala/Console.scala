package org.jetbrains.plugins.scala

/**
  * Pavel.Fatin, 19.05.2010
  */
object Console
  private val Enabled = false

  def print(x: Any)
    if (Enabled) Predef.print(x)

  def println()
    if (Enabled) Predef.println()

  def println(x: Any)
    if (Enabled) Predef.println(x)

  def printf(text: String, xs: Any*)
    if (Enabled) Predef.printf(text, xs: _*)
