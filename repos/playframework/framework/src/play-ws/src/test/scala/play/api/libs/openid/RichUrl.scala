/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.openid

trait RichUrl[A]
  def hostAndPath: String
