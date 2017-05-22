/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.core.server.common

import java.net.InetAddress.getByName
import org.specs2.mutable.Specification

import ForwardedHeaderHandler.{ForwardedHeaderVersion, Rfc7239, Xforwarded}
import NodeIdentifierParser._

class NodeIdentifierParserSpec extends Specification

  def parseNode(version: ForwardedHeaderVersion, str: String) =
    val parser = new NodeIdentifierParser(version)
    parser.parseNode(str)

  "NodeIdentifierParser" should

    "parse an ip v6 address with port" in
      parseNode(Rfc7239, "[8F:F3B::FF]:9000") must beRight(
          Ip(getByName("8F:F3B::FF")) -> Some(PortNumber(9000)))

    "not parse unescaped ip v6 address in rfc7239 header" in
      parseNode(Rfc7239, "8F:F3B::FF") must beLeft

    "parse unescaped ip v6 address in x-forwarded-for header" in
      parseNode(Xforwarded, "8F:F3B::FF") must beRight(
          Ip(getByName("8F:F3B::FF")) -> None)

    "parse an ip v6 address with obfuscated port" in
      parseNode(Rfc7239, "[::FF]:_obf") must beRight(
          Ip(getByName("::FF")) -> Some(ObfuscatedPort("_obf")))

    "parse an ip v4 address with port" in
      parseNode(Rfc7239, "127.0.0.1:8080") must beRight(
          Ip(getByName("127.0.0.1")) -> Some(PortNumber(8080)))

    "parse an ip v4 address without port" in
      parseNode(Rfc7239, "192.168.0.1") must beRight(
          Ip(getByName("192.168.0.1")) -> None)

    "parse an unknown ip address without port" in
      parseNode(Rfc7239, "unknown") must beRight(UnknownIp -> None)

    "parse an obfuscated ip address without port" in
      parseNode(Rfc7239, "_harry") must beRight(ObfuscatedIp("_harry") -> None)
