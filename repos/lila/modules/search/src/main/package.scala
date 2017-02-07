package lila

package object search extends PackageObject with WithPlay {

  object Date {
    import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
    val format = "YYYY-MM-dd HH:mm:ss"
    val formatter: DateTimeFormatter = DateTimeFormat.forPattern(format)
  }
}
