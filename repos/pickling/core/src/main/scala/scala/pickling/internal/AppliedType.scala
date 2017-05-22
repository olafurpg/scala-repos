package scala.pickling.internal

import scala.pickling.PicklingException

object AppliedType
  // the delimiters in an applied type
  private val delims = List(',', '[', ']')

  /* Parse an applied type. (Helper method)
   *
   * @param  s the string that is parsed
   * @return   a pair with the parsed applied type and the remaining string.
   */
  @deprecated("Will no longer be public", "0.11.0")
  def parse(s: String): (AppliedType, String) =
    // shape of `s`: fqn[at_1, ..., at_n]
    val (typename, rem) = s.span(!delims.contains(_))

    if (rem.isEmpty || rem.startsWith(",") || rem.startsWith("]"))
      (AppliedType(typename, List()), rem)
    else
      // parse type arguments
      var typeArgs = List[AppliedType]()
      var remaining = rem

      while (remaining.startsWith("[") || remaining.startsWith(","))
        remaining = remaining.substring(1)
        val (nextAppliedType, rem) = parse(remaining)
        typeArgs = typeArgs :+ nextAppliedType
        remaining = rem

      (AppliedType(typename, typeArgs),
       if (remaining.startsWith("]")) remaining.substring(1) else remaining)

  /* Parse an applied type.
   *
   * @param  s the string that is parsed
   * @return   The applied type, or None if we found leftovers in the string.
   */
  def parseFull(s: String): Option[AppliedType] =
    val (result, remaining) = parse(s)
    if (remaining.isEmpty) Some(result)
    else None

/**
  * Simple representation of an applied type. Used for reading pickled types.
  */
case class AppliedType(typename: String, typeargs: List[AppliedType])
  override def toString =
    typename + (if (typeargs.isEmpty) "" else typeargs.mkString("[", ",", "]"))
