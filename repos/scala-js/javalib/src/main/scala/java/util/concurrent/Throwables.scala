package java.util.concurrent

class ExecutionException(message: String, cause: Throwable)
    extends Exception(message, cause)

  protected def this() = this(null, null)
  protected def this(message: String) = this(message, null)
  def this(cause: Throwable) = this(null, cause)

class CancellationException(message: String)
    extends IllegalStateException(message)

  def this() = this(null)

class TimeoutException(message: String) extends Exception(message)
  def this() = this(null)
