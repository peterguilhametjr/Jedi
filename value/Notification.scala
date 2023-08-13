package value

class Notification(value: String) extends Value:
  override def toString: String = this.value

object Notification:
  val OK = new Notification("ok")
  val DONE = new Notification("done")
  val UNSPECIFIED = new Notification("unspecified")
