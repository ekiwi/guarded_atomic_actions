package main.scala


case class StateElement(name: String) {
    override def toString: String = s"$name"
}



object Main {
    def main(args: Array[String]): Unit = {
        val s = StateElement("test")
        print(s"Hello World: $s")
    }
}
