import chisel3._
import chisel3.util._

object Main {
    def main(args: Array[String]): Unit = {


        val ir = chisel3.Driver.elaborate(() => new ReferenceGcd(4))
        val firrtl = chisel3.Driver.emit(ir)
        println(firrtl)
    }
}
