import chisel3._
import chisel3.util._

object Main {
    def main(args: Array[String]): Unit = {

        // print firrtl to command line
        val ir = chisel3.Driver.elaborate(() => new ReferenceGcd(4))
        val firrtl = chisel3.Driver.emit(ir)
        println(firrtl)

        // generate verilog and save to file
        chisel3.Driver.execute(args, () => new ReferenceGcd(4))
    }
}
