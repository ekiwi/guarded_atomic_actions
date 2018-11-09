package main


import chisel3._

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// this is what the generated code should look like
class ReferenceGcd(width: Int) extends Module {
    val io = IO(new Bundle{

    })


    val x = Reg(UInt(width.W))
    val y = RegInit(0.U(width.W))


    //override def start(a: UInt, b: UInt): Unit =
    //    guard(y === 0.U, { x := a; y := b })

    //override def result(): UInt =
    //    guard(y === 0.U, { x })

    // TODO: make a Module(...) like wrapper that evaluates `io`
    //io
}


object Main {
    def main(args: Array[String]): Unit = {


        val ir = chisel3.Driver.elaborate(() => new ReferenceGcd(4))
        val firrtl = chisel3.Driver.emit(ir)
        println(firrtl)
    }
}
