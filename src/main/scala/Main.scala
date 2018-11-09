package main


import chisel3._

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


class Gcd extends IGcd {
    val x = RegInit(0.U(32.W))
    val y = RegInit(0.U(32.W))


    //override def start(a: UInt, b: UInt): Unit =
    //    guard(y === 0.U, { x := a; y := b })

    //override def result(): UInt =
    //    guard(y === 0.U, { x })

    // TODO: make a Module(...) like wrapper that evaluates `io`
    //io
}


object Main {
    def main(args: Array[String]): Unit = {


        val ir = chisel3.Driver.elaborate(() => new Gcd)
        val firrtl = chisel3.Driver.emit(ir)
        println(firrtl)
    }
}
