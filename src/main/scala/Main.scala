package main


import chisel3._
import chisel3.util._

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// this is what the generated code should look like
class ReferenceGcd(width: Int) extends Module {
    val io = IO(new Bundle{
        // start action method IO
        val start = Flipped(Decoupled(new Bundle{
            val a = UInt(width.W)
            val b = UInt(width.W)
        }))
        // result value method IO
        val result = new Bundle {
            val ready = Output(Bool())
            val data = Output(UInt(width.W))
        }
    })

    // helper function
    def implies(a: Bool, b: Bool) : Bool = !a || b

    // state elements
    val x = Reg(UInt(width.W))
    val y = RegInit(0.U(width.W))

    // helper expression
    val is_active = y =/= 0.U

    // rule: swap
    val swap_can_fire = x > y && is_active
    val swap_firing = Wire(Bool())
    assert(implies(swap_firing, swap_can_fire))
    when (swap_firing) {
        x := y
        y := x
    }

    // rule: subtract
    val subtract_can_fire = x <= y && is_active
    val subtract_firing = Wire(Bool())
    assert(implies(subtract_firing, subtract_can_fire))
    when (subtract_firing) {
        y := y - x
    }

    // method: start
    val start_can_fire = !is_active
    val start_firing = Wire(Bool())
    assert(implies(start_firing, start_can_fire))
    when (start_firing) {
        x := io.start.bits.a
        y := io.start.bits.b
    }
    io.start.ready := start_can_fire
    start_firing := io.start.valid

    // method: result
    val result_can_fire = !is_active
    val result_firing = Wire(Bool())
    assert(implies(result_firing, result_can_fire))
    when (result_firing) {
        io.result.data := x
    }
    io.result.ready := result_can_fire
    result_firing := true.B

    // scheduler
    // input:
    val can_will = Seq((swap_can_fire, swap_firing), (subtract_can_fire, subtract_firing))
    // algorithm:
    def priority_scheduler(can_fire: Seq[Bool]): Seq[Bool] = {
        can_fire.scanLeft(false.B)((high, low) => !high && low).drop(1)
    }
    val firing = priority_scheduler(can_will.map(_._1))
    can_will.map(_._2).zip(firing).foreach{case (lhs: Bool, rhs: Bool) => lhs := rhs}
}


object Main {
    def main(args: Array[String]): Unit = {


        val ir = chisel3.Driver.elaborate(() => new ReferenceGcd(4))
        val firrtl = chisel3.Driver.emit(ir)
        println(firrtl)
    }
}
