import gaa.GaaModule
import chisel3._

class GuardedAtomicActionGcd(width: Int) extends GaaModule {
    // state elements
    val x = Reg(UInt(width.W))
    val y = RegInit(0.U(width.W))

    // helper expression
    val is_active : Bool = y =/= 0.U

    rule ("swap") .when(x > y && is_active) {
        x := y
        y := x
    }

    rule ("subtract") .when(x <= y && is_active) {
        y := y - x
    }

    action ("start") .arg("a", UInt(width.W)) .arg("b", UInt(width.W)) .when(!is_active) {
        x := arg("a")
        y := arg("b")
    }

    value ("result", UInt(width.W)) .when(!is_active) {
        x
    }

    end
}
