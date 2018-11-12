import gaa.GaaModule
import chisel3._

class GuardedAtomicActionGcd(width: Int) extends GaaModule {
    // state elements

    // TODO: write a macro
    private val _x_reg = Reg(UInt(width.W))
    private val _x_index = register_state("x", _x_reg)
    def x = state(_x_index, _x_reg)

    private val _y_reg = RegInit(0.U(width.W))
    private val _y_index = register_state("y", _y_reg)
    def y = state(_y_index, _y_reg)


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
