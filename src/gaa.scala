import chisel3._
import chisel3.util._


class RuleBuilder(_name: String) {
    val name = _name

    def arg(name: String, typ: Data) = {
        this
    }

    def when(cond: => Bool)(block: => Unit) = {

    }
}

class GaaModule extends Module {
//    def toModule(): Module = {
//        new Module { val io = IO(new Bundle{val in = Input(Bool())}) }
//    }

    // TODO: generate correct ios
    val io = IO(new Bundle{val in = Input(Bool())})

    def rule(name: String)= {
        new RuleBuilder(name)
    }

    def action(name: String)= {
        new RuleBuilder(name)
    }

    def value(name: String)= {
        new RuleBuilder(name)
    }

    def arg(name: String) : Data = {
        0.U
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

    value ("result") .when(!is_active) {
        x
    }
}
