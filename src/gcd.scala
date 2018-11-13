import gaa.GaaModule
import chisel3._

class GuardedAtomicActionGcd(width: Int) extends GaaModule {
    // state elements
    val x = Reg("x", UInt(width.W))
    val y = RegInit("y", 0.U(width.W))

    // helper expression
    def is_active : Bool = y.read =/= 0.U

    rule ("swap") .when(x.read > y.read && is_active) {
        x.write(y.read)
        y.write(x.read)
    }

    rule ("subtract") .when(x.read <= y.read && is_active) {
        y.write(y.read - x.read)
    }

    action ("start") .arg("a", UInt(width.W)) .arg("b", UInt(width.W)) .when(!is_active) {
        x.write(arg("a"))
        y.write(arg("b"))
    }

    value ("result", UInt(width.W)) .when(!is_active) {
        x.read
    }

    end(schedule_one_rule_at_a_time)
}
