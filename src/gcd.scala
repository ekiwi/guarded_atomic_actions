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


    // time for 255*255 = 65025 tests
       end(schedule_one_rule_at_a_time)                                    // 1708470 cycles
    // end(schedule_all_rules())                                           // 1399330 cycles
    // end(schedule_all_rules(Seq("start", "swap", "subtract", "result"))) // 1334560 cycles
}
