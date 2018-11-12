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