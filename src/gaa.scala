package gaa

import chisel3._
import chisel3.util._
import scala.collection.immutable.ListMap


abstract class RuleBase(parent: GaaModule, _name: String) {
    val name = _name
    val firing = Wire(Bool()).suggestName(s"${name}_firing")
    val can_fire = Wire(Bool()).suggestName(s"${name}_can_fire")


}

class Rule(parent: GaaModule, _name: String) extends RuleBase(parent, _name) {
    def when(cond: => Bool)(block: => Unit) = {
        parent.active_rule = Some(this)
        can_fire := cond
        chisel3.when(firing)(block)
        parent.active_rule = None
    }
}

class Value(parent: GaaModule, _name: String, typ: Data) extends RuleBase(parent, _name) {
    val valid = Wire(Bool()).suggestName(s"${name}_valid")
    val bits = Wire(typ).suggestName(s"${name}_bits")

    // TODO: make more type safe by concretizing Data
    def when(cond: => Bool)(block: => Data) = {
        parent.active_rule = Some(this)
        can_fire := cond
        chisel3.when(firing){bits := block}
        valid := can_fire
        firing := true.B && can_fire
        parent.active_rule = None
    }
}

class Action(parent: GaaModule, _name: String) extends RuleBase(parent, _name) {
    var args : ListMap[String, Data] = ListMap.empty[String, Data]

    def arg(name: String, typ: Data) = {
        assert(args.get(name).isEmpty)
        val wire = Wire(typ).suggestName(s"${this.name}_${name}")
        args = args ++ ListMap(name -> wire)
        this
    }

    def when(cond: => Bool)(block: => Unit) = {
        parent.active_rule = Some(this)
        can_fire := cond
        chisel3.when(firing)(block)
        parent.active_rule = None
    }
}

class GaaModule extends Module {

    private[gaa] var active_rule : Option[RuleBase] = None


    // TODO: generate correct ios
    val io = IO(new Bundle{val in = Input(Bool())})

    def rule(name: String)= {
        new Rule(this, name)
    }

    def action(name: String)= {
        new Action(this, name)
    }

    def value(name: String, typ: Data)= {
        new Value(this, name, typ)
    }

    def arg(name: String) : Data = {
        active_rule.get match {
            case r: Action => r.args(name)
        }
    }

}