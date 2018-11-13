package gaa

import chisel3._
import chisel3.util._

import scala.collection.immutable.ListMap

object ListBundle {
    def apply(el: ListMap[String, Data]): Record = {
        new Record {
            val elements: ListMap[String, Data] = el
            override def cloneType : this.type = { ListBundle(elements).asInstanceOf[this.type] }
        }
    }
}

class State[+D<:Data](_name: String, _reg: D, index: Int, parent: GaaModule) {
    private[gaa] val name = _name
    private[gaa] val reg = _reg
    def read : D = parent.active_rule.get.state_in(index).asInstanceOf[D]
    def write[W<:Data](value: W) : Unit = { parent.active_rule.get.state_out(index) := value }
}

abstract class RuleBase(parent: GaaModule, _name: String) {
    val name = _name
    val firing = Wire(Bool()).suggestName(s"${name}_firing")
    val can_fire = Wire(Bool()).suggestName(s"${name}_can_fire")

    def make_wires(prefix: String) = parent._state.map{
        case s : State[Data] => Wire(s.reg.cloneType).suggestName(s"${name}_${prefix}${s.name}")
    }
    val state_in = make_wires("in_")
    val state_out = make_wires("out_")

    def implies(a: Bool, b: Bool) : Bool = !a || b
    assert(implies(firing, can_fire))

    protected def start(cond: => Bool) = {
        state_out.zip(state_in).foreach { case (out, in) => out := in }
        parent.active_rule = Some(this)
        can_fire := cond
    }

    protected def end = {
        parent.active_rule = None
    }
}

class Rule(parent: GaaModule, _name: String) extends RuleBase(parent, _name) {
    def when(cond: => Bool)(block: => Unit) = {
        start(cond)
        chisel3.when(firing)(block)
        end
    }
}

class Value(parent: GaaModule, _name: String, typ: Data) extends RuleBase(parent, _name) {
    val valid = Wire(Bool()).suggestName(s"${name}_valid")
    val bits = Wire(typ).suggestName(s"${name}_bits")

    bits := DontCare
    valid := can_fire
    firing := true.B && can_fire

    // TODO: make more type safe by concretizing Data
    def when(cond: => Bool)(block: => Data) = {
        start(cond)
        chisel3.when(firing){
            bits := block
        }
        end
    }

    def make_io = {
        Valid(typ)
    }

    def connect_io(global_io: Record) = {
        val io = global_io.elements(this.name).asInstanceOf[chisel3.util.Valid[Data]]
        io.valid := valid
        io.bits := bits
    }
}

class Action(parent: GaaModule, _name: String) extends RuleBase(parent, _name) {
    val valid = Wire(Bool()).suggestName(s"${name}_valid")
    val ready = Wire(Bool()).suggestName(s"${name}_ready")
    var args : ListMap[String, Data] = ListMap.empty[String, Data]

    ready := can_fire
    firing := valid && can_fire

    def arg(name: String, typ: Data) = {
        assert(args.get(name).isEmpty)
        val wire = Wire(typ).suggestName(s"${this.name}_${name}")
        args = args ++ ListMap(name -> wire)
        this
    }

    def when(cond: => Bool)(block: => Unit) = {
        start(cond)
        chisel3.when(firing)(block)
        end
    }

    def make_io = {
        val elements = args.map{
            case (name, wire) => (name, wire.cloneType)
        }
        Flipped(Decoupled(ListBundle(elements)))
    }

    def connect_io(global_io: Record) = {
        val io = global_io.elements(this.name).asInstanceOf[chisel3.util.DecoupledIO[Record]]
        io.ready := ready
        valid := io.valid
        args.foreach {
            case (name, wire) => wire := io.bits.elements(name)
        }
    }
}

class GaaModule extends Module {

    private[gaa] var active_rule : Option[RuleBase] = None
    private var rules : List[RuleBase] = List.empty[RuleBase]

    private def add_rule[R <: RuleBase](r: R) = {
        rules = rules ++ List(r)
        r
    }

    private def find_rule(name: String): RuleBase = {
        rules.filter(r => r.name == name).head
    }

    private[gaa] var _state : Array[State[Data]] = Array.empty[State[Data]]

    def Reg[D<:Data](name: String, typ: D) = {
        add_state(name, chisel3.Reg(typ))
    }

    def RegInit[D<:Data](name: String, value: D) = {
        add_state(name, chisel3.RegInit(value))
    }

    private def add_state[D<:Data](name: String, reg: D) = {
        reg.suggestName(name)
        val index = _state.length
        val new_state = new State(name, reg, index, this)
        _state = _state ++ Array(new_state)
        new_state
    }


    def rule(name: String)= {
        add_rule(new Rule(this, name))
    }

    def action(name: String)= {
        add_rule(new Action(this, name))
    }

    def value(name: String, typ: Data)= {
        add_rule(new Value(this, name, typ))
    }

    def arg(name: String) : Data = {
        active_rule.get match {
            case r: Action => r.args(name)
        }
    }

    private def modifies_state(rule: RuleBase): Boolean = rule match {
        case _: Value => false
        case _ => true
    }

    private def not_a_method(rule: RuleBase): Boolean = rule match {
        case _: Rule => true
        case _ => false
    }

    def schedule_one_rule_at_a_time(): Unit = {
        // connect rule inputs and outputs
        for (rule <- rules) {
            rule.state_in.zip(_state).foreach { case (in, st) => in := st.reg }
            if (modifies_state(rule)) {
                chisel3.when(rule.firing) {
                    rule.state_out.zip(_state).foreach { case (out, st) => st.reg := out }
                }
            }
        }

        // schedule one rule at a time
        val can_will = rules.flatMap{
            case r : Rule => Some((r.can_fire, r.firing))
            case _ => None
        }
        def priority_scheduler(can_fire: Seq[Bool]): Seq[Bool] = {
            can_fire.scanLeft(false.B)((high, low) => !high && low).drop(1)
        }
        val firing = priority_scheduler(can_will.map(_._1))
        can_will.map(_._2).zip(firing).foreach{case (lhs: Bool, rhs: Bool) => lhs := rhs}
    }

    def schedule_all_rules(order: Seq[String] = Seq.empty[String])(): Unit = {
        val rr = if(order.isEmpty) { this.rules } else {
            assert(order.length == _state.length)
            order.map(find_rule)
        }

        // chain rules
        var state : Seq[Data] = _state.map(_.reg)
        for(rule <- rr) {
            rule.state_in.zip(state).foreach { case (in, st) => in := st }
            state = rule.state_out
        }
        _state.map(_.reg).zip(state).foreach { case (in, st) => in := st }

        // all rules fire when they can
        for(rule <- rr.filter(not_a_method)) {
            rule.firing := rule.can_fire
        }

    }

    private def make_io : chisel3.Record = {
        val elements = ListMap(rules.flatMap{
            case rule: Action => Some((rule.name, rule.make_io))
            case rule: Value  => Some((rule.name, rule.make_io))
            case _ => None
        } : _*)
        ListBundle(elements)
    }

    lazy val io = IO(make_io)

    private def eval_io = { io }

    // IOs can only be connected once they have been wrapped in IO(..)
    private def connect_io = {
        rules.foreach{
            case rule: Action => rule.connect_io(io)
            case rule: Value  => rule.connect_io(io)
            case _ => None
        }
    }

    def end(scheduler: () => Unit) = {
        scheduler()
        eval_io
        connect_io
    }

}