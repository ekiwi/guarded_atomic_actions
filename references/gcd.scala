class GuardedAtomicActionGcd(width: Int) extends GaaModule {
	val x = Reg("x", UInt(width.W))
	val y = RegInit("y", 0.U(width.W))

	rule ("swap") .when(x.read > y.read && y.read =/= 0.U) {
		x.write(y.read)
		y.write(x.read)
	}

	rule ("subtract") .when(x.read <= y.read && y.read =/= 0.U) {
		y.write(y.read - x.read)
	}

	action ("start") .arg("a", UInt(width.W)) .arg("b", UInt(width.W)) .when(y.read === 0.U) {
		x.write(arg("a"))
		y.write(arg("b"))
	}

	value ("result", UInt(width.W)) .when(y.read === 0.U) {
		x.read
	}

	end(schedule_one_rule_at_a_time)
}
