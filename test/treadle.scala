// based on:
// https://github.com/freechipsproject/treadle#use-the-tester-metaphor

import chisel3._
import treadle.TreadleTester
import org.scalatest.{Matchers, FlatSpec}


class TreadleUsageSpec extends FlatSpec with Matchers {

    "GCD" should "return correct values for a range of inputs" in {
        val s = Driver.emit(() => new GuardedAtomicActionGcd(8))

        val tester = new TreadleTester(s)

        tester.reset(1)
        tester.poke("io_start_valid", 0)
        tester.step()

        for {
            i <- 1 to 255
            j <- 1 to 255
        } {
            val (gcd_value, _) = GCDCalculator.computeGcdResultsAndCycles(i, j)

            //println(s"GCD($i, $j) -> $gcd_value")

            tester.poke("io_start_bits_a", i)
            tester.poke("io_start_bits_b", j)
            tester.poke("io_start_valid", 1)
            tester.step()
            tester.poke("io_start_valid", 0)

            var cycles = 0
            while (tester.peek("io_result_valid") != BigInt(1)) {
                tester.step()
                cycles += 1
            }
            tester.expect("io_result_valid", 1)
            tester.expect("io_result_bits", BigInt(gcd_value))
            tester.step()
        }
        tester.report()
    }
}
