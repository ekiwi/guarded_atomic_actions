// based on:
// https://github.com/freechipsproject/chisel-testers/blob/master/src/test/scala/examples/GCDSpec.scala

import org.scalatest._
import chisel3._
import chisel3.iotesters._
import java.io.File

object GCDCalculator {
    def computeGcdResultsAndCycles(a: Int, b: Int, depth: Int = 1): (Int, Int) = {
        if(b == 0) { (a, depth) } else { computeGcdResultsAndCycles(b, a%b, depth+1 ) }
    }
}

class GCDPeekPokeTester(c: ReferenceGcd) extends PeekPokeTester(c)  {
    // reset GCD
    reset()
    poke(c.io.start.valid, 0)
    step(1)

    for {
        i <- 1 to 10
        j <- 1 to 10
    } {

        val (gcd_value, _) = GCDCalculator.computeGcdResultsAndCycles(i, j)

        poke(c.io.start.bits.a, i)
        poke(c.io.start.bits.b, j)
        poke(c.io.start.valid, 1)
        step(1)
        poke(c.io.start.valid, 0)

        var count = 0
        while(peek(c.io.result.valid) == BigInt(0) && count <= 30) {
            step(1)
            count += 1
        }
        if(count > 30) {
            // println(s"Waited $count cycles on gcd inputs $i, $j, giving up")
            System.exit(0)
        }
        expect(c.io.result.valid, 1)
        expect(c.io.result.bits, gcd_value)
        step(1)
    }
}

class GCDSpec extends FlatSpec with Matchers {
    behavior of "GCDSpec"

    val width = 16

   it should "compute gcd excellently" in {
       val manager = new TesterOptionsManager {
           testerOptions = testerOptions.copy(backendName = "firrtl", testerSeed = 7L)
           interpreterOptions = interpreterOptions.copy(setVerbose = false, writeVCD = true, showFirrtlAtLoad = true)
           commonOptions = commonOptions.copy(targetDirName="test_run_dir", topName = "gcd")
       }
        iotesters.Driver.execute(() => new ReferenceGcd(width), manager) { c =>
            new GCDPeekPokeTester(c)
        } should be(true)
    }
}


