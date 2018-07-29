// See README.md for license details.

package sgemm

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import scala.util.Random

class SgemmUnitTester(c: Sgemm) extends PeekPokeTester(c) {
	val m = BLOCK_SIZE.M
	val n = BLOCK_SIZE.N
	val l = BLOCK_SIZE.L
	val rand = new Random(1)
	val matA = Array.fill(m*l){rand.nextInt(16)}
	val matB = Array.fill(l*n){rand.nextInt(16)}
	val matC = Array.fill(m*n){0}
	var sum = 0
	for(i <- 0 until m) {
		for(j <- 0 until n) {
			sum = 0
			for(k <- 0 until l) {
				sum = sum + matA(i*l+k) * matB(k*n+j)
			}
			matC(i*n+j) = sum
		}
	}

	for (i <- 0 until m*l) {
		poke(c.io.matA(i), matA(i))
	}
	for (i <- 0 until l*n) {
		poke(c.io.matB(i), matB(i))
	}
	poke(c.io.load, 1)
	step(1)
	expect(c.io.valid, 1)
	for (i <- 0 until m*n) {
		expect(c.io.matC(i), matC(i))
	}
}

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly example.test.SgemmTester
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly example.test.SgemmTester'
  * }}}
  */
class SgemmTester extends ChiselFlatSpec {
  // Disable this until we fix isCommandAvailable to swallow stderr along with stdout
  private val backendNames = if(false && firrtl.FileUtils.isCommandAvailable(Seq("verilator", "--version"))) {
    Array("firrtl", "verilator")
  }
  else {
    Array("firrtl")
  }
  for ( backendName <- backendNames ) {
    "Sgemm" should s"calculate proper greatest common denominator (with $backendName)" in {
      Driver(() => new Sgemm, backendName) {
        c => new SgemmUnitTester(c)
      } should be (true)
    }
  }

  "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
    iotesters.Driver.execute(Array(), () => new Sgemm) {
      c => new SgemmUnitTester(c)
    } should be (true)
  }

  "using --backend-name verilator" should "be an alternative way to run using verilator" in {
    if(backendNames.contains("verilator")) {
      iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new Sgemm) {
        c => new SgemmUnitTester(c)
      } should be(true)
    }
  }

  "running with --is-verbose" should "show more about what's going on in your tester" in {
    iotesters.Driver.execute(Array("--is-verbose"), () => new Sgemm) {
      c => new SgemmUnitTester(c)
    } should be(true)
  }

  "running with --fint-write-vcd" should "create a vcd file from your test" in {
    iotesters.Driver.execute(Array("--fint-write-vcd"), () => new Sgemm) {
      c => new SgemmUnitTester(c)
    } should be(true)
  }
}
