package sgemm

import chisel3._

object BLOCK_SIZE {
	val M = 8
	val N = 8
	val L = 8
}

class Mac(val w: Int = 32) extends Module {
	val io = IO(new Bundle {
		val a = Input(SInt(w.W))
		val b = Input(SInt(w.W))
		val c = Input(SInt(w.W))
		val out = Output(SInt(w.W))
	})
	io.out := io.a * io.b + io.c
}

class Reduction(val w: Int = 32, val k: Int = BLOCK_SIZE.L) extends Module {
	val io = IO(new Bundle {
		val row = Input(Vec(k, SInt(w.W)))
		val col = Input(Vec(k, SInt(w.W)))
		val res = Output(SInt(w.W))
	})
	val res = Wire(Vec(k, SInt(w.W)))
	res(0) := io.row(0) * io.col(0)
	for(i <- 1 until k) {
		val mac = Module(new Mac(w))
		res(i) := mac.io.out
		mac.io.a := io.row(i)
		mac.io.b := io.col(i)
		mac.io.c := res(i-1)
	}
	io.res := res(k-1)
}

class Sgemm(val w: Int = 32, val m: Int = BLOCK_SIZE.M, val n: Int = BLOCK_SIZE.N, val l: Int = BLOCK_SIZE.L) extends Module {
	val io = IO(new Bundle {
		val load  = Input(Bool())
		val matA  = Input(Vec(m*l, SInt(w.W)))
		val matB  = Input(Vec(l*n, SInt(w.W)))
		val matC  = Output(Vec(m*n, SInt(w.W)))
		val valid = Output(Bool())
	})
	when (io.load) {
		for(i <- 0 until m) {
			for(j <- 0 until n) {
				val red = Module(new Reduction(w,l))
				for(k <- 0 until l) {
					red.io.row(k) := io.matA(i*l+k)
					red.io.col(k) := io.matB(k*n+j)
				}
				io.matC(i*n+j) := red.io.res
			}
		}
		io.valid := true.B
	} .otherwise {
		for(i <- 0 until m*n) {
			io.matC(i) := 0.S
		}
		io.valid := false.B
	}
}
