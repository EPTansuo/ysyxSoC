package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val io = IO(Flipped(new SPIIO(1)))
  io.miso := true.B

  // ss 低电平片选, 没有被片选中时就复位
  val bitregs = withClockAndReset(io.sck.asClock, !io.ss.asBool) { RegInit(0.U(8.W)) }
  val counter = withClockAndReset(io.sck.asClock, !io.ss.asBool) { RegInit(0.U(3.W)) }
  val sending = withClockAndReset(io.sck.asClock, !io.ss.asBool) { RegInit(0.U(1.W))}

  counter := counter + 1.U
  bitregs := Cat(bitregs(6,0), io.mosi)

  when(counter === 7.U) {
    sending := ~sending
  }
  
  when(sending === 1.U) {
    io.miso := bitregs(7)
  }
}
