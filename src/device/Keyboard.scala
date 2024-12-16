package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2 = new PS2IO
}

class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

class ps2Chisel extends Module {
  val io = IO(new PS2CtrlIO)
  val cnt = RegInit(0.U(4.W))
  val data = RegInit(0.U(9.W))
  val ps2_clk_reg = RegInit(io.ps2.clk)
  val fifo = Module(new Queue(UInt(8.W), 8))

  ps2_clk_reg := io.ps2.clk
  val next_bit = ps2_clk_reg && (!io.ps2.clk)

  when(cnt === 10.U && next_bit){
    cnt := 0.U
  }.elsewhen(next_bit){
    cnt := cnt + 1.U
  }


  when(cnt =/= 0.U && cnt =/= 10.U && next_bit){
    data := Cat(io.ps2.data, data(8,1))
  }

  fifo.io.enq.valid := (cnt === 10.U) && next_bit
  fifo.io.enq.bits := data(7,0)


  // APB
  val apb_idle :: apb_setup :: apb_enable :: Nil = Enum(3)

  val psel = io.in.psel
  val penable = io.in.penable
  val state_apb = RegInit(apb_idle)
  state_apb := MuxLookup(state_apb, apb_idle)(Seq(
    (apb_idle   -> Mux(psel   , apb_setup ,  apb_idle )),
    (apb_setup  -> Mux(penable, apb_enable,  apb_setup)),
    (apb_enable -> apb_idle)
  ))

  when(fifo.io.deq.valid && state_apb === apb_enable){
    fifo.io.deq.ready := true.B
    //printf("data: %x\n", fifo.io.deq.bits)
  }.otherwise{
    fifo.io.deq.ready := false.B
  }
  io.in.prdata := Mux(fifo.io.deq.valid, fifo.io.deq.bits, 0.U)
  io.in.pready := true.B

  io.in.pslverr := 0.U
  dontTouch(io.in)
}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = address,
      executable    = true,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = 4)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
