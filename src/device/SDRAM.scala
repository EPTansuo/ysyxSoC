package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(4.W))
  val dq  = Analog(32.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}


class sdramblock extends BlackBox {
  val io = IO(new Bundle {
    val clock     = Input(Clock())
    val bank      = Input(UInt(2.W))
    val col       = Input(UInt(9.W))
    val row       = Input(UInt(13.W))
    val block_num = Input(UInt(2.W))
    val we        = Input(Bool())
    val wdata   = Input(UInt(16.W))
    val dqm       = Input(UInt(2.W))
    val rdata  = Output(UInt(16.W))
  })
}


class sdramChisel extends RawModule {
  val io = IO(Flipped(new SDRAMIO))

  val dout = Wire(UInt(32.W))
  val out_en = Wire(Bool())
  
  val dq = TriStateInBuf(io.dq, dout, out_en)
  
  val block00 = Module(new sdramblock())
  val block01 = Module(new sdramblock())
  val block10 = Module(new sdramblock())
  val block11 = Module(new sdramblock())

  block00.io.block_num := 0.U
  block01.io.block_num := 1.U
  block10.io.block_num := 2.U
  block11.io.block_num := 3.U
  block00.io.clock := io.clk.asClock
  block01.io.clock := io.clk.asClock
  block10.io.clock := io.clk.asClock
  block11.io.clock := io.clk.asClock

 

  val cs = io.cs
  val ras = io.ras
  val cas = io.cas
  val we = io.we
  val is_active     = (!cs) && (!ras) && cas    && we
  val is_read       = (!cs) && ras    && (!cas) && we
  val is_write      = (!cs) && ras    && (!cas) && (!we)
  val is_write_mode = (!cs) && (!ras) && (!cas) && (!we)

  val cnt = withClockAndReset(io.clk.asClock, cs.asBool) { Reg(UInt(3.W)) }
  

  val s_idle :: s_read :: s_write :: Nil = Enum(3)
  val state = withClockAndReset(io.clk.asClock, cs.asBool) { RegInit(s_idle) }
  state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(is_read, s_read, Mux(is_write, s_write, s_idle)),
      s_read -> Mux(cnt === 2.U, s_idle, s_read),
      s_write -> s_idle
    )
  )

  when(state === s_read || state === s_write) {
    cnt:= cnt+ 1.U
  }                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         

  val bankid = withClockAndReset(io.clk.asClock, cs.asBool) { RegInit(0.U(2.W)) }
  block00.io.bank := bankid
  block01.io.bank := bankid
  block10.io.bank := bankid
  block11.io.bank := bankid
  
  val col = withClockAndReset(io.clk.asClock, cs.asBool) { RegInit(0.U(13.W)) }
  block00.io.col := col(8, 0)
  block01.io.col := block00.io.col
  block10.io.col := block00.io.col
  block11.io.col := block00.io.col

  val row = withClockAndReset(io.clk.asClock, cs.asBool) { RegInit(VecInit(Seq.fill(4)(0.U(13.W)))) }
  block00.io.row := row(bankid)
  block01.io.row := block00.io.row
  block10.io.row := block00.io.row
  block11.io.row := block00.io.row

  val wdata = withClockAndReset(io.clk.asClock, cs.asBool) { RegInit(0.U) }
  wdata:= dq
  block00.io.wdata  := wdata(31, 16)
  block01.io.wdata  := wdata(15, 0)
  block10.io.wdata := wdata(31, 16)
  block11.io.wdata := wdata(15, 0)


  val dqm_reg = withClockAndReset(io.clk.asClock, cs.asBool) { RegInit(0.U(4.W)) }
  dqm_reg := io.dqm
  block00.io.dqm  := dqm_reg(3, 2)
  block01.io.dqm  := dqm_reg(1, 0)

  block10.io.dqm := dqm_reg(3, 2)
  block11.io.dqm := dqm_reg(1, 0)

  dout := Mux( col(9) === 1.U,
    Cat(block10.io.rdata, block11.io.rdata),
    Cat(block00.io.rdata, block01.io.rdata)
  )

    
  out_en  := state === s_read

  
  when(state === s_idle && is_active) {
    row(io.ba) := io.a
    cnt   := 0.U
    bankid     := io.ba
  }
  when((is_read || is_write)) {
    col     := io.a
    cnt:= 0.U
    bankid  := io.ba
  }


  block00.io.we := col(9) === 0.U && state === s_write
  block01.io.we := block00.io.we
  block10.io.we := col(9) === 1.U && state === s_write
  block11.io.we := block10.io.we
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 4
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
        address       = address,
        executable    = true,
        supportsWrite = TransferSizes(1, beatBytes),
        supportsRead  = TransferSizes(1, beatBytes),
        interleavedId = Some(0))
    ),
    beatBytes  = beatBytes)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
