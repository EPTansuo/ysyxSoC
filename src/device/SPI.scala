package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val spi_bundle = IO(new SPIIO)

    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    //mspi.io.in <> in
    spi_bundle <> mspi.io.spi

    //val XIP_en = (in.paddr >= 0x30000000.U) && (in.paddr <= 0x3fffffff.U)
    val XIP_en = false.B
    val spi_rw = Module(new APBSPIMasterReadWrite)
/*
    mspi.io.in.psel := 0.U
    mspi.io.in.penable := 0.U
    mspi.io.in.pwrite := 0.U
    mspi.io.in.paddr := 0.U
    mspi.io.in.pprot := 0.U
    mspi.io.in.pwdata  := 0.U
    mspi.io.in.pstrb := 0.U*/

    spi_rw.io.out.pready := 0.U
    spi_rw.io.out.pslverr := 0.U
    spi_rw.io.out.prdata := 0.U



    val s_idle :: s_ss :: s_cmd ::s_start :: s_isbusy :: s_reading :: s_stop :: Nil = Enum(7);
    val state = RegInit(s_idle);
    val isbusy = Wire(UInt(1.W))
    isbusy := spi_rw.io.rdata(8) && (state === s_isbusy)
    dontTouch(isbusy)

    state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(XIP_en && in.psel, s_ss, s_idle),
      s_ss   -> Mux(spi_rw.io.idle, s_cmd, s_ss),
      s_cmd  -> Mux(spi_rw.io.idle, s_start, s_cmd),
      s_start-> Mux(spi_rw.io.idle, s_isbusy, s_start),
      s_isbusy  -> Mux(spi_rw.io.idle && (!isbusy), s_reading, s_isbusy),
      s_reading -> Mux(spi_rw.io.idle , s_stop, s_reading),
      s_stop    -> Mux(spi_rw.io.idle, s_idle, s_stop)
    ))

    val SPI_BASE = 0x10001000

    val flash_addr = in.paddr - 0x30000000.U

    spi_rw.io.addr := MuxLookup(state, 0.U(32.W))(Seq(
      s_ss      -> (SPI_BASE + 0x18).U,
      s_cmd     -> (SPI_BASE + 4).U,
      s_start   -> (SPI_BASE + 0x10).U,
      s_isbusy  -> (SPI_BASE + 0x10).U,
      s_reading -> (SPI_BASE + 0).U,
      s_stop    -> (SPI_BASE + 0x18).U
    ))
    spi_rw.io.wdata := MuxLookup(state, 0.U(32.W))(Seq(
      s_ss      -> 0x01.U(32.W),
      s_cmd     -> Cat(0x03.U(8.W), flash_addr(23,0)),
      s_start   -> 0x140.U,
      s_stop    -> 0x0.U(32.W)
    ))



    spi_rw.io.write := (state === s_ss || state === s_cmd || state === s_start || state === s_stop)
                    .&& (!spi_rw.io.idle)
    spi_rw.io.en := state =/= s_idle && (!spi_rw.io.idle)
    spi_rw.io.strb := Mux(spi_rw.io.write, 0xF.U, 0.U)

    val rdata_raw = RegInit(0.U(32.W))

    when(state === s_reading && spi_rw.io.idle) {
      rdata_raw := spi_rw.io.rdata
    }

    val rdata = Cat(rdata_raw(7,0), rdata_raw(15,8), rdata_raw(23,16), rdata_raw(31,24))


    in.prdata := rdata
    in.pready := state === s_stop
    in.pslverr := 0.U


    // The Code should be put at the end
    when(XIP_en){
      mspi.io.in <> spi_rw.io.out
    }.otherwise{
      mspi.io.in <> in
    }

    // 在XIP下不能写，或者写的时候不在XIP
    assert((XIP_en ^ in.pwrite) || (!reset.asBool), "Can not write to FLASH while in XIP!" );


  }
}

class APBArbiter

class APBSPIMasterReadWrite extends Module {
  val io = IO(new Bundle{
    val addr = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
    val strb  = Input(UInt(4.W))
    val en = Input(Bool())
    val write = Input(Bool())
    val idle = Output(Bool())
    val out = new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
  })

  val s_idle :: s_wsetup :: s_waccess :: s_rsetup :: s_raccess :: s_ready ::Nil = Enum(6)

  val state = RegInit(s_idle)
  val state_last = RegInit(s_idle)
  state_last := state;

  state:= MuxLookup(state, s_idle)(Seq(
    s_idle    -> Mux(io.en, Mux(io.write, s_wsetup, s_rsetup), s_idle),
    s_wsetup  -> s_waccess,
    s_waccess -> Mux(io.out.pready, s_idle, s_waccess),
    s_rsetup  -> s_raccess,
    s_raccess -> Mux(io.out.pready, s_idle, s_raccess),
    s_ready   -> s_idle
  ))

  io.out.penable := state === s_waccess || state === s_raccess
  io.out.pwrite  := state === s_wsetup || state === s_waccess
  io.out.pwdata  := io.wdata
  io.out.pstrb   := io.strb

  val rdata_reg = RegInit(0.U(32.W))
  when(state === s_raccess){
    rdata_reg := io.out.prdata
  }

  io.rdata := rdata_reg
  io.idle := state === s_idle && ((state_last === s_waccess) || (state_last === s_raccess))
  dontTouch(state_last)

  io.out.pprot := 0.U
  io.out.psel := io.en
  io.out.paddr := io.addr

}

class read_flash extends BlackBox {
  val io = IO(new Bundle{
    val en = Input(Bool())
    val addr = Input(UInt(32.W))
    val data_out = Output(UInt(32.W))
  })
}
