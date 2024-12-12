package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class QSPIIO extends Bundle {
  val sck = Output(Bool())
  val ce_n = Output(Bool())
  val dio = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi = new QSPIIO
  })
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramChisel extends RawModule {
  val io = IO(Flipped(new QSPIIO))

  // if(en)  dout == dio == din  else  dio == din
  val out_en = Wire(Bool())
  val dout = Wire(UInt(4.W))
  dontTouch(dout)
  val din = TriStateInBuf(io.dio, dout, out_en) // change this if you need
  
  val first_cycle = withClockAndReset((!io.sck).asClock, io.ce_n.asAsyncReset) { RegInit(1.U(1.W)) }
  first_cycle := io.ce_n

  val cnt = withClockAndReset((!io.sck).asClock, io.ce_n.asAsyncReset) { RegInit(0.U(3.W)) }
  val cnt_next = Wire(UInt(3.W))
  val isread = Wire(Bool())
  val s_cmd :: s_addr :: s_wait :: s_data :: Nil = Enum(4)
  val state = withClockAndReset((!io.sck).asClock, io.ce_n.asAsyncReset) { RegInit(s_cmd) }
  val state_next = Wire(UInt(s_cmd.getWidth.W))
  state_next := MuxLookup(state, s_cmd)( Seq(
    (s_cmd  -> Mux(cnt === 7.U, s_addr, s_cmd)),   // 8 bits
    (s_addr -> Mux(cnt === 5.U, Mux(isread, s_wait, s_data), s_addr)), // 4*6 bits
    (s_wait -> Mux(cnt === 5.U, s_data, s_wait)),  // wait 6 cycles
    (s_data -> Mux(cnt === 7.U, s_cmd, s_data))   // Max 4*8 bits
  ))  
  state := state_next
  dontTouch(cnt_next)
  cnt_next := cnt + 1.U
  when(state =/= state_next) {
    cnt := 0.U
  }.otherwise {
    cnt := cnt_next(2,0)
  }

  out_en := (state === s_data) && isread

  val cmd = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(8.W)) }
  when(state === s_cmd) {
    cmd := Cat(cmd(6,0), din(0))
  }
  isread := cmd === 0xEB.U


  val addr_raw = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(VecInit(Seq.fill(4)(0.U(6.W)))) }


  when(state === s_addr) {
    for(i <- 0 until 4){
      addr_raw(i) := Cat(addr_raw(i)(4,0), din(i))
    }
  }
  
  val addr = Wire(UInt(24.W))

  addr := Cat(addr_raw(3)(5), addr_raw(2)(5), addr_raw(1)(5), addr_raw(0)(5),
              addr_raw(3)(4), addr_raw(2)(4), addr_raw(1)(4), addr_raw(0)(4),
              addr_raw(3)(3), addr_raw(2)(3), addr_raw(1)(3), addr_raw(0)(3),
              addr_raw(3)(2), addr_raw(2)(2), addr_raw(1)(2), addr_raw(0)(2),
              addr_raw(3)(1), addr_raw(2)(1), addr_raw(1)(1), addr_raw(0)(1),
              addr_raw(3)(0), addr_raw(2)(0), addr_raw(1)(0), addr_raw(0)(0))

  val cnt_data = withClockAndReset( (io.sck|io.ce_n).asClock, io.ce_n.asBool) { RegInit(0.U(3.W)) }
  cnt_data := Mux(state === s_data, cnt, 0.U)
  val psram_rw_inst = Module(new psram_rw)
  psram_rw_inst.io.clock := (io.sck|io.ce_n).asClock
  psram_rw_inst.io.reset := 0.U.asBool//io.ce_n.asBool
  psram_rw_inst.io.sel := ((cnt_data(0) && (!isread) ) ||
                          (isread)) &&
                           (state === s_data || cnt_data.orR)
  psram_rw_inst.io.write := !isread
  psram_rw_inst.io.addr := addr(23, 0)

  val wdata = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(8.W)) }

  when(state === s_data && (!isread)) {
    wdata := Cat(wdata(3,0),din)
  }


  psram_rw_inst.io.wdata := Mux((state === s_data || cnt_data.orR) && (!isread), Fill(4, wdata), 0.U)

  when((state === s_data || cnt_data.orR) && (!isread)) {
    psram_rw_inst.io.wmask := MuxLookup(cnt_data(2,1), 0.U)(Seq(
      (0.U -> "b0001".U),
      (1.U -> "b0010".U),
      (2.U -> "b0100".U),
      (3.U -> "b1000".U),
      ))
  }.otherwise {
    psram_rw_inst.io.wmask := 0.U
  }

  val rdata_dout = Wire(UInt(4.W)) //withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(4.W)) }
  val rdata = psram_rw_inst.io.rdata

  rdata_dout := Mux(state === s_data && isread,
            MuxLookup(cnt_data, 0.U)(Seq(
              (0.U -> rdata(7,4)),
              (1.U -> rdata(3,0)),
              (2.U -> rdata(15,12)),
              (3.U -> rdata(11,8)),
              (4.U -> rdata(23,20)),
              (5.U -> rdata(19,16)),
              (6.U -> rdata(31,28)),
              (7.U -> rdata(27,24)),
            )), 0.U)


  dout := rdata_dout


  
  dontTouch(dout)
  withClockAndReset(io.sck.asClock, io.ce_n.asBool) {
    when(state === s_wait || state === s_data){
      assert(((cmd === 0xEB.U) || (cmd === 0x38.U)) , "Invalid cmd: Only support 0xEB and 0x38!" )
    }

  }
  
}

class psram_rw extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val sel = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(24.W))
    val wdata = Input(UInt(32.W))
    val wmask = Input(UInt(4.W))
    val rdata = Output(UInt(32.W))
  })
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
