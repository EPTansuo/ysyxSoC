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
  
  // We only need to wait when reading data
  val cnt = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(4.W)) }
  val cnt_next = Wire(UInt(4.W))
  val isread = Wire(Bool())
  val s_cmd :: s_addr :: s_wait :: s_data :: Nil = Enum(4)
  val state = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(s_cmd) }
  val state_next = Wire(UInt(s_cmd.getWidth.W))
  state_next := MuxLookup(state, s_cmd)( Seq(
    //(s_idle -> Mux(io.ce_n, s_idle, s_cmd)),
    (s_cmd  -> Mux(cnt === 8.U, s_addr, s_cmd)),   // 8 bits  
    (s_addr -> Mux(cnt === 5.U, Mux(isread, s_wait, s_data), s_addr)), // 4*6 bits
    (s_wait -> Mux(cnt === 5.U, s_data, s_wait)),  // wait 6 cycles
    (s_data -> Mux(cnt === Mux(isread, 7.U, 6.U), s_cmd, s_data))   // 4*8 bits  
  ))  
  state := state_next

  cnt_next := cnt + 1.U
  when(state =/= state_next) {
    cnt := 0.U
  }.otherwise {
    cnt := cnt_next
  }

  out_en := (state === s_data) && isread

  val cmd = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(8.W)) }
  when(state_next === s_cmd) {
    cmd := Cat(cmd(6,0), din(0))
  }
  isread := cmd === 0xEB.U

  
  

  //val addr_raw = VecInit(Seq.fill(4)(withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(6.W)) }))
//  val addr_raw = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(VecInit(Seq.fill(4)(0.U(6.W)))) }

  val addr_raw = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(VecInit(Seq.fill(4)(0.U(6.W)))) }


  when(state_next === s_addr) {
    for(i <- 0 until 4){
      addr_raw(i) := Cat(addr_raw(i)(4,0), din(i))
    }
  }
  
  val addr = Wire(UInt(24.W))

  //  addr := Cat(addr_raw.reverse.map(_.asUInt).reduce(_ ## _))
  addr := Cat(addr_raw(3)(5), addr_raw(2)(5), addr_raw(1)(5), addr_raw(0)(5),
              addr_raw(3)(4), addr_raw(2)(4), addr_raw(1)(4), addr_raw(0)(4),
              addr_raw(3)(3), addr_raw(2)(3), addr_raw(1)(3), addr_raw(0)(3),
              addr_raw(3)(2), addr_raw(2)(2), addr_raw(1)(2), addr_raw(0)(2),
              addr_raw(3)(1), addr_raw(2)(1), addr_raw(1)(1), addr_raw(0)(1),
              addr_raw(3)(0), addr_raw(2)(0), addr_raw(1)(0), addr_raw(0)(0))
  // for(i <- 0 until 4) {
  //   for(j <- 0 until 6) {
  //     addr(j*4+i) := addr_raw(i)(j)
  //   }
  // }

  val psram_rw_inst = Module(new psram_rw)
  psram_rw_inst.io.clock := io.sck.asClock 
  psram_rw_inst.io.reset := io.ce_n.asBool
  psram_rw_inst.io.sel := (state === s_data && cnt === 6.U && (!isread)) || 
                          (isread  && state === s_wait && cnt === 5.U)
  psram_rw_inst.io.write := !isread
  psram_rw_inst.io.addr := addr(23, 0)

  val data_raw = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(VecInit(Seq.fill(4)(0.U(8.W)))) }
  //val data_raw = Vec(4, withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(8.W)) })
  when(state_next === s_data && (!isread)) {
    for(i <- 0 until 4) {
      data_raw(i) := Cat(data_raw(i)(7,0), din(i))
    }
  }
  val data = Wire(UInt(32.W))

  // data := Cat(data_raw.reverse.map(_.asUInt).reduce(_ ## _))

 data := Cat( data_raw(3)(0), data_raw(2)(0), data_raw(1)(0), data_raw(0)(0),
              din, 
              data_raw(3)(2), data_raw(2)(2), data_raw(1)(2), data_raw(0)(2),
              data_raw(3)(1), data_raw(2)(1), data_raw(1)(1), data_raw(0)(1),
              data_raw(3)(4), data_raw(2)(4), data_raw(1)(4), data_raw(0)(4),
              data_raw(3)(3), data_raw(2)(3), data_raw(1)(3), data_raw(0)(3),
              data_raw(3)(6), data_raw(2)(6), data_raw(1)(6), data_raw(0)(6),
              data_raw(3)(5), data_raw(2)(5), data_raw(1)(5), data_raw(0)(5))
  
                 
  // for(i <- 0 until 4){
  //   for(j <- 0 until 8) {
  //     data(j*4+i) := data_raw(i)(j)
  //   }
  // }

  when(state === s_data && (!isread)) {
    psram_rw_inst.io.wdata := data
    psram_rw_inst.io.wmask := 0xF.U
  }.otherwise {
    psram_rw_inst.io.wdata := 0.U
    psram_rw_inst.io.wmask := 0.U
  }
  //val data_read =withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(VecInit(Seq.fill(8)(0.U(4.W)))) }
  val data_read_reg = withClockAndReset(io.sck.asClock, io.ce_n.asBool) { RegInit(0.U(32.W)) }
  //dontTouch(data_read)
  //val data_read = Vec(8, withClockAndReset(io.sck.asClock, io.ce_n.asBool) { Wire(UInt(4.W)) })
  when(state === s_wait && cnt === 5.U) {
  //   for(j <- 0 until 8) {
  //     data_read(j) := psram_rw_inst.io.rdata(j*4+3, j*4)
  // }
    data_read_reg := psram_rw_inst.io.rdata
  }

  dout := Mux(state === s_data && isread, 
              MuxLookup(cnt, 0.U)( Seq(
                0.U -> data_read_reg(7, 4),
                1.U -> data_read_reg(3, 0),
                2.U -> data_read_reg(15, 12),
                3.U -> data_read_reg(11, 8),
                4.U -> data_read_reg(23, 20),
                5.U -> data_read_reg(19, 16),
                6.U -> data_read_reg(31, 28),
                7.U -> data_read_reg(27, 24)
              )), 0.U)

  // when(state_next === s_data && isread && cnt === 5.U) {
  //   dout := data_read(Mux(cnt === "b101".U, 0.U, cnt+1.U)) //需要提前读
  // }.otherwise {
  //   dout := 0.U
  // }
  
  dontTouch(dout)
  withClockAndReset(io.sck.asClock, io.ce_n.asBool) {
   //assert(((cmd === 0xEB.U) || (cmd === 0x38.U)) ^ (state === s_addr), "Invalid cmd: Only support 0xEB and 0x38!" )
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
