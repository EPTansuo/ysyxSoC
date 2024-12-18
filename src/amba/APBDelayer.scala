package ysyx

import chisel3._
import chisel3.util._
import scala.math
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class APBDelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val out = new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
}

class apb_delayer extends BlackBox {
  val io = IO(new APBDelayerIO)
}

class APBDelayerChisel extends Module {
  val io = IO(new APBDelayerIO)
  //io.out <> io.in
  val NPC_FREQ = 890 // 890 MHz
  val DEVICE_FREQ = 100 // 100 MHz
  val SCALE = 256   // must be 2^n
  val RATIO = NPC_FREQ.toDouble / DEVICE_FREQ.toDouble 
  val CNT_ACC = (RATIO * SCALE).toInt 
  val CNT_SHIFT = log2Ceil(SCALE) 

  if(math.abs(CNT_SHIFT - math.log(SCALE)/math.log(2)) > 0.001){
    println("ERROR: SCALE in APBDelayerChisel must be 2^n!")
    sys.exit(1)
  }

  val cnt = RegInit(0.U(16.W))          // count for delayer
  val delay_finish = Wire(Bool())
  val rdata_reg = RegInit(0.U(io.in.prdata.getWidth.W))


  val s_idle :: s_transfer :: s_delay :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)( Seq(
    (s_idle     -> Mux(io.in.psel && io.in.psel, 
                      Mux(io.out.pready, s_delay, s_transfer), s_idle)),
    (s_transfer -> Mux(io.out.pready, s_delay,    s_transfer)),
    (s_delay    -> Mux(delay_finish,  s_done,     s_delay)),
    (s_done     -> Mux(io.in.pready,  s_idle,     s_done))
  ))
  
  val final_cnt = Wire(UInt((cnt.getWidth-CNT_SHIFT).W))
  final_cnt := cnt >> CNT_SHIFT.U

  when(state === s_idle){
    cnt := 0.U
  }.elsewhen(state === s_transfer && !io.out.pready){
    cnt := cnt + CNT_ACC.U
  }.elsewhen(state === s_transfer && io.out.pready){
    cnt := final_cnt

  }.elsewhen(state === s_delay){
    cnt := cnt - 1.U
  }

  
  dontTouch(final_cnt)

  delay_finish := state === s_delay && cnt === 0.U

  when(io.out.penable && io.out.psel && io.out.pready){
    rdata_reg := io.out.prdata
  }

  io.out.penable := Mux(state === s_transfer || state === s_idle, io.in.penable, false.B)
  io.out.psel := Mux(state === s_transfer || state === s_idle, io.in.psel, false.B)
  io.in.prdata := rdata_reg
  io.in.pready := state === s_done 

  io.out.paddr := io.in.paddr 
  io.out.pwdata := io.in.pwdata
  io.out.pwrite := io.in.pwrite
  io.out.pstrb := io.in.pstrb 
  io.out.pprot := io.in.pprot 
  io.in.pslverr := io.out.pslverr
  
}

class APBDelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = APBIdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new APBDelayerChisel)
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object APBDelayer {
  def apply()(implicit p: Parameters): APBNode = {
    val apbdelay = LazyModule(new APBDelayerWrapper)
    apbdelay.node
  }
}
