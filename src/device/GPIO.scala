package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}


class gpioChisel extends Module {
  val io = IO(new GPIOCtrlIO)

  val apb = io.in 
  val led = io.gpio.out
  val key = io.gpio.in
  val seg = io.gpio.seg

  val led_reg = RegInit(0.U(16.W))
  val seg_reg = RegInit(0.U(32.W))
  led := led_reg
  


  val s_idle :: s_setup :: s_access :: Nil = Enum(3)
  val state = RegInit(s_idle)
  state := MuxLookup(state, s_idle)(Seq(
    (s_idle -> Mux(apb.psel && apb.penable, s_setup, s_idle)),
    (s_setup -> Mux(apb.psel && apb.penable, s_access, s_idle)),
    (s_access -> Mux(apb.psel && apb.penable, s_idle, s_idle))
  ))

  apb.pready := state === s_access
  apb.pslverr := false.B 


  val wen = apb.penable & apb.pwrite & apb.psel 
  val ren = apb.penable & !apb.pwrite & apb.psel

  def segDecoder(input: UInt): UInt = {
    MuxLookup(input, "b11111111".U)(Seq( 
      0.U  -> "b00000011".U,
      1.U  -> "b10011111".U,
      2.U  -> "b00100101".U,
      3.U  -> "b00001101".U,
      4.U  -> "b10011001".U,
      5.U  -> "b01001001".U,
      6.U  -> "b01000001".U,
      7.U  -> "b00011111".U,
      8.U  -> "b00000001".U,
      9.U  -> "b00001001".U,
      10.U -> "b00010001".U,
      11.U -> "b11000001".U,
      12.U -> "b01100011".U,
      13.U -> "b10000101".U,
      14.U -> "b01100001".U,
      15.U -> "b01110001".U
    ))
  }

  for (i <- 0 until 8) {
    seg(i) := segDecoder(seg_reg(i*4+3,i*4))
  }
  

  led_reg := Mux(wen && apb.paddr(3, 2) === 0.U,
    MuxLookup(apb.pstrb(1,0), led_reg)(Seq(
      "b01".U -> Cat(led_reg(15,8), apb.pwdata(7,0)),      // low 8 bits
      "b10".U -> Cat(apb.pwdata(15,8), led_reg(7,0)),     // high 8 bits
      "b11".U -> apb.pwdata(15,0),                         // all 16 bits
    )),
    led_reg)

  seg_reg := Mux(wen && apb.paddr(3, 2) === 2.U,
    MuxLookup(apb.pstrb, seg_reg)(Seq(
      "b0001".U -> Cat(seg_reg(31,8), apb.pwdata(7,0)),   
      "b0010".U -> Cat(seg_reg(31,16), apb.pwdata(15,8), seg_reg(7,0)),
      "b0100".U -> Cat(seg_reg(31,24), apb.pwdata(23,16), seg_reg(15,0)), 
      "b1000".U -> Cat(apb.pwdata(31,24), seg_reg(23,16)),
      "b0011".U -> Cat(seg_reg(31,16), seg_reg(15,0)), 
      "b0110".U -> Cat(seg_reg(31,24), apb.pwdata(23,8), seg_reg(7,0)), 
      "b1100".U -> Cat(apb.pwdata(31,16), seg_reg(15,0)), 
      "b1111".U -> apb.pwdata(31,0) 
    )),
    seg_reg)

  io.in.prdata := key 
  io.in.pready := wen || ren 
  io.in.pslverr :=false.B 

  

}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
