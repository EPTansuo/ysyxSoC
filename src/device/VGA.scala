package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r = Output(UInt(8.W))
  val g = Output(UInt(8.W))
  val b = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga = new VGAIO
}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)

  val fb = Mem(640 * 480, UInt(24.W))


  val h_frontporch = 96
  val h_active = 144
  val h_backporch = 784
  val h_total = 800

  val v_frontporch = 2
  val v_active = 35
  val v_backporch = 515
  val v_total = 525

  val x_cnt = RegInit(1.U(10.W))
  val y_cnt = RegInit(1.U(10.W))
  val h_valid = Wire(Bool())
  val v_valid = Wire(Bool())

  when(x_cnt === h_total.U){
    x_cnt := 1.U
  }.otherwise{
    x_cnt := x_cnt + 1.U
  }

  when(y_cnt === v_total.U && x_cnt === h_total.U){
    y_cnt := 1.U
  }.elsewhen(x_cnt === h_total.U){
    y_cnt := y_cnt + 1.U
  }

  io.vga.hsync := x_cnt > h_frontporch.U
  io.vga.vsync := y_cnt > v_frontporch.U
  
  h_valid := x_cnt > h_active.U & (x_cnt <= h_backporch.U)
  v_valid := y_cnt > v_active.U & (y_cnt <= v_backporch.U) 
  io.vga.valid := h_valid & v_valid

  val h_addr = Wire(UInt(10.W))
  val v_addr = Wire(UInt(10.W))

  h_addr := Mux(h_valid, (x_cnt - h_active.U), 0.U)
  v_addr := Mux(v_valid, (y_cnt - v_active.U), 0.U)


  io.vga.r := fb(h_addr + v_addr * 640.U)(23, 16)
  //io.vga.r := 0xff.U
  io.vga.g := fb(h_addr + v_addr * 640.U)(15, 8)
  io.vga.b := fb(h_addr + v_addr * 640.U)(7, 0)

  // APB
  // VGA_BASE = 0x21000000
  val addr = io.in.paddr(23, 2)

  when(io.in.psel & io.in.penable & io.in.pwrite){
    fb(addr) := io.in.pwdata
  }

  io.in.pready := true.B
  io.in.prdata := fb(addr)
  io.in.pslverr := false.B
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val vga_bundle = IO(new VGAIO)

    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
