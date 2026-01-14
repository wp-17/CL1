// SPDX-License-Identifier: MulanPSL-2.0

package cl1

import chisel3._
import chisel3.util._
import chisel3.util.circt.ClockGate
import cl1.Cl1Config._
import chisel3.util.experimental.BoringUtils

class diff extends Bundle {
  val commit  = Output(Bool())
  val insn    = Output(UInt(32.W))
  val pc      = Output(UInt(32.W))
  val mode    = Output(UInt(32.W))
  val rd_addr = Output(UInt(5.W))
  val rd_wdata = Output(UInt(32.W))
}

class Cl1Top extends Module{
  val io = IO(new Bundle {
    val boot_addr = if(globalConfig.syn || globalConfig.fullSocTest) Some(Input(UInt(32.W))) else None
    val ext_irq   = Input(Bool())
    val sft_irq   = Input(Bool())
    val tmr_irq   = Input(Bool())
    val dbg_req_i = Input(Bool())
    val diff_o    = if(SOC_DIFF) Some(new diff) else None
    val master = new AXI4(BUS_WIDTH, if (SOC_D64) 64 else BUS_WIDTH, 4)
  })

  val rst0  = if(RST_ACTIVELOW) !reset.asBool else reset
  val rst1  = if(RST_ASYNC)     rst0.asAsyncReset else rst0

  val core = if(CKG_EN) {
    val core_wfi    = Wire(Bool())
    val core_clk_en = ~core_wfi
    val gatedClock  = ClockGate(clock, core_clk_en).suggestName(s"clk_gate")
    val core_u      = withClockAndReset(gatedClock, rst1) {
      Module(new Cl1Core)
    }
    core_wfi       := core_u.io.core_wfi
    core_u
  } else {
      withReset(rst1) {Module(new Cl1Core)}
  }
  

  core.io.dbg_req_i := io.dbg_req_i
  core.io.ext_irq   := io.ext_irq
  core.io.sft_irq   := io.sft_irq
  core.io.tmr_irq   := io.tmr_irq

  val core_axi = if(SOC_D64) {
    val AXIWidthC = withReset(rst1) {Module(new AXIWidthConverter)}
    core.io.master <> AXIWidthC.io.in
    AXIWidthC.io.out
  } else {
    core.io.master
  }
  io.master <> core_axi

//  BoringUtils.addSource(io.boot_addr, "boot_addr_signal")
  if(globalConfig.syn || globalConfig.fullSocTest) {
    BoringUtils.bore(io.boot_addr.get, Seq(core.ifStage.boot_addr))
  }

if(SOC_DIFF) {

  val wb_pc     = BoringUtils.bore(core.wbStage.wb_pc)
  val wb_is_c   = BoringUtils.bore(core.wbStage.pplIn.isCInst)
  val wb_cinst  = BoringUtils.bore(core.wbStage.io.cInst)
  val wb_inst   = BoringUtils.bore(core.wbStage.io.inst)
  val rd_idx    = BoringUtils.bore(core.wbStage.rd_idx)
  val rd_wdata  = BoringUtils.bore(core.wbStage.io.wdata)

  val wb_ecall  = BoringUtils.bore(core.wbStage.isValidEcall)
  val rd_wen    = BoringUtils.bore(core.wbStage.io.wen)
  val wb_valid  = BoringUtils.bore(core.wbStage.wb_valid)
  val wb_flush  = BoringUtils.bore(core.wbStage.io.flush)

  val diff_port = io.diff_o.get 
  diff_port.commit   := wb_valid & !wb_flush & rd_wen || wb_ecall
  diff_port.insn     := Mux(wb_is_c, wb_cinst, wb_inst)
  diff_port.pc       := wb_pc
  diff_port.mode     := 3.U
  diff_port.rd_addr  := rd_idx
  diff_port.rd_wdata := rd_wdata
}


}
