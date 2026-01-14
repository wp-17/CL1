package utils

import chisel3._
import chisel3.util._
import cl1.Cl1Config._

class sramIO(val WordDepth:Int = 256, val DW: Int = 32, val BE: Boolean = false) extends Bundle {
    val addr = Input(UInt(log2Ceil(WordDepth).W))
    val din  = Input(UInt(DW.W))
    val wea  = Input(if (BE) UInt((DW/8).W) else Bool())
    val ena  = Input(Bool())
    val dout = Output(UInt(DW.W))
}

class sram(val WordDepth:Int = 256, val DW: Int = 32, val BE: Boolean = false) extends Module {
    val io = IO(new sramIO(WordDepth, DW, BE))
    val Syn = SramFoundary
    if(BE) {
        if(!Syn) {
            val mem = SyncReadMem(WordDepth, Vec(DW/8, UInt(8.W)))
            val dataAsVec = io.din.asTypeOf(Vec(DW/8, UInt(8.W)))
            when(io.ena && io.wea =/= 0.U) {
                mem.write(io.addr, dataAsVec, io.wea.asBools)
            }
            io.dout := mem.read(io.addr, io.ena).asUInt
        } else {
            require(DW == 32, s"The SRAM macro S55NLLG1PH_X128Y4D32_BW only supports a data width (DW) of 32, but got ${DW}.")
            val s55Sram = Module(new S55NLLG1PH_X128Y1D32_BW(log2Ceil(WordDepth), DW))
            s55Sram.io.CLK := clock
            s55Sram.io.A := io.addr
            s55Sram.io.D := io.din
            s55Sram.io.CEN := !io.ena
            s55Sram.io.WEN := !(io.wea =/= 0.U)
            s55Sram.io.BWEN := Cat(io.wea.asBools.map(b => Fill(8,!b)).reverse)
            io.dout := s55Sram.io.Q
        }
    } else {
        if(!Syn) {
            val mem = SyncReadMem(WordDepth, UInt(DW.W))
            when(io.ena && io.wea =/= 0.U) {
                mem.write(io.addr, io.din)
            }
            io.dout := mem.read(io.addr, io.ena)
        } else {
            require(DW == 32 || DW == 22, s"The SRAM macro S55NLLG1PH_X128Y4D32_BW only supports a data width (DW) of 32 or 22, but got ${DW}.")
            if (DW == 32) {
                val s55Sram = Module(new S55NLLG1PH_X128Y1D32(log2Ceil(WordDepth), DW))
                s55Sram.io.CLK := clock
                s55Sram.io.A := io.addr
                s55Sram.io.D := io.din
                s55Sram.io.CEN := !io.ena
                s55Sram.io.WEN := !(io.wea =/= 0.U)
                io.dout := s55Sram.io.Q
            } else {
                val s55Sram = Module(new S55NLLG1PH_X128Y1D22(log2Ceil(WordDepth), DW))
                s55Sram.io.CLK := clock
                s55Sram.io.A := io.addr
                s55Sram.io.D := io.din
                s55Sram.io.CEN := !io.ena
                s55Sram.io.WEN := !(io.wea =/= 0.U)
                io.dout := s55Sram.io.Q
            }
        }
    }
}



class S55NLLG1PH_X128Y1D32_BW(AW: Int = 10, DW: Int = 32) extends BlackBox {
    val io = IO(new Bundle {
        val CLK = Input(Clock())
        val A       = Input(UInt(AW.W))
        val D       = Input(UInt(DW.W))
        val CEN     = Input(Bool())
        val WEN     = Input(Bool())
        val BWEN    = Input(UInt(DW.W))
        val Q       = Output(UInt(DW.W))
    })
}

class S55NLLG1PH_X128Y1D32(AW: Int = 10, DW: Int = 32) extends BlackBox {
    val io = IO(new Bundle {
        val CLK = Input(Clock())
        val A       = Input(UInt(AW.W))
        val D       = Input(UInt(DW.W))
        val CEN     = Input(Bool())
        val WEN     = Input(Bool())
        val Q       = Output(UInt(DW.W))
    })
}

class S55NLLG1PH_X128Y1D22(AW: Int = 10, DW: Int = 21) extends BlackBox {
    val io = IO(new Bundle {
        val CLK = Input(Clock())
        val A       = Input(UInt(AW.W))
        val D       = Input(UInt(DW.W))
        val CEN     = Input(Bool())
        val WEN     = Input(Bool())
        val Q       = Output(UInt(DW.W))
    })
}

