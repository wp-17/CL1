// SPDX-License-Identifier: MulanPSL-2.0

package cl1

object globalConfig {
  val syn = false
  val simpleSocTest = true
  val fullSocTest  = false
  require (
    Seq(syn, simpleSocTest, fullSocTest).count(_ == true) == 1, 
    "Error: Exactly one of 'syn', 'smipleSocTest', or 'fullSocTest' must be set to true in globalConfig."
  )
}

object Cl1Config {
  val BOOT_ADDR  = if(globalConfig.syn || globalConfig.fullSocTest) "h01000000" else "h80000000"
  val TVEC_ADDR  = "h20000000"
  val BUS_WIDTH  = 32
  val CKG_EN     = false
  val difftest   = if(globalConfig.simpleSocTest) false else false
  val DBG_ENTRYADDR = "h800"
  val DBG_EXCP_BASE = "h800"
  val MDU_SHAERALU = true
  val WB_PIPESTAGE = true
  val HAS_ICACHE   = true
  val HAS_DCACHE   = true
  val RST_ACTIVELOW = true
  val RST_ASYNC     = true
  val SOC_DIFF     = if(globalConfig.fullSocTest) true else false
  val SramFoundary = if(globalConfig.syn || globalConfig.fullSocTest) true else false
  val SOC_D64      = if(globalConfig.syn || globalConfig.fullSocTest) true else false
}
