package ddecs_project

import chisel3._
import chisel3.util._

import chisel3.experimental._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import dsptools.numbers._
import freechips.rocketchip.interrupts._



class AXI4StreamRepacking(val useAXIStream: Boolean = true, val useAXI4Input: Boolean = false, val useAXI4Output: Boolean = false, addrWidth: Int = 32, lenWidth: Int = 8, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  
  require(useAXIStream || useAXI4Input || useAXI4Output, s"At least one of AXI-Stream and AXI4 protocols must exist!")
  
  val inSlaveNode = Some(AXI4StreamSlaveNode(AXI4StreamSlaveParameters()))
  val inMasterNodeAxiS = if (useAXIStream) Some(AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes)))))) else None
  val inMasterNodeAxi4 = if (useAXI4Input) Some(AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes)))))) else None
  val outSlaveNodeAxiS = if (useAXIStream || useAXI4Input) Some(AXI4StreamSlaveNode(AXI4StreamSlaveParameters())) else None
  val outSlaveNodeAxi4 = if (useAXI4Output) Some(AXI4StreamSlaveNode(AXI4StreamSlaveParameters())) else None
  val outMasterNode = Some(AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("outNode", n = beatBytes))))))
  
  lazy val module = new LazyModuleImp(this) {
    
    /*
    val out = outNode.out(0)._1
    val in = inNode.in(0)._1
    
    val streamToMemRequest = if (useAXI4Output) Some(
      IO(Decoupled(
        DMARequest(addrWidth = addrWidth, lenWidth = lenWidth)
      ))
    ) else None
    
    val memToStreamRequest = if (useAXI4Output) Some(
      IO(Decoupled(
        DMARequest(addrWidth = addrWidth, lenWidth = lenWidth)
      ))
    ) else None
    
    if(!useAXI4Output) {
      val indicator = RegInit(Bool(), false.B)
      
      when(out.fire() || streamToMemRequest.fire()) {
        indicator := !indicator
      }
      
      streamToMemRequest.bits.length := 0.U
      streamToMemRequest.bits.cycles := 0.U
      streamToMemRequest.bits.fixedAddress := 1.U
      
      streamToMemRequest.valid := Mux(indicator, false.B, in.valid)
      streamToMemRequest.bits.baseAddress := Mux(indicator, 0.U, in.bits.data)
      
      out.valid := Mux(indicator, in.valid, false.B)
      out.bits.data := Mux(indicator, in.bits.data, 0.U)
      out.bits.last := Mux(indicator, in.bits.last, false.B)
      
      in.ready := (indicator && out.ready) || (!indicator && streamToMemRequest.ready)
      
    } else {
      streamToMemRequest.bits.length := 0.U
      streamToMemRequest.bits.cycles := 0.U
      streamToMemRequest.bits.fixedAddress := 1.U
      
      streamToMemRequest.valid := in.valid
      streamToMemRequest.bits.baseAddress := in.bits.data
      
      out.valid := false.B
      out.bits.data := 0.U
      out.bits.last := false.B
      
      in.ready := streamToMemRequest.ready
    }*/
    
    val streamToMemRequest = if (useAXI4Input) Some(
      IO(Decoupled(
        DMARequest(addrWidth = addrWidth, lenWidth = lenWidth)
      ))
    ) else None
    
    val memToStreamRequest = if (useAXI4Output) Some(
      IO(Decoupled(
        DMARequest(addrWidth = addrWidth, lenWidth = lenWidth)
      ))
    ) else None
    
    val indicator = if ((useAXIStream && useAXI4Input) || (useAXIStream && useAXI4Output) || (useAXI4Input && useAXI4Output)) Some(
      IO(Input(UInt(2.W)))
    ) else None
    
    /************************************************************************************************** AXI-Stream ************************************************************************************************************/
    if(useAXIStream && !useAXI4Input && !useAXI4Output) {
      
      val inSlave = inSlaveNode.get.in(0)._1
      val inMasterAxiS = inMasterNodeAxiS.get.out(0)._1
      val outSlaveAxiS = outSlaveNodeAxiS.get.in(0)._1
      val outMaster = outMasterNode.get.out(0)._1
      
      inSlave.ready := inMasterAxiS.ready
      inMasterAxiS.bits.data := inSlave.bits.data
      inMasterAxiS.valid := inSlave.valid
      inMasterAxiS.bits.last := inSlave.bits.last
      
      outSlaveAxiS.ready := outMaster.ready
      outMaster.bits.data := outSlaveAxiS.bits.data
      outMaster.valid := outSlaveAxiS.valid
      outMaster.bits.last := outSlaveAxiS.bits.last

    /************************************************************************************************ AXI4 Input *************************************************************************************************************/
    } else if(!useAXIStream && useAXI4Input && !useAXI4Output) {
      
      val inSlave = inSlaveNode.get.in(0)._1
      val inMasterAxi4 = inMasterNodeAxi4.get.out(0)._1
      val outSlaveAxiS = outSlaveNodeAxiS.get.in(0)._1
      val outMaster = outMasterNode.get.out(0)._1
      
      val indicator = RegInit(Bool(), false.B)
      
      when(inMasterAxi4.fire() || streamToMemRequest.get.fire()) {
        indicator := !indicator
      }
      
      streamToMemRequest.get.bits.length := 0.U
      streamToMemRequest.get.bits.cycles := 0.U
      streamToMemRequest.get.bits.fixedAddress := 1.U
      streamToMemRequest.get.valid := Mux(indicator, false.B, inSlave.valid)
      streamToMemRequest.get.bits.baseAddress := Mux(indicator, 0.U, inSlave.bits.data)
      
      inMasterAxi4.valid := Mux(indicator, inSlave.valid, false.B)
      inMasterAxi4.bits.data := Mux(indicator, inSlave.bits.data, 0.U)
      inMasterAxi4.bits.last := Mux(indicator, inSlave.bits.last, false.B)
      inSlave.ready := (indicator && inMasterAxi4.ready) || (!indicator && streamToMemRequest.get.ready)
      
      outSlaveAxiS.ready := outMaster.ready
      outMaster.bits.data := outSlaveAxiS.bits.data
      outMaster.valid := outSlaveAxiS.valid
      outMaster.bits.last := outSlaveAxiS.bits.last
    
    /************************************************************************************************ AXI4 Output *************************************************************************************************************/
    } else if (!useAXIStream && !useAXI4Input && useAXI4Output) {
      
      val inSlave = inSlaveNode.get.in(0)._1
      val outSlaveAxi4 = outSlaveNodeAxi4.get.in(0)._1
      val outMaster = outMasterNode.get.out(0)._1
      
      memToStreamRequest.get.bits.length := 0.U
      memToStreamRequest.get.bits.cycles := 0.U
      memToStreamRequest.get.bits.fixedAddress := 1.U
      
      memToStreamRequest.get.valid := inSlave.valid
      memToStreamRequest.get.bits.baseAddress := inSlave.bits.data
      
      //outSlaveAxi4.valid := false.B
      //outSlaveAxi4.bits.data := 0.U
      //outSlaveAxi4.bits.last := false.B
      outSlaveAxi4.ready := outMaster.ready
      outMaster.bits.data := outSlaveAxi4.bits.data
      outMaster.valid := outSlaveAxi4.valid
      outMaster.bits.last := outSlaveAxi4.bits.last
      
      inSlave.ready := memToStreamRequest.get.ready
    
    /********************************************************************************************** AXI-Stream AXI4 Input **************************************************************************************************/
    } else if (useAXIStream && useAXI4Input && !useAXI4Output) {
      
      val inSlave = inSlaveNode.get.in(0)._1
      val inMasterAxiS = inMasterNodeAxiS.get.out(0)._1
      val inMasterAxi4 = inMasterNodeAxi4.get.out(0)._1
      val outSlaveAxiS = outSlaveNodeAxiS.get.in(0)._1
      val outMaster = outMasterNode.get.out(0)._1
      
      outSlaveAxiS.ready := outMaster.ready
      outMaster.bits.data := outSlaveAxiS.bits.data
      outMaster.valid := outSlaveAxiS.valid
      outMaster.bits.last := outSlaveAxiS.bits.last
      
      when(indicator.get === 0.U) {
        inSlave.ready := inMasterAxiS.ready
        inMasterAxiS.bits.data := inSlave.bits.data
        inMasterAxiS.valid := inSlave.valid
        inMasterAxiS.bits.last := inSlave.bits.last
        
        streamToMemRequest.get.valid := false.B
        inMasterAxiS.valid := false.B
      }.elsewhen(indicator.get === 1.U) {
        val indicator2 = RegInit(Bool(), false.B)
      
        when(inMasterAxi4.fire() || streamToMemRequest.get.fire()) {
          indicator2 := !indicator2
        }
        
        streamToMemRequest.get.bits.length := 0.U
        streamToMemRequest.get.bits.cycles := 0.U
        streamToMemRequest.get.bits.fixedAddress := 1.U
        streamToMemRequest.get.valid := Mux(indicator2, false.B, inSlave.valid)
        streamToMemRequest.get.bits.baseAddress := Mux(indicator2, 0.U, inSlave.bits.data)
        
        inMasterAxi4.valid := Mux(indicator2, inSlave.valid, false.B)
        inMasterAxi4.bits.data := Mux(indicator2, inSlave.bits.data, 0.U)
        inMasterAxi4.bits.last := Mux(indicator2, inSlave.bits.last, false.B)
        inSlave.ready := (indicator2 && inMasterAxi4.ready) || (!indicator2 && streamToMemRequest.get.ready)
        inMasterAxiS.valid := false.B
      }.otherwise {
        inMasterAxiS.valid := false.B
        inMasterAxi4.valid := false.B
        streamToMemRequest.get.valid := false.B
      }
    
    /********************************************************************************************** AXI-Stream AXI4 Output **************************************************************************************************/
    } else if(useAXIStream && !useAXI4Input && useAXI4Output) {
      
      val inSlave = inSlaveNode.get.in(0)._1
      val inMasterAxiS = inMasterNodeAxiS.get.out(0)._1
      val outSlaveAxiS = outSlaveNodeAxiS.get.in(0)._1
      val outSlaveAxi4 = outSlaveNodeAxi4.get.in(0)._1
      val outMaster = outMasterNode.get.out(0)._1
      
      //outSlave.ready := outMaster.ready
      //outMaster.bits.data := outSlave.bits.data
      //outMaster.valid := outSlave.valid
      //outMaster.bits.last := outSlave.bits.last
      
      when(indicator.get === 0.U) {
        inSlave.ready := inMasterAxiS.ready
        inMasterAxiS.bits.data := inSlave.bits.data
        inMasterAxiS.valid := inSlave.valid
        inMasterAxiS.bits.last := inSlave.bits.last
        
        outSlaveAxiS.ready := outMaster.ready
        outMaster.bits.data := outSlaveAxiS.bits.data
        outMaster.valid := outSlaveAxiS.valid
        outMaster.bits.last := outSlaveAxiS.bits.last
        
        outSlaveAxi4.ready := false.B
        memToStreamRequest.get.valid := false.B
      }.elsewhen(indicator.get === 2.U) {
        memToStreamRequest.get.bits.length := 0.U
        memToStreamRequest.get.bits.cycles := 0.U
        memToStreamRequest.get.bits.fixedAddress := 1.U
        
        memToStreamRequest.get.valid := inSlave.valid
        memToStreamRequest.get.bits.baseAddress := inSlave.bits.data
        
        //outSlaveAxi4.valid := false.B
        //outSlaveAxi4.bits.data := 0.U
        //outSlaveAxi4.bits.last := false.B
        outSlaveAxi4.ready := outMaster.ready
        outMaster.bits.data := outSlaveAxi4.bits.data
        outMaster.valid := outSlaveAxi4.valid
        outMaster.bits.last := outSlaveAxi4.bits.last
        
        outSlaveAxiS.ready := false.B
        inMasterAxiS.valid := false.B
        inSlave.ready := memToStreamRequest.get.ready
      }.otherwise {
        inMasterAxiS.valid := false.B
        outSlaveAxiS.ready := false.B
        outSlaveAxi4.ready := false.B
        memToStreamRequest.get.valid := false.B
      }
    
    /********************************************************************************************** AXI4 Input Output **************************************************************************************************/
    } else if(!useAXIStream && useAXI4Input && useAXI4Output) {
      
      val inSlave = inSlaveNode.get.in(0)._1
      val inMasterAxi4 = inMasterNodeAxi4.get.out(0)._1
      val outSlaveAxi4 = outSlaveNodeAxi4.get.in(0)._1
      val outMaster = outMasterNode.get.out(0)._1
      
      //outSlave.ready := outMaster.ready
      //outMaster.bits.data := outSlave.bits.data
      //outMaster.valid := outSlave.valid
      //outMaster.bits.last := outSlave.bits.last
      
      when(indicator.get === 1.U) {
        val indicator2 = RegInit(Bool(), false.B)
      
        when(inMasterAxi4.fire() || streamToMemRequest.get.fire()) {
          indicator2 := !indicator2
        }
        
        streamToMemRequest.get.bits.length := 0.U
        streamToMemRequest.get.bits.cycles := 0.U
        streamToMemRequest.get.bits.fixedAddress := 1.U
        streamToMemRequest.get.valid := Mux(indicator2, false.B, inSlave.valid)
        streamToMemRequest.get.bits.baseAddress := Mux(indicator2, 0.U, inSlave.bits.data)
        
        inMasterAxi4.valid := Mux(indicator2, inSlave.valid, false.B)
        inMasterAxi4.bits.data := Mux(indicator2, inSlave.bits.data, 0.U)
        inMasterAxi4.bits.last := Mux(indicator2, inSlave.bits.last, false.B)
        inSlave.ready := (indicator2 && inMasterAxi4.ready) || (!indicator2 && streamToMemRequest.get.ready)
        
        memToStreamRequest.get.valid := false.B
      }.elsewhen(indicator.get === 2.U) {
        memToStreamRequest.get.bits.length := 0.U
        memToStreamRequest.get.bits.cycles := 0.U
        memToStreamRequest.get.bits.fixedAddress := 1.U
        
        memToStreamRequest.get.valid := inSlave.valid
        memToStreamRequest.get.bits.baseAddress := inSlave.bits.data
        
        outSlaveAxi4.ready := outMaster.ready
        outMaster.bits.data := outSlaveAxi4.bits.data
        outMaster.valid := outSlaveAxi4.valid
        outMaster.bits.last := outSlaveAxi4.bits.last
        
        inSlave.ready := memToStreamRequest.get.ready
        
        inMasterAxi4.valid := false.B
        streamToMemRequest.get.valid := false.B
      }.otherwise {
        inMasterAxi4.valid := false.B
        streamToMemRequest.get.valid := false.B
        memToStreamRequest.get.valid := false.B
      }
    
    /*********************************************************************************************** AXIS AXI4 Input Output ***********************************************************************************************/
    } else if(useAXIStream && useAXI4Input && useAXI4Output) {
      
      val inSlave = inSlaveNode.get.in(0)._1
      val inMasterAxiS = inMasterNodeAxiS.get.out(0)._1
      val inMasterAxi4 = inMasterNodeAxi4.get.out(0)._1
      val outSlaveAxiS = outSlaveNodeAxiS.get.in(0)._1
      val outSlaveAxi4 = outSlaveNodeAxi4.get.in(0)._1
      val outMaster = outMasterNode.get.out(0)._1
      
      //outSlave.ready := outMaster.ready
      //outMaster.bits.data := outSlave.bits.data
      //outMaster.valid := outSlave.valid
      //outMaster.bits.last := outSlave.bits.last
      
      when(indicator.get === 0.U) {
        inSlave.ready := inMasterAxiS.ready
        inMasterAxiS.bits.data := inSlave.bits.data
        inMasterAxiS.valid := inSlave.valid
        inMasterAxiS.bits.last := inSlave.bits.last
        
        outSlaveAxiS.ready := outMaster.ready
        outMaster.bits.data := outSlaveAxiS.bits.data
        outMaster.valid := outSlaveAxiS.valid
        outMaster.bits.last := outSlaveAxiS.bits.last
        
        inMasterAxi4.valid := false.B
        outSlaveAxi4.ready := false.B
        streamToMemRequest.get.valid := false.B
        memToStreamRequest.get.valid := false.B
      }.elsewhen(indicator.get === 1.U) {
        val indicator2 = RegInit(Bool(), false.B)
        when(inMasterAxi4.fire() || streamToMemRequest.get.fire()) {
          indicator2 := !indicator2
        }
        
        outSlaveAxiS.ready := outMaster.ready
        outMaster.bits.data := outSlaveAxiS.bits.data
        outMaster.valid := outSlaveAxiS.valid
        outMaster.bits.last := outSlaveAxiS.bits.last
        
        streamToMemRequest.get.bits.length := 0.U
        streamToMemRequest.get.bits.cycles := 0.U
        streamToMemRequest.get.bits.fixedAddress := 1.U
        streamToMemRequest.get.valid := Mux(indicator2, false.B, inSlave.valid)
        streamToMemRequest.get.bits.baseAddress := Mux(indicator2, 0.U, inSlave.bits.data)
        
        inMasterAxi4.valid := Mux(indicator2, inSlave.valid, false.B)
        inMasterAxi4.bits.data := Mux(indicator2, inSlave.bits.data, 0.U)
        inMasterAxi4.bits.last := Mux(indicator2, inSlave.bits.last, false.B)
        inSlave.ready := (indicator2 && inMasterAxi4.ready) || (!indicator2 && streamToMemRequest.get.ready)
        
        inMasterAxiS.valid := false.B
        outSlaveAxi4.ready := false.B
        memToStreamRequest.get.valid := false.B
      }.elsewhen(indicator.get === 2.U) {
        memToStreamRequest.get.bits.length := 0.U
        memToStreamRequest.get.bits.cycles := 0.U
        memToStreamRequest.get.bits.fixedAddress := 1.U
        
        memToStreamRequest.get.valid := inSlave.valid
        memToStreamRequest.get.bits.baseAddress := inSlave.bits.data
        
        //outSlave.valid := false.B
        //outSlave.bits.data := 0.U
        //outSlave.bits.last := false.B
        outSlaveAxi4.ready := outMaster.ready
        outMaster.bits.data := outSlaveAxi4.bits.data
        outMaster.valid := outSlaveAxi4.valid
        outMaster.bits.last := outSlaveAxi4.bits.last
        
        inSlave.ready := memToStreamRequest.get.ready
        
        inMasterAxiS.valid := false.B
        outSlaveAxiS.ready := false.B
        streamToMemRequest.get.valid := false.B
      }.otherwise {
        inMasterAxiS.valid := false.B
        inMasterAxi4.valid := false.B
        outSlaveAxiS.ready := false.B
        outSlaveAxi4.ready := false.B
        streamToMemRequest.get.valid := false.B
        memToStreamRequest.get.valid := false.B
      }
      
    }
    
  }

}


trait AXI4StreamRepackingPins extends AXI4StreamRepacking {
  
    val ioInSlaveNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes, i = 0)))
    inSlaveNode.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInSlaveNode
    val in = InModuleBody { ioInSlaveNode.makeIO() }
    
    val outAxiS = 
      if(useAXIStream || useAXI4Input) Some({
        val ioInMasterNode = BundleBridgeSink[AXI4StreamBundle]()
        ioInMasterNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := inMasterNodeAxiS.get
        InModuleBody { ioInMasterNode.makeIO() }
      })
    else None
    
    val outAxi4 = 
      if(useAXI4Output) Some({
        val ioInMasterNode = BundleBridgeSink[AXI4StreamBundle]()
        ioInMasterNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := inMasterNodeAxi4.get
        InModuleBody { ioInMasterNode.makeIO() }
      })
    else None
    
    val inAxiS = 
      if(useAXIStream) Some({
        val ioOutSlaveNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes, i = 0)))
        outSlaveNodeAxiS.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioOutSlaveNode
        InModuleBody { ioOutSlaveNode.makeIO() }
      })
    else None
    
    val inAxi4 = 
      if(useAXI4Input || useAXI4Output) Some({
        val ioOutSlaveNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes, i = 0)))
        outSlaveNodeAxi4.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioOutSlaveNode
        InModuleBody { ioOutSlaveNode.makeIO() }
      })
    else None
    
    val ioOutMasterNode = BundleBridgeSink[AXI4StreamBundle]()
    ioOutMasterNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outMasterNode.get
    val out = InModuleBody { ioOutMasterNode.makeIO() }
}


object AXI4StreamRepackingApp extends App
{ 
  
  val beatBytes = 4
  val useAXIStream = true
  val useAXI4Input = true
  val useAXI4Output = true
  
  val lazyDut = LazyModule(new AXI4StreamRepacking(useAXIStream = useAXIStream, useAXI4Input = useAXI4Input, useAXI4Output = useAXI4Output, beatBytes = beatBytes) with AXI4StreamRepackingPins)
  chisel3.Driver.execute(Array("--target-dir", "verilog/AXI4StreamRepacking"), () => lazyDut.module)
}
