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

import gbemac._


class SimpleMultiplexerBlock

class EthernetToAxi(val useAXIStream: Boolean = true, val useAXI4Input: Boolean = false, val useAXI4Output: Boolean = false, val paramsGbemac: GbEMACConfigParams, gbemacAddress: AddressSet, val beatBytes: Int) extends LazyModule()(Parameters.empty) {

  
  val gbemac = LazyModule(new GbemacWrapper(paramsGbemac, gbemacAddress, beatBytes))
  
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)

  gbemac.mem.get := AXI4Buffer() := bus.node
  
  val dma = if(useAXI4Input || useAXI4Output) Some(LazyModule(new StreamingAXI4DMA())) else None
  val AxiMultiplexerBlock = LazyModule(new AXI4StreamRepacking(useAXIStream = useAXIStream, useAXI4Input = useAXI4Input, useAXI4Output = useAXI4Output, beatBytes = beatBytes))
  
  if(useAXI4Input || useAXI4Output) {
    mem.get := dma.get.axiNode
  }
  if(useAXI4Input) {
    dma.get.streamNode := AxiMultiplexerBlock.inMasterNodeAxi4.get
  }
  if(useAXI4Output) {
    AxiMultiplexerBlock.outSlaveNodeAxi4.get := dma.get.streamNode
  }
  
  AxiMultiplexerBlock.inSlaveNode.get := gbemac.masterNode.get
  gbemac.slaveNode.get := AxiMultiplexerBlock.outMasterNode.get
  
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new GbemacWrapperIO)
    io <> gbemac.module.io
    
    if(useAXI4Input) {
      dma.get.module.enable := 1.U
      dma.get.module.watchdogInterval := 1000.U
      
      dma.get.module.streamToMemRequest.bits.length := AxiMultiplexerBlock.module.streamToMemRequest.get.bits.length
      dma.get.module.streamToMemRequest.bits.cycles := AxiMultiplexerBlock.module.streamToMemRequest.get.bits.cycles
      dma.get.module.streamToMemRequest.bits.fixedAddress := AxiMultiplexerBlock.module.streamToMemRequest.get.bits.fixedAddress
      dma.get.module.streamToMemRequest.valid := AxiMultiplexerBlock.module.streamToMemRequest.get.valid
      dma.get.module.streamToMemRequest.bits.baseAddress := AxiMultiplexerBlock.module.streamToMemRequest.get.bits.baseAddress
      AxiMultiplexerBlock.module.streamToMemRequest.get.ready := dma.get.module.streamToMemRequest.ready
      
      dma.get.module.arprot := 0.U
      dma.get.module.awprot := 0.U
      dma.get.module.arcache := 0.U
      dma.get.module.awcache := 0.U
    }
    if(useAXI4Output) {
      dma.get.module.memToStreamRequest.valid := AxiMultiplexerBlock.module.memToStreamRequest.get.valid
      dma.get.module.memToStreamRequest.bits.baseAddress := AxiMultiplexerBlock.module.memToStreamRequest.get.bits.baseAddress
      dma.get.module.memToStreamRequest.bits.length := AxiMultiplexerBlock.module.memToStreamRequest.get.bits.length
      dma.get.module.memToStreamRequest.bits.cycles := AxiMultiplexerBlock.module.memToStreamRequest.get.bits.cycles
      dma.get.module.memToStreamRequest.bits.fixedAddress := AxiMultiplexerBlock.module.memToStreamRequest.get.bits.fixedAddress
      AxiMultiplexerBlock.module.memToStreamRequest.get.ready := dma.get.module.memToStreamRequest.ready
      
      dma.get.module.arprot := 0.U
      dma.get.module.awprot := 0.U
      dma.get.module.arcache := 0.U
      dma.get.module.awcache := 0.U
    }
    
    
    val indicator = if ((useAXIStream && useAXI4Input) || (useAXIStream && useAXI4Output) || (useAXI4Input && useAXI4Output)) Some(
      IO(Input(UInt(2.W)))
    ) else None
    if ((useAXIStream && useAXI4Input) || (useAXIStream && useAXI4Output) || (useAXI4Input && useAXI4Output)) {
      AxiMultiplexerBlock.module.indicator.get := indicator.get
    }
  }

}


trait EthernetToAxiPins extends EthernetToAxi {
  
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
  val outAxiS =
    if(useAXIStream || useAXI4Input) Some({
      val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
      ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := AxiMultiplexerBlock.inMasterNodeAxiS.get
      //val outDma = InModuleBody { ioOutNode.makeIO() }
      InModuleBody { ioOutNode.makeIO() }
    })
    else None
  
  val inAxiS = 
    if(useAXIStream) Some({
      val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
      AxiMultiplexerBlock.outSlaveNodeAxiS.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
      InModuleBody { ioInNode.makeIO() }
    })
    else None
}


object EthernetToAxiApp extends App {
  
  val beatBytes = 4
  val gbemacAddress = AddressSet(0x20000000, 0xFFF)
  val params = GbEMACConfigParams()
  val useAXIStream = true
  val useAXI4Input = false
  val useAXI4Output = false
  
  
  implicit val p: Parameters = Parameters.empty
  val lazyModule = LazyModule(new EthernetToAxi(useAXIStream, useAXI4Input, useAXI4Output, params, gbemacAddress, beatBytes) with EthernetToAxiPins)
  chisel3.Driver.execute(Array("--target-dir", "./verilog/EthernetToAxi"), ()=> lazyModule.module)
}

