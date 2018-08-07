#light "off"
module Z3fa7f77b56f083b3013db0ce16381f918efb929074e896a270502ca35cf532d8
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____101  :  'Auu____49 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( uu____103  :  'Auu____50 ) ( uu____104  :  'Auu____51 ) ( uu____105  :  'Auu____52 ) ( uu____106  :  'Auu____53 ) ( state  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) -> (Zen.Cost.Realized.inc 194L 36L (Zen.Cost.Extracted.letBang 64L 130L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (

let spend = {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = (FStar.UInt64.uint_to_t 1000L)}
in (

let lock = Zen.Types.Extracted.ContractLock (contractId)
in (

let output = {Zen.Types.Extracted.lock = lock; Zen.Types.Extracted.spend = spend}
in (

let pInput = Zen.Types.Extracted.Mint (spend)
in (Zen.Cost.Extracted.letBang 128L 2L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.addInput pInput txSkeleton) (Zen.TxSkeleton.lockToContract spend.asset spend.amount contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Cost.Extracted.letBang 2L 0L (Zen.Data.op_Greater_Bang_Equals 2L state Zen.Data.tryU32) (fun ( counter  :  FStar.UInt32.t FStar.Pervasives.Native.option ) -> (

let updatedCounter = (match (counter) with
| FStar.Pervasives.Native.Some (counter1) -> begin
(FStar.UInt32.add_mod counter1 (FStar.UInt32.uint_to_t 1L))
end
| FStar.Pervasives.Native.None -> begin
(FStar.UInt32.uint_to_t 10L)
end)
in (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.Update (Zen.Types.Data.U32 (updatedCounter))}))))))))))))))


let cf = (fun ( uu____298  :  'Auu____257 ) ( uu____299  :  'Auu____258 ) ( uu____300  :  'Auu____259 ) ( uu____301  :  'Auu____260 ) ( uu____302  :  'Auu____261 ) ( uu____303  :  'Auu____262 ) ( uu____304  :  'Auu____263 ) -> (Zen.Cost.Realized.inc 0L 15L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 230L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (15L, cf), main)




