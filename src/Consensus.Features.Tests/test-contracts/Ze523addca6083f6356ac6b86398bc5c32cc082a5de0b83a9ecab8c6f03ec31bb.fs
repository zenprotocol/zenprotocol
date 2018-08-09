#light "off"
module Ze523addca6083f6356ac6b86398bc5c32cc082a5de0b83a9ecab8c6f03ec31bb
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____100  :  'Auu____49 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____50 ) ( sender  :  'Auu____51 ) ( messageBody  :  'Auu____52 ) ( wallet  :  'Auu____53 ) ( state  :  'Auu____54 ) -> (Zen.Cost.Realized.inc 192L 25L (Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (

let spend = {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = (FStar.UInt64.uint_to_t 1000L)}
in (

let lock = Zen.Types.Extracted.ContractLock (contractId)
in (

let output = {Zen.Types.Extracted.lock = lock; Zen.Types.Extracted.spend = spend}
in (

let pInput = Zen.Types.Extracted.Mint (spend)
in (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.addInput pInput txSkeleton) (Zen.TxSkeleton.lockToContract spend.asset spend.amount contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))))))))


let cf = (fun ( uu____284  :  'Auu____243 ) ( uu____285  :  'Auu____244 ) ( uu____286  :  'Auu____245 ) ( uu____287  :  'Auu____246 ) ( uu____288  :  'Auu____247 ) ( uu____289  :  'Auu____248 ) ( uu____290  :  'Auu____249 ) -> (Zen.Cost.Realized.inc 0L 13L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 217L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (13L, cf), main)




