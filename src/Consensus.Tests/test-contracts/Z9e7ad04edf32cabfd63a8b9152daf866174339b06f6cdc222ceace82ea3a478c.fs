#light "off"
module Z9e7ad04edf32cabfd63a8b9152daf866174339b06f6cdc222ceace82ea3a478c
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____99  :  'Auu____48 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____49 ) ( sender  :  'Auu____50 ) ( messageBody  :  'Auu____51 ) ( wallet  :  'Auu____52 ) ( state  :  'Auu____53 ) -> (Zen.Cost.Realized.inc 192L 23L (Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (

let lock = Zen.Types.Extracted.ContractLock (contractId)
in (

let spend = {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = (FStar.UInt64.uint_to_t 0L)}
in (

let pInput = Zen.Types.Extracted.Mint (spend)
in (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.addInput pInput txSkeleton) (Zen.TxSkeleton.lockToContract spend.asset spend.amount contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange}))))))))))


let cf = (fun ( uu____281  :  'Auu____240 ) ( uu____282  :  'Auu____241 ) ( uu____283  :  'Auu____242 ) ( uu____284  :  'Auu____243 ) ( uu____285  :  'Auu____244 ) ( uu____286  :  'Auu____245 ) ( uu____287  :  'Auu____246 ) -> (Zen.Cost.Realized.inc 0L 13L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 215L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (13L, cf), main)




