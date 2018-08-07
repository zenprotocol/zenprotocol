#light "off"
module Z057940919d3069153e500baa59a0499b0796f424fb68c2531b9fe7dfd6b57f36
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____99  :  'Auu____48 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____49 ) ( sender  :  'Auu____50 ) ( messageBody  :  'Auu____51 ) ( wallet  :  'Auu____52 ) ( state  :  'Auu____53 ) -> (Zen.Cost.Realized.inc 128L 20L (

let asset = Zen.Asset.zenAsset
in (

let lock = Zen.Types.Extracted.ContractLock (contractId)
in (

let spend = {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = (FStar.UInt64.uint_to_t 1000L)}
in (

let pInput = Zen.Types.Extracted.Mint (spend)
in (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.addInput pInput txSkeleton) (Zen.TxSkeleton.lockToContract spend.asset spend.amount contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))))))


let cf = (fun ( uu____259  :  'Auu____218 ) ( uu____260  :  'Auu____219 ) ( uu____261  :  'Auu____220 ) ( uu____262  :  'Auu____221 ) ( uu____263  :  'Auu____222 ) ( uu____264  :  'Auu____223 ) ( uu____265  :  'Auu____224 ) -> (Zen.Cost.Realized.inc 0L 9L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 148L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (9L, cf), main)




