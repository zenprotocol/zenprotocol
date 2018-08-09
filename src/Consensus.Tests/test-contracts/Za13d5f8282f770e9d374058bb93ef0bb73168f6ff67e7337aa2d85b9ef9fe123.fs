#light "off"
module Za13d5f8282f770e9d374058bb93ef0bb73168f6ff67e7337aa2d85b9ef9fe123
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____96  :  'Auu____45 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____46 ) ( sender  :  'Auu____47 ) ( messageBody  :  'Auu____48 ) ( wallet  :  'Auu____49 ) ( state  :  'Auu____50 ) -> (Zen.Cost.Realized.inc 128L 13L (Zen.Cost.Extracted.letBang 64L 64L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 64L 0L (Zen.TxSkeleton.destroy (FStar.UInt64.uint_to_t 1000L) asset txSkeleton) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))))


let cf = (fun ( uu____244  :  'Auu____203 ) ( uu____245  :  'Auu____204 ) ( uu____246  :  'Auu____205 ) ( uu____247  :  'Auu____206 ) ( uu____248  :  'Auu____207 ) ( uu____249  :  'Auu____208 ) ( uu____250  :  'Auu____209 ) -> (Zen.Cost.Realized.inc 0L 11L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 141L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (11L, cf), main)




