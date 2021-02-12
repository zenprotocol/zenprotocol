#light "off"
module Z60de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____98  :  'Auu____47 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____48 ) ( sender  :  'Auu____49 ) ( messageBody  :  'Auu____50 ) ( wallet  :  'Auu____51 ) ( state  :  'Auu____52 ) -> (Zen.Cost.Realized.inc 192L 22L (Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (

let spend = {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = (FStar.UInt64.uint_to_t 1000L)}
in (

let pInput = Zen.Types.Extracted.Mint (spend)
in (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.addInput pInput txSkeleton) (Zen.TxSkeleton.lockToContract spend.asset spend.amount contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))))))


let cf = (fun ( uu____266  :  'Auu____225 ) ( uu____267  :  'Auu____226 ) ( uu____268  :  'Auu____227 ) ( uu____269  :  'Auu____228 ) ( uu____270  :  'Auu____229 ) ( uu____271  :  'Auu____230 ) ( uu____272  :  'Auu____231 ) -> (Zen.Cost.Realized.inc 0L 13L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 214L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (13L, cf), main)




