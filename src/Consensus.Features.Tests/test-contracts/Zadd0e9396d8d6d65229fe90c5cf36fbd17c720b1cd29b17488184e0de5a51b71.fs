#light "off"
module Zadd0e9396d8d6d65229fe90c5cf36fbd17c720b1cd29b17488184e0de5a51b71
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____96  :  'Auu____46 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  Prims.string ) ( sender  :  Zen.Types.Main.sender ) ( messageBody  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( wallet  :  'Auu____47 ) ( state  :  'Auu____48 ) -> (Zen.Cost.Realized.inc 332L 57L (match ((Prims.op_Equality command "mint"B)) with
| true -> begin
(match (sender) with
| Zen.Types.Main.Contract (contract') -> begin
(Zen.ResultT.autoFailw 332L "unexpected sender: Contract"B)
end
| Zen.Types.Main.PK (pk) -> begin
(Zen.ResultT.autoFailw 332L "unexpected sender: PK"B)
end
| Zen.Types.Main.Anonymous -> begin
(Zen.Cost.Extracted.letBang 70L 262L (Zen.Data.op_Greater_Question_Equals 68L 2L (Zen.Data.op_Greater_Question_Equals 4L 64L (Zen.Data.op_Greater_Bang_Equals 4L messageBody Zen.Data.tryDict) (Zen.Dictionary.tryFind "returnAddress"B)) Zen.Data.tryLock) (fun ( returnAddress  :  Zen.Types.Extracted.lock FStar.Pervasives.Native.option ) -> (Zen.Cost.Extracted.letBang 70L 192L (Zen.Data.op_Greater_Question_Equals 68L 2L (Zen.Data.op_Greater_Question_Equals 4L 64L (Zen.Data.op_Greater_Bang_Equals 4L messageBody Zen.Data.tryDict) (Zen.Dictionary.tryFind "amount"B)) Zen.Data.tryU64) (fun ( amount  :  FStar.UInt64.t FStar.Pervasives.Native.option ) -> (match (((returnAddress), (amount))) with
| (FStar.Pervasives.Native.Some (returnAddress1), FStar.Pervasives.Native.Some (amount1)) -> begin
(Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (

let spend = {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = amount1}
in (

let pInput = Zen.Types.Extracted.Mint (spend)
in (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.addInput pInput txSkeleton) (Zen.TxSkeleton.lockToAddress spend.asset spend.amount returnAddress1)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))))
end
| uu____230 -> begin
(Zen.ResultT.autoFailw 192L "both returnAddress and amount are required"B)
end)))))
end)
end
| uu____239 -> begin
(Zen.ResultT.autoFailw 332L "unsupported command"B)
end)))


let cf = (fun ( uu____323  :  'Auu____282 ) ( uu____324  :  'Auu____283 ) ( uu____325  :  'Auu____284 ) ( uu____326  :  'Auu____285 ) ( uu____327  :  'Auu____286 ) ( uu____328  :  'Auu____287 ) ( uu____329  :  'Auu____288 ) -> (Zen.Cost.Realized.inc 0L 25L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 389L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (25L, cf), main)




