#light "off"
module Z247fa9ec1dc70db512f46b837df5023c08562ea2ce897cebe531342e3880205e
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____92  :  'Auu____43 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  Prims.string ) ( sender  :  Zen.Types.Main.sender ) ( messageBody  :  'Auu____44 ) ( wallet  :  'Auu____45 ) ( state  :  'Auu____46 ) -> (Zen.Cost.Realized.inc 128L 27L (

let isFromContract = (match (sender) with
| Zen.Types.Main.Contract (contractId') -> begin
(Prims.op_disEquality contractId' contractId)
end
| uu____124 -> begin
false
end)
in (match ((isFromContract && (Prims.op_Equality command "contract2_test"B))) with
| true -> begin
(Zen.Cost.Extracted.letBang 64L 64L (Zen.TxSkeleton.getAvailableTokens Zen.Asset.zenAsset txSkeleton) (fun ( tokens  :  FStar.UInt64.t ) -> (Zen.Cost.Extracted.letBang 64L 0L (Zen.TxSkeleton.lockToContract Zen.Asset.zenAsset tokens contractId txSkeleton) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))
end
| uu____157 -> begin
(Zen.ResultT.autoFailw 128L "unsupported command"B)
end))))


let cf = (fun ( uu____241  :  'Auu____200 ) ( uu____242  :  'Auu____201 ) ( uu____243  :  'Auu____202 ) ( uu____244  :  'Auu____203 ) ( uu____245  :  'Auu____204 ) ( uu____246  :  'Auu____205 ) ( uu____247  :  'Auu____206 ) -> (Zen.Cost.Realized.inc 0L 11L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 155L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (11L, cf), main)




