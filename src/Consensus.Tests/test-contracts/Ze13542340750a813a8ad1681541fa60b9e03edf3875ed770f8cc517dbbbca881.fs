#light "off"
module Ze13542340750a813a8ad1681541fa60b9e03edf3875ed770f8cc517dbbbca881
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____94  :  'Auu____43 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  Prims.string ) ( sender  :  'Auu____44 ) ( messageBody  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( wallet  :  'Auu____45 ) ( state  :  'Auu____46 ) -> (Zen.Cost.Realized.inc 256L 33L (match ((Prims.op_Equality command "contract1_test"B)) with
| true -> begin
(Zen.Cost.Extracted.letBang 64L 192L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 128L 64L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.mint (FStar.UInt64.uint_to_t 25L) asset txSkeleton) (Zen.TxSkeleton.lockToContract asset (FStar.UInt64.uint_to_t 25L) contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Cost.Extracted.letBang 64L 0L (Zen.ContractId.parse "0000000044af513a65616c543b1c3267bc97a12fe0071226725d443c392b7ae808b4e3e9"B) (fun ( contractId1  :  Zen.Types.Extracted.contractId FStar.Pervasives.Native.option ) -> (match (contractId1) with
| FStar.Pervasives.Native.Some (contractId2) -> begin
(

let message = {Zen.Types.Main.recipient = contractId2; Zen.Types.Main.command = "contract2_test"B; Zen.Types.Main.body = messageBody}
in (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.Some (message); Zen.Types.Main.state = Zen.Types.Main.NoChange}))
end
| FStar.Pervasives.Native.None -> begin
(Zen.ResultT.autoFailw 0L "could not parse contractId from string"B)
end)))))))
end
| uu____225 -> begin
(Zen.ResultT.autoFailw 256L "unsupported command"B)
end)))


let cf = (fun ( uu____309  :  'Auu____268 ) ( uu____310  :  'Auu____269 ) ( uu____311  :  'Auu____270 ) ( uu____312  :  'Auu____271 ) ( uu____313  :  'Auu____272 ) ( uu____314  :  'Auu____273 ) ( uu____315  :  'Auu____274 ) -> (Zen.Cost.Realized.inc 0L 15L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 289L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (15L, cf), main)




