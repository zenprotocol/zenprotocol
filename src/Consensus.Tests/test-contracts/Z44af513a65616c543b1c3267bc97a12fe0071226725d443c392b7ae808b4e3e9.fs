#light "off"
module Z44af513a65616c543b1c3267bc97a12fe0071226725d443c392b7ae808b4e3e9
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____93  :  'Auu____43 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  Prims.string ) ( sender  :  'Auu____44 ) ( messageBody  :  'Auu____45 ) ( wallet  :  'Auu____46 ) ( state  :  'Auu____47 ) -> (Zen.Cost.Realized.inc 192L 23L (match ((Prims.op_Equality command "contract2_test"B)) with
| true -> begin
(Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.getDefault contractId) (fun ( contractToken  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.mint (FStar.UInt64.uint_to_t 50L) contractToken txSkeleton) (Zen.TxSkeleton.lockToContract contractToken (FStar.UInt64.uint_to_t 50L) contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))
end
| uu____190 -> begin
(Zen.ResultT.autoFailw 192L "unsupported command"B)
end)))


let cf = (fun ( uu____274  :  'Auu____233 ) ( uu____275  :  'Auu____234 ) ( uu____276  :  'Auu____235 ) ( uu____277  :  'Auu____236 ) ( uu____278  :  'Auu____237 ) ( uu____279  :  'Auu____238 ) ( uu____280  :  'Auu____239 ) -> (Zen.Cost.Realized.inc 0L 13L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 215L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (13L, cf), main)




