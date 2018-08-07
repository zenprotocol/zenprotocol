#light "off"
module Zbd366cbde5d947296f869b96c1b20ae2564a749a04bce32b6532f45bcfd5c412
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____96  :  'Auu____46 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____47 ) ( sender  :  Zen.Types.Main.sender ) ( messageBody  :  'Auu____48 ) ( wallet  :  'Auu____49 ) ( state  :  'Auu____50 ) -> (Zen.Cost.Realized.inc 312L 30L (Zen.Cost.Extracted.letBang 120L 192L (Zen.Crypto.parsePublicKey "03f307752a4f1f0fb0f7fcb3055a5a1977a173ec54007f48a78b498ff51307ee47"B) (fun ( pk  :  Zen.Types.Extracted.publicKey FStar.Pervasives.Native.option ) -> (match (sender) with
| Zen.Types.Main.PK (pk') -> begin
(match ((Prims.op_Equality (FStar.Pervasives.Native.Some (pk')) pk)) with
| true -> begin
(Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.getDefault contractId) (fun ( contractAsset  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.mint (FStar.UInt64.uint_to_t 1L) contractAsset txSkeleton) (Zen.TxSkeleton.lockToContract contractAsset (FStar.UInt64.uint_to_t 1L) contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))
end
| uu____229 -> begin
(Zen.ResultT.autoFailw 192L "expected different pk"B)
end)
end
| uu____230 -> begin
(Zen.ResultT.autoFailw 192L "expected pk"B)
end)))))


let cf = (fun ( uu____314  :  'Auu____273 ) ( uu____315  :  'Auu____274 ) ( uu____316  :  'Auu____275 ) ( uu____317  :  'Auu____276 ) ( uu____318  :  'Auu____277 ) ( uu____319  :  'Auu____278 ) ( uu____320  :  'Auu____279 ) -> (Zen.Cost.Realized.inc 0L 15L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 342L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (15L, cf), main)




