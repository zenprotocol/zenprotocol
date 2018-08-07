#light "off"
module Z0ca41657cc75e5e40cb85c90a0d09706b22323276819cb5563415d83ccf12e68
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____99  :  'Auu____48 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____49 ) ( sender  :  'Auu____50 ) ( messageBody  :  'Auu____51 ) ( wallet  :  'Auu____52 ) ( state  :  'Auu____53 ) -> (Zen.Cost.Realized.inc 256L 28L (

let str = "Test"B
in (match (((FStar.String.length str) < 29L)) with
| true -> begin
(Zen.Cost.Extracted.letBang 64L 192L (Zen.Asset.fromSubtypeString contractId str) (fun ( assetString  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.fromSubtypeInt contractId (FStar.UInt32.uint_to_t 9999999L)) (fun ( assetInt  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.mint (FStar.UInt64.uint_to_t 10L) assetInt txSkeleton) (Zen.TxSkeleton.mint (FStar.UInt64.uint_to_t 20L) assetString)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))))
end
| uu____214 -> begin
(Zen.ResultT.autoFailw 256L "unexpected"B)
end))))


let cf = (fun ( uu____298  :  'Auu____257 ) ( uu____299  :  'Auu____258 ) ( uu____300  :  'Auu____259 ) ( uu____301  :  'Auu____260 ) ( uu____302  :  'Auu____261 ) ( uu____303  :  'Auu____262 ) ( uu____304  :  'Auu____263 ) -> (Zen.Cost.Realized.inc 0L 15L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 284L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (15L, cf), main)




