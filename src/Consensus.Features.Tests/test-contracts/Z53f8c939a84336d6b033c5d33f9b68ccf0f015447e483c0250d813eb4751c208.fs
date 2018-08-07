#light "off"
module Z53f8c939a84336d6b033c5d33f9b68ccf0f015447e483c0250d813eb4751c208
open Prims
open FStar.Pervasives

let cf = (fun ( uu____80  :  'Auu____40 ) ( uu____81  :  'Auu____41 ) ( uu____82  :  'Auu____42 ) ( uu____83  :  'Auu____43 ) ( uu____84  :  'Auu____44 ) ( wallet  :  Zen.Types.Realized.wallet ) ( uu____86  :  'Auu____45 ) -> (Zen.Cost.Realized.inc 0L 26L (Zen.Base.op_Bar_Greater (((70L + (((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) + 13L)) + 5L) + 22L) Zen.Cost.Realized.ret)))


let get : Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.contractId  ->  Zen.Types.Realized.wallet  ->  Zen.Types.Extracted.lock  ->  (Zen.Types.Realized.txSkeleton FStar.Pervasives.result, unit) Zen.Cost.Realized.cost = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( contractId  :  Zen.Types.Extracted.contractId ) ( wallet  :  Zen.Types.Realized.wallet ) ( returnAddress  :  Zen.Types.Extracted.lock ) -> (Zen.Cost.Realized.inc ((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) 13L (Zen.Cost.Realized.bind (64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) 0L (Zen.Cost.Realized.bind 64L ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L) (Zen.TxSkeleton.lockToAddress Zen.Asset.zenAsset (FStar.UInt64.uint_to_t 10000L) returnAddress txSkeleton) (Zen.TxSkeleton.fromWallet Zen.Asset.zenAsset (FStar.UInt64.uint_to_t 10000L) contractId wallet)) (Zen.ResultT.ofOption "contract doesn\'t have enough zens to pay you"B))))


let init : Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.contractId  ->  (Zen.Types.Realized.txSkeleton, unit) Zen.Cost.Realized.cost = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( contractId  :  Zen.Types.Extracted.contractId ) -> (Zen.Cost.Realized.inc 128L 8L (Zen.Cost.Extracted.letBang 64L 64L (Zen.TxSkeleton.getAvailableTokens Zen.Asset.zenAsset txSkeleton) (fun ( tokens  :  FStar.UInt64.t ) -> (Zen.TxSkeleton.lockToContract Zen.Asset.zenAsset tokens contractId txSkeleton)))))


let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____271  :  'Auu____221 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  Prims.string ) ( uu____274  :  'Auu____222 ) ( messageBody  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( wallet  :  Zen.Types.Realized.wallet ) ( uu____277  :  'Auu____223 ) -> (Zen.Cost.Realized.inc ((70L + (((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) + 13L)) + 5L) 22L (Zen.ResultT.op_Greater_Greater_Equals (70L + (((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) + 13L)) 5L (match ((Prims.op_disEquality command "init"B)) with
| true -> begin
(Zen.ResultT.op_Greater_Greater_Equals 70L (((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) + 13L) (Zen.Base.op_Bar_Greater (Zen.Data.op_Greater_Question_Equals 68L 2L (Zen.Data.op_Greater_Question_Equals 4L 64L (Zen.Data.op_Greater_Bang_Equals 4L messageBody Zen.Data.tryDict) (Zen.Dictionary.tryFind "returnAddress"B)) Zen.Data.tryLock) (Zen.ResultT.ofOptionT 70L "returnAddress is required"B)) (get txSkeleton contractId wallet))
end
| uu____310 -> begin
(Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater (init txSkeleton contractId) (Zen.ResultT.liftCost 136L)) (Zen.Cost.Extracted.autoInc 136L (70L + (((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) + 13L))))
end) (fun ( tx  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Cost.Realized.inc 0L 5L (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = tx; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange}))))))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (26L, cf), main)




