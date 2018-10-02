#light "off"
module Z3a18eaefff885ba65d4b976dce8ec09543addb5c5e9bfb2c2524f133b486c521
open Prims
open FStar.Pervasives

let buy : Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.contractId  ->  Zen.Types.Extracted.lock  ->  (Zen.Types.Main.contractResult, unit) Zen.Cost.Realized.cost = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( contractId  :  Zen.Types.Extracted.contractId ) ( returnAddress  :  Zen.Types.Extracted.lock ) -> (Zen.Cost.Realized.inc 323L 23L (Zen.Cost.Extracted.letBang 64L 259L (Zen.Asset.getDefault contractId) (fun ( contractToken  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 64L 195L (Zen.TxSkeleton.getAvailableTokens Zen.Asset.zenAsset txSkeleton) (fun ( amount  :  FStar.UInt64.t ) -> (Zen.Cost.Extracted.letBang 192L 3L (Zen.Cost.Realized.bind 128L 64L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.lockToContract Zen.Asset.zenAsset amount contractId txSkeleton) (Zen.TxSkeleton.mint amount contractToken)) (Zen.TxSkeleton.lockToAddress contractToken amount returnAddress)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.ContractResult.ofTxSkel txSkeleton1)))))))))


let redeem : Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.contractId  ->  Zen.Types.Extracted.lock  ->  Zen.Types.Realized.wallet  ->  (Zen.Types.Main.contractResult, unit) Zen.Cost.Realized.cost = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( contractId  :  Zen.Types.Extracted.contractId ) ( returnAddress  :  Zen.Types.Extracted.lock ) ( wallet  :  Zen.Types.Realized.wallet ) -> (Zen.Cost.Realized.inc (64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L))) 25L (Zen.Cost.Extracted.letBang 64L (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L)) (Zen.Asset.getDefault contractId) (fun ( contractToken  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 64L ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L) (Zen.TxSkeleton.getAvailableTokens contractToken txSkeleton) (fun ( amount  :  FStar.UInt64.t ) -> (Zen.Cost.Extracted.letBang (128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) 3L (Zen.Cost.Realized.bind 128L ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L) (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.destroy amount contractToken txSkeleton) (Zen.TxSkeleton.lockToAddress Zen.Asset.zenAsset amount returnAddress)) (Zen.TxSkeleton.fromWallet Zen.Asset.zenAsset amount contractId wallet)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton FStar.Pervasives.Native.option ) -> (Zen.ContractResult.ofOptionTxSkel "This contract doesn\'t have enough ZP in order to redeem the requested amount"B txSkeleton1)))))))))


let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____316  :  'Auu____268 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  Prims.string ) ( sender  :  'Auu____269 ) ( messageBody  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( wallet  :  Zen.Types.Realized.wallet ) ( state  :  'Auu____270 ) -> (Zen.Cost.Realized.inc (70L + ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L))) + 25L)) 31L (Zen.Cost.Extracted.letBang 70L ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L))) + 25L) (Zen.Data.op_Greater_Question_Equals 68L 2L (Zen.Data.op_Greater_Question_Equals 4L 64L (Zen.Data.op_Greater_Bang_Equals 4L messageBody Zen.Data.tryDict) (Zen.Dictionary.tryFind "returnAddress"B)) Zen.Data.tryLock) (fun ( returnAddress  :  Zen.Types.Extracted.lock FStar.Pervasives.Native.option ) -> (match (returnAddress) with
| FStar.Pervasives.Native.Some (returnAddress1) -> begin
(match ((Prims.op_Equality command "redeem"B)) with
| true -> begin
(redeem txSkeleton contractId returnAddress1 wallet)
end
| uu____361 -> begin
(match (((Prims.op_Equality command ""B) || (Prims.op_Equality command "buy"B))) with
| true -> begin
(Zen.Base.op_Bar_Greater (buy txSkeleton contractId returnAddress1) (Zen.Cost.Extracted.autoInc 346L ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L))) + 25L)))
end
| uu____388 -> begin
(Zen.ResultT.autoFailw ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L))) + 25L) "unsupported command"B)
end)
end)
end
| FStar.Pervasives.Native.None -> begin
(Zen.ResultT.autoFailw ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L))) + 25L) "returnAddress is required"B)
end)))))


let cf = (fun ( uu____468  :  'Auu____428 ) ( uu____469  :  'Auu____429 ) ( uu____470  :  'Auu____430 ) ( uu____471  :  'Auu____431 ) ( uu____472  :  'Auu____432 ) ( wallet  :  Zen.Types.Realized.wallet ) ( uu____474  :  'Auu____433 ) -> (Zen.Cost.Realized.inc 0L 28L (Zen.Base.op_Bar_Greater ((70L + ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 3L))) + 25L)) + 31L) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (28L, cf), main)




