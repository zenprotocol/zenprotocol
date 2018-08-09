#light "off"
module Zed0807565f65ced941a922afeedc06a2f3b6197ec19ded7d5679ba951afa4225
open Prims
open FStar.Pervasives

let buy : Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.contractId  ->  Zen.Types.Extracted.lock  ->  (Zen.Types.Main.contractReturn FStar.Pervasives.result, unit) Zen.Cost.Realized.cost = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( contractId  :  Zen.Types.Extracted.contractId ) ( returnAddress  :  Zen.Types.Extracted.lock ) -> (Zen.Cost.Realized.inc 320L 27L (Zen.Cost.Extracted.letBang 64L 256L (Zen.Asset.getDefault contractId) (fun ( contractToken  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 64L 192L (Zen.TxSkeleton.getAvailableTokens Zen.Asset.zenAsset txSkeleton) (fun ( amount  :  FStar.UInt64.t ) -> (Zen.Cost.Extracted.letBang 192L 0L (Zen.Cost.Realized.bind 128L 64L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.lockToContract Zen.Asset.zenAsset amount contractId txSkeleton) (Zen.TxSkeleton.mint amount contractToken)) (Zen.TxSkeleton.lockToAddress contractToken amount returnAddress)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})))))))))


let redeem : Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.contractId  ->  Zen.Types.Extracted.lock  ->  Zen.Types.Realized.wallet  ->  (Zen.Types.Main.contractReturn FStar.Pervasives.result, unit) Zen.Cost.Realized.cost = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( contractId  :  Zen.Types.Extracted.contractId ) ( returnAddress  :  Zen.Types.Extracted.lock ) ( wallet  :  Zen.Types.Realized.wallet ) -> (Zen.Cost.Realized.inc (64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L))) 33L (Zen.Cost.Extracted.letBang 64L (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L)) (Zen.Asset.getDefault contractId) (fun ( contractToken  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 64L ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) (Zen.TxSkeleton.getAvailableTokens contractToken txSkeleton) (fun ( amount  :  FStar.UInt64.t ) -> (Zen.Cost.Extracted.letBang (128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) 0L (Zen.Cost.Realized.bind 128L ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L) (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.destroy amount contractToken txSkeleton) (Zen.TxSkeleton.lockToAddress Zen.Asset.zenAsset amount returnAddress)) (Zen.TxSkeleton.fromWallet Zen.Asset.zenAsset amount contractId wallet)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton FStar.Pervasives.Native.option ) -> (

let result = (match (txSkeleton1) with
| FStar.Pervasives.Native.Some (tx) -> begin
(Zen.Base.op_At (fun ( _0_16  :  Zen.Types.Main.contractReturn ) -> FStar.Pervasives.Native.Some (_0_16)) {Zen.Types.Main.tx = tx; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})
end
| FStar.Pervasives.Native.None -> begin
FStar.Pervasives.Native.None
end)
in (Zen.ResultT.ofOption "contract doesn\'t have enough zens tokens"B result))))))))))


let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____357  :  'Auu____307 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  Prims.string ) ( sender  :  'Auu____308 ) ( messageBody  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( wallet  :  Zen.Types.Realized.wallet ) ( state  :  'Auu____309 ) -> (Zen.Cost.Realized.inc (70L + ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L))) + 33L)) 31L (Zen.Cost.Extracted.letBang 70L ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L))) + 33L) (Zen.Data.op_Greater_Question_Equals 68L 2L (Zen.Data.op_Greater_Question_Equals 4L 64L (Zen.Data.op_Greater_Bang_Equals 4L messageBody Zen.Data.tryDict) (Zen.Dictionary.tryFind "returnAddress"B)) Zen.Data.tryLock) (fun ( returnAddress  :  Zen.Types.Extracted.lock FStar.Pervasives.Native.option ) -> (match (returnAddress) with
| FStar.Pervasives.Native.Some (returnAddress1) -> begin
(match ((Prims.op_Equality command "redeem"B)) with
| true -> begin
(redeem txSkeleton contractId returnAddress1 wallet)
end
| uu____410 -> begin
(match (((Prims.op_Equality command ""B) || (Prims.op_Equality command "buy"B))) with
| true -> begin
(Zen.Base.op_Bar_Greater (buy txSkeleton contractId returnAddress1) (Zen.Cost.Extracted.autoInc 347L ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L))) + 33L)))
end
| uu____445 -> begin
(Zen.ResultT.autoFailw ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L))) + 33L) "unsupported command"B)
end)
end)
end
| FStar.Pervasives.Native.None -> begin
(Zen.ResultT.autoFailw ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L))) + 33L) "returnAddress is required"B)
end)))))


let cf = (fun ( uu____525  :  'Auu____485 ) ( uu____526  :  'Auu____486 ) ( uu____527  :  'Auu____487 ) ( uu____528  :  'Auu____488 ) ( uu____529  :  'Auu____489 ) ( wallet  :  Zen.Types.Realized.wallet ) ( uu____531  :  'Auu____490 ) -> (Zen.Cost.Realized.inc 0L 30L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater ((70L + ((64L + (64L + ((128L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L))) + 33L)) + 31L) Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (30L, cf), main)




