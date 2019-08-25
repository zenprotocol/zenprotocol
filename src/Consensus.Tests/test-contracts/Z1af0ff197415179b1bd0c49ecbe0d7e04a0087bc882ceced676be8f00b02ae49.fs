#light "off"
module Z1af0ff197415179b1bd0c49ecbe0d7e04a0087bc882ceced676be8f00b02ae49
open Prims
open FStar.Pervasives

let maxOutputs : Prims.nat = 100L


let payoutBlock : Prims.nat = 10L


let intervalLength : Prims.nat = 100L


let rec tryMap = (fun ( n  :  Prims.nat ) ( f  :  'Aa  ->  ('Ab FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost ) ( ls  :  'Aa Prims.list ) -> (Zen.Cost.Realized.inc (Prims.op_Star (Prims.length ls) (n + 20L)) 20L (match (ls) with
| Prims.Cons (hd, tl) -> begin
(Zen.Cost.Extracted.letBang n (((Prims.op_Star (Prims.length tl) (n + 20L)) + 20L) + 0L) (f hd) (fun ( hd'  :  'Ab FStar.Pervasives.Native.option ) -> (Zen.Cost.Extracted.letBang ((Prims.op_Star (Prims.length tl) (n + 20L)) + 20L) 0L (tryMap n f tl) (fun ( tl'  :  'Ab Prims.list FStar.Pervasives.Native.option ) -> (match (((hd'), (tl'))) with
| (FStar.Pervasives.Native.Some (hd'1), FStar.Pervasives.Native.Some (tl'1)) -> begin
(

let result = Prims.Cons (hd'1, tl'1)
in ((Zen.OptionT.some ()) result))
end
| uu____223 -> begin
(Zen.OptionT.none ())
end)))))
end
| Prims.Nil -> begin
(Zen.Base.op_Bar_Greater Prims.Nil (Zen.OptionT.incSome (Prims.op_Star (Prims.length ls) (n + 20L))))
end)))


let rec tryFold = (fun ( n  :  Prims.nat ) ( f  :  'Ab  ->  'Aa  ->  ('Ab FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost ) ( x  :  'Ab ) ( ls  :  'Aa Prims.list ) -> (Zen.Cost.Realized.inc (Prims.op_Star (Prims.length ls) (n + 12L)) 10L (match (ls) with
| Prims.Nil -> begin
(Zen.OptionT.incSome (Prims.op_Star (Prims.length ls) (n + 12L)) x)
end
| Prims.Cons (hd, tl) -> begin
(Zen.OptionT.op_Greater_Greater_Equals ((Prims.op_Star (Prims.length tl) (n + 12L)) + 10L) (n + 2L) (tryFold n f x tl) (fun ( r  :  'Ab ) -> (Zen.Cost.Realized.inc n 2L (f r hd))))
end)))


let parseDict : Zen.Types.Data.data FStar.Pervasives.Native.option  ->  (Zen.Types.Data.data Zen.Dictionary.t FStar.Pervasives.Native.option FStar.Pervasives.result, unit) Zen.Cost.Realized.cost = (fun ( data  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) -> (Zen.Cost.Realized.inc 4L 11L (match (data) with
| FStar.Pervasives.Native.Some (data1) -> begin
(Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater data1 Zen.Data.tryDict) (Zen.ResultT.ofOptionT 4L "Data parsing failed - the message body isn\'t a dictionary"B)) (Zen.ResultT.map 4L (fun ( _0_18  :  Zen.Types.Data.data Zen.Dictionary.t ) -> FStar.Pervasives.Native.Some (_0_18))))
end
| FStar.Pervasives.Native.None -> begin
(Zen.ResultT.incFailw 4L "Data parsing failed - the message body is empty"B)
end)))


let parseField = (fun ( uu____558  :  Prims.nat ) ( parser  :  Zen.Types.Data.data  ->  ('Auu____510 FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost ) ( fieldName  :  Prims.string ) ( errMsg  :  Prims.string ) ( dict  :  Zen.Types.Data.data Zen.Dictionary.t FStar.Pervasives.Native.option ) -> (Zen.Cost.Realized.inc ((64L + uu____558) + 0L) 11L (Zen.Cost.Extracted.letBang (64L + uu____558) 0L (Zen.Data.op_Greater_Question_Equals 64L uu____558 (Zen.Data.op_Greater_Bang_Equals 64L dict (Zen.Dictionary.tryFind fieldName)) parser) (fun ( value  :  'Auu____510 FStar.Pervasives.Native.option ) -> (match (value) with
| FStar.Pervasives.Native.Some (value1) -> begin
((Zen.ResultT.ok ()) value1)
end
| FStar.Pervasives.Native.None -> begin
((Zen.ResultT.failw ()) errMsg)
end)))))


let extractSpend : Zen.Types.Data.data Prims.list  ->  (Zen.Types.Extracted.spend FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( ls  :  Zen.Types.Data.data Prims.list ) -> (Zen.Cost.Realized.inc 73L 11L (match (ls) with
| Prims.Cons (asset', Prims.Cons (amount', Prims.Nil)) -> begin
(

let sAsset = (Zen.Data.tryString asset')
in (

let oAsset = (Zen.OptionT.op_Greater_Greater_Equals 2L 64L sAsset Zen.Asset.parse)
in (

let oAmount = (Zen.Data.tryU64 amount')
in (Zen.OptionT.op_Greater_Greater_Equals 66L 7L oAsset (fun ( asset  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Realized.inc 5L 2L (Zen.OptionT.op_Greater_Greater_Equals 2L 3L oAmount (fun ( amount  :  FStar.UInt64.t ) -> (Zen.Cost.Realized.inc 0L 3L ((Zen.OptionT.ret ()) {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = amount}))))))))))
end
| uu____723 -> begin
(Zen.OptionT.incNone 73L)
end)))


let trySpend : Zen.Types.Data.data  ->  (Zen.Types.Extracted.spend FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( d  :  Zen.Types.Data.data ) -> (Zen.Cost.Realized.inc 88L 3L (Zen.OptionT.op_Greater_Greater_Equals 4L 84L (Zen.Data.tryList d) extractSpend)))


let extractOutput : Zen.Types.Data.data Prims.list  ->  (Zen.Types.Extracted.output FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( ls  :  Zen.Types.Data.data Prims.list ) -> (Zen.Cost.Realized.inc 98L 9L (match (ls) with
| Prims.Cons (lock', Prims.Cons (spend', Prims.Nil)) -> begin
(

let oLock = (Zen.Data.tryLock lock')
in (

let oSpend = (trySpend spend')
in (Zen.OptionT.op_Greater_Greater_Equals 2L 96L oLock (fun ( lock  :  Zen.Types.Extracted.lock ) -> (Zen.Cost.Realized.inc 94L 2L (Zen.OptionT.op_Greater_Greater_Equals 91L 3L oSpend (fun ( spend  :  Zen.Types.Extracted.spend ) -> (Zen.Cost.Realized.inc 0L 3L ((Zen.OptionT.ret ()) {Zen.Types.Extracted.lock = lock; Zen.Types.Extracted.spend = spend})))))))))
end
| uu____803 -> begin
(Zen.OptionT.incNone 98L)
end)))


let tryOutput : Zen.Types.Data.data  ->  (Zen.Types.Extracted.output FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( d  :  Zen.Types.Data.data ) -> (Zen.Cost.Realized.inc 111L 3L (Zen.OptionT.op_Greater_Greater_Equals 4L 107L (Zen.Data.tryList d) extractOutput)))


let extractOutputList : Zen.Types.Data.data Prims.list  ->  (Zen.Types.Extracted.output Prims.list FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( ls  :  Zen.Types.Data.data Prims.list ) -> (Zen.Cost.Realized.inc ((Prims.op_Star (Prims.length ls) 134L) + 20L) 2L (tryMap 114L tryOutput ls)))


let toBounded : Prims.nat  ->  Zen.Types.Data.data Prims.list  ->  (Zen.Types.Data.data Prims.list FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( size1  :  Prims.nat ) ( ls  :  Zen.Types.Data.data Prims.list ) -> (Zen.Cost.Realized.inc 0L 7L (match (((Prims.length ls) <= size1)) with
| true -> begin
((Zen.OptionT.ret ()) ls)
end
| uu____902 -> begin
(Zen.OptionT.none ())
end)))


let extractOutputListBounded : Prims.nat  ->  Zen.Types.Data.data Prims.list  ->  (Zen.Types.Extracted.output Prims.list FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( size1  :  Prims.nat ) ( ls  :  Zen.Types.Data.data Prims.list ) -> (Zen.Cost.Realized.inc ((Prims.op_Star size1 134L) + 22L) 11L (Zen.Base.op_Bar_Greater (Zen.OptionT.op_Dollar_Greater ((Prims.op_Star (Prims.length ls) 134L) + 22L) (extractOutputList ls) (fun ( ls'  :  Zen.Types.Extracted.output Prims.list ) -> ls')) (Zen.Cost.Realized.inc ((Prims.op_Star (Prims.length ls) 134L) + 22L) (Prims.op_Star (size1 - (Prims.length ls)) 134L)))))


let parseRawOutputs : Zen.Types.Data.data Zen.Dictionary.t FStar.Pervasives.Native.option  ->  (Zen.Types.Data.data Prims.list FStar.Pervasives.result, unit) Zen.Cost.Realized.cost = (fun ( dict  :  Zen.Types.Data.data Zen.Dictionary.t FStar.Pervasives.Native.option ) -> (Zen.Cost.Realized.inc 79L 4L (parseField 4L Zen.Data.tryList "Outputs"B "Couldn\'t parse Outputs"B dict)))


let parseOutputs : Prims.nat  ->  Zen.Types.Data.data Zen.Dictionary.t FStar.Pervasives.Native.option  ->  (Zen.Types.Extracted.output Prims.list FStar.Pervasives.result, unit) Zen.Cost.Realized.cost = (fun ( size1  :  Prims.nat ) ( dict  :  Zen.Types.Data.data Zen.Dictionary.t FStar.Pervasives.Native.option ) -> (Zen.Cost.Realized.inc (90L + (((Prims.op_Star size1 134L) + 22L) + 11L)) 15L (Zen.ResultT.op_Greater_Greater_Equals 90L (((Prims.op_Star size1 134L) + 22L) + 11L) (Zen.ResultT.op_Greater_Greater_Equals 83L 7L (Zen.ResultT.op_Greater_Greater_Equals 0L 83L ((Zen.ResultT.ret ()) dict) parseRawOutputs) (Zen.Base.op_Greater_Greater (toBounded size1) (Zen.ResultT.ofOptionT 7L "Outputs list is too long"B))) (Zen.Base.op_Greater_Greater (extractOutputListBounded size1) (Zen.ResultT.ofOptionT (((Prims.op_Star size1 134L) + 22L) + 11L) "Invalid outputs structure"B)))))


let lockOutput : Zen.Types.Extracted.contractId  ->  Zen.Types.Realized.wallet  ->  Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.output  ->  (Zen.Types.Realized.txSkeleton FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( contractId  :  Zen.Types.Extracted.contractId ) ( w  :  Zen.Types.Realized.wallet ) ( txSkel  :  Zen.Types.Realized.txSkeleton ) ( outp  :  Zen.Types.Extracted.output ) -> (Zen.Cost.Realized.inc ((0L + ((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L)) + 64L) 19L (

let asset = outp.spend.asset
in (

let amount = outp.spend.amount
in (Zen.OptionT.op_Greater_Greater_Equals (0L + ((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L)) 64L (Zen.OptionT.op_Greater_Greater_Equals 0L ((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) ((Zen.OptionT.ret ()) txSkel) (Zen.TxSkeleton.fromWallet asset amount contractId w)) (Zen.Base.op_Greater_Greater (Zen.TxSkeleton.lockToAddress asset amount outp.lock) (Zen.OptionT.liftCost 64L)))))))


let lockOutputs : Zen.Types.Extracted.contractId  ->  Zen.Types.Realized.wallet  ->  Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.output Prims.list  ->  (Zen.Types.Realized.txSkeleton FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( contractId  :  Zen.Types.Extracted.contractId ) ( w  :  Zen.Types.Realized.wallet ) ( txSkel  :  Zen.Types.Realized.txSkeleton ) ( ls  :  Zen.Types.Extracted.output Prims.list ) -> (Zen.Cost.Realized.inc ((Prims.op_Star (Prims.length ls) (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) 5L (tryFold ((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) (lockOutput contractId w) txSkel ls)))


let lockOutputsBounded : Prims.nat  ->  Zen.Types.Extracted.contractId  ->  Zen.Types.Realized.wallet  ->  Zen.Types.Realized.txSkeleton  ->  Zen.Types.Extracted.output Prims.list  ->  (Zen.Types.Realized.txSkeleton FStar.Pervasives.Native.option, unit) Zen.Cost.Realized.cost = (fun ( size1  :  Prims.nat ) ( contractId  :  Zen.Types.Extracted.contractId ) ( w  :  Zen.Types.Realized.wallet ) ( txSkel  :  Zen.Types.Realized.txSkeleton ) ( ls  :  Zen.Types.Extracted.output Prims.list ) -> (Zen.Cost.Realized.inc (((Prims.op_Star size1 (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) 23L (Zen.Base.op_Bar_Greater (lockOutputs contractId w txSkel ls) (Zen.Cost.Realized.inc (((Prims.op_Star (Prims.length ls) (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) (Prims.op_Star (size1 - (Prims.length ls)) (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L))))))


let isPayoutBlock : Zen.Types.Main.context  ->  (Prims.bool, unit) Zen.Cost.Realized.cost = (fun ( context  :  Zen.Types.Main.context ) -> (Zen.Cost.Realized.inc 0L 9L (

let r = (FStar.UInt32.rem context.blockNumber (FStar.UInt32.uint_to_t intervalLength))
in (Zen.Base.op_Bar_Greater (FStar.UInt32.eq r (FStar.UInt32.uint_to_t payoutBlock)) Zen.Cost.Realized.ret))))


let validateBlockNumber : Zen.Types.Main.context  ->  Zen.Types.Realized.txSkeleton  ->  (Zen.Types.Realized.txSkeleton FStar.Pervasives.result, unit) Zen.Cost.Realized.cost = (fun ( context  :  Zen.Types.Main.context ) ( txSkel  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Cost.Realized.inc 9L 7L (Zen.Cost.Extracted.letBang 9L 0L (isPayoutBlock context) (fun ( b  :  Prims.bool ) -> (match (b) with
| true -> begin
((Zen.ResultT.ret ()) txSkel)
end
| uu____1532 -> begin
((Zen.ResultT.failw ()) "Not a payout block"B)
end)))))


let main = (fun ( txSkel  :  Zen.Types.Realized.txSkeleton ) ( context  :  Zen.Types.Main.context ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____1569 ) ( sender  :  'Auu____1570 ) ( messageBody  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( w  :  Zen.Types.Realized.wallet ) ( state  :  'Auu____1571 ) -> (Zen.Cost.Realized.inc ((((15L + ((90L + (((Prims.op_Star maxOutputs 134L) + 22L) + 11L)) + 15L)) + ((((Prims.op_Star maxOutputs (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) + 23L)) + 16L) + 3L) 20L (Zen.ResultT.op_Greater_Greater_Equals (((15L + ((90L + (((Prims.op_Star maxOutputs 134L) + 22L) + 11L)) + 15L)) + ((((Prims.op_Star maxOutputs (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) + 23L)) + 16L) 3L (Zen.ResultT.op_Greater_Greater_Equals ((15L + ((90L + (((Prims.op_Star maxOutputs 134L) + 22L) + 11L)) + 15L)) + ((((Prims.op_Star maxOutputs (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) + 23L)) 16L (Zen.ResultT.op_Greater_Greater_Equals (15L + ((90L + (((Prims.op_Star maxOutputs 134L) + 22L) + 11L)) + 15L)) ((((Prims.op_Star maxOutputs (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) + 23L) (Zen.ResultT.op_Greater_Greater_Equals 15L ((90L + (((Prims.op_Star maxOutputs 134L) + 22L) + 11L)) + 15L) (Zen.ResultT.op_Greater_Greater_Equals 0L 15L ((Zen.ResultT.ret ()) messageBody) parseDict) (parseOutputs maxOutputs)) (Zen.Base.op_Greater_Greater (lockOutputsBounded maxOutputs contractId w txSkel) (Zen.ResultT.ofOptionT ((((Prims.op_Star maxOutputs (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) + 23L) "Insufficient funds"B))) (validateBlockNumber context)) Zen.ContractResult.ofTxSkel)))


let cf : Zen.Types.Realized.txSkeleton  ->  Zen.Types.Main.context  ->  Prims.string  ->  Zen.Types.Main.sender  ->  Zen.Types.Data.data FStar.Pervasives.Native.option  ->  Zen.Types.Realized.wallet  ->  Zen.Types.Data.data FStar.Pervasives.Native.option  ->  (Prims.nat, unit) Zen.Cost.Realized.cost = (fun ( uu____1720  :  Zen.Types.Realized.txSkeleton ) ( uu____1721  :  Zen.Types.Main.context ) ( uu____1722  :  Prims.string ) ( uu____1723  :  Zen.Types.Main.sender ) ( uu____1724  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( w  :  Zen.Types.Realized.wallet ) ( uu____1726  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) -> (Zen.Cost.Realized.inc 0L 43L (Zen.Base.op_Bar_Greater (((((15L + ((90L + (((Prims.op_Star maxOutputs 134L) + 22L) + 11L)) + 15L)) + ((((Prims.op_Star maxOutputs (((((Prims.op_Star (Zen.Wallet.size w) 128L) + 192L) + 64L) + 19L) + 12L)) + 10L) + 5L) + 23L)) + 16L) + 3L) + 20L) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (43L, cf), main)




