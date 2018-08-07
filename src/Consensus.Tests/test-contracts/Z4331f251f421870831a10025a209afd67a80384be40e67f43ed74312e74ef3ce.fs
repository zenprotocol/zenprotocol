#light "off"
module Z4331f251f421870831a10025a209afd67a80384be40e67f43ed74312e74ef3ce
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( contractContext  :  Zen.Types.Main.context ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____45 ) ( sender  :  'Auu____46 ) ( messageBody  :  'Auu____47 ) ( wallet  :  'Auu____48 ) ( state  :  'Auu____49 ) -> (Zen.Cost.Realized.inc 192L 24L (Zen.Cost.Extracted.letBang 64L 128L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (

let lock = Zen.Types.Extracted.ContractLock (contractId)
in (

let spend = {Zen.Types.Extracted.asset = asset; Zen.Types.Extracted.amount = contractContext.timestamp}
in (

let pInput = Zen.Types.Extracted.Mint (spend)
in (Zen.Cost.Extracted.letBang 128L 0L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.addInput pInput txSkeleton) (Zen.TxSkeleton.lockToContract spend.asset spend.amount contractId)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange}))))))))))


let cf = (fun ( uu____277  :  'Auu____236 ) ( uu____278  :  'Auu____237 ) ( uu____279  :  'Auu____238 ) ( uu____280  :  'Auu____239 ) ( uu____281  :  'Auu____240 ) ( uu____282  :  'Auu____241 ) ( uu____283  :  'Auu____242 ) -> (Zen.Cost.Realized.inc 0L 13L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 216L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (13L, cf), main)




