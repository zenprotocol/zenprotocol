#light "off"
module Zec488d5eb7466867e6e05b31cb900f0376c8fd96cb3e5e31062f1e6f5b43385d
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____100  :  'Auu____48 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____49 ) ( sender  :  'Auu____50 ) ( messageBody  :  Zen.Types.Data.data FStar.Pervasives.Native.option ) ( wallet  :  'Auu____51 ) ( state  :  'Auu____52 ) -> (Zen.Cost.Realized.inc 390L 44L (Zen.Cost.Extracted.letBang 70L 320L (Zen.Data.op_Greater_Question_Equals 68L 2L (Zen.Data.op_Greater_Question_Equals 4L 64L (Zen.Data.op_Greater_Bang_Equals 4L messageBody Zen.Data.tryDict) (Zen.Dictionary.tryFind "returnAddress"B)) Zen.Data.tryLock) (fun ( returnAddress  :  Zen.Types.Extracted.lock FStar.Pervasives.Native.option ) -> (match (returnAddress) with
| FStar.Pervasives.Native.Some (returnAddress1) -> begin
(Zen.Cost.Extracted.letBang 64L 256L (Zen.TxSkeleton.getAvailableTokens Zen.Asset.zenAsset txSkeleton) (fun ( tokens  :  FStar.UInt64.t ) -> (Zen.Cost.Extracted.letBang 64L 192L (Zen.Asset.getDefault contractId) (fun ( asset  :  Zen.Types.Extracted.asset ) -> (Zen.Cost.Extracted.letBang 128L 64L (Zen.Cost.Realized.bind 64L 64L (Zen.TxSkeleton.mint tokens asset txSkeleton) (Zen.TxSkeleton.lockToAddress asset tokens returnAddress1)) (fun ( txSkeleton1  :  Zen.Types.Realized.txSkeleton ) -> (Zen.Cost.Extracted.letBang 64L 0L (Zen.ContractId.parse "00000000247fa9ec1dc70db512f46b837df5023c08562ea2ce897cebe531342e3880205e"B) (fun ( contractId1  :  Zen.Types.Extracted.contractId FStar.Pervasives.Native.option ) -> (match (contractId1) with
| FStar.Pervasives.Native.Some (contractId2) -> begin
(

let message = {Zen.Types.Main.recipient = contractId2; Zen.Types.Main.command = "contract2_test"B; Zen.Types.Main.body = messageBody}
in (Zen.Base.op_At (Zen.ResultT.ok ()) {Zen.Types.Main.tx = txSkeleton1; Zen.Types.Main.message = FStar.Pervasives.Native.Some (message); Zen.Types.Main.state = Zen.Types.Main.NoChange}))
end
| FStar.Pervasives.Native.None -> begin
(Zen.ResultT.autoFailw 0L "could not parse contractId from string"B)
end)))))))))
end
| FStar.Pervasives.Native.None -> begin
(Zen.ResultT.autoFailw 320L "returnAddress is required"B)
end)))))


let cf = (fun ( uu____332  :  'Auu____291 ) ( uu____333  :  'Auu____292 ) ( uu____334  :  'Auu____293 ) ( uu____335  :  'Auu____294 ) ( uu____336  :  'Auu____295 ) ( uu____337  :  'Auu____296 ) ( uu____338  :  'Auu____297 ) -> (Zen.Cost.Realized.inc 0L 23L (Zen.Base.op_Bar_Greater (Zen.Base.op_Bar_Greater 434L Zen.Base.cast) Zen.Cost.Realized.ret)))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (23L, cf), main)




