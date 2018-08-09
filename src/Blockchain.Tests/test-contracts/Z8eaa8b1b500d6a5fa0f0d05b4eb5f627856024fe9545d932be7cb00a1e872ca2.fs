#light "off"
module Z8eaa8b1b500d6a5fa0f0d05b4eb5f627856024fe9545d932be7cb00a1e872ca2
open Prims
open FStar.Pervasives

let main = (fun ( txSkeleton  :  Zen.Types.Realized.txSkeleton ) ( uu____93  :  'Auu____43 ) ( contractId  :  Zen.Types.Extracted.contractId ) ( command  :  'Auu____44 ) ( sender  :  'Auu____45 ) ( messageBody  :  'Auu____46 ) ( wallet  :  Zen.Types.Realized.wallet ) ( state  :  'Auu____47 ) -> (Zen.Cost.Realized.inc ((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) 22L (Zen.Cost.Extracted.letBang (64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) 0L (Zen.Cost.Realized.bind 64L ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L) (Zen.TxSkeleton.lockToPubKey Zen.Asset.zenAsset (FStar.UInt64.uint_to_t 10L) Zen.Asset.zeroHash txSkeleton) (Zen.TxSkeleton.fromWallet Zen.Asset.zenAsset (FStar.UInt64.uint_to_t 10L) contractId wallet)) (fun ( result  :  Zen.Types.Realized.txSkeleton FStar.Pervasives.Native.option ) -> (

let result' = (match (result) with
| FStar.Pervasives.Native.Some (tx) -> begin
(Zen.Base.op_At (fun ( _0_16  :  Zen.Types.Main.contractReturn ) -> FStar.Pervasives.Native.Some (_0_16)) {Zen.Types.Main.tx = tx; Zen.Types.Main.message = FStar.Pervasives.Native.None; Zen.Types.Main.state = Zen.Types.Main.NoChange})
end
| FStar.Pervasives.Native.None -> begin
FStar.Pervasives.Native.None
end)
in (Zen.ResultT.ofOption "not enough Zens"B result'))))))


let cf = (fun ( uu____212  :  'Auu____172 ) ( uu____213  :  'Auu____173 ) ( uu____214  :  'Auu____174 ) ( uu____215  :  'Auu____175 ) ( uu____216  :  'Auu____176 ) ( wallet  :  Zen.Types.Realized.wallet ) ( uu____218  :  'Auu____177 ) -> (Zen.Cost.Realized.inc 0L 12L (Zen.Cost.Realized.ret (((64L + ((Prims.op_Star (Zen.Wallet.size wallet) 128L) + 192L)) + 0L) + 22L))))


let mainFunction : Zen.Types.Main.mainFunction = Zen.Types.Main.MainFunc (Zen.Types.Main.CostFunc (12L, cf), main)




