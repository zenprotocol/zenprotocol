module Swaps.Imp

open Swaps.Selling

let shouldCreateContract order buyer =
    let isMatchingOrder order (output: NBitcoin.TxOut) =
        output.ScriptPubKey = order.BTCAddress.ScriptPubKey && uint64(output.Value.Satoshi) = order.BTCAmount

    if buyer.BTCTx.Check() = NBitcoin.TransactionCheckResult.Success
    then
        let outputs = List.ofSeq buyer.BTCTx.Outputs
        match List.tryFind (isMatchingOrder order) outputs with 
            | Some _ -> true
            | None -> false
    else
    false
        

let activateContract getZenClient generateContractCode getPassword order (buyer: Buyer) =
    let result = Messaging.Services.Wallet.activateContract (getZenClient ()) true (generateContractCode order buyer) order.ZPActivationBlocks (getPassword ())
    match result with
    | Ok res ->
        let _, contract = res
        Ok contract
    | Error err -> Error err


let isSwapExpired getZenClient order =
    let info = Messaging.Services.Blockchain.getBlockChainInfo (getZenClient ())
    info.headers > order.ZPExpiry


let isBTCConfirmed (getNinjaClient: unit -> QBitNinja.Client.QBitNinjaClient) order buyer = 
    let client = getNinjaClient ()
    let res = (client.GetTransaction (buyer.BTCTx.GetHash ())).Result
    match res with
    | null -> false 
    | _ -> uint32(res.Block.Confirmations) >= order.BTCRequiredConfirmations

