module Swaps.Selling

type Order =
    {
        ZPAmount: uint64
        ZPExpiry: uint32
        ZPActivationBlocks: uint32
        BTCAmount: uint64
        BTCRequiredConfirmations: uint32
        BTCAddress: NBitcoin.BitcoinAddress
    }

type Buyer =
    {
        BTCTx: NBitcoin.Transaction
        ZPLock: Consensus.Types.Lock
    }
    
type ContractActivationResult = Result<Consensus.Types.ContractId, string>

// State Data

type OrderCreatedData = Order
type ReadyForContractData = Order * Buyer
type ContractActivatedData = Order * Buyer * Consensus.Types.ContractId
type SuccessData = Order * Buyer * Consensus.Types.ContractId
type FailureData = string

// States

type Swap =
| OrderCreatedState of OrderCreatedData
| ReadyForContractState of ReadyForContractData
| ContractActivatedState of ContractActivatedData
| SuccessState of SuccessData
| FailureState of FailureData

// Transitions

let transitionFromFailure (data: FailureData) = FailureState data


let transitionFromSuccess (data: SuccessData) = SuccessState data


let transitionFromOrderCreated shouldCreateContract (data: OrderCreatedData) (buyer: Buyer) =
    if shouldCreateContract data buyer
    then
        (data, buyer) |> ReadyForContractState
    else 
        FailureState "buyer doesn't match order"


let transitionFromReadyForContract (activateContract: Order -> Buyer -> ContractActivationResult) (data: ReadyForContractData) =
    let order, buyer = data
    let result = activateContract order buyer
    match result with 
    | Ok contract -> (order, buyer, contract) |> ContractActivatedState
    | Error err -> err |> FailureState


let transitionFromContractActivated isSwapExpired isBTCConfirmed (data: ContractActivatedData) =
    let order, buyer, _ = ContractActivatedData data
    if isBTCConfirmed order buyer
    then
        data |> SuccessState
    else
        if !isSwapExpired order
        then
            data |> ContractActivatedState
        else 
            FailureState "swap expired with no payment"