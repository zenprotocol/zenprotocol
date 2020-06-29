module Consensus.Contract

open Microsoft.FSharp.Core
open System.Reflection
open System.Text
open FsBech32
open Hash
open Types
open Infrastructure
open Zen.Types.Extracted
open FStar.Pervasives
open System
open System.IO
open Zen.Cost.Realized
open Zen.Types.Realized
open Zen.Types.Data
open Zen.Types.Main

type private zfstarMainFn =
    txSkeleton
        -> context
        -> contractId
        -> Prims.string
        -> sender
        -> Native.option<data>
        -> wallet
        -> Native.option<data>
        -> cost<contractResult, Prims.unit>

type private zfstarCostFn =
    txSkeleton
        -> context
        -> Prims.string
        -> sender
        -> Native.option<data>
        -> wallet
        -> Native.option<data>
        -> cost<Prims.nat, Prims.unit>

type ContractWallet = PointedOutput list

type ContractMainFn =
    TxSkeleton.T
        -> ContractContext
        -> ContractId
        -> string
        -> sender
        -> data option
        -> ContractWallet
        -> data option
        -> Result<(TxSkeleton.T * Message Option * stateUpdate),string>

type ContractCostFn =
    TxSkeleton.T
        -> ContractContext
        -> string
        -> sender
        -> data option
        -> ContractWallet
        -> data option
        -> Result<int64,string>

type Contract = {
    contractId: ContractId
    expiry: uint32
    code: string
    mainFn: ContractMainFn
    costFn: ContractCostFn
}
with
    member x.version =
        let (ContractId (version,_)) = x.contractId
        version

    member x.hash =
        let (ContractId (_,cHash)) = x.contractId
        cHash

type T = Contract

let compatibilityFiles = 
    ZFStar.compatibilityPath
    |> Directory.GetFiles
    |> Array.map Path.GetFileName

let private getMainFunction assembly =
    try
        let getProperty name =
            (assembly:Assembly)
                .GetModules().[0]  // Should get ModuleName.dll
                .GetTypes().[0]    // Should get ModuleName
                .GetProperty(name) // ModuleName.name
        (getProperty "mainFunction").GetValue null
        :?> mainFunction
        |> Ok
    with _ as ex ->
        Exception.toError "get contract mainFunc" ex

let private wrapMainFn (mainFn : zfstarMainFn) : ContractMainFn =
    fun txSkeleton (context:ContractContext) (ContractId (version, Hash.Hash cHash)) command sender messageBody contractWallet contractState ->
        try
            let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
            let command' = ZFStar.fsToFstString command
            let messageBody' = ZFStar.fsToFstOption messageBody
            let contractWallet' = ZFStar.fsToFstWallet contractWallet
            let context' = ZFStar.convertContext context
            let contractState' = ZFStar.fsToFstOption contractState
            mainFn txSkeleton' context' (version,cHash) command' sender messageBody' contractWallet' contractState'
            |> ZFStar.unCost
            |> ZFStar.toResult
            |> Result.map ZFStar.convertResult
        with _ as ex ->
            Exception.toError "mainFn" ex

let private wrapCostFn (costFn: zfstarCostFn) : ContractCostFn =
    fun txSkeleton context command sender messageBody contractWallet contractState ->
        try
            let txSkeleton' = ZFStar.fsToFstTxSkeleton txSkeleton
            let command' = ZFStar.fsToFstString command
            let data' = ZFStar.fsToFstOption messageBody
            let contractWallet' = ZFStar.fsToFstWallet contractWallet
            let context' = ZFStar.convertContext context
            let contractState' = ZFStar.fsToFstOption contractState
            costFn txSkeleton' context' command' sender data' contractWallet' contractState'
            |> ZFStar.unCost
            |> Ok
        with _ as ex ->
            Exception.toError "costFn" ex

let public getModuleName : Hash -> string =
    Hash.bytes
    >> Base16.encode
    >> (+) "Z"

let private computeHash version (code:string) =
    let versionBytes = BigEndianBitConverter.uint32ToBytes version
    let codeBytes = Encoding.UTF8.GetBytes code

    Hash.computeMultiple [versionBytes; codeBytes]

let makeContractId version code =
    ContractId (version, computeHash version code)

let load contractsPath expiry code (ContractId (version,hash)) =
    getModuleName hash
    |> ZFStar.load contractsPath
    |> Result.bind getMainFunction
    |> Result.map (fun (MainFunc (CostFunc (_, costFn), mainFn) : mainFunction) ->
        {
            contractId = ContractId (version,hash)
            mainFn = wrapMainFn mainFn
            costFn = wrapCostFn costFn
            expiry = expiry
            code = code
        })

let getFunctions assembly =
    getMainFunction assembly
    |> Result.map (fun (MainFunc (CostFunc (_, costFn), mainFn) : mainFunction) ->
        wrapMainFn mainFn, wrapCostFn costFn
        )

let compile (contractsPath:string)
            (contract:ContractV0) =
    let hash = computeHash Version0 contract.code
    
    let hashString = hash.AsString

    let hintsFile = sprintf "Z%s" <| Infrastructure.ZFStar.changeExtention ".fst.hints" hashString
    
    let hints =
        match Array.tryFind ((=) hintsFile) compatibilityFiles with
        | Some file ->
            (ZFStar.compatibilityPath, file)
            |> Path.Combine
            |> File.ReadAllText
        | None ->
            contract.hints

    hash
    |> getModuleName
    |> ZFStar.compile contractsPath contract.code hints contract.rlimit
    |> Result.map (fun _ -> ContractId (Version0,hash))

let recordHints (code:string) : Result<string, string> =
    code
    |> computeHash Version0
    |> getModuleName
    |> ZFStar.recordHints code

let getCost contract = contract.costFn

let run contract txSkeleton context command sender messageBody wallet state =
    contract.mainFn txSkeleton context contract.contractId command sender messageBody wallet state

let getContractWallet (txSkeleton:TxSkeleton.T) (w:ContractWitness) =
    txSkeleton.pInputs.[int w.beginInputs .. int w.endInputs]
    |> List.choose (fun input ->
        match input with
        | TxSkeleton.PointedOutput (outpoint,output) when output.lock = Consensus.Types.Contract w.contractId ->
            Some (outpoint,output)
        | _ -> None
    )
