module Blockchain.Tally.VoteParser

open Consensus
open Consensus.Chain
open Consensus.Crypto
open Types
open Hash
open Infrastructure
open Serialization
open Zen.Types.Data
open System.Text
open FSharpx.Option

module S         = Serialization
module CryptoSig = Crypto.Signature

let option = FSharpx.Option.maybe

// Bifunctor (bimap)
let (|*|) f g (x,y) = option {
    let! fx = f x
    let! gx = g y
    return (fx, gx)
}

let hashBallot (chainParam : ChainParameters) (blockNumber : uint32) (ballotId : Prims.string) : Hash =
    let interval    = CGP.getInterval chainParam blockNumber
    let isNominee   = CGP.isNomineePhase chainParam blockNumber
    let phase       = if isNominee then "Contestant" else "Candidate"
    let serBallot   = ballotId |> String |> Data.serialize |> FsBech32.Base16.encode
    let serPhase    = phase |> ZFStar.fsToFstString |> String |> Data.serialize |> FsBech32.Base16.encode
    let serInterval = interval |> U32    |> Data.serialize |> FsBech32.Base16.encode
    [serInterval; serPhase; serBallot]
    |> String.concat ""
    |> Encoding.Default.GetBytes
    |> Hash.compute

let tryDict (data : data) : Map<FStar.String.t, data> option =
    match data with | Collection (Dict (map,_)) -> Some map | _ -> None

let tryString (data : data) : Prims.string option =
    match data with | String s -> Some s | _ -> None

let getBallot (command : string) (dict : Map<Prims.string, data>) : Prims.string option =
    dict
    |> Map.tryFind (ZFStar.fsToFstString command)
    >>= tryString

let getSignatures (dict : Map<Prims.string, data>) : Map<Prims.string,data> option = 
    dict
    |> Map.tryFind (ZFStar.fsToFstString "Signature")
    >>= tryDict

let parsePk (pk : Prims.string) : PublicKey option =
    pk
    |> ZFStar.fstToFsString
    |> PublicKey.fromString

let parseSig (sign : data) : Signature option =
    match sign with
    | Signature sign ->
        sign
        |> ZFStar.fstToFsSignature
        |> Some
    | _ ->
        None

let verifySig message (pk, signature) =
    match Crypto.verify pk signature message with
    | Valid ->
        Some (pk, signature)
    | Invalid ->
        None

let parseValidSignatures (message : Hash) (sigs : Map<Prims.string,data>) : Map<PublicKey, Signature> =
    sigs
    |> Map.toSeq
    |> Seq.choose ((parsePk |*| parseSig) >=> verifySig message)
    |> Map.ofSeq

let parseBallot (command : string) (encBallot : Prims.string) : Ballot option = option {
        
    let serBallot =
        encBallot
        |> ZFStar.fstToFsString
        |> FsBech32.Base16.decode
        |> Option.defaultValue ""B
    
    match! S.Ballot.deserialize serBallot with
    | Allocation allocation      when command = "Allocation" ->
        return Allocation allocation
    | Payout (recipient, spends) when command = "Payout"     ->
        return Payout (recipient, spends)
    | _ ->
        return! None
}

let parseMessageBody
    (chainParams : ChainParameters)
    (blockNumber : uint32)
    (command : string)
    (messageBody : data option)
    : (Ballot * PublicKey list) option
    = option {
        let! dict      = messageBody >>= tryDict
        let! encBallot = dict |> getBallot command
        let! encSigs   = dict |> getSignatures
        let  msg       = hashBallot chainParams blockNumber encBallot
        let! ballot    = encBallot |> parseBallot command
        let  sigs      = encSigs |> parseValidSignatures msg
        let  pks       = sigs |> Map.toList |> List.map fst
        return (ballot, pks)
    }