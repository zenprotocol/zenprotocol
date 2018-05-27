module Zen.Types.Main

open Zen.Types.Extracted
open Zen.Types.Realized
open Zen.Cost
open Zen.Types.Data

module U32 = FStar.UInt32


val maxCost: nat
let maxCost = 200

type message =
    { recipient: contractId;
      command: string;
      body: option data }

type sender =
    | PK of publicKey
    | Contract of contractId
    | Anonymous

type context =
    { blockNumber: U32.t;
      timestamp: timestamp }

type stateUpdate =
    | Delete
    | NoChange
    | Update of data

type contractReturn =
    { tx: txSkeleton;
      message: option message;
      state: stateUpdate }

type contractResult = result contractReturn

noeq type costFunction =
    | CostFunc:
        #n:nat{n<=maxCost}
        -> f:(txSkeleton
              -> context:context
              -> command:string
              -> sender
              -> messageBody:option data
              -> wallet
              -> state:option data
              -> nat `cost` n)
        -> costFunction

noeq type mainFunction =
    | MainFunc:
        cf:costFunction
        -> mf:(txSkel:txSkeleton
                -> context:context
                -> contractId
                -> command:string
                -> sender:sender
                -> messageBody:option data
                -> wallet:wallet
                -> state:option data
                -> contractResult `cost` force ((CostFunc?.f cf) txSkel context command sender messageBody wallet state)
              )
        -> mainFunction
