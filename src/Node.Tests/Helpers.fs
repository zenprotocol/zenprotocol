module Node.Tests.Helpers

open Messaging.Services
open Messaging.Events
open Infrastructure

let rec waitForTx subscriber tx =
    match EventBus.Subscriber.recv subscriber with
    | TransactionAddedToMemPool (_, tx) when tx = tx -> ()
    | _ -> waitForTx subscriber tx

let rec waitForBk subscriber bk =
    match EventBus.Subscriber.recv subscriber with
    | BlockAdded (_, bk) when bk = bk -> ()
    | _ -> waitForBk subscriber bk
