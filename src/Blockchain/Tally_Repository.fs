module Blockchain.Tally.Repository

open DataAccess
open Blockchain.DatabaseContext
open Wallet


module PKBalance =
    let get (session : Session) = Collection.get session.context.pkbalance

    let tryGet (session : Session) = Collection.tryGet session.context.pkbalance
    let put (session : Session) = Collection.put session.context.pkbalance

    let delete (session : Session) = Collection.delete session.context.pkbalance
    let truncate (session : Session) = Collection.truncate session.context.pkbalance

    let contains (session : Session) = Collection.containsKey session.context.pkbalance

module PKAllocation =
    let get (session : Session) = Collection.get session.context.allocationVoters

    let tryGet (session : Session) = Collection.tryGet session.context.allocationVoters
    let put (session : Session) = Collection.put session.context.allocationVoters

    let delete (session : Session) = Collection.delete session.context.allocationVoters
    let truncate (session : Session) = Collection.truncate session.context.allocationVoters

    let contains (session : Session) = Collection.containsKey session.context.allocationVoters

module PKPayout =
    let get (session : Session) = Collection.get session.context.payoutVoters

    let tryGet (session : Session) = Collection.tryGet session.context.payoutVoters
    let put (session : Session) = Collection.put session.context.payoutVoters

    let delete (session : Session) = Collection.delete session.context.payoutVoters
    let truncate (session : Session) = Collection.truncate session.context.payoutVoters

    let contains (session : Session) = Collection.containsKey session.context.payoutVoters

module PKNominee =
    let get (session : Session) = Collection.get session.context.nomineesVoters

    let tryGet (session : Session) = Collection.tryGet session.context.nomineesVoters
    let put (session : Session) = Collection.put session.context.nomineesVoters

    let delete (session : Session) = Collection.delete session.context.nomineesVoters
    let truncate (session : Session) = Collection.truncate session.context.nomineesVoters

    let contains (session : Session) = Collection.containsKey session.context.nomineesVoters

module Candidates =
    let get (session : Session) = Collection.get session.context.nomineesWinner

    let tryGet (session : Session) = Collection.tryGet session.context.nomineesWinner
    let put (session : Session) = Collection.put session.context.nomineesWinner

    let delete (session : Session) = Collection.delete session.context.nomineesWinner
    let truncate (session : Session) = Collection.truncate session.context.nomineesWinner

    let contains (session : Session) = Collection.containsKey session.context.nomineesWinner


module Fund =
    let get (session : Session) = Collection.get session.context.funds

    let tryGet (session : Session) = Collection.tryGet session.context.funds
    let put (session : Session) = Collection.put session.context.funds

    let delete (session : Session) = Collection.delete session.context.funds
    let truncate (session : Session) = Collection.truncate session.context.funds

    let contains (session : Session) = Collection.containsKey session.context.funds


module Winner =
    let get (session : Session) = Collection.get session.context.winner

    let tryGet (session : Session) = Collection.tryGet session.context.winner
    let put (session : Session) = Collection.put session.context.winner

    let delete (session : Session) = Collection.delete session.context.winner
    let truncate (session : Session) = Collection.truncate session.context.winner

    let contains (session : Session) = Collection.containsKey session.context.winner

module VoteTip =
    let tryGet (session : Session) = SingleValue.tryGet session.context.voteTip

    let put (session : Session) = SingleValue.put session.context.voteTip

    let delete (session : Session) = SingleValue.delete session.context.voteTip
