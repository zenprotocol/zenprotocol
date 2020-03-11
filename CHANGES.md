# Changes

This proposed protocol upgrade would seek to create a '**Common Goods Pool**' (or '**CGP**'), which would hold assets, and distribute its funds according to an on-chain vote open to ZP holders.

The CGP contract is a custodian contract which can unlock ('**pay out**') some of its available assets to a specified recipient - a PK address or a contract.

For each block - some portion of the coinbase transaction (called "**allocation**")
will be locked to the CGP contract instead of the miner.

Since the CGP is a contract, users would also be able to **donate** any
amount of any asset they own to the CGP, allowing the CGP to hold tokens
of any kind of Zen Protocol asset.

Users will be able to vote on the portion of the coinbase outputs locked to the CGP contract ("**allocation**" vote),
and on the recipient and spends (assets and amounts) of each payout ("**payout**" vote).

Voting for allocation could be done anytime during the voting phase, however - voting for payout is split into 2 phases - nomination phase and voting phase.

1. The **Nomination Phase** - in which payout contestants are trying to become valid payout **nominees**.

2. The **Voting Phase** - in which valid payout nominees are trying to get the payout.

### Interval blocks

  * **Blockchain Interval:**

      From `10,000 * (N-1) + 1` to `10,000 * N` (including boundaries)

      The whole process takes places in intervals of `10,000` blocks
      starting from the genesis.

  * **Voting Interval:**

      From `10,000 * (N-1) + 9,001` to `10,000 * N` (including boundaries)

      The actual voting takes place here.

      Split into 2 **phases**:

      1. **Nomination Phase:**

        From `10,000 * (N-1) + 9,001` to `10,000 * (N-1) + 9,500` (including boundaries)

        Nominating contestants for the payout

      2. **Voting Phase:**

        From `10,000 * (N-1) + 9,501` to `10,000 * N` (including boundaries)

        Voting on which nominee gets the payout

  * **Snapshot Block:**

      `10,000 * (N-1) + 9,000`

      All the votes are weighted and validated by using the state of the UTXOs set at this block.

  * **Payout Block:**

      `10,000 * N + 100`

      The payout and allocation change takes place at this block.

## Payout

The **CGP Contract** execution will return a valid transaction only if the following conditions hold:

  1. There are sufficient funds for the CGP contract to cover all the outputs of the payout transaction.

  2. The payout transaction contains at least 1 output and no more than 100 outputs.

## Allocation

With each mined block, in the coinbase transaction, a percentage of the reward of that block will be sent by the miner to the CGP contract.

This percentage is called “**The Coinbase Allocation**”.

## Voting

Users cast a ballot by sending the voting data (using the contract message body) to the Voting contract.

The Tally will count the votes, and determine the winner.

Users may vote for 2 independent items:

* `Allocation`: Users vote on the % of the block reward to be allocated to the CGP each block.
(Nodes will not build on top of a block that does not contain a coinbase transaction with sufficient allocation output)

* `Payout`: Each voting interval users can vote on the recipient and the list of assets & amounts the recipient will receive

### Tally Calculation

At the end of the voting interval the tally will calculate the votes.

The Tally will only count the 1st vote for each PK,
and only when it is executed within the voting interval.
The vote also has to have a valid ballot to count.

### Allocation Ballot Validation

An allocation ballot is considered as valid as long as the following conditions hold:

1. It doesn’t exceed the upper allocation bound (`90%`).

2. It is no bigger and no smaller than the allocation correction cap (`15%`) relative (proportional) to the allocation of the last block.

### Payout Ballot Validation

Voting on payout is done in 2 phases, each with its own validity rules:

1. **Nomination Phase:**

    A payout ballot will become a valid nominee if the following conditions hold:

    a. At the snapshot the CGP contract has enough funds to cover it

    b. The spend list length is between 1 and 100 elements

    c. The spend list must be sorted in lexicographical order (over the asset ids) and the assets must to be unique

    d. The aggregated vote weight of the contestant is at least 3% of the total issued ZP at the snapshot block

2. **Voting Phase:**

    During the voting phase payout votes can only be made for a valid nominee.

    There is a special nominee which is always valid as long as there are sufficient funds in the CGP regardless of the results of the nomination, which is a payout of 1 kalapa to the CGP itself - this is the **default** vote which a user can vote on to effectively prevent a payout.

## Block Validation

The following validation rule has been added:

* The **coinbase transaction** may now contain an output with a **contract lock**, along with the usual **coinbase** output.

* To be sure that the CGP contract can always be executed we increase the upper limit of the block weight.

### Block Rewards

The previous implementation invalidates a coinbase transaction if it locks an output to anything besides the Coinbase lock.

We relax this restriction and consider a coinbase transaction valid if its outputs are locked to either a Coinbase lock or a Contract lock.

Whereas currently there is a fixed amount paid entirely to the miner who mines a block (using a Coinbase lock), this proposal would allow users to accept a block as valid only if the correct amount of ZP is locked to the CGP contract.

It will be possible to vote during a voting interval for a particular distribution of the block reward between the miner and the CGP.

The distribution of the block reward between the miner and CGP in a particular block will be determined by the vote in the previous voting interval.

The allocation winner resulting from votes in a particular voting interval will be the weighted median of votes for the allocation in that voting interval.

There will be a limit as to the largest possible percentage change in block reward distribution resulting from a vote in a single voting interval.

This proposal sets the bounds to the largest possible percentage change in the block reward distribution (% to miner's income) resulting from a vote in a single voting interval at `15%`.

The winning allocation % comes into effect 100 blocks (the size of the coinbase maturity) after the voting interval finishes.

## Connection

When extending the local blockchain we check:

  1. The correct distribution of the block reward + fees, according to the allocations declared by the allocation tally winner.

  2. The contract lock output is locked to the CGP contract, and the amount matches the payout tally winner.

The CGP contract must be executed one block before and added to the payout block.

For the payout block - the node checks that the following conditions hold:

  1. If there is a payout winner for the tally of the previous interval - there must also be a payout in the payout block after that interval.

  2. There is at most one payout transaction per payout block.

  3. The payout transaction matches the payout winner of the tally.

## Links

Try the [Release Candidate](https://blockchaindevelopmentltd.com/proposal)
