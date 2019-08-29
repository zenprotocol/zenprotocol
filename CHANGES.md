# Changes

This proposed protocol upgrade would seek to create a '**Common Goods Pool**' (or '**CGP**'), which would hold assets, and distribute its funds according to an on-chain vote open to ZP holders.

Users will be able to vote on the proportion of the coinbase outputs locked to the CGP contract.

### Interval blocks
    VoteInterval        = from (10,000 * N + 9,001) to (10,000 * N + 10,000)
    TallyActorExecution = 10,000 * N + 10,050
    AllocationInterval  = from (10,000 * N + 10,101) to (10,000 * N + 20,100)


# Voting

### Voting Intervals

Vote Intervals occur every 10,000 blocks.

In order to vote you must have your ZP in your wallet by the vote snapshot, after which users are provided with a time frame of 1,000 blocks to cast a vote.

    VoteSnapshot      = 10,000 * N + 9,000
    VoteIntervalStart = 10,000 * N + 9,001
    VoteIntervalEnd   = 10,000 * N + 10,000
    (where N is the interval number)

The weight of your vote will be determined by the amount of available ZP you have by the time of the snapshot.

### Casting a Ballot

Users cast a ballot by sending meta data (using the contract message body) to the Voting contract.

The Tally Actor will count the votes, and determine the winner.

Users may vote for 2 independent items:

    • 'CgpAllocation': Users vote on the % of the block reward to be gifted to the CGP each block.
    (Nodes will not build on top of a block that does not contain an coinbase transaction with a sufficient AllocationOutput)

    • 'PayoutTx': Each voting interval people can vote on the recipient and the list of assets & amounts the recipient will receive.

### Tallying

At the end of the voting interval an external actor will calculate the vote (this provides for proper modulation and implements best - such as soft fork - practices).

    TallyActorExecution = 10,000 * N + 10,050

The Tally Actor will consider a vote as valid if it:

    1. Does not attempt to vote with the same ZP more than once per interval for each item.

    2. That the %Δ of the CgpAllocation between intervals is => 15%

    3. That the CgpAllocation % is no larger than 90%.

    4. That the PayoutTx votes to pay out assets which are UTXOs of the CgpContract at the snapshot block.

### Block Rewards

Our previous implementation invalidates a coinbase transaction if it locks an output to anything besides the "CoinbaseLocktype"
We relax this restriction and consider a coinbase transaction valid if its outputs are locked to either a Coinbase lock or a Contract lock.
Whereas currently there is a fixed  paid entirely to the miner who mines a block (using a Coinbase lock), this proposal would allow users to accept a block as valid only if a sufficient amount
of ZP are locked to the CgpContract.

It will be possible to vote during a voting interval for a particular distribution of the blockReward between the miner and the CGP.

The distribution of the blockReward between the miner and CGP in a particular block shall be determined by the the vote in the previous voting interval.

The allocation winner resulting from votes in a particular voting interval shall be considered to be the weighted median of votes for particular distributions in that voting interval.

There shall be a limit as to the largest possible percentage change in blockReward distribution resulting from a vote in a single voting interval.

This proposal sets the bounds to the largest possible percentage change in blockReward distribution (Δ to miner's income) resulting from a vote in a single voting interval at `15%`.

The winning allocation % comes into effect 100 blocks after the voting interval finishes.

    AllocationIntervalStart = 10,000 * N + 10,101
    AllocationIntervalEnd   = 10,000 * N + 20,100

## Payout Transactions
Payout Transactions occur at the beginning of an allocation interval.

PayoutBlock = 10,000 * N + 10,101

### Example

Suppose the vote at the end of the voting interval is as follows:

| CGP Block Reward Allocation (%) | Total number of votes | Cumulative Total |
|:-------------------------------:|:---------------------:|:----------------:|
| 0                               | 1800                  | 1800
| 10                              | 1560                  | 3360
| 20                              | 1353                  | 4713
| 30                              | 1173                  | 5886
| 40                              | 1016                  | 6902
| 50                              | 881                   | 7783
| 60                              | 764                   | 8547
| 70                              | 662                   | 9209
| 80                              | 574                   | 9783
| 90                              | 217                   | 10000

Here the weighted median would be the midpoint of the reward vs cumulative total votes, which would be `30%`. If the distribution of block rewards at the start of the voting interval was set at `0%` allocated to the CGP, and `100%` allocated to miners, then the weighted median presents a change greater than `15%`, and so the outcome distribution would be `15%` of the block reward going to the CGP, and `85%` going to the miner during the next voting interval.

## CGP Payouts

It will be possible to vote during a voting interval for a specific payout of funds controlled by the CgpContract to a public key hash or contract.
The CgpContract will pay out at the start of an allocation interval, and the amount and recipient for this payout of its funds shall be determined by the vote in the previous voting interval.
As there is no objectively superior voting scheme for this case (consider approval voting, alternative voting, etc.) the scheme used to determine the payout from votes in the previous voting interval shall be 'winner takes all', as in the proposed payout with the greatest amount of votes in the previous voting interval will be the payout that is made.
### Example

Suppose the vote at the end of the a voting interval is as follows:

| Payout Recipient | Payout List    | Total number of votes |
|:----------------:|:--------------:|:---------------------:|
| PKHash Alice     | 4000 ZP        | 2700
| PKHash Alice     | 9000 Asset1    | 2800
| PKHash Bob       | 8000 ZP        | 1500
| ContractID C     | 2000 ZP, Asset1| 3000

Here the proposal with the greatest total number of votes is that `ContractID C` would receive `2000ZP and 10 USDT`, and so the CGP will make this payout at the start of the next voting interval.

