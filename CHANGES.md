# Changes

This proposed protocol upgrade would seek to create a 'Common Goods Pool'( or 'CGP'), which would hold ZP, and distribute it's ZP according to an on-chain vote open to ZP holders.

## Voting Intervals

This proposal would introduce to the protocol the concept of a '*voting interval*' - a span of `L` blocks.
Voting intervals begin at blocks with block number `n` such that `n % L = n0 % L`, where `n0` is the genesis block number.
Voting intervals end at blocks with block number `n` such that `(n + 1) % L = n0 % L`, where `n0` is the genesis block number.

This proposal sets the voting interval size `L` at `10,000` blocks.

## Block Rewards

Whereas currently there is a fixed block reward paid entirely to the miner who mines a block as using a coinbase lock, this proposal would make variable the portion of the block reward paid to the miner using a coinbase lock.
The portion of the block reward not paid to the miner would be allocated to the CGP.

It will be possible to vote during a voting interval for a particular distribution of the block reward between the miner and the CGP.
The distribution of the block reward between the miner and CGP in a particular block shall be determined by the the vote in the previous voting interval.

The distribution resulting from votes in a particular voting interval shall be considered to be the weighted median of votes for particular distributions in that voting interval.

There shall be a limit as to the largest possible percentage change in block reward distribution resulting from a vote in a single voting interval.

This proposal sets the limit to the largest possible percentage change in block reward distribution resulting from a vote in a single voting interval at `15%`.

### Example

Suppose the vote at the end of the a voting interval is as follows:

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

It will be possible to vote during a voting interval for a specific payout of funds controlled by the CGP to a public key hash or contract.
The CGP will pay out at the start of a voting interval, and the amount and recipient for this payout of it's funds shall be determined by the vote in the previous voting interval.
As there is no objectively superior voting scheme for this case (consider approval voting, alternative voting, etc.) the scheme used to determine the payout from votes in the previous voting interval shall be 'winner takes all', as in the proposed payout with the greatest amount of votes in the previous voting interval will be the payout that is made.
If a different voting scheme is desired, it could be implemented via a contract, and users could vote to have the CGP pay out to the aforementioned contract, which would in turn pay out according to the outcome of a vote administered by the contract.

### Example

Suppose the vote at the end of the a voting interval is as follows:

| Payout Recipient | Payout Amount | Total number of votes |
|:----------------:|:-------------:|:---------------------:|
| PKHash Alice     | 4000          | 2700
| PKHash Alice     | 9000          | 2800
| PKHash Bob       | 8000          | 1500
| ContractID C     | 2000          | 3000

Here the proposal with the greatest total number of votes is that `ContractID C` would receive `2000ZP`, and so the CGP will make this payout at the start of the next voting interval.

## Voting Locks

This proposal would introduce a new lock type - a 'voting lock'. A voting lock includes four fields,

| Field Name | Description
|:----------:| -----------
| Interval   | Indicates the voting interval in which the vote should be considered
| Allocation | The percentage of the block reward that should be allocated to the CGP
| Payout     | The recipient of the payout and the amount of ZP to be paid out to that recipient
| PKHash     | The hash of the public key of the voting party

where the Allocation and Payout fields are each optional.

Voting locks are unlocked in the same manner as PKLocks, where proving ownership of the corresponding secret key allows spending a voting lock.
Voting locks may appear as the output to transactions only when the PKHash in the voting lock appears also in the inputs being spent.
Only the ZP token may be locked to a vote.

It is possible to create voting locks specifying future or past voting intervals.
In the case of indicating a past voting interval, the voting lock will have no effect. In the case of indicating a future voting interval, the votes will take effect in the indicated voting interval.
