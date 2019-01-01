Feature: CGP
  
  Scenario: resetting

    Given chain params
      | Key                     | Value |
      | coinbaseMaturity        | 2     |
      | allocationCorrectionCap | 50    |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on payout of 11 for id1 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey2

    # Interval 0 - Block 1 (1st vote)
    When validating a block containing voteTx1 extending tip
    Then CGP allocation should be 0
    Then CGP payout should be none

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then CGP allocation should be 0
    Then CGP payout should be none
    
    # Interval 1 - Block 0
    When validating a block containing voteTx2 extending tip
    Then CGP allocation should be 50
    Then CGP payout should be none
    
    # Interval 1 - Block 1
    When validating an empty block extending tip
    Then CGP allocation should be 50
    Then CGP payout should be none
    
    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then CGP allocation should be 50
    Then CGP payout should be none

    # Interval 2 - Block 0
    When validating an empty block extending tip
    Then CGP allocation should be 50
    Then CGP payout should be of 11 Zen to id1

    # Interval 2 - Block 1
    When validating an empty block extending tip
    Then CGP allocation should be 50
    Then CGP payout should be of 11 Zen to id1
    
    # Interval 2 - Block 2
    When validating an empty block extending tip
    Then CGP allocation should be 50
    Then CGP payout should be of 11 Zen to id1
    
    # Interval 3 - Block 0
    When validating an empty block extending tip
    Then CGP allocation should be 50
    Then CGP payout should be none
  
    
  Scenario: payout occurs on interval + maturity

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on payout of 11 for id1 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey2

    # Interval 0 - Block 1 (1st vote)
    When validating block bk1 containing voteTx1 extending tip
    Then there shouldn't be a payout on block bk1

    # Interval 0 - Block 2
    When validating an empty block bk2 extending tip
    Then there shouldn't be a payout on block bk2
    
    # Interval 1 - Block 0
    When validating block bk3 containing voteTx2 extending tip
    Then there shouldn't be a payout on block bk3
    
    # Interval 1 - Block 1
    When validating an empty block bk4 extending tip
    Then there shouldn't be a payout on block bk4
    
    # Interval 1 - Block 2
    When validating an empty block bk5 extending tip
    Then there shouldn't be a payout on block bk5

    # Interval 2 - Block 0
    When validating an empty block bk6 extending tip
    Then there shouldn't be a payout on block bk6

    # Interval 2 - Block 1 (Interval + maturity)
    When validating an empty block bk7 extending tip
    Then there should be a payout on block bk7
    
  
  Scenario: sum and address are right

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on payout of 11 for id1 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey2

    # Interval 0 - Block 1 (1st vote)
    When validating block bk1 containing voteTx1 extending tip

    # Interval 0 - Block 2
    When validating an empty block bk2 extending tip
    
    # Interval 1 - Block 0
    When validating block bk3 containing voteTx2 extending tip

    # Interval 1 - Block 1
    When validating an empty block bk4 extending tip

    # Interval 1 - Block 2
    When validating an empty block bk5 extending tip

    # Interval 2 - Block 0
    When validating an empty block bk6 extending tip

    # Interval 2 - Block 1
    When validating an empty block bk7 extending tip
    Then there should be a payout on block bk7
    Then coinbase payout in block bk7 should be 11 Zen to id1
  
  
  Scenario: funds are deducted of, using payout as amount

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on payout of 11 for id1 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey2

    # Interval 0 - Block 1 (1st vote)
    When validating block bk1 containing voteTx1 extending tip
    Then CGP allocation should be 0
    Then coinbase reward in block bk1 should be 50 Zen
    Then CGP amount should be 0 Zen

    # Interval 0 - Block 2
    When validating an empty block bk2 extending tip
    Then CGP allocation should be 0
    Then coinbase reward in block bk2 should be 50 Zen
    Then CGP amount should be 0 Zen
    
    # Interval 1 - Block 0
    When validating block bk3 containing voteTx2 extending tip
    Then CGP allocation should be 50
    Then coinbase reward in block bk3 should be 25 Zen
    Then CGP amount should be 25 Zen

    # Interval 1 - Block 1
    When validating an empty block bk4 extending tip
    Then CGP allocation should be 50
    Then coinbase reward in block bk4 should be 25 Zen
    Then CGP amount should be 50 Zen

    # Interval 1 - Block 2
    When validating an empty block bk5 extending tip
    Then CGP allocation should be 50
    Then coinbase reward in block bk5 should be 25 Zen
    Then CGP amount should be 75 Zen

    # Interval 2 - Block 0
    When validating an empty block bk6 extending tip
    Then CGP allocation should be 50
    Then coinbase reward in block bk6 should be 25 Zen
    Then CGP amount should be 89 Zen

    # Interval 2 - Block 1
    When validating an empty block bk7 extending tip
    Then CGP allocation should be 50
    Then coinbase reward in block bk7 should be 25 Zen
    Then CGP amount should be 114 Zen
    
  
  Scenario: insufficient funds
    
    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 200 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 20 with 100 Zen in interval 1
    And voteTx1 votes on payout of 300 for id1 with 100 Zen in interval 1
    And voteTx1 is signed with genesisKey1

    # Interval 0 - Block 1
    When validating an empty block extending tip
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 1 - Block 0
    When validating an empty block bk1 extending tip
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 1 - Block 1
    When validating a block containing voteTx1 extending tip
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 2 - Block 0
    When validating an empty block extending tip
    Then CGP payout should be none
    Then CGP amount should be 10 Zen

    # Interval 2 - Block 1
    When validating an empty block bk2 extending tip
    Then CGP payout should be none
    Then CGP amount should be 20 Zen
    Then there shouldn't be a payout on block bk2
  
  
  Scenario: if funds are avail and payout votes are avail - expect payout output

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx2 has the input genesisTx index 1
    And voteTx1 votes on allocation of 20 with 100 Zen in interval 1
    And voteTx2 votes on payout of 3 for id1 with 100 Zen in interval 2
    And voteTx1 is signed with genesisKey1
    And voteTx2 is signed with genesisKey2

    # Interval 0 - Block 1
    When validating an empty block extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 1 - Block 0
    When validating an empty block bk1 extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 1 - Block 1 (Voting to allocate resources)
    When validating a block containing voteTx1 extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be none
    Then CGP amount should be 0 Zen

    # Interval 2 - Block 0
    When validating an empty block bk2 extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be none
    Then CGP amount should be 10 Zen
    
    # Interval 2 - Block 1 (Voting on payout)
    When validating a block containing voteTx2 extending tip
    Then tally should have a total of 100 payout votes
    Then tally payout result should be of 3 Zen to id1
    Then CGP payout should be none
    Then CGP amount should be 20 Zen

    # Interval 2 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 payout votes
    Then tally payout result should be of 3 Zen to id1
    Then CGP payout should be none
    Then CGP amount should be 30 Zen

    # Interval 3 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 37 Zen

    # Interval 3 - Block 1
    When validating an empty block bkp extending tip
    Then tally should have a total of 0 payout votes
    Then tally payout result should be none
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 47 Zen
    Then there should be a payout on block bkp
  

  Scenario: Fund should be empty in the first interval

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 200 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 20 with 100 Zen in interval 0
    And voteTx1 votes on payout of 300 for id1 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    # Interval 0 - Block 1
    When validating a block containing voteTx1 extending tip
    Then CGP amount should be 0 Zen
    
    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then CGP amount should be 0 Zen
