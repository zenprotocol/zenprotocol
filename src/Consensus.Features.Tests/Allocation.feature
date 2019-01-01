Feature: Allocation


  Scenario: Capping (increase and decrease)

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 10    |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1
    
    And voteTx2 has the input voteTx1 index 0
    And voteTx2 votes on allocation of 15 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey1

    And voteTx3 has the input voteTx2 index 0
    And voteTx3 votes on allocation of 13 with 100 Zen in interval 2
    And voteTx3 is signed with genesisKey1

    And voteTx4 has the input voteTx3 index 0
    And voteTx4 votes on allocation of 1 with 100 Zen in interval 3
    And voteTx4 is signed with genesisKey1

    # Interval 0 - Block 1 (1st vote)
    When validating a block containing voteTx1 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 1 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    # min(0+10, 50) = 10 
    Then CGP allocation should be 10

    # Interval 1 - Block 1 (2nd vote)
    When validating a block containing voteTx2 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 15
    Then CGP allocation should be 10

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 15
    Then CGP allocation should be 10

    # Interval 2 - Block 0 (3rd vote)
    When validating a block containing voteTx3 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 13
    # min(10+10, 15) = 15
    Then CGP allocation should be 15

    # Interval 2 - Block 1
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 13
    Then CGP allocation should be 15

    # Interval 2 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 13
    Then CGP allocation should be 15

    # Interval 3 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    # max(15-10, 13) = 13
    Then CGP allocation should be 13

    # Interval 3 - Block 1 (4th vote)
    When validating a block containing voteTx4 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 1
    Then CGP allocation should be 13
    
    # Interval 3 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 1
    Then CGP allocation should be 13

    # Interval 4 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    # max(13-10, 1) = 3
    Then CGP allocation should be 3


  Scenario: Coinbase is calculated correctly
    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 20 with 100 Zen in interval 1
    And voteTx1 is signed with genesisKey1

    # Interval 0 - Block 1
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 0

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 0

    # Interval 1 - Block 0
    When validating an empty block bk1 extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 0
    Then coinbase reward in block bk1 should be 50 Zen

    # Interval 1 - Block 1
    When validating a block containing voteTx1 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20
    Then CGP allocation should be 0

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20
    Then CGP allocation should be 0

    # Interval 2 - Block 0
    When validating an empty block bk2 extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 20
    Then coinbase reward in block bk2 should be 40 Zen


  Scenario: Allocation should stay the same when no allocation votes are given
    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 10    |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input voteTx1 index 0
    And voteTx2 votes on allocation of 50 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey1

    And voteTx3 has the input voteTx2 index 0
    And voteTx3 votes on allocation of 25 with 100 Zen in interval 2
    And voteTx3 is signed with genesisKey1

    # Interval 0 - Block 1 (1st vote)
    When validating a block containing voteTx1 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 1 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 10

    # Interval 1 - Block 1 (2nd vote)
    When validating a block containing voteTx2 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 10

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 10

    # Interval 2 - Block 0 (3rd vote)
    When validating a block containing voteTx3 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 25
    Then CGP allocation should be 20

    # Interval 2 - Block 1
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 25
    Then CGP allocation should be 20

    # Interval 2 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 25
    Then CGP allocation should be 20

    # EMPTY INTERVAL
    # +----------------------------------------------------+
    # Interval 3 - Block 0 (No more rising)
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 25
    
    # Interval 3 - Block 1
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 25
    
    # Interval 3 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 25
    # +----------------------------------------------------+
    
    # Interval 4 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    # The allocation stays the same
    Then CGP allocation should be 25


  Scenario: Allocation should converge to 0% given 0% allocation votes (with capped steps)
    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 10    |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input voteTx1 index 0
    And voteTx2 votes on allocation of 50 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey1

    And voteTx3 has the input voteTx2 index 0
    And voteTx3 votes on allocation of 25 with 100 Zen in interval 2
    And voteTx3 is signed with genesisKey1

    And voteTx4 has the input voteTx3 index 0
    And voteTx4 votes on allocation of 0 with 100 Zen in interval 3
    And voteTx4 is signed with genesisKey1

    And voteTx5 has the input voteTx4 index 0
    And voteTx5 votes on allocation of 0 with 100 Zen in interval 4
    And voteTx5 is signed with genesisKey1

    And voteTx6 has the input voteTx5 index 0
    And voteTx6 votes on allocation of 0 with 100 Zen in interval 5
    And voteTx6 is signed with genesisKey1

    # Interval 0 - Block 1 (1st vote)
    When validating a block containing voteTx1 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 1 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 10

    # Interval 1 - Block 1 (2nd vote)
    When validating a block containing voteTx2 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 10

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 10

    # Interval 2 - Block 0 (3rd vote)
    When validating a block containing voteTx3 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 25
    Then CGP allocation should be 20

    # Interval 2 - Block 1
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 25
    Then CGP allocation should be 20

    # Interval 2 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 25
    Then CGP allocation should be 20

    # Interval 3 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 25

    # Interval 3 - Block 1 (4th vote)
    When validating a block containing voteTx4 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 0
    Then CGP allocation should be 25

    # Interval 3 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 0
    Then CGP allocation should be 25

    # Interval 4 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 15

    # Interval 4 - Block 1 (5th vote)
    When validating a block containing voteTx5 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 0
    Then CGP allocation should be 15

    # Interval 4 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 0
    Then CGP allocation should be 15

    # Interval 5 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 5

    # Interval 5 - Block 1 (6th vote)
    When validating a block containing voteTx6 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 0
    Then CGP allocation should be 5

    # Interval 5 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 0
    Then CGP allocation should be 5
    
    # Interval 6 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 0


  Scenario: Allocation should reach 99% given allocation votes of 99% (with capped steps)
    
    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 25    |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx0 has the input genesisTx index 0
    And voteTx0 votes on allocation of 5 with 100 Zen in interval 0
    And voteTx0 is signed with genesisKey1

    And voteTx1 has the input voteTx0 index 0
    And voteTx1 votes on allocation of 99 with 100 Zen in interval 1
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input voteTx1 index 0
    And voteTx2 votes on allocation of 99 with 100 Zen in interval 2
    And voteTx2 is signed with genesisKey1

    And voteTx3 has the input voteTx2 index 0
    And voteTx3 votes on allocation of 99 with 100 Zen in interval 3
    And voteTx3 is signed with genesisKey1

    And voteTx4 has the input voteTx3 index 0
    And voteTx4 votes on allocation of 99 with 100 Zen in interval 4
    And voteTx4 is signed with genesisKey1

    # Interval 0 - Block 1 (1st vote)
    When validating a block containing voteTx0 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 5
    Then CGP allocation should be 0

    # Interval 0 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 5
    Then CGP allocation should be 0

    # Interval 1 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 5

    # Interval 1 - Block 1 (2nd vote)
    When validating a block containing voteTx1 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 5

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 5

    # Interval 2 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 30

    # Interval 2 - Block 1 (3rd vote)
    When validating a block containing voteTx2 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 30

    # Interval 2 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 30

    # Interval 3 - Block 0 (4th vote)
    When validating a block containing voteTx3 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 55

    # Interval 3 - Block 1
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 55

    # Interval 3 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 55

    # Interval 4 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 80

    # Interval 4 - Block 1 (5th vote)
    When validating a block containing voteTx4 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 80

    # Interval 4 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 99
    Then CGP allocation should be 80

    # Interval 5 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 99
