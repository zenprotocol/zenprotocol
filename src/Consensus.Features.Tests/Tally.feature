Feature: Tally

# -------------------------------------------------------------------------------------------------------------------- #
# ======================================== VOTING ==================================================================== #
# -------------------------------------------------------------------------------------------------------------------- #

  Scenario: Resetting

    Given chain params
      | Key      | Value |
      | interval | 3     |

    Given genesisTx locks 200 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx
    
    And voteTx has the input genesisTx index 0
    And voteTx votes on allocation of 50 with 100 Zen in interval 0
    And voteTx votes on payout of 3 for id1 with 100 Zen in interval 0
    And voteTx is signed with genesisKey1

    # Interval 0 - Block 1
    When validating a block containing voteTx extending tip
    Then tally should have a total of 100 allocation votes
    Then tally should have a total of 100 payout votes

    # Interval 0 - Block 2 (end of interval)
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally should have a total of 100 payout votes

    # Interval 1 - Block 0 (tally should reset) 
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally should have a total of 0 payout votes


  Scenario: Unvoting in new interval

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | interval                | 4     |

    # Interval 1 - Block 0
    Given genesisTx locks 100 Zen to genesisKey1
    Given genesisTx locks 50 Zen to genesisKey2
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on allocation of 45 with 50 Zen in interval 1
    And voteTx2 is signed with genesisKey2

    And unvoteTx1 has the input voteTx1 index 0
    And unvoteTx1 locks 100 Zen to key1
    And unvoteTx1 is signed with genesisKey1

    # Interval 1 - Block 1       (voting)
    When validating a block containing voteTx1 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 1 - Block 3
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0

    # Interval 2 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 50

    # Interval 2 - Block 1       (unvoting)
    When validating a block containing unvoteTx1 extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 50

    # Interval 2 - Block 2       (another vote)
    When validating a block containing voteTx2 extending tip
    Then tally should have a total of 50 allocation votes
    Then tally allocation result should be 45
    Then CGP allocation should be 50

    # Interval 2 - Block 3
    When validating an empty block extending tip
    Then tally should have a total of 50 allocation votes
    Then tally allocation result should be 45
    Then CGP allocation should be 50
    
    # Interval 3 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 45


  Scenario: Double voting in same block

    Given genesisTx locks 100 Zen to genesisKey1
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    # Revote with same resources.  
    And voteTx2 has the input voteTx1 index 0
    And voteTx2 votes on allocation of 75 with 100 Zen in interval 0
    And voteTx2 is signed with genesisKey1

    When validating a block containing voteTx1,voteTx2 extending tip

    # The 2nd vote is what counts
    Then tally should have a total of 100 allocation votes
    Then tally should have a total of 0 allocation vote for allocation of 50
    Then tally should have a total of 100 allocation vote for allocation of 75


  Scenario: Double voting in new block

    Given genesisTx locks 100 Zen to genesisKey1
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    # Revote with same resources.
    And voteTx2 has the input voteTx1 index 0
    And voteTx2 votes on allocation of 75 with 100 Zen in interval 0
    And voteTx2 is signed with genesisKey1

    When validating a block containing voteTx1 extending tip
    And validating a block containing voteTx2 extending tip

    # The 2nd vote is what counts
    Then tally should have a total of 100 allocation votes
    Then tally should have a total of 0 allocation vote for allocation of 50
    Then tally should have a total of 100 allocation vote for allocation of 75


  Scenario: Double voting in new interval

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | interval                | 4     |
    And genesisTx locks 100 Zen to genesisKey1
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input voteTx1 index 0
    And voteTx2 votes on allocation of 20 with 100 Zen in interval 1
    And voteTx2 is signed with genesisKey1

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
    
    # Interval 0 - Block 3
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 50
    Then CGP allocation should be 0
    
    # Interval 1 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 50
    
    # Interval 1 - Block 1 (2nd vote)
    When validating a block containing voteTx2 extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20
    Then CGP allocation should be 50
    
    # Interval 1 - Block 2
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20
    Then CGP allocation should be 50
    
    # Interval 1 - Block 3
    When validating an empty block extending tip
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20
    Then CGP allocation should be 50
    
    # Interval 2 - Block 0
    When validating an empty block extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then CGP allocation should be 20
    
    
  Scenario: Unvoting in same block

    Given genesisTx locks 100 Zen to genesisKey1
    And genesis has genesisTx

    And voteTx has the input genesisTx index 0
    And voteTx votes on allocation of 50 with 100 Zen in interval 0
    And voteTx is signed with genesisKey1

    And unvoteTx has the input voteTx index 0
    And unvoteTx locks 100 Zen to key1
    And unvoteTx is signed with genesisKey1

    When validating a block containing voteTx,unvoteTx extending tip
    Then tally should have a total of 0 allocation votes


  Scenario: Unvoting in new block

    Given genesisTx locks 100 Zen to genesisKey1
    And genesis has genesisTx

    And voteTx has the input genesisTx index 0
    And voteTx votes on allocation of 50 with 100 Zen in interval 0
    And voteTx is signed with genesisKey1

    And unvoteTx has the input voteTx index 0
    And unvoteTx locks 100 Zen to key1
    And unvoteTx is signed with genesisKey1

    # Block 1
    When validating a block containing voteTx extending tip
    Then tally should have a total of 100 allocation votes

    # Block 2
    When validating a block containing unvoteTx extending tip
    Then tally should have a total of 0 allocation votes
    
    
  Scenario: Voting with an amount smaller than inputs

    Given genesisTx locks 100 Zen to genesisKey
    And genesis has genesisTx

    And voteTx has the input genesisTx index 0
    And voteTx votes on allocation of 50 with 50 Zen in interval 0
    And voteTx locks 50 Zen to key1
    And voteTx is signed with genesisKey

    When validating a block containing voteTx extending tip

    Then tally should have a total of 50 allocation votes


  Scenario: Voting with an amount smaller than inputs, sending the rest to another voter (same block)

    Given genesisTx locks 100 Zen to genesisKey
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 50 Zen in interval 0
    And voteTx1 locks 50 Zen to key1
    And voteTx1 is signed with genesisKey

    And voteTx2 has the input voteTx1 index 1
    And voteTx2 votes on allocation of 50 with 50 Zen in interval 0
    And voteTx2 is signed with key1

    When validating a block containing voteTx1,voteTx2 extending tip

    Then tally should have a total of 100 allocation votes


  Scenario: Voting with an amount smaller than inputs, sending the rest to another voter (another block)

    Given genesisTx locks 100 Zen to genesisKey
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 50 Zen in interval 0
    And voteTx1 locks 50 Zen to key1
    And voteTx1 is signed with genesisKey

    And voteTx2 has the input voteTx1 index 1
    And voteTx2 votes on allocation of 50 with 50 Zen in interval 0
    And voteTx2 is signed with key1

    When validating a block containing voteTx1 extending tip
    And validating a block containing voteTx2 extending tip

    # Both votes count
    Then tally should have a total of 100 allocation votes


  Scenario: Voting with an amount greater than inputs

    Given genesisTx locks 100 Zen to genesisKey
    And genesis has genesisTx

    And voteTx has the input genesisTx index 0
    And voteTx votes on allocation of 50 with 200 Zen in interval 0
    And voteTx is signed with genesisKey
    
    Then voteTx validation should yield invalid amounts

    
    
# -------------------------------------------------------------------------------------------------------------------- #
# ======================================== TALLYING ================================================================== #
# -------------------------------------------------------------------------------------------------------------------- #

  Scenario: Single vote in a block

    Given genesisTx locks 100 Zen to genesisKey1
    And genesis has genesisTx

    And voteTx has the input genesisTx index 0
    And voteTx votes on allocation of 50 with 100 Zen in interval 0
    And voteTx is signed with genesisKey1

    When validating a block containing voteTx extending tip

    Then tally should have a total of 100 allocation vote


  Scenario: Two votes in a block, same allocation

    Given genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx2 is signed with genesisKey2

    When validating a block containing voteTx1,voteTx2 extending tip

    Then tally should have a total of 200 allocation votes


  Scenario: Two votes in a block, different allocations

    Given genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on allocation of 75 with 100 Zen in interval 0
    And voteTx2 is signed with genesisKey2

    When validating a block containing voteTx1,voteTx2 extending tip

    Then tally should have a total of 200 allocation votes
    Then tally should have a total of 100 allocation vote for allocation of 50
    Then tally should have a total of 100 allocation vote for allocation of 75


  Scenario: Should pick max allocation (1)

    Given genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 200 Zen to genesisKey2
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 50 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on allocation of 75 with 200 Zen in interval 0
    And voteTx2 is signed with genesisKey2

    When validating a block containing voteTx1,voteTx2 extending tip

    Then tally should have a total of 300 allocation votes
    Then tally should have a total of 100 allocation vote for allocation of 50
    Then tally should have a total of 200 allocation vote for allocation of 75

    Then tally allocation result should be 75

    
  Scenario: Should pick max allocation (2)

    Given genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 50 Zen to genesisKey2
    And genesisTx locks 50 Zen to genesisKey3
    And genesisTx locks 50 Zen to genesisKey4
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on allocation of 75 with 100 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on allocation of 15 with 50 Zen in interval 0
    And voteTx2 is signed with genesisKey2

    And voteTx3 has the input genesisTx index 2
    And voteTx3 votes on allocation of 15 with 50 Zen in interval 0
    And voteTx3 is signed with genesisKey3

    And voteTx4 has the input genesisTx index 3
    And voteTx4 votes on allocation of 15 with 50 Zen in interval 0
    And voteTx4 is signed with genesisKey4

    When validating a block containing voteTx1,voteTx2 extending tip
    When validating a block containing voteTx3,voteTx4 extending tip

    Then tally should have a total of 250 allocation votes
    Then tally should have a total of 150 allocation vote for allocation of 15
    Then tally should have a total of 100 allocation vote for allocation of 75

    Then tally allocation result should be 15


  Scenario: Should pick max address * amount (1)

    Given genesisTx locks 99 Zen to genesisKey1
    And genesisTx locks 50 Zen to genesisKey2
    And genesisTx locks 50 Zen to genesisKey3
    And genesisTx locks 50 Zen to genesisKey4
    And genesisTx locks 50 Zen to genesisKey5
    And genesisTx locks 50 Zen to genesisKey6
    And genesisTx locks 50 Zen to genesisKey7
    And genesisTx locks 50 Zen to genesisKey8
    And genesis has genesisTx

    And voteTx1 has the input genesisTx index 0
    And voteTx1 votes on payout of 150 for pk1 with 99 Zen in interval 0
    And voteTx1 is signed with genesisKey1

    And voteTx2 has the input genesisTx index 1
    And voteTx2 votes on payout of 150 for pk1 with 50 Zen in interval 0
    And voteTx2 is signed with genesisKey2

    And voteTx3 has the input genesisTx index 2
    And voteTx3 votes on payout of 50 for pk1 with 50 Zen in interval 0
    And voteTx3 is signed with genesisKey3

    And voteTx4 has the input genesisTx index 3
    And voteTx4 votes on payout of 25 for pk1 with 50 Zen in interval 0
    And voteTx4 is signed with genesisKey4

    And voteTx5 has the input genesisTx index 4
    And voteTx5 votes on payout of 24 for pk1 with 50 Zen in interval 0
    And voteTx5 is signed with genesisKey5

    And voteTx6 has the input genesisTx index 5
    And voteTx6 votes on payout of 123 for pk2 with 50 Zen in interval 0
    And voteTx6 is signed with genesisKey6

    And voteTx7 has the input genesisTx index 6
    And voteTx7 votes on payout of 123 for pk2 with 50 Zen in interval 0
    And voteTx7 is signed with genesisKey7

    And voteTx8 has the input genesisTx index 7
    And voteTx8 votes on payout of 123 for pk2 with 50 Zen in interval 0
    And voteTx8 is signed with genesisKey8

    When validating a block containing voteTx1,voteTx2,voteTx5,voteTx7 extending tip
    When validating a block containing voteTx4,voteTx8 extending tip
    When validating a block containing voteTx3,voteTx6 extending tip

    Then tally should have a total of 449 payout votes
    Then tally should have a total of 449 payout votes
    Then tally should have a total of 149 payout votes for payout of 150 Zen to pk1
    Then tally should have a total of 50 payout votes for payout of 50 Zen to pk1
    Then tally should have a total of 50 payout votes for payout of 25 Zen to pk1
    Then tally should have a total of 50 payout votes for payout of 24 Zen to pk1
    Then tally should have a total of 150 payout votes for payout of 123 Zen to pk2

    Then tally payout result should be of 123 Zen to pk2
