Feature: CGP Blockchain

  Scenario: Connecting longest chain available after an orphan is found

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1alloc
    And genesisTx locks 100 Zen to genesisKey1payout
    And genesisTx locks 100 Zen to genesisKey2alloc
    And genesisTx locks 100 Zen to genesisKey2payout
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1alloc has the input genesisTx index 0
    And voteTx1payout has the input genesisTx index 1
    And voteTx1alloc votes on allocation of 20 with 100 Zen in interval 0
    And voteTx1payout votes on payout of 3 for id1 with 100 Zen in interval 1
    And voteTx1alloc is signed with genesisKey1alloc
    And voteTx1payout is signed with genesisKey1payout

    And voteTx2alloc has the input genesisTx index 2
    And voteTx2payout has the input genesisTx index 3
    And voteTx2alloc votes on allocation of 50 with 100 Zen in interval 0
    And voteTx2payout votes on payout of 15 for id2 with 100 Zen in interval 1
    And voteTx2alloc is signed with genesisKey2alloc
    And voteTx2payout is signed with genesisKey2payout

    # Interval 0 - Block 1 (fork block)
    When validating an empty block bkfork extending tip
    Then tally should have a total of 0 payout votes
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 0
    Then CGP payout should be none
    Then CGP amount should be 0 Zen
    Then coinbase reward in block bkfork should be 50 Zen

    # Chain A - Interval 0 - Block 2
    When validating block bka1 containing voteTx1alloc extending bkfork
    Then tally allocation result should be 20
    Then tally payout result should be none
    Then CGP allocation should be 0
    Then CGP payout should be none
    Then CGP amount should be 0 Zen
    Then coinbase reward in block bka1 should be 50 Zen

    # Chain A - Interval 1 - Block 0
    When validating an empty block bka2 extending bka1
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 10 Zen
    Then coinbase reward in block bka2 should be 40 Zen

    # Chain A - Interval 1 - Block 1
    When validating block bka3 containing voteTx1payout extending bka2
    Then tally allocation result should be none
    Then tally payout result should be of 3 Zen to id1
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 20 Zen
    Then coinbase reward in block bka3 should be 40 Zen

    # Chain A - Interval 1 - Block 2
    When validating an empty block bka4 extending bka3
    Then tally allocation result should be none
    Then tally payout result should be of 3 Zen to id1
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 30 Zen
    Then coinbase reward in block bka4 should be 40 Zen

    # Chain A - Interval 2 - Block 0
    When validating an empty block bka5 extending bka4
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 37 Zen
    Then coinbase reward in block bka5 should be 40 Zen

    # Chain A - Interval 2 - Block 1
    When validating an empty block bka6 extending bka5
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 47 Zen
    Then there should be a payout on block bka6
    Then coinbase payout in block bka6 should be 3 Zen to id1
    Then coinbase reward in block bka6 should be 40 Zen

    ### Side chain
    
    # Chain B - Interval 0 - Block 2
    When validating a detached block bkb1 containing voteTx2alloc extending bkfork
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb1 should be 50 Zen

    # Chain B - Interval 1 - Block 0
    When validating an empty block bkb2 extending bkb1
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On orphaned sidechain: CGP amount should be 0+25=25 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb2 should be 25 Zen

    # Chain B - Interval 1 - Block 1
    When validating block bkb3 containing voteTx2payout extending bkb2
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On orphaned sidechain: CGP amount should be 25+25=50 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb3 should be 25 Zen

    # Chain B - Interval 1 - Block 2
    When validating an empty block bkb4 extending bkb3
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On orphaned sidechain: CGP amount should be 50+25=75 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb4 should be 25 Zen

    # Chain B - Interval 2 - Block 0
    When validating an empty block bkb5 extending bkb4
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On orphaned sidechain: CGP amount should be 75+25=100 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb5 should be 25 Zen

    # Chain B - Interval 2 - Block 1
    When validating an empty block bkb6 extending bkb5
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On orphaned sidechain: CGP amount should be 100+25=125 Zen
    Then CGP amount should be 47 Zen
    Then there should be a payout on block bkb6
    Then coinbase payout in block bkb6 should be 15 Zen to id2
    Then coinbase reward in block bkb6 should be 25 Zen
               
    # NOT REORG (because the longer chain is orphaned)
    # Chain B - Interval 2 - Block 2
    When validating an empty block bkb7 extending bkb6
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then tally payout result should be none
    # On orphaned sidechain: CGP allocation should be 50
    Then CGP allocation should be 20
    # On orphaned sidechain: CGP payout should be of 15 Zen to id2
    Then CGP payout should be of 3 Zen to id1
    # On orphaned sidechain: CGP amount should be 125+25-15=135 Zen
    Then CGP amount should be 47 Zen

    # Chain B - Interval 3 - Block 0
    When validating an empty block bkb8 extending bkb7
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then tally payout result should be none
    # On orphaned sidechain: CGP allocation should be 50
    Then CGP allocation should be 20
    # On orphaned sidechain: CGP payout should be none
    Then CGP payout should be of 3 Zen to id1
    # On orphaned sidechain: CGP amount should be 135+25=160 Zen
    Then CGP amount should be 47 Zen
    
    When reconnecting block bkb1
    Then CGP allocation should be 50
    Then CGP payout should be none
    Then CGP amount should be 160 Zen


  Scenario: Reorg gives expected outcome

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1alloc
    And genesisTx locks 100 Zen to genesisKey1payout
    And genesisTx locks 100 Zen to genesisKey2alloc
    And genesisTx locks 100 Zen to genesisKey2payout
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1alloc has the input genesisTx index 0
    And voteTx1payout has the input genesisTx index 1
    And voteTx1alloc votes on allocation of 20 with 100 Zen in interval 0
    And voteTx1payout votes on payout of 3 for id1 with 100 Zen in interval 1
    And voteTx1alloc is signed with genesisKey1alloc
    And voteTx1payout is signed with genesisKey1payout

    And voteTx2alloc has the input genesisTx index 2
    And voteTx2payout has the input genesisTx index 3
    And voteTx2alloc votes on allocation of 50 with 100 Zen in interval 0
    And voteTx2payout votes on payout of 15 for id2 with 100 Zen in interval 1
    And voteTx2alloc is signed with genesisKey2alloc
    And voteTx2payout is signed with genesisKey2payout

    # Interval 0 - Block 1 (fork block)
    When validating an empty block bkfork extending tip
    Then tally should have a total of 0 payout votes
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 0
    Then CGP payout should be none
    Then CGP amount should be 0 Zen
    Then coinbase reward in block bkfork should be 50 Zen

    # Chain A - Interval 0 - Block 2
    When validating block bka1 containing voteTx1alloc extending bkfork
    Then tally allocation result should be 20
    Then tally payout result should be none
    Then CGP allocation should be 0
    Then CGP payout should be none
    Then CGP amount should be 0 Zen
    Then coinbase reward in block bka1 should be 50 Zen

    # Chain A - Interval 1 - Block 0
    When validating an empty block bka2 extending bka1
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 10 Zen
    Then coinbase reward in block bka2 should be 40 Zen

    # Chain A - Interval 1 - Block 1
    When validating block bka3 containing voteTx1payout extending bka2
    Then tally allocation result should be none
    Then tally payout result should be of 3 Zen to id1
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 20 Zen
    Then coinbase reward in block bka3 should be 40 Zen

    # Chain A - Interval 1 - Block 2
    When validating an empty block bka4 extending bka3
    Then tally allocation result should be none
    Then tally payout result should be of 3 Zen to id1
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 30 Zen
    Then coinbase reward in block bka4 should be 40 Zen

    # Chain A - Interval 2 - Block 0
    When validating an empty block bka5 extending bka4
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 37 Zen
    Then coinbase reward in block bka5 should be 40 Zen

    # Chain A - Interval 2 - Block 1
    When validating an empty block bka6 extending bka5
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 47 Zen
    Then there should be a payout on block bka6
    Then coinbase payout in block bka6 should be 3 Zen to id1
    Then coinbase reward in block bka6 should be 40 Zen

    ### Side chain
    
    # Chain B - Interval 0 - Block 2
    When validating block bkb1 containing voteTx2alloc extending bkfork
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb1 should be 50 Zen

    # Chain B - Interval 1 - Block 0
    When validating an empty block bkb2 extending bkb1
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 0+25=25 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb2 should be 25 Zen

    # Chain B - Interval 1 - Block 1
    When validating block bkb3 containing voteTx2payout extending bkb2
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 25+25=50 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb3 should be 25 Zen

    # Chain B - Interval 1 - Block 2
    When validating an empty block bkb4 extending bkb3
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 50+25=75 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb4 should be 25 Zen

    # Chain B - Interval 2 - Block 0
    When validating an empty block bkb5 extending bkb4
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 75+25=100 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb5 should be 25 Zen

    # Chain B - Interval 2 - Block 1
    When validating an empty block bkb6 extending bkb5
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 100+25=125 Zen
    Then CGP amount should be 47 Zen
    Then there should be a payout on block bkb6
    Then coinbase payout in block bkb6 should be 15 Zen to id2
    Then coinbase reward in block bkb6 should be 25 Zen
               
    # REORG
    # Chain B - Interval 2 - Block 2
    When validating an empty block bkb7 extending bkb6
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 50
    Then CGP payout should be of 15 Zen to id2
    # 125+25-15=135 Zen
    Then CGP amount should be 135 Zen

    # Chain B - Interval 3 - Block 0
    When validating an empty block bkb8 extending bkb7
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 50
    Then CGP payout should be none
    # 135+25=160 Zen
    Then CGP amount should be 160 Zen


  Scenario: Reorg removes all votes (first interval)

    Given genesisTx locks 100 Zen to genesisKey1alloc
    # Block 0
    And genesis has genesisTx

    And voteTx1alloc has the input genesisTx index 0
    And voteTx1alloc votes on allocation of 20 with 100 Zen in interval 0
    And voteTx1alloc is signed with genesisKey1alloc
    
    # Block 1 (fork block)
    When validating an empty block bkfork extending tip
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none

    # Chain A - Block 2
    When validating block bka1 containing voteTx1alloc extending bkfork
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain A - Block 3
    When validating an empty block bka2 extending bka1
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain A - Block 4
    When validating an empty block bka3 extending bka2
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain A - Block 5
    When validating an empty block bka4 extending bka3
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain A - Block 6
    When validating an empty block bka5 extending bka4
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain A - Block 7
    When validating an empty block bka6 extending bka5
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    ### Side chain
    
    # Chain B - Block 2
    When validating an empty block bkb1 extending bkfork
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain B - Block 3
    When validating an empty block bkb2 extending bkb1
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain B - Block 4
    When validating an empty block bkb3 extending bkb2
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain B - Block 5
    When validating an empty block bkb4 extending bkb3
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain B - Block 6
    When validating an empty block bkb5 extending bkb4
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20

    # Chain B - Block 7
    When validating an empty block bkb6 extending bkb5
    Then tally should have a total of 100 allocation votes
    Then tally allocation result should be 20
               
    # REORG
    # Chain B - Block 8
    When validating an empty block bkb7 extending bkb6
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none

    # Chain B - Block 9
    When validating an empty block bkb8 extending bkb7
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    
    
  Scenario: Reorg removes all votes (through intervals)

    Given chain params
      | Key                     | Value |
      | allocationCorrectionCap | 50    |
      | coinbaseMaturity        | 2     |
      | interval                | 3     |
    And genesisTx locks 100 Zen to genesisKey1alloc
    And genesisTx locks 100 Zen to genesisKey1payout
    And genesisTx locks 100 Zen to genesisKey2alloc
    # Interval 0 - Block 0
    And genesis has genesisTx

    And voteTx1alloc has the input genesisTx index 0
    And voteTx1payout has the input genesisTx index 1
    And voteTx1alloc votes on allocation of 20 with 100 Zen in interval 0
    And voteTx1payout votes on payout of 3 for id1 with 100 Zen in interval 1
    And voteTx1alloc is signed with genesisKey1alloc
    And voteTx1payout is signed with genesisKey1payout

    And voteTx2alloc has the input genesisTx index 2
    And voteTx2alloc votes on allocation of 50 with 100 Zen in interval 0
    And voteTx2alloc is signed with genesisKey2alloc

    # Interval 0 - Block 1 (fork block)
    When validating an empty block bkfork extending tip
    Then tally should have a total of 0 payout votes
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 0
    Then CGP payout should be none
    Then CGP amount should be 0 Zen
    Then coinbase reward in block bkfork should be 50 Zen

    # Chain A - Interval 0 - Block 2
    When validating block bka1 containing voteTx1alloc extending bkfork
    Then tally allocation result should be 20
    Then tally payout result should be none
    Then CGP allocation should be 0
    Then CGP payout should be none
    Then CGP amount should be 0 Zen
    Then coinbase reward in block bka1 should be 50 Zen

    # Chain A - Interval 1 - Block 0
    When validating an empty block bka2 extending bka1
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 10 Zen
    Then coinbase reward in block bka2 should be 40 Zen

    # Chain A - Interval 1 - Block 1
    When validating block bka3 containing voteTx1payout extending bka2
    Then tally allocation result should be none
    Then tally payout result should be of 3 Zen to id1
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 20 Zen
    Then coinbase reward in block bka3 should be 40 Zen

    # Chain A - Interval 1 - Block 2
    When validating an empty block bka4 extending bka3
    Then tally allocation result should be none
    Then tally payout result should be of 3 Zen to id1
    Then CGP allocation should be 20
    Then CGP payout should be none
    Then CGP amount should be 30 Zen
    Then coinbase reward in block bka4 should be 40 Zen

    # Chain A - Interval 2 - Block 0
    When validating an empty block bka5 extending bka4
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 37 Zen
    Then coinbase reward in block bka5 should be 40 Zen

    # Chain A - Interval 2 - Block 1
    When validating an empty block bka6 extending bka5
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 47 Zen
    Then there should be a payout on block bka6
    Then coinbase payout in block bka6 should be 3 Zen to id1
    Then coinbase reward in block bka6 should be 40 Zen

    ### Side chain
    
    # Chain B - Interval 0 - Block 2
    When validating block bkb1 containing voteTx2alloc extending bkfork
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb1 should be 50 Zen

    # Chain B - Interval 1 - Block 0
    When validating an empty block bkb2 extending bkb1
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 0+25=25 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb2 should be 25 Zen

    # Chain B - Interval 1 - Block 1
    When validating an empty block bkb3 extending bkb2
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 25+25=50 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb3 should be 25 Zen

    # Chain B - Interval 1 - Block 2
    When validating an empty block bkb4 extending bkb3
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 50+25=75 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb4 should be 25 Zen

    # Chain B - Interval 2 - Block 0
    When validating an empty block bkb5 extending bkb4
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 75+25=100 Zen
    Then CGP amount should be 47 Zen
    Then coinbase reward in block bkb5 should be 25 Zen

    # Chain B - Interval 2 - Block 1
    When validating an empty block bkb6 extending bkb5
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 20
    Then CGP payout should be of 3 Zen to id1
    # On sidechain: CGP amount should be 100+25=125 Zen
    Then CGP amount should be 47 Zen
    Then there shouldn't be a payout on block bkb6
    Then coinbase reward in block bkb6 should be 25 Zen

    # REORG
    # Chain B - Interval 2 - Block 2
    When validating an empty block bkb7 extending bkb6
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 50
    Then CGP payout should be none
    # 125+25=150 Zen
    Then CGP amount should be 150 Zen

    # Chain B - Interval 3 - Block 0
    When validating an empty block bkb8 extending bkb7
    Then tally should have a total of 0 allocation votes
    Then tally allocation result should be none
    Then tally payout result should be none
    Then CGP allocation should be 50
    Then CGP payout should be none
    # 150+25=175 Zen
    Then CGP amount should be 175 Zen

    
  Scenario: Validating a block with many transactions
    Given genesisTx locks 100 Zen to key1
    And genesis has genesisTx
    
    And tx1 locks 100 Zen to key3
    And tx1 has the input genesisTx index 0
    And tx1 is signed with key1
    And txArr is array of 3000 from tx1
    
    When validating a block containing tx1 extending tip
    When validating block bkMain containing txArr extending tip
    Then tip should be bkMain
