Feature: Contract states

  Scenario: A contract records it's state
    Given genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    And genesis has genesisTx
    
    And activationTx has the input genesisTx index 0
    And activationTx activates c3 for 1000 blocks
    And activationTx locks change Zen to activationTxChangeKey
    And activationTx is signed with genesisKey1

    And inputTx has the input genesisTx index 1
    And inputTx locks 100 Zen to outputKey
    When executing c3 on inputTx returning outputTx
    And signing outputTx with genesisKey2

    And validating a block containing outputTx,activationTx extending tip
    Then state of c3 should be u32 of 10
  
  Scenario: A contract records it's state; multiple executions
    Given genesisTx locks 100 Zen to genesisKey1
    And genesisTx locks 100 Zen to genesisKey2
    And genesis has genesisTx

    And activationTx has the input genesisTx index 0
    And activationTx activates c3 for 1000 blocks
    And activationTx locks change Zen to activationTxChangeKey
    And activationTx is signed with genesisKey1
    
    And tx1 has the input genesisTx index 1
    And tx1 locks 10 Zen to key2
    And tx1 locks 90 Zen to key3
    
    When validating a block containing activationTx,tx1 extending tip
    And executing c3 on tx1 returning tx2
    And signing tx2 with genesisKey2
    And validating a block containing tx2 extending tip
    Then state of c3 should be u32 of 10
    
    Given tx3 has the input tx2 index 1
    And tx3 locks 90 Zen to tx3_key1
    
    When executing c3 on tx3 returning tx4
    And signing tx4 with key3
    And validating a block containing tx4 extending tip
    Then state of c3 should be u32 of 11
  
  Scenario: A contract records it's state; multiple executions and a reorg
    Given tx1 locks 100 Zen to key1
    And tx1 locks 10 Zen to key100
    And tx1 locks 10 Zen to key101
    And tx1 locks 10 Zen to key102
    And tx1 locks 100 Zen to activationKey
    And tx2 has the input tx1 index 0
    And tx2 locks 10 Zen to key2
    And tx2 locks 90 Zen to key3
    And genesis has tx1

    And activationTx has the input tx1 index 4
    And activationTx activates c3 for 1000 blocks
    And activationTx locks change Zen to activationTxChangeKey
    And activationTx is signed with activationKey

    When executing c3 on tx2 returning tx3
    And signing tx3 with key1
    And validating a block containing tx3,activationTx extending tip
    Then c3 should be active for 1000 blocks
    And state of c3 should be u32 of 10
    
    Given tx4 has the input tx3 index 1
    And tx4 locks 90 Zen to key4
    
    When executing c3 on tx4 returning tx5
    And signing tx5 with key3
    And validating a block containing tx5 extending tip
    Then c3 should be active for 1000 blocks
    And state of c3 should be u32 of 11
    
    Given tx100 has the input tx1 index 1
    And tx100 locks 10 Zen to key key100_out
    When signing tx100 with key100
    And validating block bk1 containing tx100 extending genesis
    Then c3 should be active for 1000 blocks
    And state of c3 should be u32 of 11
    
    Given tx101 has the input tx1 index 2
    And tx101 locks 10 Zen to key key101_out
    When signing tx101 with key101
    And validating block bk2 containing tx101 extending bk1
    Then c3 should be active for 1000 blocks
    And state of c3 should be u32 of 11
    
    Given tx102 has the input tx1 index 3
    And tx102 locks 10 Zen to key key102_out
    When signing tx102 with key102
    And validating a block containing tx102 extending bk2
    Then c3 should be active for 1000 blocks
    And state of c3 should be u32 of 11
    