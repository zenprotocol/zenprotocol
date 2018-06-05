Feature: Active Contract Set

  Scenario: A contract is activated
    Given genesisTx locks 10 Zen to key1
    And genesis has genesisTx
    And activationTx has the input genesisTx index 0
    And activationTx activates c1 for 1000 blocks
    And activationTx locks change Zen to activationTxChangeKey
    When signing activationTx with key1
    And validating a block containing activationTx extending tip
    Then c1 should be active for 1000 blocks

  Scenario: A contract is activated and executed in main chain
    Given genesisTx locks 10 Zen to key1
    And genesisTx locks 10 Zen to key2
    And genesis has genesisTx
    And activationTx has the input genesisTx index 0
    And activationTx activates c1 for 1000 blocks
    And activationTx locks change Zen to activationTxChangeKey
    When signing activationTx with key1
    And validating a block containing activationTx extending tip
    Then c1 should be active for 1000 blocks

    Given txMain locks 10 Zen to mainKey3
    And txMain has the input genesisTx index 1
    When executing c1 on txMain returning txMainOfContract
    And signing txMainOfContract with key2
    And validating block bkMain containing txMainOfContract extending tip
    Then tip should be bkMain

  Scenario: A contract becomes deactivated on a reorg
    Given genesisTx locks 10 Zen to key1
    And genesis has genesisTx
    
    And activationTx has the input genesisTx index 0
    And activationTx activates c1 for 1000 blocks
    And activationTx locks change Zen to activationTxChangeKey
    When signing activationTx with key1
    And validating block bkMain1 containing activationTx extending tip
    Then c1 should be active for 1000 blocks

    When validating an empty block bkSide1 extending genesis
    Then tip should be bkMain1
    And c1 should be active for 1000 blocks
    
    When validating an empty block bkSide2 extending bkSide1
    Then tip should be bkSide2
    And c1 should not be active

  Scenario: A contract becomes reactivated on a second reorg
    Given genesisTx locks 10 Zen to key1
    And genesis has genesisTx

    And activationTx has the input genesisTx index 0
    And activationTx activates c1 for 1000 blocks
    And activationTx locks change Zen to activationTxChangeKey
    When signing activationTx with key1
    And validating block bkMain1 containing activationTx extending tip
    Then c1 should be active for 1000 blocks

    When validating an empty block bkSide1 extending genesis
    Then tip should be bkMain1
    And c1 should be active for 1000 blocks

    When validating an empty block bkSide2 extending bkSide1
    Then tip should be bkSide2
    And c1 should not be active

    When validating an empty block bkMain2 extending bkMain1
    Then tip should be bkSide2
    And c1 should not be active

    When validating an empty block bkMain3 extending bkMain2
    Then tip should be bkMain3
    And c1 should be active for 998 blocks
