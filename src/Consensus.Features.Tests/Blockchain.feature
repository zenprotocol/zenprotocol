Feature: Blockchain tests

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