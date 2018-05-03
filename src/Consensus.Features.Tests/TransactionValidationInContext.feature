Feature: Transaction validation

  Scenario: A transaction gets validated with utxo-set context
    Given utxoset
      | Tx        | Key  | Asset | Amount |
      | utxo-tx-1 | key1 | Zen   | 100    |
      | utxo-tx-1 | key2 | Zen   |  20    |
    When tx1 is added an output of 120 Zen locked to pk key3
    And tx1 is added an input pointing to utxo-tx-1 with index 0
    And tx1 is added an input pointing to utxo-tx-1 with index 1
    And tx1 is signed with key1,key2
    Then tx1 should pass validation