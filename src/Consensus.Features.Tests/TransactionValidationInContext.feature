Feature: Transaction validation with utxo-set context

  Scenario: A transaction gets validated, result is Ok
    Given utxoset
      | Tx   | Key  | Asset | Amount |
      | tx-1 | key1 | Zen   | 100    |
      | tx-2 | key2 | Zen   |  20    |
    And tx1 locks 120 Zen to key3
    And tx1 has the input tx-1 index 0
    And tx1 has the input tx-2 index 0
    When tx1 is signed with key1,key2
    Then tx1 should pass validation

 