Feature: Transaction validation with utxo-set context

  Scenario: A transaction gets validated, result is Ok
    Given utxoset
      | Tx   | Key  | Asset | Amount |
      | tx-1 | key1 | Zen   |  10    |
      | tx-1 | key2 | Zen   |   5    |
      | tx-2 | key3 | Zen   |   6    |
      | tx-3 | key3 | Zen   |   5    |
    And tx1 locks 26 Zen to key3
    And tx1 has the input tx-1 index 0
    And tx1 has the input tx-1 index 1
    And tx1 has inputs
      | Tx   | Index |
      | tx-2 | 0     |
      | tx-3 | 0     |
    When signing tx1 with key1,key2,key3,key3
    Then tx1 should pass validation
