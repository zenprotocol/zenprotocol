Feature: Transaction validation with utxo-set context

  Scenario: PK witness validation tests
    Given utxoset
      | Tx   | Key  | Asset | Amount |
      | tx-1 | key1 | Zen   |  10    |
      | tx-1 | key2 | Zen   |   5    |
      | tx-2 | key3 | Zen   |   6    |
      | tx-3 | key3 | Zen   |   5    |
    And tx1 locks 26 Zen to key3
    And tx1 has inputs
      | Tx   | Index |
      | tx-1 | 0     |
      | tx-1 | 1     |
      | tx-2 | 0     |
      | tx-3 | 0     |
    When signing tx1 with key1,key2,key3,key3
    Then tx1 should pass validation

    When tx3 results by pushing input 0 of tx1 at index 0
    Then tx3 validation should yield inputs duplicated

    # additional invalid amount output
    Given tx10 has inputs
      | Tx   | Index |
      | tx-1 | 0     |
    And tx10 locks 10 Zen to key10
    And tx10 locks 0 Zen to key11
    When signing tx10 with key1
    Then tx10 validation should yield structurally invalid output(s)
    
    # malleating the asset
    When tx2 results by changing asset of tx1 output 0 to c1
    Then tx2 validation should yield invalid amounts

    # malleating the amount
    When tx2 results by changing amount of tx1 output 0 to 0
    Then tx2 validation should yield structurally invalid output(s)
    
    # missing key
    When tx2 results by re-signing tx1 with key1,key2,key3
    Then tx2 validation should yield expecting a public key witness

    # invalid key
    When tx2 results by re-signing tx1 with key1,key2,key2,key3
    Then tx2 validation should yield PK witness mismatch

    # duplidate key
    When tx2 results by re-signing tx1 with key1,key2,key2,key3,key3
    Then tx2 validation should yield expecting a contract 0 witness

    # invalid order
    When tx2 results by re-signing tx1 with key2,key1,key3,key3
    Then tx2 validation should yield PK witness mismatch

    # malleating the witness list
    When tx2 results by pushing witness 0 of tx1 at index 0
    Then tx2 validation should yield expecting a contract 0 witness

    When tx2 results by pushing witness 1 of tx1 at index 0
    Then tx2 validation should yield expecting a contract 0 witness

    # malleating the input list
    When tx2 results by pushing input 0 of tx1 at index 0
    Then tx2 validation should yield inputs duplicated

    # malleating the input list
    When tx2 results by pushing input 1 of tx1 at index 0
    Then tx2 validation should yield inputs duplicated

    # malleating the output list
    When tx2 results by pushing input 0 of tx1 at index 0
    Then tx2 validation should yield inputs duplicated
    
  Scenario: Contract witness validation tests
    Given utxoset
      | Tx   | Key  | Asset | Amount |
      | tx-1 | key1 | Zen   | 100    |
    And tx1 locks 100 Zen to key3
    And tx1 has the input tx-1 index 0
    And data is a dictionary of
      | Key     | Type | Value |
      | amount  | u64  | 1200  |
    When executing c2 on tx1 returning tx2 with
      | Argument      | Value            |
      | Command       | mint             |
      | Sender        | anonymous        |
      | MessageBody   | data             |
      | ReturnAddress | returnAddressKey |
    When signing tx2 with key1
    Then tx2 should pass validation
    And tx2 should lock 1200 c2 to returnAddressKey

    # malleating the contractId
    When tx3 results by changing contractId of tx2 contract witness 1 to mock
    Then tx3 validation should yield ContractNotActive

    # malleating the command
    When tx3 results by changing command of tx2 contract witness 1 to test
    Then tx3 validation should yield unsupported command

    # malleating the begin-inputs
    When tx3 results by changing beginInputs of tx2 contract witness 1 to 0
    Then tx3 validation should yield invalid prefix

    When tx3 results by changing beginInputs of tx2 contract witness 1 to 2
    Then tx3 validation should yield structurally invalid witness(es)

    # malleating the begin-outputs
    When tx3 results by changing beginOutputs of tx2 contract witness 1 to 0
    Then tx3 validation should yield invalid prefix

    # malleating the begin-outputs
    When tx3 results by changing beginOutputs of tx2 contract witness 1 to 2
    Then tx3 validation should yield invalid prefix

    # malleating the inputs-length
    When tx3 results by changing inputsLength of tx2 contract witness 1 to 0
    Then tx3 validation should yield input/output length mismatch

    When tx3 results by changing inputsLength of tx2 contract witness 1 to 2
    Then tx3 validation should yield structurally invalid witness(es)

    # malleating the outputs-length
    When tx3 results by changing outputsLength of tx2 contract witness 1 to 0
    Then tx3 validation should yield input/output length mismatch

    When tx3 results by changing outputsLength of tx2 contract witness 1 to 2
    Then tx3 validation should yield input/output length mismatch

    # malleating the cost
    When tx3 results by changing cost of tx2 contract witness 1 to 1000
    Then tx3 validation should yield execution cost commitment mismatch

    # malleating the witness list
    When tx3 results by pushing witness 0 of tx2 at index 0
    Then tx3 validation should yield expecting a contract 0 witness

    When tx3 results by pushing witness 1 of tx2 at index 0
    Then tx3 validation should yield expecting a public key witness

    # malleating the input list
    When tx3 results by pushing input 0 of tx2 at index 0
    Then tx3 validation should yield inputs duplicated
