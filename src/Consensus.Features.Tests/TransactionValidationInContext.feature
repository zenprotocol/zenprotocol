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
    When tx1 is signed with key1,key2,key3,key3
    Then tx1 should pass validation

  Scenario: A contract is executed and the result is validated, result is Ok
    Given utxoset
      | Tx   | Key  | Asset | Amount |
      | tx-1 | key1 | Zen   | 100    |
    And tx1 locks 100 Zen to key3
    And tx1 has the input tx-1 index 0
    When c1 executes on tx1 returning tx2
    When tx2 is signed with key1
    Then tx2 should pass validation
    And tx2 should lock 1000 c1 to c1

  Scenario: Data is defined
    Given str1 is a string of "string1"
    And str2 is a string of string2
    And strLst2 is a string list of str1,str2,"string3"
    And num1 is an i64 of 100
    And numArr is an i64 array of 98,99,num1
    And dict1 is a dictionary of
      | Key     | Type        | Value               |
      | str1    | string      | a string            |
      | str2    | string      | "a string"          |
      | strList | string list | str1,str2,"string3" |
    And dict2 is a dictionary of
      | Key    | Type      | Value      |
      | intArr | i64 array | 98,99,num1 |
    And dict3 is a dictionary of
      | Key   | Type | Value |
      | dict1 | dict | dict1 |
    And dict4 is a dictionary of
      | Key    | Type       | Value       |
      | dict1  | dict       | dict1       |
      | dictArr| dict array | dict1,dict2 |

  Scenario: A contract is executed with additional arguments
            and the result is validated, result is Ok
    Given utxoset
      | Tx   | Key  | Asset | Amount |
      | tx-1 | key1 | Zen   | 100    |
    And tx1 locks 100 Zen to key3
    And tx1 has the input tx-1 index 0
    And data is a dictionary of
      | Key     | Type | Value |
      | amount  | u64  | 1200  |
    When c2 executes on tx1 returning tx2 with
      | Argument      | Value            |
      | Command       | mint             |
      | Sender        | anonymous        |
      | MessageBody   | data             |
      | ReturnAddress | returnAddressKey |
    When tx2 is signed with key1
    Then tx2 should pass validation
    And tx2 should lock 1200 c2 to returnAddressKey

  Scenario: A contract records it's state
    Given tx1 locks 100 Zen to key1
    And tx2 has the input tx1 index 0
    And tx2 locks 100 Zen to key2
    And genesis has tx1
    And data is a u32 of 1
    When c3 executes on tx2 returning tx3
    And tx3 is signed with key1
    And tx3 is put in a block
    Then c3 state is u32 of 1
