Feature: Simple contract execution and valudation with utxo-set context

  Scenario: A contract is executed and the result is validated, result is Ok
    Given utxoset
    | Tx   | Key  | Asset | Amount |
    | tx-1 | key1 | Zen   | 100    |
    And tx1 locks 100 Zen to key3
    And tx1 has the input tx-1 index 0
    When executing c1 on tx1 returning tx2
    And signing tx2 with key1
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
    When executing c2 on tx1 returning tx2 with
    | Argument      | Value            |
    | Command       | mint             |
    | Sender        | anonymous        |
    | MessageBody   | data             |
    | ReturnAddress | returnAddressKey |
    When signing tx2 with key1
    Then tx2 should pass validation
    And tx2 should lock 1200 c2 to returnAddressKey
