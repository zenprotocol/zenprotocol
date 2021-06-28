module Api.Tests.Constants

open Consensus.Tests.Helper
open Consensus.Tests.SampleContract

let password = "1234"

let import =
    sprintf "{\"words\": [%A], \"password\": %A}" rootMnemonicPhrase password
let importWithoutPassField =
    sprintf "{\"words\": [%A], \"password\": \"test\"}" rootMnemonicPhrase

let changePassword =
    sprintf "{\"old\": \"test\", \"new\": %A}" password
    
let justPassword =
    sprintf "{\"password\": %A}" password

let publicKeyRecord =
    sprintf "{\"path\": \"m/44'/258'/0'/0/0\",\"password\": %A}" password

let zenPublicKey =
    sprintf "{\"publicKey\": \"tpubDCeTZv9MDcNe6Ahv8UQaoAWhK9XKmpzzJjpiVbCZ4jhsJYdN67Qh18nDjuJFtWfGfLL2hRkGid6Ga5h2FoW9QoRjdcEUQRBW4tkpkCbMtKb\"}"

let signMessage =
    sprintf "{\"path\": \"m/44'/258'/0'/0/0\",\"message\": \"3338be694f50c5f338814986cdf0686453a888b84f424d792af4b9202398f392\",\"password\": %A}" password

let publishBlock =
    "{\"block\": \"000000000000000000000000000000000000000000000000000000000000000000000000000000013f563d44d85c35cf82d099b9e0fc1eb5ceda824471b7e16852f1602513a2281900000160e073fe8f20ffffffaf1f420357100067380df4da400497fa03835300081736d721821c6316bd8f2324ce973e02ab3e579e1230c30bd5f02f17ffe9349a96ca3a0adf637c33a88933eb937f8369a8339a31dcb789dae18c5dc3be653064be80f760b9d471dc9afbac2b24236c9f2eb0f08b7427942852dc780201000000000001022030759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4002c170000\"}"
    
let publishSecondBlock =
    "{\"block\": \"0000000146d1e8e4d397bbcf13f96346dfb998d7445600806d09e6e5d1740b3d8412fd2400000002ca42763cfe7d87f7764817a6f97e4bab53c76d358ba405752c16f2f586d34ca800000176a10e3b7c20ffffffd4aa88cc24e72dca00000000000000010376567fbaffa524424dc79b6f46bdfe85340ae8dd0581c8298260349c7130f7c2413fd845e3e170c48790e0b35465d009f228b658f292374d6c9afbc6eb09b5e0be653064be80f760b9d471dc9afbac2b24236c9f2eb0f08b7427942852dc78020100000000000106240000000230759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b40024050000\"}"
    
let sendTo =
    sprintf "{\"outputs\": [{\"address\": \"tzn1q9v8sc0js2s77546lxvxcdg7aqx3ee97r2gjrx5snnduelafncw7sz5hhpf\",\"asset\": \"00\", \"amount\": \"1\"}], \"password\": %A}" password

let activateContract =
    sprintf "{\"code\": %A, \"rlimit\": 2723280, \"numberOfBlocks\": 10, \"password\": %A}" sampleContractCode password

let contractInfo =
    sprintf "{\"code\": %A, \"rlimit\": 2723280}" sampleContractCode

let walletKeys =
    "[{\"publicKey\": \"02b43a1cb4cb6472e1fcd71b237eb9c1378335cd200dd07536594348d9e450967e\",\"path\": \"m/44'/258'/0'/0/0\"},{\"publicKey\": \"029ae9b49e60259958302fab6c9be333775fd7ada72f11643218dcf23e5f37ec92\",\"path\": \"m/44'/258'/0'/1/0\"}]"

let address =
    "tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr"

let transactionSkipTakeOneAddress =
    "{\"addresses\": [\"tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr\"], \"skip\": 0, \"take\": 10}"
let transactionOneAddress =
    "{\"addresses\": [\"tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr\"]}"

let transactionTwoAddress =
    "{\"addresses\": [\"tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr\",\"tzn1qnw2cxku67eaacdzupt58nwf6trg087g8snyc9gk62taaw8v8qz3sy7v0d9\"]}"

let transactionSkipTakeTwoAddress =
    "{\"addresses\": [\"tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr\",\"tzn1qnw2cxku67eaacdzupt58nwf6trg087g8snyc9gk62taaw8v8qz3sy7v0d9\"], \"skip\": 0, \"take\": 10}"

let addressDBOutputs =
    "{\"addresses\": [\"tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr\",\"tzn1qnw2cxku67eaacdzupt58nwf6trg087g8snyc9gk62taaw8v8qz3sy7v0d9\"], \"mode\": \"unspentOnly\"}"

let executeContract =
    """
{
    "address": "ctzn1qqqqqqqrqm6z6y9y9p0mpjtnugst0hn908n7vp2f8m44hwznsluydyszx6vjk2kpw",
    "command": "this command",
    "options": {
        "returnAddress": false
    },
    "password": "1234"
}
"""
let oneTransactionResponse =
    """
[
  {
    "txHash": "835300081736d721821c6316bd8f2324ce973e02ab3e579e1230c30bd5f02f17",
    "asset": "00",
    "amount": "2300000000000",
    "confirmations": 1,
    "lock": {
      "PK": {
        "hash": "30759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4",
        "address": "tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr"
      }
    }
  }
]
"""
let twoTransactionResponse =
    """
[
  {
    "txHash": "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784",
    "asset": "00",
    "amount": "-1",
    "confirmations": 0,
    "lock": {
      "PK": {
        "hash": "9b95835b9af67bdc345c0ae879b93a58d0f3f90784c982a2da52fbd71d8700a3",
        "address": "tzn1qnw2cxku67eaacdzupt58nwf6trg087g8snyc9gk62taaw8v8qz3sy7v0d9"
      }
    }
  },
  {
    "txHash": "835300081736d721821c6316bd8f2324ce973e02ab3e579e1230c30bd5f02f17",
    "asset": "00",
    "amount": "2300000000000",
    "confirmations": 1,
    "lock": {
      "PK": {
        "hash": "30759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4",
        "address": "tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr"
      }
    }
  }
]
"""
let oneTransactionResponseWithTimestamp =
    """
[
  {
    "txHash": "835300081736d721821c6316bd8f2324ce973e02ab3e579e1230c30bd5f02f17",
    "asset": "00",
    "amount": "2300000000000",
    "confirmations": 1,
    "timestamp": 1515594186383,
    "lock": {
      "PK": {
        "hash": "30759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4",
        "address": "tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr"
      }
    }
  }
]
"""

let oneOutputResponse =
    """
[
  {
    "outpoint": {
      "txHash": "835300081736d721821c6316bd8f2324ce973e02ab3e579e1230c30bd5f02f17",
      "index": 0
    },
    "lock": {
      "PK": {
        "hash": "30759b07ca01caf8e524fc279946a1e96afc3546ee5f1fd4a1cfaf644763c2b4",
        "address": "tzn1qxp6ekp72q8903efylsnej34pa940cd2xae03l49pe7hkg3mrc26qyh2rgr"
      }
    },
    "spend": {
      "asset": "00",
      "amount": "2300000000000"
    }
  }
]
"""
let oneOutpointAfterSend =
   """
[
  {
    "outpoint": {
      "txHash": "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784",
      "index": 1
    },
    "lock": {
      "PK": {
        "hash": "9b95835b9af67bdc345c0ae879b93a58d0f3f90784c982a2da52fbd71d8700a3",
        "address": "tzn1qnw2cxku67eaacdzupt58nwf6trg087g8snyc9gk62taaw8v8qz3sy7v0d9"
      }
    },
    "spend": {
      "asset": "00",
      "amount": "2299999999999"
    }
  }
]
"""
let twoOutputResponse =
    """
[
  {
    "outpoint": {
      "txHash": "03ae57cd8ee6339ad138bf2c1c7dae7b3571d839bc987a818d8c067a7801a784",
      "index": 1
    },
    "lock": {
      "PK": {
        "hash": "9b95835b9af67bdc345c0ae879b93a58d0f3f90784c982a2da52fbd71d8700a3",
        "address": "tzn1qnw2cxku67eaacdzupt58nwf6trg087g8snyc9gk62taaw8v8qz3sy7v0d9"
      }
    },
    "spend": {
      "asset": "00",
      "amount": "2299999999999"
    }
  }
]
"""

let contractMint =
    """
{
  "executionBlock": 2,
  "sender": null,
  "command": "this command",
  "messageBody": null,
  "messageBodyRaw": null
}
"""

let contractInfoResponse =
#if DEBUG
    """
{
    "contractId": "0000000060de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3",
    "address": "ctzn1qqqqqqqrqm6z6y9y9p0mpjtnugst0hn908n7vp2f8m44hwznsluydyszx6vjk2kpw",
    "hints": "[\"dec09bb14444fd7a849d2a2b0c04bd60\",[[\"Z60de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3.mainFunction\",1,2,1,[\"@MaxIFuel_assumption\",\"@query\",\"Prims_pretyping_ae567c2fb75be05905677af440075565\",\"Zen.Cost.Realized_interpretation_Tm_arrow_8f884e5a479333b9416793675b7e962b\",\"Zen.Types.Main_pretyping_5bd8c5a85db081605d2f2b0ef5761cbb\",\"Zen.Types.Main_pretyping_8d0bd552ce32ff91a3c9f4725122e3be\",\"data_typing_intro_Zen.Types.Main.Anonymous@tok\",\"equation_Prims.nat\",\"equation_Z60de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3.cf\",\"equation_Zen.Base.cast\",\"equation_Zen.Base.op_Bar_Greater\",\"equation_Zen.Types.Main.maxCost\",\"fuel_guarded_inversion_Zen.Types.Main.context\",\"function_token_typing_Prims.nat\",\"function_token_typing_Zen.Cost.Realized.ret\",\"int_typing\",\"lemma_Zen.Cost.Realized.force_inc\",\"lemma_Zen.Cost.Realized.force_ret\",\"primitive_Prims.op_Addition\",\"proj_equation_Zen.Types.Main.CostFunc_f\",\"proj_equation_Zen.Types.Main.CostFunc_n\",\"projection_inverse_BoxInt_proj_0\",\"projection_inverse_Zen.Types.Main.CostFunc_f\",\"projection_inverse_Zen.Types.Main.CostFunc_n\",\"refinement_interpretation_Prims_Tm_refine_ba523126f67e00e7cd55f0b92f16681d\",\"refinement_interpretation_Zen.Types.Main_Tm_refine_8ae4abcfc6bc8d4903b7e1f40e070ec2\",\"string_inversion\",\"token_correspondence_Z60de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3.cf\",\"token_correspondence_Zen.Base.cast\",\"token_correspondence_Zen.Cost.Realized.ret\",\"token_correspondence_Zen.Types.Main.__proj__CostFunc__item__f\"],0,\"3fdd7bdf779881c809f45995a79e96d4\"]]]\n",
    "queries": 31,
    "rlimit": 2723280
}
"""
#else
    """
{
    "contractId": "0000000060de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3",
    "address": "ctzn1qqqqqqqrqm6z6y9y9p0mpjtnugst0hn908n7vp2f8m44hwznsluydyszx6vjk2kpw",
    "hints": "[\"dec09bb14444fd7a849d2a2b0c04bd60\",[[\"Z60de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3.mainFunction\",1,2,1,[\"@MaxIFuel_assumption\",\"@query\",\"Prims_pretyping_ae567c2fb75be05905677af440075565\",\"Zen.Cost.Realized_interpretation_Tm_arrow_8f884e5a479333b9416793675b7e962b\",\"Zen.Types.Main_pretyping_5bd8c5a85db081605d2f2b0ef5761cbb\",\"Zen.Types.Main_pretyping_8d0bd552ce32ff91a3c9f4725122e3be\",\"data_typing_intro_Zen.Types.Main.Anonymous@tok\",\"equation_Prims.nat\",\"equation_Z60de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3.cf\",\"equation_Zen.Base.cast\",\"equation_Zen.Base.op_Bar_Greater\",\"equation_Zen.Types.Main.maxCost\",\"fuel_guarded_inversion_Zen.Types.Main.context\",\"function_token_typing_Prims.nat\",\"function_token_typing_Zen.Cost.Realized.ret\",\"int_typing\",\"lemma_Zen.Cost.Realized.force_inc\",\"lemma_Zen.Cost.Realized.force_ret\",\"primitive_Prims.op_Addition\",\"proj_equation_Zen.Types.Main.CostFunc_f\",\"proj_equation_Zen.Types.Main.CostFunc_n\",\"projection_inverse_BoxInt_proj_0\",\"projection_inverse_Zen.Types.Main.CostFunc_f\",\"projection_inverse_Zen.Types.Main.CostFunc_n\",\"refinement_interpretation_Prims_Tm_refine_ba523126f67e00e7cd55f0b92f16681d\",\"refinement_interpretation_Zen.Types.Main_Tm_refine_8ae4abcfc6bc8d4903b7e1f40e070ec2\",\"string_inversion\",\"token_correspondence_Z60de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3.cf\",\"token_correspondence_Zen.Base.cast\",\"token_correspondence_Zen.Cost.Realized.ret\",\"token_correspondence_Zen.Types.Main.__proj__CostFunc__item__f\"],0,\"60bd37f1ba6f035bc725b38ead0d4c5a\"]]]\n",
    "queries": 31,
    "rlimit": 2723280
}
"""
#endif
let contractActivateResponse =
#if DEBUG
    "{\"address\": \"ctzn1qqqqqqqrqm6z6y9y9p0mpjtnugst0hn908n7vp2f8m44hwznsluydyszx6vjk2kpw\",\"contractId\": \"0000000060de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3\",\"txHash\": \"a4279de5058efaa4c38054acfe9a84ec87eea6c0690e195db31769b647e2cbde\",\"numberOfBlocks\": \"10\"}"
#else
    "{\"address\": \"ctzn1qqqqqqqrqm6z6y9y9p0mpjtnugst0hn908n7vp2f8m44hwznsluydyszx6vjk2kpw\",\"contractId\": \"0000000060de85a214850bf6192e7c4416fbccaf3cfcc0a927dd6b770a70ff08d24046d3\",\"txHash\": \"0814631952dd143e9771958ed69bbff300ca6b866077d819a621fdd342ff8a2c\",\"numberOfBlocks\": \"10\"}"
#endif

let contractExecuteResponse =
#if DEBUG
    "5886229dd4d3002d34918dd2649c79b0f28c235dc4b8637984151ba75488ed66"
#else
    "42cf3f7cac8baa63f620d770a82e03cef9fd877da450e8b6b47cf267d3ad732a"
#endif

let contractExecute =
#if DEBUG
    "[{\"command\": \"this command\",\"messageBody\": null,\"txHash\": \"5886229dd4d3002d34918dd2649c79b0f28c235dc4b8637984151ba75488ed66\",\"confirmations\": 0}]"
#else
    "[{\"command\": \"this command\",\"messageBody\": null,\"txHash\": \"42cf3f7cac8baa63f620d770a82e03cef9fd877da450e8b6b47cf267d3ad732a\",\"confirmations\": 0}]"
#endif
