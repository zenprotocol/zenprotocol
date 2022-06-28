# Changelog

All notable changes to this project will be documented in this file. See [standard-version](https://github.com/conventional-changelog/standard-version) for commit guidelines.

### [1.0.9](https://gitlab.com/zenprotocol/zenprotocol/compare/v1.0.8...v1.0.9) (2022-06-28)


### Features

* update version expiry ([459ddb7](https://gitlab.com/zenprotocol/zenprotocol/commit/459ddb72898eefc7965d1dbcea2d59ed1a824ba1))

### [1.0.8](https://gitlab.com/zenprotocol/zenprotocol/compare/v1.0.7...v1.0.8) (2022-01-11)


### Features

* mono must be at least 6.12.0 ([2f8b08e](https://gitlab.com/zenprotocol/zenprotocol/commit/2f8b08e20400086d1f8847c09c3f85a6155c4225))
* update version expiry ([b56a04b](https://gitlab.com/zenprotocol/zenprotocol/commit/b56a04bfe367e8f93566fe1035a0804a4b635a52))
* use async in HTTP module ([2639633](https://gitlab.com/zenprotocol/zenprotocol/commit/263963360d096d3026294a32304376ba3468a718))

### Bug Fixes

* ContentType must be provided in POST ([992c167](https://gitlab.com/zenprotocol/zenprotocol/commit/992c167d9c88718a7c6252fdd482b894a8dadfbb))
* update mono image in gitlab ci ([d30e991](https://gitlab.com/zenprotocol/zenprotocol/commit/d30e9915a88332736ff51d84ce4b0b2f7c03e8fe))

## [1.0.7](https://gitlab.com/zenprotocol/zenprotocol/compare/v1.0.6...v1.0.7) (2021-06-28)


### Features

* update version expiry ([c1c080a](https://gitlab.com/zenprotocol/zenprotocol/commit/c1c080a6eb9520c5b17858a0933d7242cfb1f7af))

### Bug Fixes

* wrong subtraction when filtering ([9485a22](https://gitlab.com/zenprotocol/zenprotocol/commit/9485a225564be5b4c19f49f4071c4b08adc463e3))


## [1.0.6](https://gitlab.com/zenprotocol/zenprotocol/compare/v1.0.5...v1.0.6) (2021-03-17)


### Features

* add timestamp to address history ([cf3e023](https://gitlab.com/zenprotocol/zenprotocol/commit/cf3e023a60b68402b84b72cfdc6b983f2ba2e7d4))

## [1.0.5](https://gitlab.com/zenprotocol/zenprotocol/compare/v1.0.3...v1.0.5) (2021-02-11)


### Features

* add api testing module ([52384cd](https://gitlab.com/zenprotocol/zenprotocol/commit/52384cd9a0b7ad0c3d2355c90b939dd7d672ff1b))
* add blockchain/winner api ([3d4c428](https://gitlab.com/zenprotocol/zenprotocol/commit/3d4c428545f4b16cfc0790a31dc7b643dcf2f60b))
* add context blockNumber to contract mints and add mempool ([ad6a389](https://gitlab.com/zenprotocol/zenprotocol/commit/ad6a3892072e9a099f7186e369e1f52180cf0f34))
* add contract info api ([f2cc85d](https://gitlab.com/zenprotocol/zenprotocol/commit/f2cc85d4ffc600760fdd878a8d699822d6a9a819))
* add cors ([47f3949](https://gitlab.com/zenprotocol/zenprotocol/commit/47f39499776abceac9b7613edb7c6e069818ddd4))
* add origin flag for cors support ([c9638d5](https://gitlab.com/zenprotocol/zenprotocol/commit/c9638d5fcf93050107deeea845178677351d110e))
* add sender to contract assets ([9eae3c9](https://gitlab.com/zenprotocol/zenprotocol/commit/9eae3c9bb8157d8b2568566a3dac26b5eda86213))
* added block number in balance and output addressdb ([80f5a19](https://gitlab.com/zenprotocol/zenprotocol/commit/80f5a196355f1b50ea880c237d41531b56c7d35c))
* bind runtime FSharp Core 4.4.3 ([54c501a](https://gitlab.com/zenprotocol/zenprotocol/commit/54c501aad086e7ac438bae6ad80b98ae33a56d76))
* bump FSBech32 to 0.1.15 ([b9a6f0d](https://gitlab.com/zenprotocol/zenprotocol/commit/b9a6f0d93004e0570d44ea5f9aac38ee16c04de2))
* connect to a desktop wallet with -—connectwallet ([2311f85](https://gitlab.com/zenprotocol/zenprotocol/commit/2311f858b7e80d89a9fd3613200e6c18549519b0))
* keeps the wallet and addressdb always active ([2fbb9cd](https://gitlab.com/zenprotocol/zenprotocol/commit/2fbb9cdc6f4eeaae294530d82bb5e6afaff18175))
* return txHash and blocks in contracts activation ([3510483](https://gitlab.com/zenprotocol/zenprotocol/commit/3510483926fd6f4a314f65146f4e1f0aedd25136))
* update version expiry ([8c37db9](https://gitlab.com/zenprotocol/zenprotocol/commit/8c37db93c8b19298e4f44331f8f1c63ce860df8f))
* update zulib 0.3.42 ([c76b469](https://gitlab.com/zenprotocol/zenprotocol/commit/c76b4698fbd5f53ecdc59ca00f7353ca4363b906))
* use string instead of int for amounts ([9ea1be9](https://gitlab.com/zenprotocol/zenprotocol/commit/9ea1be9679e030654cb114c34dd3a659da92b698))
* fromString in Hash returns Option ([d934135](https://gitlab.com/zenprotocol/zenprotocol/commit/d934135e433fedc83b4622b86a56b4e4ce618a06))

### Bug Fixes

* api get cgp history ([01e7543](https://gitlab.com/zenprotocol/zenprotocol/commit/01e7543803a91bc34a0d9e6236b67ef6ed52c4f1))


## [1.0.4](https://gitlab.com/zenprotocol/zenprotocol/compare/v1.0.3...v1.0.4) (2020-10-11)


### Features

* add data path in logger ([306bfa5](https://gitlab.com/zenprotocol/zenprotocol/commit/306bfa54878c7b609232e5510360c0f59b482ee2))
* add expire block in contract encoder ([69b048a](https://gitlab.com/zenprotocol/zenprotocol/commit/69b048aedafa27db2be6c0a0303471e7eab28ff4))
* add get blocks endpoint ([5758c85](https://gitlab.com/zenprotocol/zenprotocol/commit/5758c8569863b219e7fcf7a676bbabafffaeb523))
* add rlimit in activation api ([522d5fe](https://gitlab.com/zenprotocol/zenprotocol/commit/522d5fee3fd36a4767d645dde612db08545e8301))
* add wallet utxo api ([734cdee](https://gitlab.com/zenprotocol/zenprotocol/commit/734cdeef6c274ddd8ba204aa34be98bc59301e3c))
* add witness information ([8193d60](https://gitlab.com/zenprotocol/zenprotocol/commit/8193d60563c5b82a33b1cad4d8ed4835c2ec03ea))
* bump db version in addressdb ([d8059e5](https://gitlab.com/zenprotocol/zenprotocol/commit/d8059e506e05c69ea698ab641da44ad425c129d4))
* bump db version in walletdb ([d7d2bc7](https://gitlab.com/zenprotocol/zenprotocol/commit/d7d2bc792a3d7c48bbec64e35af5eebcfef424ac))
* fast sync actor + refactor ([b9a7bd7](https://gitlab.com/zenprotocol/zenprotocol/commit/b9a7bd71d5f8d7c6517c34e6a919a8e4c8dfe6b0))
* get contract assets information ([a8c3847](https://gitlab.com/zenprotocol/zenprotocol/commit/a8c3847f8b72ffbd6a18e8915881a0295d7e5d3c))
* getHeaders api improved ([9041543](https://gitlab.com/zenprotocol/zenprotocol/commit/90415435d74e2c3a44f5b12cac3ba97190a293ca))
* improve Dockerfile ([8f71e3c](https://gitlab.com/zenprotocol/zenprotocol/commit/8f71e3c0964fd42cb89c8a16d4d29157cb2ca163))
* wallet can sync faster ([fc19ed9](https://gitlab.com/zenprotocol/zenprotocol/commit/fc19ed905e5e22d0f9a30652bdce65efe529200d))


### Bug Fixes

* improve candidates api ([2dd40a6](https://gitlab.com/zenprotocol/zenprotocol/commit/2dd40a65ff7d46a2ef65797d1a8273a7456e7133))
* made the syncronization faster ([845466b](https://gitlab.com/zenprotocol/zenprotocol/commit/845466b5b25a6b9fd1f8deddac67721c321bc6c3))
* MMR implementation ([fc103f3](https://gitlab.com/zenprotocol/zenprotocol/commit/fc103f33dd9af58098765ae057029aef4580eefe))
* removed unnecessary chainparams ([48523bb](https://gitlab.com/zenprotocol/zenprotocol/commit/48523bbe3cb7d753f26a19150e6c6f0787a22a53))
* use correct chain in local net ([36f0cc0](https://gitlab.com/zenprotocol/zenprotocol/commit/36f0cc0695c1b661b32964c5f38510d1a3acf889))
* utxo retrival during a reorg ([3782c6c](https://gitlab.com/zenprotocol/zenprotocol/commit/3782c6c2bff8c588f76b9a63abeb670b410c9c27))
