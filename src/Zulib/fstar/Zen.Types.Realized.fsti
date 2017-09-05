
module Zen.Types.Realized

assume type lockCore
assume LC_hasEq: hasEq lockCore
assume type contract
assume Contract_hasEq: hasEq contract
assume type extendedContract
assume ExtendedContract_hasEq: hasEq extendedContract
assume type extraData:eqtype
