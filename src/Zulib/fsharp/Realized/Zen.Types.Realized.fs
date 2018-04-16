#light "off"
module Zen.Types.Realized

open Zen.Types.Extracted

type txSkeleton =
    { inputs : uint64 * // The number of elements in the map
               // the uint64 field in the map codomain represents cumulative asset amount totals
               // the uint64 field in the list in the map codomain represents the insertion index of each pointedOutput
               Collections.Map<asset, uint64 * list<uint64 * input>>;
      outputs : uint64 * // The number of elements in the map
                // the uint64 field in the map codomain represents cumulative asset amount totals
                // the uint64 field in the list in the map codomain represents the insertion index of each output
                Collections.Map<asset, uint64 * list<uint64 * output>> }

type wallet = pointedOutput Prims.list