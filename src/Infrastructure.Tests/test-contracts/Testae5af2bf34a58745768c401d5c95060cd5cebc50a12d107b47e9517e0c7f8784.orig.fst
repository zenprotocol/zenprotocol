module Testae5af2bf34a58745768c401d5c95060cd5cebc50a12d107b47e9517e0c7f8784

    open Zen.Types
    open Zen.Base
    open Zen.Cost
    open Zen.ResultT

    module C = Zen.Cost
    
    let main tx _ _ _ _ _ _ _ =
        ok @ { tx = tx; message = None; state = NoChange }

    let cf _ _ _ _ _ _ _ = 
        0 + 5
        |> cast nat
        |> C.ret
    