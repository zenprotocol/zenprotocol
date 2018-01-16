module Blockchain.Tests.OkConstraint

open System
open NUnit.Framework.Constraints
        
type OkConstraint<'ok,'error>() =
  inherit Constraint() with
    override __.Description with get () = "is result ok" 

   

    override this.ApplyTo(actual: 'TActual) : ConstraintResult =        
        let actual = box actual
        //ConstraintResult(this,actual.GetType(),true)
        
        match actual with
        | :? Result<'ok,'error> -> ConstraintResult(this,actual.GetType(),true)
        | _ -> ConstraintResult(this,actual.GetType(),false)
                                    
//        let actual:Result<'ok,'error> = downcast actual
//        
//        match actual with 
//        | Ok _ -> ConstraintResult(this,actual.GetType(),true)
//        | Error _ -> ConstraintResult(this,actual.GetType(),false)
        
let ok<'ok,'error> = 
    new OkConstraint<'ok,'error>()            