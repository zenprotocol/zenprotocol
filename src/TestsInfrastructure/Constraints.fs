module TestsInfrastructure.Constraints

open System
open NUnit.Framework.Constraints
        
type OkConstraint<'ok,'error>() =
  inherit Constraint() with
    override __.Description with get () = "is result ok" 
   
    override this.ApplyTo(actual: 'TActual) : ConstraintResult =
        let actual = box actual
       
        if actual = null then         
            failwith "null"          
        else                          
            let t = actual.GetType()
                                                                              
            if t.Name = "FSharpResult`2" then                                
                let result = t.GetMethod("get_IsOk").Invoke(actual,[||]) :?> bool
       
                ConstraintResult(this,actual.GetType(),result)     
            else                                                  
                failwith "invalid type"    
                
type SomeConstraint() =
  inherit Constraint() with
    override __.Description with get () = "is option have some value" 
   
    override this.ApplyTo(actual: 'TActual) : ConstraintResult =
        let actual = box actual
       
        if actual = null then         
            ConstraintResult(this, typeof<Option<_>> ,false)          
        else
            let t = actual.GetType()
            
            if t.Name = "FSharpOption`1" then                                
                let result = t.GetMethod("get_IsSome").Invoke(actual,[|actual|]) :?> bool
       
                ConstraintResult(this,actual.GetType(),result)     
            else                                                  
                failwith "invalid type"                                   
                                     
let ok<'ok,'error> = 
    new OkConstraint<'ok,'error>()     
    
let some = 
    new SomeConstraint()           