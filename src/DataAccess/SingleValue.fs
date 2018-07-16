module DataAccess.SingleValue

let create (databaseContext:DatabaseContext) (name:string) serializer deserialzer =
    {
        collection=databaseContext.values
        name=System.Text.Encoding.ASCII.GetBytes(name);
        serializer=serializer;
        deserializer=deserialzer;
    }
    
let tryGet<'value> (singleValue:SingleValue<'value>) (session:Session) =         
    Collection.tryGet singleValue.collection session singleValue.name
    |> Option.bind singleValue.deserializer        
        
let put<'value> (singleValue:SingleValue<'value>) (session:Session) value = 
    Collection.put singleValue.collection session singleValue.name (singleValue.serializer value)     

let delete<'value> (singleValue:SingleValue<'value>) (session:Session) = 
    Collection.delete singleValue.collection session singleValue.name     
