module DataAccess.SingleValue

let create (databaseContext:DatabaseContext) (name:string) serializer deserialzer =
    {
        collection=databaseContext.values
        name=System.Text.Encoding.ASCII.GetBytes(name);
        serializer=serializer;
        deserializer=deserialzer;
    }
    
let tryGet<'value> (singleValue:SingleValue<'value>) (session:Session) =
    let exist, value = session.TryGet (singleValue.collection,singleValue.name)
    
    if exist then
        Some (singleValue.deserializer value)
    else 
        None
        
let put<'value> (singleValue:SingleValue<'value>) (session:Session) value = 
    session.Put (singleValue.collection, singleValue.name, singleValue.serializer value)        
     
