module TestsInfrastructure.Nunit

open FsUnit

let shouldEqual (was, expected) =
    try 
        should equal expected was
    with _ as ex ->
        failwithf "\nexpected: %A\nbut was:  %A" expected was