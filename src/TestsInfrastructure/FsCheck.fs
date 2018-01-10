module TestsInfrastructure.FsCheck

let shouldEqual (was, expected) =
    if was <> expected then
        printfn "expected %A\nbut was %A" expected was 
        false
    else
        true