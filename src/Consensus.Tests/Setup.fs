namespace Consensus.Tests

open NUnit.Framework

#if DEBUG

[<SetUpFixture>]
type Setup() =

    [<OneTimeSetUp>]
    member this.Setup () = Infrastructure.ZFStar.unitTesting <- true

    [<OneTimeTearDown>]
    member this.TearDown () = Infrastructure.ZFStar.unitTesting <- false

#endif