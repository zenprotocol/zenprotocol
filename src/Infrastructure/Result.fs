module Infrastructure.Result

type ResultBuilder<'err>() =
    member this.Return<'res> a : Result<'res,'err> = Ok a
    member this.Bind<'a, 'b> (m:Result<'a,'err>, f:'a->Result<'b,'err>) = Result.bind f m
    member this.ReturnFrom<'res> m : Result<'res,'err> = m
    member this.Zero() = Ok ()
    member this.Combine(a:Result<unit,'err>, f:unit -> _) = Result.bind f a
    member this.Yield<'res> a : Result<'res,'err> = Ok a
    member this.YieldFrom<'res> a : Result<'res,'err> = a
    member this.Delay (f : unit -> 'res) = f
    member this.Run<'res> f : Result<'res,'err> = f()
    member this.While (cond, f) =
        if cond() then this.Bind(f(), fun _ -> this.While(cond, f))
        else this.Zero()
    member this.For (xs:seq<'res>, f) =
        using (xs.GetEnumerator()) (fun it ->
            this.While (
                (fun () -> it.MoveNext()),
                (fun () -> f it.Current) )
        )