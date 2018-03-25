module Infrastructure.Writer

open Infrastructure.EventBus

// TODO: currently we only need events, however we will also need commands and DB changes in the future
type Writer<'effect,'a> = Writer of 'effect list * 'a

let unwrap (Writer (events,x)) = events,x

let bind (Writer (events,x)) f =
    let (Writer (events', x')) = f x
    let events'' = List.append events events'

    Writer (events'', x')

let ret x =
    Writer ([],x)

type WriterBuilder<'effect>() =
    member this.Bind<'a,'b> ((mx:Writer<'effect,'a>), (f:'a-> Writer<'effect, 'b>)) =
        bind mx f
    member this.Return<'a> (x) : Writer<'effect, 'a> = ret x
    member this.ReturnFrom<'a> (mx: Writer<'effect, 'a>) = mx
    member this.Zero() = Writer([],())
    member this.Combine(m,f) = bind m f
    member this.Delay f = f
    member this.Run  f = f()
    member this.While (cond, f) =

        let mutable events = []

        while cond() do
            let (Writer (events', _)) = f ()

            events <- events @ events'

        Writer(events,())

    member this.For (xs:seq<'res>, f) =
        using (xs.GetEnumerator()) (fun it ->
            this.While (
                (fun () -> it.MoveNext()),
                (fun () -> f it.Current) ))