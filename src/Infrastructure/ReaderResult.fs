module Infrastructure.ReaderResult

type ReaderResult<'env, 'err, 'a> = 'env -> Result<'a, 'err>

let bind (k : 'a -> 'env -> Result<'b, 'err>) (m : 'env -> Result<'a, 'err>) (env : 'env) : Result<'b, 'err> =
    m env |> Result.bind (fun x -> k x env)

let ret (x : 'a) (env : 'env) : Result<'a, 'err> =
    Ok x

let eval (env : 'env) (m : ReaderResult<'env, 'err, 'a>) : Result<'a, 'err> =
    m env

type ReaderResultBuilder() =
    
    member this.Return(x : 'a) : ReaderResult<'env, 'err, 'a> =
        ret x
    
    member this.ReturnFrom(m : ReaderResult<'env, 'err, 'a>) =
        m
    
    member this.Bind(m : ReaderResult<'env, 'err, 'a>, k : 'a -> ReaderResult<'env, 'err, 'b>) : ReaderResult<'env, 'err, 'b> =
        bind k m
    
    member this.Zero() = this.Return ()
    
    member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)

let readerResult = new ReaderResultBuilder()

let inline (>>=) m f =
    bind f m

let inline (=<<) f m =
    bind f m 

let inline (<*>) mf mx =
    mx >>= fun x -> mf >>= fun f -> ret (f x)

let inline ap mx mf =
    mf <*> mx

let inline map f m =
    m >>= (f >> ret)

let inline (<!>) f m =
    map f m

let inline lift2 f a b =
    ret f <*> a <*> b

let inline ( *>) x y =
    lift2 (fun _ z -> z) x y

let inline ( <*) x y =
    lift2 (fun z _ -> z) x y

let inline (>=>) f g x =
    f x >>= g

let inline (<=<) f g =
    (>=>) g f

let ask : ReaderResult<'env, 'err, 'env> =
    Ok

let asks (f : 'env -> 'a) : ReaderResult<'env, 'err, 'a> =
    map f ask

let local (f : 'env1 -> 'env2) (m : ReaderResult<'env2, 'err, 'a>) : ReaderResult<'env1, 'err, 'a> =
    f >> m

let foldM (f : 's -> 'a -> ReaderResult<'env,'err,'s>) (s : ReaderResult<'env,'err,'s>) (xs : seq<'a>) : ReaderResult<'env,'err,'s> =
    Seq.fold (fun acc t -> acc >>= (fun y -> f y t)) s xs

let inline sequence s =
    let inline cons a b = lift2 (fun x xs -> x :: xs) a b
    List.foldBack cons s (ret [])

let inline mapM f x = sequence (List.map f x)

let ok (x : 'a) : ReaderResult<'env, 'err, 'a> =
    fun _ -> Ok x

let error (err: 'err) : ReaderResult<'env, 'err, 'a> =
    fun _ -> Error err

let liftResult (res : Result<'a, 'err>) : ReaderResult<'env, 'err, 'a> =
    fun _ -> res