type Expression1() =
    let mutable list:int list = []
    member this.Bind(x,f) =
        printfn "%A" f
        Some (f x)

    //member this.For(x,f) =
    //    this.Bind(x,f)

    //[<CustomOperation("get")>]
    //member this.Get(x,f) =
    //    list <- list @ [x]
    //    this.Bind(x,f)



    member this.Yield(x) =
        x

let expr1 = Expression1()

let g = expr1 {
    let! x = 1
    yield x
}
