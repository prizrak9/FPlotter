namespace PMath

//let inline internal applyLeft f a b = [for a in a -> f a b]
//let inline internal applyRight f a b = [for b in b -> f a b]
//let inline internal applyBoth f a b = [for a,b in List.zip a b -> f a b]
//let inline internal apply f a = [for a in a -> f a]

//let inline sqrMagnitude a = a |> List.map (fun a -> a ** 2.) |> List.sum
//let inline magnitude a = a |> sqrMagnitude |> sqrt
//let inline norm a = 
//    match magnitude a with
//    | 1. -> a
//    | m -> applyLeft (/) a m

//let inline dotProduct a b = List.zip a b |> List.map (fun (a,b) -> a * b) |> List.sum
//let inline crossProduct a b =
//    match a |> List.length, b |> List.length with
//    | 3,3 ->
//        [
//            a.[1] * b.[2] - a.[2] * b.[1]
//            a.[2] * b.[0] - a.[0] * b.[2]
//            a.[0] * b.[1] - a.[1] * b.[0]

//        ]
//    | a,b when a <> b -> failwith "lengths of vectors are not equal"
//    | _ -> failwith "lengths of vectors are not suported by 'crossProduct'"
 

type Vector = 
    { x:float; y:float; z:float }

    static member private applyRight f a b = { x = f a b.x; y = f a b.y; z = f a b.z }
    static member private applyLeft f a b = { x = f a.x b; y = f a.y b; z = f a.z b }
    static member private applyBoth f a b = { x = f a.x b.x; y = f a.y b.y; z = f a.z b.z }
    static member private apply f a = { x = f a.x; y = f a.y; z = f a.z }

    static member sqrMagnitude a = a.x ** 2. + a.y ** 2. + a.z ** 2.
    static member magnitude a = a |> Vector.sqrMagnitude |> sqrt
    static member norm a = 
        match Vector.magnitude a with
        | 1. -> a
        | m -> Vector.applyLeft (/) a m

    static member dotProduct a b = a.x * b.x + a.y * b.y + a.z * b.z
    static member crossProduct a b =
        {
            x = a.y * b.z - a.z * b.y
            y = a.z * b.x - a.x * b.z
            z = a.x * b.y - a.y * b.x 
        }
    static member cos a b = Vector.dotProduct a b / Vector.magnitude a / Vector.magnitude b
   
    static member (*) (a,b) = Vector.applyBoth (*) a b 
    static member (*) (a,b) = Vector.applyLeft (*) a b
    static member (*) (a,b) = Vector.applyRight (*) a b
    static member (+) (a,b) = Vector.applyBoth (+) a b
    static member (/) (a,b) = Vector.applyBoth (/) a b
    static member (/) (a,b) = Vector.applyLeft (/) a b
    static member (-) (a,b) = Vector.applyBoth (-) a b
    static member (~-) a = Vector.apply (~-) a
    static member (.*.) (a,b) = Vector.dotProduct a b
    static member (|*|) (a,b) = Vector.crossProduct a b

module Vector =
    let inline vector x y z = { x = float x; y = float y; z = float z }
    let one = vector 1 1 1
    let oneX = vector 1 0 0
    let oneY = vector 0 1 0

module List =
    let xs = List.map (fun (a:Vector) -> a.x)   
    let ys = List.map (fun (a:Vector) -> a.y)   
    let zs = List.map (fun (a:Vector) -> a.z)   