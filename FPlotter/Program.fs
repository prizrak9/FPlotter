// Learn more about F# at http://fsharp.org
open PMath
open PPlot

let inline vector x y z = Vector.vector x y z


let l1() =
   // main line
   let a = vector 1 1 0
   let b = vector 2 0 0
   // separate point
   let c = vector 1.5 3 0

   // function
   let func (r0:Vector) (r1:Vector) t =
       (r0 * (1. - t)) + (r1 * t)

   // build with step
   let build f count = 
       [for t in 0. .. 1./(float count) .. 1. -> f t]

   // main line
   let ab = build (func a b) 1

   // perpendicular
   let cd = build (func c (Math.project a b c)) 1

   let size =  vector 700 400 0
    
   [
        Plot.Build.line ab
        Plot.Build.segment cd
   ]
   |> List.choose id
   |> Plot.build
   |> Plot.withSize size

let l2() =
    let func (r0:Vector) (r1:Vector) (r2:Vector) u v =
        r0 * (1. - u - v) + r1 * u + r2 * v

    let build f count1 count2 = 
        [for u in 0. .. 1./(float count1) .. 1. do
            for v in 0. .. 1./(float count2) .. 1. -> 
                f u v]

    let a = vector 1 1 1
    let b = vector 1 0 2
    let c = vector 1 2 1
    let d = vector 2 1 1

    let abc = build (func a b c) 1 1
    let abd = build (func a b d) 1 1

    let norm (a:list<Vector>) = 
        (a.[2] - a.[0]) |*| (a.[1] - a.[0])
        |> Vector.norm

    let norm1 = norm abc
    let norm2 = norm abd

    let intersectionLine = 
        Math.planePlaneIntersection 
            abc.[0] norm1
            abd.[0] norm2

    let k = 1.

    //printfn "%A" abc

    [
        yield Plot.Build.plane abc
        yield Plot.Build.plane abd

        match intersectionLine with
        | Some (a, v) -> yield Plot.Build.segment3d [a + v * k; a - v * k]
        | None -> ()
    ]
    |> List.choose id
    |> Plot.build




let l3() =
    let inline v x y = vector x y 0

    let func (r0:Vector) (r1:Vector) (r2:Vector) (r3:Vector) t =
        (r0 * (1. - t) ** 3.) + (r1 * (1. - t) ** 2. * t * 3.) + (r2 * (1. - t) * t ** 2. * 3.) + (r3 * t ** 3.)

    let build f count = 
        [for t in 0. .. 1./(float count) .. 1. -> f t]

    let size =  vector 700 400 0
    
    [
        let points =
            [
                v 0.5 5 
                v 2 2 
                v 4 4 

                v 5.5 5.5 
                v 8 9 
                v 9 8 

                v 11 6 
                v 13 4
                v 15 5 

                v 13 9.5 
                v 12 11.5 
                v 13 13.5 

                v 15 11.5 
                v 15 11.5 
                v 15.3 11.4 

                v 15.3 11.4 
                v 15.5 11.0 
                v 15.5 10.5 

                v 16.5 10.4 
                v 14 10.4 
                v 13.5 10 

                v 15 8 
                v 16 7 
                v 16.2 5.5

                v 16.2 4
                v 16.2 2
                v 15.2 1.5
                
                v 15 1.5
                v 15 1.5
                v 15 1.5
                
                v 15 1.5
                v 13.5 1
                v 13 1
                
                v 13 1.5
                v 11.5 1
                v 11 1
                
                v 11 1.5
                v 9.5 1
                v 9 1
                
                v 9 1.5
                v 7.5 1
                v 7 1
                
                v 7 1.5
                v 5.5 1
                v 5 1
                
                v 5 1.5
                v 2.5 1
                v 2 1
                
                v 2 1.5
                v 0.5 1
                v 0 1.5
                
                v 1 2.5
                v 0.6 3
                v 0.6 4
                
                v 0.5 5
            ]
            
        for i in 0 .. 3 .. List.length points - 4 ->
            func points.[i] points.[i + 1] points.[i + 2] points.[i + 3]
            |> build <| 10
            |> Plot.Build.spline
    ]
    |> List.choose id
    |> Plot.build
    |> Plot.withSize size


let l4() =

    let surface a b x y =
        x ** 2. / a ** 2. - y ** 2. / b ** 2.

    let bezier (r0:Vector) (r1:Vector) (r2:Vector) (r3:Vector) t =
        (r0 * (1. - t) ** 3.) + (r1 * (1. - t) ** 2. * t * 3.) + (r2 * (1. - t) * t ** 2. * 3.) + (r3 * t ** 3.)

    let buildBezier f count = 
        [for t in 0. .. 1./(float count) .. 1. -> f t]

    let surfPrepared = surface 0.0001 0.0001

    let bezierCurves = 
        [
            let inline v x y = vector x y 0
            let points =
                [
                    v 0.5 5 
                    v 2 2 
                    v 4 4 

                    v 5.5 5.5 
                    v 8 9 
                    v 9 8 

                    v 11 6 
                    v 13 4
                    v 15 5 

                    v 13 9.5 
                    v 12 11.5 
                    v 13 13.5 

                    v 15 11.5 
                    v 15 11.5 
                    v 15.3 11.4 

                    v 15.3 11.4 
                    v 15.5 11.0 
                    v 15.5 10.5 

                    v 16.5 10.4 
                    v 14 10.4 
                    v 13.5 10 

                    v 15 8 
                    v 16 7 
                    v 16.2 5.5

                    v 16.2 4
                    v 16.2 2
                    v 15.2 1.5
                       
                    v 15 1.5
                    v 15 1.5
                    v 15 1.5
                       
                    v 15 1.5
                    v 13.5 1
                    v 13 1
                       
                    v 13 1.5
                    v 11.5 1
                    v 11 1
                       
                    v 11 1.5
                    v 9.5 1
                    v 9 1
                       
                    v 9 1.5
                    v 7.5 1
                    v 7 1
                       
                    v 7 1.5
                    v 5.5 1
                    v 5 1
                       
                    v 5 1.5
                    v 2.5 1
                    v 2 1
                       
                    v 2 1.5
                    v 0.5 1
                    v 0 1.5
                       
                    v 1 2.5
                    v 0.6 3
                    v 0.6 4
                       
                    v 0.5 5
                ]
                   
            let points = [for p in points -> {p with z = surfPrepared p.x p.y}]

            for i in 0 .. 3 .. List.length points - 4 ->
                bezier points.[i] points.[i + 1] points.[i + 2] points.[i + 3]
                |> buildBezier <| 10
        ]
    
    let buildSurface =
        [for u in -20. .. 1. .. 20. ->
            [for v in -20. .. 1. .. 20. ->
                vector u v (surfPrepared u v)]]


    [buildSurface |> Plot.Build.surface]
    @ (bezierCurves |> List.map Plot.Build.spline3D)
    |> List.choose id
    |> Plot.build

[
    l1()
    l2()
    l3()
    l4()
]
|> Plot.showAll



//[<EntryPoint>]
//let main argv =
//    //printfn "%A %A" good bad


//    //printfn "%A" g
//    0 // return an integer exit code
