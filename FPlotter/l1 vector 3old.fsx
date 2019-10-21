#r "f://work/0/XPlot.Plotly.dll"

module List =
    let inline some a = List.choose (fun a -> a) a

module Vector =
    type Vector = { x:float; y:float; z:float }

    let inline private applyLeft f a b = { x = f a b.x; y = f a b.y; z = f a b.z }
    let inline private applyRight f a b = { x = f a.x b; y = f a.y b; z = f a.z b }
    let inline private applyBoth f a b = { x = f a.x b.x; y = f a.y b.y; z = f a.z b.z }
    let inline private apply f a = { x = f a.x; y = f a.y; z = f a.z }
    
    let inline sqrMagnitude a = a.x ** 2. + a.y ** 2. + a.z ** 2.
    let inline magnitude a = a |> sqrMagnitude |> sqrt
    let inline norm a = 
        let m = magnitude a
        applyRight (/) a m

    let inline dotProduct a b = a.x * b.x + a.y * b.y + a.z * b.z
    let inline crossProduct a b =
        {
            x = a.y * b.z - a.z * b.y
            y = a.z * b.x - a.x * b.z
            z = a.x * b.y - a.y * b.x 
        }
    let inline Cos a b = dotProduct a b / magnitude a / magnitude b

    type Vector with
        member a.SqrMagnitude = sqrMagnitude a
        member a.Magnitude = magnitude a
        member a.Normalized = norm a

        static member (*) (a,b) = applyBoth (*) a b 
        static member (*) (a,b) = applyLeft (*) a b
        static member (*) (a,b) = applyRight (*) a b
        static member (+) (a,b) = applyBoth (+) a b
        static member (/) (a,b) = applyBoth (/) a b
        static member (-) (a,b) = applyBoth (-) a b
        static member (~-) a = apply (~-) a
        static member (.*.) (a,b) = dotProduct a b
        static member (|*|) (a,b) = crossProduct a b


    let inline vector x y z = { x = float x; y = float y; z = float z }
    let one = vector 1 1 1
    let oneX = vector 1 0 0
    let oneY = vector 0 1 0

    module List =
        let xs = List.map (fun (a:Vector) -> a.x)   
        let ys = List.map (fun (a:Vector) -> a.y)   
        let zs = List.map (fun (a:Vector) -> a.z)   

module Math =
    open Vector

    //Calculate the intersection point of two lines. Returns true if lines intersect, otherwise false.
    //Note that in 3d, two lines do not intersect most of the time. So if the two lines are not in the 
    //same plane, use ClosestPointsOnTwoLines() instead.
    let lineLineIntersection (line1:Vector list) (line2:Vector list) =
        let fl (a:list<_>) = List.last a, a.Head

        let r1 = fl line1 ||> (-)
        let r2 = fl line2 ||> (-)
        let r3 = line2.Head - line1.Head

        let crossr1r2  = r1 |*| r2
        let crossr3r2  = r3 |*| r2

        let planarFactor = r3 .*. crossr1r2

        if abs planarFactor < 1e-4 && crossr1r2.SqrMagnitude > 1e-4
        then
            let s = crossr3r2 .*. crossr1r2 / crossr1r2.SqrMagnitude
            (line1.Head) + (r1 * s) |> Some
        else None

    // Finds the closest point on line to given point
    let project (start:Vector) ``end`` point =
        let r1 = point - start
        let r2 = ``end`` - start
        
        let perpendicular1 = r2 |*| r1
        let perpendicular2 = perpendicular1 |*| r2
        
        let dist = perpendicular1.Magnitude / r2.Magnitude
        
        point - perpendicular2.Normalized * dist

    let isStraightLine =
        function
        | a::t when t 
            |> List.map (fun x -> Vector.Cos (x - a) Vector.oneX) 
            |> List.distinct
            |> List.length = 1 -> true
        | _ -> false

module Plot =
    open Vector
    open XPlot.Plotly

    type PlotObj =
        | Line of list<Vector>
        | Segment of list<Vector>
        | Spline of list<Vector>
        | Plane of list<list<Vector>>
        static member points =
            function
            | Line a | Segment a | Spline a -> a
            | Plane a -> [for b in a do yield! b]

    let buildLine (points:list<Vector>) =
        if Math.isStraightLine points
        then points |> Line |> Some
        else None
        
    let buildSegment (points:list<Vector>) =
        if Math.isStraightLine points
        then points |> Segment |> Some
        else None

    let buildPlane (points:list<list<Vector>>) =
        points |> Plane |> Some

    let scatter line:Trace =
        match line with
        | Line points ->
            Graph.Scatter(x = List.xs points, y = List.ys points, mode="lines")
            :> Trace
        | Segment points ->
            Graph.Scatter(x = List.xs points, y = List.ys points, mode="lines+markers")
            :> Trace
        | Plane points ->
            Graph.Surface(z = points)
            :> Trace
        | _ -> failwith "not implemented"


    let scatterMany lines =
        [for line in lines -> scatter line]

    let makeSquareRanges lines canvasSize =
        let points = [for line in lines do yield! line]
    
        let extreme selectProperty selectPoint = 
            points |> selectPoint |> selectProperty
    
        let get f = vector (f List.xs) (f List.ys) 0
    
        let max = get (extreme List.max)
        let min = get (extreme List.min)
    
        let plotSize = max - min

        // Compensates for xplot dev's retartedness
        let magicK = 0.85
        let plotSize = vector plotSize.x (plotSize.y * magicK) 0
    
        let ratioCanvasToPlot = 
            let ratioCanvas = canvasSize.x / canvasSize.y
            let ratioPlot = plotSize.x / plotSize.y

            ratioCanvas / ratioPlot
        
        // canvas = plot
        // a/b = c/d
        if ratioCanvasToPlot > 1.
        then
            // increase x
            // a/b = cx/d
            let k = plotSize.y * canvasSize.x / canvasSize.y / plotSize.x
            let sizeToAdd = plotSize.x * k - plotSize.x
            let xRange = [min.x - sizeToAdd; max.x + sizeToAdd]
            let yRange = [min.y; max.y]
    
            xRange, yRange
        else
            // increase y
            // a/b = c/dx
            let k = plotSize.x * canvasSize.y / canvasSize.x / plotSize.y
            let sizeToAdd = plotSize.y * k - plotSize.y
            let xRange = [min.x; max.x]
            let yRange = [min.y - sizeToAdd; max.y + sizeToAdd]
    
            xRange, yRange

    let extendLineToBorders borders line =
        match line with
        | Line points | Segment points ->
            let fl (a:list<_>) = a.Head, List.last a
            let (minx,maxx),(miny,maxy) = fl (fst borders), fl (snd borders)
           
            let intersections = 
                [
                    for i in [
                        [vector maxx maxy 0; vector maxx miny 0]
                        [vector maxx maxy 0; vector minx maxy 0]
                        [vector maxx miny 0; vector minx miny 0]
                        [vector minx maxy 0; vector minx miny 0]
                    ] -> Math.lineLineIntersection i points
                ]
                |> List.some

            if intersections.Length = 0
            then failwith "line doesn't intersect any plot border"
            else
                let start,``end`` = line |> PlotObj.points |> fl
                let r = ``end`` - start

                let areCollinear a b = Vector.dotProduct a b > 0.

                let newStart = 
                    intersections 
                    |> List.where ((-) ``end`` >> areCollinear r)
                    |> List.sortBy ((-) ``end`` >> Vector.magnitude)
                    |> List.head

                let newEnd = 
                    intersections 
                    |> List.where ((-) start >> areCollinear -r)
                    |> List.sortBy ((-) start >> Vector.magnitude)
                    |> List.head

                Line [newStart;newEnd] 
        | _ -> failwith "not implemented"
  


    type Plot = { objects:PlotObj list; size:Vector }
          
    let private plot = { objects = []; size = vector 700 400 0 }

    let build objects =
        { plot with objects = objects; }

    let withObject object plot =
        { plot with objects = object::plot.objects }

    let withSize size plot =
        { plot with size = size }

        
    let show plot =
        let lines = plot.objects |> List.map PlotObj.points
     
        let newRange = makeSquareRanges lines plot.size 

        let optimize a =
            match a with 
            | Line _ -> extendLineToBorders newRange a
            | _ -> a

        let lines = plot.objects |> List.map optimize

        let layout = Options(
            xaxis = Graph.Xaxis(range = fst newRange),
            yaxis = Graph.Yaxis(range = snd newRange)
        )

        lines
        |> scatterMany
        |> Chart.Plot
        |> Chart.WithSize(int plot.size.x, int plot.size.y)
        |> Chart.WithOptions layout
        |> Chart.WithLegend true
        |> Chart.Show


open Vector
  
// l1
//let l1 =
//    // main line
//    let a = vector 1 1 0
//    let b = vector 2 2 0

//    // separate point
//    let c = vector 1.5 3 0

//    // function
//    let func (r0:Vector) (r1:Vector) t =
//        (r0 * (1. - t)) + (r1 * t)

//    // build with step
//    let build f count = 
//        [for t in 0. .. 1./(float count) .. 1. -> f t]

//    // main line
//    let ab = build (func a b) 1

//    // perpendicular
//    let cd = build (func c (Math.project a b c)) 1

//    let size =  vector 700 400 0

//    [
//         Plot.buildLine ab
//         Plot.buildSegment cd
//    ]
//    |> List.some
//    |> Plot.build
//    |> Plot.withSize size
//    |> Plot.show



// l2
let l2 =
    let func (r0:Vector) (r1:Vector) (r2:Vector) u v =
        r0 * (1. - u - v) + r1 * u + r2 * v

    let build f count1 count2 = 
        [for u in 0. .. 1./(float count1) .. 1. ->
            [for v in 0. .. 1./(float count2) .. 1. -> 
                f u v]]

    let a = vector 1 1 1
    let b = vector 1 1 2
    let c = vector 1 2 1
    let d = vector 2 1 1

    let abc = build (func a b c) 10 10

    [
        Plot.buildPlane abc
    ]
    |> List.some
    |> Plot.build
    |> Plot.show
    ()