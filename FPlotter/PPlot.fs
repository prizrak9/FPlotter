module PPlot.Plot
open PMath
open XPlot.Plotly
open Vector

type PlotObj =
    | Line of list<Vector>
    | Segment of list<Vector>
    | Segment3d of list<Vector>
    | Spline of list<Vector>
    | Spline3d of list<Vector>
    | Plane of list<Vector> * list<list<int>>
    | Surface of list<Vector> * list<list<int>>
    | Mesh of list<Vector> * list<list<int>>
    static member points =
        function
        | Line a | Segment a | Segment3d a | Spline a | Spline3d a 
        | Plane (a,_) | Mesh (a,_) | Surface (a,_) -> a

[<RequireQualifiedAccessAttribute>]
module Build =
    let line (points:list<Vector>) =
        if Math.isStraightLine points
        then points |> Line |> Some
        else None
    
    let segment (points:list<Vector>) =
        if Math.isStraightLine points
        then points |> Segment |> Some
        else None

    let spline (points:list<Vector>) =
        points |> Spline |> Some

    let spline3D (points:list<Vector>) =
        points |> Spline3d |> Some

    let segment3d (points:list<Vector>) =
        if Math.isStraightLine points
        then points |> Segment3d |> Some
        else None

    let plane (points:list<Vector>) =
        match points with
        | [_;_;_;_] ->
            let vertices = 
                [
                    [0;1;2]
                    [2;1;3]
                ]
            (points, vertices) |> Plane |> Some
        | _ -> failwith "not implemented"

    let surface (points:list<list<Vector>>) =
        match points with
        | [_] -> None
        | x when x |> List.forall(List.length >> (=) 1) -> None
        | x when x |> List.map(List.length) |> List.distinct |> List.length <> 1 -> None
        | _ ->
            let stride = points.Head |> List.length
            let rowCount = points |> List.length
            let flatPoints = [for p in points do yield! p]
            let vertices =
                [
                    for r in 0 .. rowCount - 2 do
                        for p in 0 .. stride - 2 do
                            let i = r * stride + p
                            yield [i;i+stride;i+1]
                            yield [i+1;i+stride;i+stride+1]
                ]

            (flatPoints, vertices) |> Surface |> Some


let scatter line:Trace =
    match line with
    | Line points | Spline points ->
        Graph.Scatter(x = List.xs points, y = List.ys points, mode="lines")
        :> Trace

    | Spline3d points ->
        Graph.Scatter3d(x = List.xs points, y = List.ys points, z = List.zs points, mode="lines")
        :> Trace

    | Segment points ->
        Graph.Scatter(x = List.xs points, y = List.ys points, mode="lines+markers")
        :> Trace

    | Segment3d points ->
        Graph.Scatter3d(x = List.xs points, y = List.ys points, z = List.zs points, mode="lines+markers")
        :> Trace

    | Plane (points, vertices) ->
        let select f = points |> List.map f
        //let select f = [for points in select f do yield! points]

        let xs = select (fun a -> a.x)
        let ys = select (fun a -> a.y)
        let zs = select (fun a -> a.z)

        let vertices = vertices |> List.transpose

        //printfn "%A %A %A" xs ys zs

        Graph.Mesh3d(
            x = xs, 
            y = ys,
            z = zs,
            i = vertices.[0],
            j = vertices.[1],
            k = vertices.[2]
            //color="lightpink", 
            //opacity=0.50,
        //    name="y",
        //    showscale=true
        )
        :> Trace
 
    //| Surface points ->
    //    Graph.Surface(z = points)
    //    :> Trace

    | Surface (points, vertices) ->
        let select f = points |> List.map f
        //let select f = [for points in select f do yield! points]

        let xs = select (fun a -> a.x)
        let ys = select (fun a -> a.y)
        let zs = select (fun a -> a.z)

        let vertices = vertices |> List.transpose

        //printfn "%A %A %A" xs ys zs

        Graph.Mesh3d(
            x = xs, 
            y = ys,
            z = zs,
            i = vertices.[0],
            j = vertices.[1],
            k = vertices.[2]
            //color="lightpink", 
            //opacity=0.50,
        //    name="y",
        //    showscale=true
        )
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
            |> List.choose id

        if intersections.Length = 0
        then failwith "line doesn't intersect any plot border"
        else
            let start,``end`` = line |> PlotObj.points |> fl
            let r = ``end`` - start

            let areCollinear a b = Vector.dotProduct a b > 0.

            let newStart = 
                intersections 
                |> List.where ((-) ``end`` >> areCollinear r)
                |> List.minBy ((-) ``end`` >> Vector.magnitude)

            let newEnd = 
                intersections 
                |> List.where ((-) start >> areCollinear -r)
                |> List.minBy ((-) start >> Vector.magnitude)

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

    
let private buildPlot plot =
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

let show = buildPlot >> Chart.Show
let showAll = List.map buildPlot >> Chart.ShowAll



