namespace PMath

module Math =
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

        if abs planarFactor < 1e-4 && Vector.sqrMagnitude crossr1r2 > 1e-4
        then
            let s = crossr3r2 .*. crossr1r2 / Vector.sqrMagnitude crossr1r2
            (line1.Head) + (r1 * s) |> Some
        else None

    // Finds the closest point on line to given point
    let project (start:Vector) ``end`` point =
        let r1 = point - start
        let r2 = ``end`` - start
        
        let perpendicular1 = r2 |*| r1
        let perpendicular2 = perpendicular1 |*| r2
        
        let dist = Vector.magnitude perpendicular1 / Vector.magnitude r2
        
        point - Vector.norm perpendicular2 * dist

    let isStraightLine =
        function
        | a::t when t 
            |> List.map (fun x -> Vector.cos (x - a) Vector.oneX) 
            |> List.distinct
            |> List.length = 1 -> true
        | _ -> false

    //Find the line of intersection between two planes.	The planes are defined by a normal and a point on that plane.
    //The outputs are a point on the line and a vector which indicates it's direction. If the planes are not parallel, 
    //the function outputs true, otherwise false.
    let planePlaneIntersection (pos1:Vector) (norm1:Vector) (pos2:Vector) (norm2:Vector) =

        //We can get the direction of the line of intersection of the two planes by calculating the 
        //cross product of the normals of the two planes. Note that this is just a direction and the line
        //is not fixed in space yet. We need a point for that to go with the line vector.
        let lineVec = norm1 |*| norm2

        //Next is to calculate a point on the line to fix it's position in space. This is done by finding a vector from
        //the plane2 location, moving parallel to it's plane, and intersecting plane1. To prevent rounding
        //errors, this vector also has to be perpendicular to lineDirection. To get this vector, calculate
        //the cross product of the normal of plane2 and the lineDirection.		
        let ldir = norm2 |*| lineVec
        let denominator = norm1 .*. ldir

        //Prevent divide by zero and rounding errors by requiring about 5 degrees angle between the planes.
        if abs denominator > 0.006
        then
            let plane1ToPlane2 = pos1 - pos2
            let t = (norm1 .*. plane1ToPlane2) / denominator
            let linePoint = pos2 + t * ldir
            
            Some (linePoint, lineVec)
        else None
