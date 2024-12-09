<Query Kind="FSharpProgram">
  <RuntimeVersion>8.0</RuntimeVersion>
</Query>

type Coordinate = {
    x: int
    y: int
}
type Direction =
    Up = 0
    | Right = 1
    | Down = 2
    | Left = 3
type DirectionVector = {
    direction: Direction
    x: int
    y: int
}

let directions = [|
    { x = 0; y = -1; direction = Direction.Up};
    { x = 1; y = 0; direction = Direction.Right};
    { x = 0; y = 1; direction = Direction.Down};
    { x = -1; y = 0; direction = Direction.Left}
|]

let lines = File.ReadAllLines(@"C:\Users\ceili\Documents\day6_input.txt")
let mutable map = lines |> Array.map(fun line -> line.ToCharArray())

let mutable coordinate = (map
                    |> Seq.indexed
                    |> Seq.map(fun (yIndex, line) ->
                                line
                                |> Seq.indexed
                                |> Seq.map(fun (xIndex, c) -> if c = '^' then Some( { x = xIndex; y = yIndex } ) else None)
                    )
                    |> Seq.collect id
                    |> Seq.where(fun x -> match x with 
                                            | Some(pos) -> true 
                                            | _ -> false)
                    |> Seq.head).Value

let mutable directionIndex = 0
let mutable route: list<DirectionVector> = []
let mutable locations: list<Coordinate> = []
let mutable shouldContinue = true

while shouldContinue do

    let direction = directions[directionIndex]
    let nextCoordinate = { x = coordinate.x + direction.x; y = coordinate.y + direction.y }
    let newRoute = route @ [{ x = nextCoordinate.x; y = nextCoordinate.y; direction = direction.direction }]
    
    shouldContinue <- nextCoordinate.y < map.Length
        && nextCoordinate.y >= 0
        && nextCoordinate.x < map[coordinate.y].Length
        && nextCoordinate.x >= 0
    
    if shouldContinue then
        match map[nextCoordinate.y][nextCoordinate.x] with
            | '#' -> 
                directionIndex <- (directionIndex + 1) % 4
                ()
            | 'X' | '0' | '1' | '2' | '3' | '^' | '.' -> 
                
                map[nextCoordinate.y][nextCoordinate.x] <- 'X' //(string directionIndex).ToCharArray()[0]
                
                //let mutable loopDirection = direction
                //let mutable loopX = coordinate.x + loopDirection.x
                //let mutable loopY = coordinate.y + loopDirection.y
                //while loopX >= 0 && loopX < map[0].Length && loopY >= 0 && loopY < map.Length do
                //    if map[loopX][loopY] = '#' then
                //    
                //        let nextLoopDirection = directions[((loopDirection.direction |> int) + 1) % 4]
                //        loopX <- loopX - loopDirection.x + nextLoopDirection.x
                //        loopY <- loopY - loopDirection.y + nextLoopDirection.y
                //        loopDirection <- nextLoopDirection
                //        ()
                //    else
                //        let isLoop = route |> Seq.exists(fun r -> r.x = loopX && r.y = loopY && r.direction = loopDirection.direction)
                //        if isLoop then
                //            locations <- locations @ [{ x = nextCoordinate.x; y = nextCoordinate.y }]
                //            map[nextCoordinate.y][nextCoordinate.x] <- 'X'
                //            loopX <- -1
                //            loopY <- -1
                //            ()
                //        else
                //            loopX <- loopX + nextDirection.x
                //            loopY <- loopY + nextDirection.y
                //            ()
                route <- newRoute
                coordinate <- nextCoordinate
                ()
            | _ -> ()
            
locations.Dump()
(map |> Seq.map(fun line -> new System.String(line))).Dump()