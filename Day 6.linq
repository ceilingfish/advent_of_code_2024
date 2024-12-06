<Query Kind="FSharpProgram" />

type Coordinate = {
    x: int
    y: int
}
let directions = [|
    { x = 0; y = -1};
    { x = 1; y = 0};
    { x = 0; y = 1};
    { x = -1; y = 0}
|]

let lines = File.ReadAllLines(@"C:\Users\ceili\Documents\day6_input.txt")
let mutable map = lines |> Array.map(fun line -> line.ToCharArray())

let start = (map
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

let rec moveNext = fun coordinate directionIndex ->
                            let direction = directions[directionIndex]
                            let nextCoordinate = { x = coordinate.x + direction.x; y = coordinate.y + direction.y }
                            if nextCoordinate.y < map.Length
                                && nextCoordinate.y >= 0
                                && nextCoordinate.x < map[nextCoordinate.y].Length
                                && nextCoordinate.x >= 0 then
                                match map[nextCoordinate.y][nextCoordinate.x] with
                                    | '#' -> 
                                        let nextDirection = (directionIndex + 1) % 4
                                        moveNext coordinate nextDirection
                                    | 'X' -> moveNext nextCoordinate directionIndex
                                    | '.' -> 
                                        map[nextCoordinate.y][nextCoordinate.x] <- 'X'
                                        1 + moveNext nextCoordinate directionIndex
                                    | '^' -> 1
                                    | _ -> 0
                            else 0


let length = moveNext start 0
length.Dump()
(map |> Seq.map(fun l -> l |> System.String)).Dump()