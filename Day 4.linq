<Query Kind="FSharpProgram" />

let grid = File.ReadAllLines(@"c:\Users\ceili\Documents\day4_input.txt")
            |> Seq.map (fun line -> line.ToCharArray())
            |> Seq.toArray
            
let letters : char[] = [| 'X'; 'M'; 'A'; 'S' |]
type coordinate = {
    x: int
    y: int
}

let rec matchWord = fun coord next i ->
    let current = grid[coord.x][coord.y]
    let expected = letters[i]

    if current = expected then
     if i = 3 then
      1
     else
       let nextCoord = next coord
       match nextCoord with
                          | { y = -1 } -> 0
                          | { x = -1 } -> 0
                          | {x = _ } when nextCoord.x = grid[0].Length -> 0
                          | {y = _ } when nextCoord.y = grid.Length -> 0
                          | _ -> matchWord nextCoord next (i+1)
    else 0
    
let mutable count = 0
for y in 0..(grid.Length - 1) do
    for x in 0..(grid[y].Length - 1) do
        
        let coord = {x = x; y = y}
        count <- count + matchWord coord (fun coord -> { coord with y = coord.y - 1 }) 0 // N
        count <- count + matchWord coord (fun coord -> { y = coord.y - 1; x = coord.x + 1 }) 0 // NE        
        count <- count + matchWord coord (fun coord -> { coord with x = coord.x + 1 }) 0 // E
        count <- count + matchWord coord (fun coord -> { y = coord.y + 1; x = coord.x + 1 }) 0 // SE
        count <- count + matchWord coord (fun coord -> { coord with y = coord.y + 1 }) 0 // S
        count <- count + matchWord coord (fun coord -> { x = coord.y - 1; y = coord.y + 1 }) 0 // SW
        count <- count + matchWord coord (fun coord -> { coord with x = coord.x - 1 }) 0 // W
        count <- count + matchWord coord (fun coord -> { x = coord.y - 1; y = coord.y - 1 }) 0 // NW

count.Dump();