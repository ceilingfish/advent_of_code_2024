<Query Kind="FSharpProgram" />

let grid = File.ReadAllLines(@"c:\Users\ceili\Documents\day4_input.txt")
            |> Seq.map (fun line -> line.ToCharArray())
            |> Seq.toArray
            
    
let mutable count = 0
for y in 1..(grid.Length - 2) do
    for x in 1..(grid[y].Length - 2) do
        
        if grid[x][y] = 'A' then
            let topLeft = grid[x-1][y-1]
            let topRight = grid[x+1][y-1]
            let bottomLeft = grid[x-1][y+1]
            let bottomRight = grid[x+1][y+1]
            
            count <- count + 
            if (topLeft = 'M' && bottomRight = 'S') || (topLeft = 'S' && bottomRight = 'M') then
                if topRight = 'M' && bottomLeft = 'S' then
                    1
                else if topRight = 'S' && bottomLeft = 'M' then
                    1
                else
                    0
            else
             0

count.Dump();