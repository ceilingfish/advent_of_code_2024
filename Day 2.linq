<Query Kind="FSharpProgram" />

let readings = File.ReadAllLines(@"C:\Users\ceili\Documents\day2_input.txt")
                |> Seq.map (fun line -> line.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun i -> (i |> int)))
    
let checkNext sequence =
    

readings
    |> Seq.where(fun reading -> query {
        for index in 0..(reading.Count() - 2) do
        let current = reading[index]
        let next = reading[index+1]
        all (current - next < 0)
    })