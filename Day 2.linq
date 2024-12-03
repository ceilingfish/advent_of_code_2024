<Query Kind="FSharpProgram" />

let locationIdsLeft = new List<int>()
let locationIdsRight = new List<int>()

File.ReadAllLines(@"C:\Users\tom\Documents\Advent Of Code 2025\day1_input.txt")
    |> Seq.map (fun line -> line.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Seq.iter( fun idPair -> match idPair with [| id1; id2 |] -> (locationIdsLeft.Add(id1 |> int);locationIdsRight.Add(id2 |> int)))

let distinctLeftIds = locationIdsLeft |> Seq.distinct
locationIdsRight.Sort();

let total = locationIdsRight
            |> Seq.where (fun v -> distinctLeftIds.Contains(v))
            |> Seq.sum
total.Dump()