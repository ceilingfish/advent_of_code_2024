<Query Kind="FSharpProgram" />

let locationIds1 = new List<int>()
let locationIds2 = new List<int>()

File.ReadAllLines(@"C:\Users\tom\Documents\Advent Of Code 2025\day1_input.txt")
    |> Seq.map (fun line -> line.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Seq.iter( fun idPair -> match idPair with [| id1; id2 |] -> (locationIds1.Add(id1 |> int);locationIds2.Add(id2 |> int)))

locationIds1.Sort()
locationIds2.Sort();

let total = locationIds1 |> Seq.mapi(fun i id1 -> match id1 with
                                                  | smaller when smaller < locationIds2[i] -> locationIds2[i] - smaller
                                                  | larger when larger > locationIds2[i] -> larger  - locationIds2[i]
                                                  | _ -> 0)
                         |> Seq.sum
total.Dump()