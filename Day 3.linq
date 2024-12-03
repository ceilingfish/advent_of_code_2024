<Query Kind="FSharpProgram" />

let expression = Regex(@"\bdo\(\)|don't\(\)|mul\((?<Left>\d+),(?<Right>\d+)\)")

let rec calculateMultiples input enabled = 
    
    let m = expression.Match(input)
    if m.Success then
        let value = m.Value
        let next = input.Substring (m.Index + m.Length)
        match m.Value with
              | "do()" -> calculateMultiples next true
              | "don't()" -> calculateMultiples next false
              | _ when enabled ->
                let left = m.Groups["Left"].Value |> int
                let right = m.Groups["Right"].Value |> int
                left * right + (calculateMultiples next enabled)
              | _ -> calculateMultiples next enabled
    else 0
    
  
let content = File.ReadAllText(@"C:\Users\tom\Documents\Advent Of Code 2025\day3_input.txt");
(calculateMultiples content true).Dump()