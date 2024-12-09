<Query Kind="FSharpProgram" />


type Equation= {
    Result: int64
    Operands: int[]
}
type Operator = {
    Name: char
    Apply: int64 -> int64 -> int64
    Precedence: int
}

let operators = [|
    {
        Name = '+'
        Apply = fun x y -> (x + y) |> int64
        Precedence = 0
    };
    //{
    //    Name = '-'
    //    Apply = fun x y -> (x - y) |> int64
    //    Precedence = 0
    //};
    {
        Name = '*'
        Apply = fun x y -> (x * y) |> int64
        Precedence = 1
    };
    //{
    //    Name = '/'
    //    Apply = fun x y -> (x / y) |> int64
    //    Precedence = 1
    //}
|]
        
let equationPattern = Regex("^(?<Result>\d+):\s+(?<Sum>[\d\s]+)$")

let lines = File.ReadAllLines(@"C:\Users\ceili\Documents\day7_input.txt")
let equations = lines 
                |> Seq.map(fun line -> equationPattern.Match(line))
                |> Seq.where(fun m -> m.Success)
                |> Seq.map(fun m -> { 
                    Result = (m.Groups["Result"].Value |> int64)
                    Operands = m.Groups["Sum"].Value.Split(' ') |> Seq.map(fun i -> i |> int) |> Seq.toArray
                })
                
let printSum = fun equation (equationOperators: int[]) result ->
    let builder = new StringBuilder()
    builder
        .Append(equation.Result)
        .Append(": ")
        .Append(equation.Operands[0])
    for operatorIndex in 0..(equation.Operands.Length-2) do
        let oi = equationOperators[operatorIndex]
        builder
            .Append(" ")
            .Append(operators[oi].Name)
            .Append(" ")
            .Append(equation.Operands[operatorIndex + 1])
    
    builder
        .Append(" = ")
        .Append(result |> string)
    
    builder.ToString()
    
let calculateSum = fun equation (equationOperatorIndices: int[]) ->
    let index = 0
    let mutable values = (equation.Operands |> Seq.map(fun i -> i |> int64)).ToList()
    let mutable equationOperators = (equationOperatorIndices |> Seq.map(fun i -> operators[i])).ToList()
    
    while values.Count > 1 do
        let t = equationOperators |> Seq.tryFindIndex(fun o -> o.Precedence = 1)
        let nextIndex = match t with
                        | Some( index ) -> index
                        | _ -> 0
        
        let left = values[nextIndex]
        let right = values[nextIndex + 1]
        let operator = equationOperators[nextIndex]
        let newLeft = operator.Apply left right
        
        values.RemoveAt(nextIndex)
        equationOperators.RemoveAt(nextIndex)
        values[nextIndex] <- operator.Apply left right
        
    let result = values.Single()
    result
        
let validCalculations = new List<int64>()
for equation in equations do
    let equationOperators = [| for i in 0..(equation.Operands.Length-2) -> 0 |] |> Seq.toArray
    let mutable watermarkIndex = equation.Operands.Length - 2
    while watermarkIndex >= 0 do
    
        equationOperators[watermarkIndex] <- equationOperators[watermarkIndex] + 1
        if equationOperators[watermarkIndex] = operators.Length then
            equationOperators[watermarkIndex] <- 0
            watermarkIndex <- watermarkIndex - 1
    
        let mutable iterationIndex = equation.Operands.Length - 2
        
        while (calculateSum equation equationOperators) <> equation.Result && (iterationIndex > watermarkIndex) do
            equationOperators[iterationIndex] <- equationOperators[iterationIndex] + 1
            if equationOperators[iterationIndex] = operators.Length then
                equationOperators[iterationIndex] <- 0
                iterationIndex <- iterationIndex - 1
            //printSum equation equationOperators
                
        if calculateSum equation equationOperators = equation.Result then
            let thing = (printSum equation equationOperators equation.Result)
            validCalculations.Add(equation.Result)
            
        for resetIndex in (watermarkIndex + 1)..(equation.Operands.Length - 2) do
            equationOperators[resetIndex] <- 0
                
(validCalculations |> Seq.sum).Dump()