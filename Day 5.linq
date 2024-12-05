<Query Kind="FSharpProgram" />

type PageOrder = {
    Pre: int
    Post: int
}

type Patch = {
    Pages: int[]
}
        
let orderPattern = Regex("^(?<Pre>\d+)\|(?<Post>\d+)$")

let lines = File.ReadAllLines(@"C:\Users\ceili\Documents\day5_input.txt")
let orders = lines 
                |> Seq.map(fun line -> orderPattern.Match(line))
                |> Seq.where(fun m -> m.Success)
                |> Seq.map(fun m -> { 
                    Pre = (m.Groups["Pre"].Value |> int)
                    Post = (m.Groups["Post"].Value |> int)
                })

let answer = lines
                |> Seq.map(fun line -> line.Split(','))
                |> Seq.where(fun p -> p.Length > 1)
                |> Seq.map(fun pages -> { 
                    Pages = pages
                                |> Seq.map(fun page -> (page |> int))
                                |> Seq.toArray
                })
                |> Seq.where(fun p -> query {
                                for index, page in Seq.indexed p.Pages do
                                let previous = p.Pages[0..(index-1)]
                                let subsequent = p.Pages[(index+1)..]
                                let preConflicts = orders.Any(fun p -> p.Pre = page && previous.Contains(p.Post))
                                let postConflicts = orders.Any(fun p -> p.Post = page && subsequent.Contains(p.Pre))
                                all(not preConflicts && not postConflicts)
                })
                |> Seq.map( fun p -> 
                    let isEven = p.Pages.Length % 2
                    p.Pages[((p.Pages.Length - isEven) / 2)])
                |> Seq.sum
                
answer.Dump()