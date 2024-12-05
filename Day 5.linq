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
                |> Seq.groupBy(fun i -> i.Pre)
                |> Seq.map(fun (i, posts) -> (i, (posts |> Seq.map(fun t -> t.Post))))
                |> Map.ofSeq

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
                                let requirements = orders[page]
                                let previous = p.Pages[0..index]
                                all(requirements.Intersect(previous).Count() = 0)
                })
                |> Seq.map( fun p -> p.Pages[((p.Pages.Length - 1) / 2) + 1])
                |> Seq.sum
                
answer.Dump()