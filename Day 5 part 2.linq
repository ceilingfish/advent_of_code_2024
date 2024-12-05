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
                |> Seq.toArray

let answer = lines
                |> Seq.map(fun line -> line.Split(','))
                |> Seq.where(fun p -> p.Length > 1)
                |> Seq.map(fun pages -> { 
                    Pages = pages
                                |> Seq.map(fun page -> (page |> int))
                                |> Seq.toArray
                })
                |> Seq.map(fun p -> 
                    let reOrderedPages = p.Pages |> Seq.toArray
                    let mutable hasReordered = false
                    for currentIndex in 0..(p.Pages.Length-2) do
                        for afterIndex in (currentIndex+1)..(p.Pages.Length-1) do
                            let currentPage = reOrderedPages[currentIndex]
                            let afterPage = reOrderedPages[afterIndex]
                            let currentBeforeAfter = orders |> Seq.exists(fun o -> o.Pre = afterPage && o.Post = currentPage)
                            hasReordered <- hasReordered or currentBeforeAfter
                            if currentBeforeAfter then
                                reOrderedPages[currentIndex] <- afterPage
                                reOrderedPages[afterIndex] <- currentPage
                    
                    (hasReordered, reOrderedPages)
                )
                |> Seq.where(fun (hasReordered, _) -> hasReordered)
                |> Seq.map( fun (_, p) -> 
                    let isEven = p.Length % 2
                    p[((p.Length - isEven) / 2)])
                |> Seq.sum
                
answer.Dump()