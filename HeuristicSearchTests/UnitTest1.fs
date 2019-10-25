module HeuristicSearchTests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    let (board : GridNavigation.Board) = Array2D.create 3 3 true
    let (start : GridNavigation.Position) = { x = 0; y = 0 }
    let (finish : GridNavigation.Position) = { x = 2; y = 2 }
    let (problem : GridNavigation.Problem) = { start = start; finish = finish; board = board }

    0
