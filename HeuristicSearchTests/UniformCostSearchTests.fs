module HeuristicSearchTests

open NUnit.Framework
open Microsoft.FSharp.Reflection
module GN = GridNavigation
module UCS = UniformCostSearch


[<SetUp>]
let Setup () =
    ()

[<Test>]
let EndToEnd () =
    let (board : GN.Board) = Array2D.create 3 3 true
    board.[1,1] <- false
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 2; y = 2 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    let (root : GN.State) = GN.make_initial_state problem
    let expand = GN.expand problem.board
    let key = GN.key problem.board
    let goal = GN.goal_test problem in
    let metrics = UCS.uniform_cost_search expand goal key root in
    let generate_solution (node : (GN.State UCS.SearchNode) UCS.SolutionNode) = UCS.generate_solution_of_sol_node node
    let validate_sol_node (node : (GN.State UCS.SearchNode) UCS.SolutionNode) = generate_solution node |> (GN.validate_solution problem) in
        GN.problem_to_string problem |> printf "%s\n";
        Assert.AreEqual(1, metrics.solution_nodes.Length);
        List.fold (fun accum e -> accum && validate_sol_node e) true metrics.solution_nodes |> Assert.True