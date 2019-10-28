module GreedyBestFirstSearchTests

open NUnit.Framework
module GN = GridNavigation
module SI = SearchInterface
module GBFS = GreedyBestFirstSearch


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
    let heuristic state = GN.manhattan_distance problem state |> float
    let (root : GN.State) = GN.make_initial_state problem
    let expand = GN.expand problem.board
    let key = GN.key problem.board
    let goal = GN.goal_test problem in
    let metrics = GBFS.greedy_best_first_search expand goal key heuristic root in
    let generate_solution (node : (GN.State GBFS.SearchNode) SI.SolutionNode) = GBFS.generate_solution_of_sol_node node
    let validate_sol_node (node : (GN.State GBFS.SearchNode) SI.SolutionNode) = generate_solution node |> (GN.validate_solution problem) in
        GN.problem_to_string problem |> printf "%s\n";
        Assert.AreEqual(1, metrics.solution_nodes.Length);
        List.iter GN.print_solution (List.map generate_solution metrics.solution_nodes);
        List.fold (fun accum e -> accum && validate_sol_node e) true metrics.solution_nodes |> Assert.True
