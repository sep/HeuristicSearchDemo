module AstarSearchTests

open NUnit.Framework
module GN = GridNavigation
module SED = StringEditDistance
module SI = SearchInterface

[<SetUp>]
let Setup () =
    ()

[<Test>]
let EndToEnd_GridNavigation () =
    let (board : GN.Board) = Array2D.create 3 3 true
    board.[1,1] <- false
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 2; y = 2 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    let (root : GN.State) = GN.make_initial_state problem
    let expand = GN.expand problem.board
    let key = GN.key problem.board
    let goal = GN.goal_test problem in
    let heuristic state = GN.manhattan_distance problem state |> float
    let metrics = Astar.astar_search expand goal key heuristic root in
    let generate_solution (node : (GN.State Astar.SearchNode) SI.SolutionNode) = Astar.generate_solution_of_sol_node node
    let validate_sol_node (node : (GN.State Astar.SearchNode) SI.SolutionNode) = generate_solution node |> (GN.validate_solution problem) in
        GN.problem_to_string problem |> printf "%s\n";
        Assert.AreEqual(1, metrics.solution_nodes.Length);
        List.fold (fun accum e -> accum && validate_sol_node e) true metrics.solution_nodes |> Assert.True

[<Test>]
let Benchmark_GridNavigation () =
    let problem = GN.random_problem None 2000 2000 0.25 in
    let (root : GN.State) = GN.make_initial_state problem
    let expand = GN.expand problem.board
    let key = GN.key problem.board
    let goal = GN.goal_test problem in
    let heuristic state = GN.manhattan_distance problem state |> float
    let metrics = Astar.astar_search expand goal key heuristic root in
    let generate_solution (node : (GN.State Astar.SearchNode) SI.SolutionNode) = Astar.generate_solution_of_sol_node node
    let validate_sol_node (node : (GN.State Astar.SearchNode) SI.SolutionNode) = generate_solution node |> (GN.validate_solution problem) in
        Assert.AreEqual(1, metrics.solution_nodes.Length);
        List.fold (fun accum e -> accum && validate_sol_node e) true metrics.solution_nodes |> Assert.True;
        SearchInterface.common_display metrics

[<Test>]
let EndToEnd_StringEditing () =
    let (problem : SED.Problem) = SED.instance_of_strings "HERE" "THERE"
    let (root : SED.State) = SED.make_initial_state problem
    let expand = SED.expand problem
    let key = SED.key
    let goal = SED.goal_test problem in
    let heuristic = SED.character_delta problem
    let metrics = Astar.astar_search expand goal key heuristic root in
    let generate_solution (node : (SED.State Astar.SearchNode) SI.SolutionNode) = Astar.generate_solution_of_sol_node node
    let validate_sol_node (node : (SED.State Astar.SearchNode) SI.SolutionNode) = generate_solution node |> (SED.validate_solution problem) in
        Assert.AreEqual(1, metrics.solution_nodes.Length);
        List.iter SED.print_solution (List.map generate_solution metrics.solution_nodes);
        List.fold (fun accum e -> accum && validate_sol_node e) true metrics.solution_nodes |> Assert.True;
        SearchInterface.common_display metrics