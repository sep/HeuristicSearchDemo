module HeuristicSearchTests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let MakeProblem_fails_on_blocked_start () =
    let (board : GridNavigation.Board) = Array2D.create 3 3 true
    let (start : GridNavigation.Position) = { x = 1; y = 1 }
    let (goal : GridNavigation.Position) = { x = 0; y = 0 }
    board.[1,1] <- false
    try
        ignore(GridNavigation.make_problem board start goal)
        Assert.Fail ()
    with
    | _ -> ()

[<Test>]
let MakeProblem_fails_on_blocked_goal () =
    let (board : GridNavigation.Board) = Array2D.create 3 3 true
    let (goal : GridNavigation.Position) = { x = 1; y = 1 }
    let (start : GridNavigation.Position) = { x = 0; y = 0 }
    board.[1,1] <- false
    try
        ignore(GridNavigation.make_problem board start goal)
        Assert.Fail ()
    with
    | _ -> ()

[<Test>]
let EndToEnd () =
    let (board : GridNavigation.Board) = Array2D.create 3 3 true
    board.[1,1] <- false
    let (start : GridNavigation.Position) = { x = 0; y = 0 }
    let (finish : GridNavigation.Position) = { x = 2; y = 2 }
    let (problem : GridNavigation.Problem) = { start = start; finish = finish; board = board }
    let (root : GridNavigation.State) = GridNavigation.make_initial_state problem
    let expand = GridNavigation.expand problem.board
    let key = GridNavigation.key problem.board
    let goal = GridNavigation.goal_test problem in
    let metrics = UniformCostSearch.uniform_cost_search expand goal key root in
    let generate_solution (node : (GridNavigation.State UniformCostSearch.SearchNode) UniformCostSearch.SolutionNode) = UniformCostSearch.generate_solution_of_sol_node node
    let print_sol_node (node : (GridNavigation.State UniformCostSearch.SearchNode) UniformCostSearch.SolutionNode) = generate_solution node |> GridNavigation.print_solution in
        GridNavigation.problem_to_string problem |> printf "%s\n";
        List.iter print_sol_node metrics.solution_nodes