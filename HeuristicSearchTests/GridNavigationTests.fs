module DomainTests

open NUnit.Framework
open Microsoft.FSharp.Reflection
module GN = GridNavigation
module UCS = UniformCostSearch

[<SetUp>]
let Setup () =
    ()

[<Test>]
let MakeProblem_fails_on_blocked_start () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (start : GN.Position) = { x = 1; y = 1 }
    let (goal : GN.Position) = { x = 0; y = 0 }
    board.[1,1] <- false
    try
        ignore(GN.make_problem board start goal)
        Assert.Fail ()
    with
    | _ -> ()

[<Test>]
let MakeProblem_fails_on_blocked_goal () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (goal : GN.Position) = { x = 1; y = 1 }
    let (start : GN.Position) = { x = 0; y = 0 }
    board.[1,1] <- false
    try
        ignore(GN.make_problem board start goal)
        Assert.Fail ()
    with
    | _ -> ()

[<Test>]
let Opposites_are_opposite () =
    let test_element (one : GN.Action) =
        Assert.True (GN.are_opposite one (GN.opposite_action one))
    let cases = [ for c in (FSharpType.GetUnionCases (typeof<GN.Action>)) do
                    let case = FSharpValue.MakeUnion (c, [||]) :?> GN.Action in
                    yield case ]
    List.iter test_element cases

[<Test>]
let empty_solution_valid_for_solved_problem () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 0; y = 0 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    let solution = [ { GN.position = start; GN.generated_by = GN.Noop}]
    Assert.True (GN.validate_solution problem solution)

[<Test>]
let empty_solution_invalid_for_unsolved_problem () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 2; y = 0 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    let solution = [ { GN.position = start; GN.generated_by = GN.Noop}]
    Assert.False (GN.validate_solution problem solution)

[<Test>]
let ValidSolutionIsValid () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 2; y = 0 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    let solution = [ 
        { GN.position = start; GN.generated_by = GN.Noop };
        { GN.position = { x = 1; y = 0 }; GN.generated_by = GN.East };
        { GN.position = { x = 2; y = 0 }; GN.generated_by = GN.East };
    ]
    Assert.True (GN.validate_solution problem solution)

[<Test>]
let EmptySolutionFails () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 2; y = 0 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    try
        ignore(GN.validate_solution problem []);
        Assert.Fail()
    with msg ->
        Assert.True ("Empty Solution is not valid" = msg.Message)

[<Test>]
let PartialSolutionFalse () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 2; y = 0 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    let solution = [ 
        { GN.position = start; GN.generated_by = GN.Noop };
        { GN.position = { x = 1; y = 0 }; GN.generated_by = GN.East };
    ]
    Assert.False (GN.validate_solution problem solution)

[<Test>]
let InvalidSolutionFalse () =
    let (board : GN.Board) = Array2D.create 3 3 true
    let (start : GN.Position) = { x = 0; y = 0 }
    let (finish : GN.Position) = { x = 2; y = 0 }
    let (problem : GN.Problem) = { start = start; finish = finish; board = board }
    let solution = [ 
        { GN.position = start; GN.generated_by = GN.Noop };
        { GN.position = { x = 2; y = 0 }; GN.generated_by = GN.West };
    ]
    Assert.False (GN.validate_solution problem solution)


[<Test>]
let RandomProblemsAreValid() =
    let test_one() =
        let problem = GN.random_problem None 1000 1000 0.25
        GN.legal_position problem.board problem.start |> Assert.True
        GN.legal_position problem.board problem.finish |> Assert.True
    for i in 0..10 do
        test_one()
    done

[<Test>]
let RandomRespectsSeed() =
    let seed_rand = System.Random()
    let test seed =
        let rand = System.Random seed
        GN.random_problem (Some rand) 1000 1000 0.25
    let seed_gen = System.Random()
    for i in 0..10 do
        let seed = seed_rand.Next()
        let p1 = test seed
        let p2 = test seed
        Assert.AreEqual(p1,p2)