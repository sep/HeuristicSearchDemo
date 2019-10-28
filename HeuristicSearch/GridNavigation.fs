
module GridNavigation

type Action =
| North
| South
| East
| West
| Noop

let string_of_action = function
| North -> "North"
| South -> "South"
| East -> "East"
| West -> "West"
| Noop -> ""

type Position = {
    x : int
    y: int
}

type Board = bool [,]

type Problem = {
    start : Position
    finish : Position
    board : Board
}

type CellType = 
    | InitialState
    | GoalState
    | Free
    | Blocked

type State = {
    position : Position;
    generated_by : Action;
}

type Solution = State list

let in_bounds (board : Board) (position : Position) =
    position.x >= 0 && 
    position.y >= 0 && 
    position.y < (board.GetLength 0) && 
    position.x < (board.GetLength 1)

let legal_position (board : Board) (position : Position) =
    in_bounds board position &&
    board.[position.y, position.x]

let random_board (src_rand : System.Random Option) (width : int) (height : int) (p_blocked : float) =
    if p_blocked < 0. || p_blocked > 1. then failwith "p_blocked should be between 0 and 1";
    let rand = match src_rand with | None -> System.Random () | Some src -> src
    let block _ _ = (float (rand.Next(100))) / 100.0 > p_blocked
    Array2D.init height width block

let random_position (src_rand : System.Random Option) (width : int) (height : int) =
    let rand = match src_rand with | None -> System.Random () | Some src -> src
    { x = rand.Next() % width; y = rand.Next() % height }

let random_problem (src_rand : System.Random Option) (width : int) (height : int) (p_blocked : float) =
    let board = random_board src_rand width height p_blocked
    let rec find_legal_position () =
        let position = random_position src_rand width height
        if legal_position board position then position else find_legal_position() in
    { board = board; start = find_legal_position(); finish = find_legal_position ()}

let string_of_position (position : Position) =
    Printf.sprintf "(%i, %i)" position.x position.y


let string_of_state (state : State) =
    match state.generated_by with
    | Noop -> Printf.sprintf "Start in %s" (string_of_position state.position)
    | action -> Printf.sprintf "%s by way of %s" (string_of_position state.position) (string_of_action action)

let string_of_solution (solution : Solution) =
    let rec walk accum = function
    | ([] : Solution) -> accum
    | hd :: tl -> walk (Printf.sprintf "%s\n%s" accum (string_of_state hd)) tl in
    walk (List.length solution |> Printf.sprintf "Solution has %i steps") solution

let print_solution (solution : Solution) = string_of_solution solution |> printf "%s"

let opposite_action = function
| North -> South
| South -> North
| East -> West
| West -> East
| Noop -> Noop

let are_opposite (act1 : Action) (act2 : Action) =
    act1 = opposite_action act2

let cell_type (problem : Problem) (position : Position) =
    if not (in_bounds problem.board position) then begin
        (string_of_position position |> Printf.sprintf "Illegal position %s\n") |> failwith 
    end else
    if problem.start = position then InitialState
    else if problem.finish = position then GoalState
    else if problem.board.[position.y, position.x] then Free
    else Blocked

let char_of_cell = function
| InitialState -> 'S'
| GoalState -> 'G'
| Free -> ' '
| Blocked -> '#'

let cell_of_char = function
| 'S'
| 's' -> InitialState
| 'G'
| 'g' -> GoalState
| ' ' -> Free
| '#' -> Blocked
| c -> failwith (Printf.sprintf "Saw unexpected character %c while interpreting board from string" c)

let problem_to_string (problem : Problem) =
    let spf = Printf.sprintf
    let height = problem.board.GetLength 0
    let width = problem.board.GetLength 1
    let dimm_string = spf "%i, %i" width height
    let board_string = ref dimm_string in
        for y in 0 .. (height - 1) do
            let row_string = ref "" in
                for x in 0 .. (width - 1) do
                    let cell_char = cell_type problem { x = x; y = y } |> char_of_cell in
                        row_string := spf "%s%c" !row_string cell_char
                done;
                board_string := spf "%s\n%s" !board_string !row_string 
        done;
        !board_string

let goal_test (problem : Problem) (state : State) =
    problem.finish = state.position

let move (position : Position) = function
| North -> { position with y = position.y - 1 }
| South -> { position with y = position.y + 1 }
| East ->  { position with x = position.x + 1 }
| West ->  { position with x = position.x - 1 }
| Noop -> position

let move_state (state : State) action =
    { position = move state.position action; generated_by = action }

let make_problem (board : Board) (initial : Position) (goal : Position) =
    if legal_position board initial |> not then 
        failwith "Starting state not a legal board position"
    if legal_position board goal |> not then
        failwith "Goal state not a legal board position"
    { start = initial; finish = goal; board = board }
  
let expand (board : Board) (state : State) = 
    let possible_actions = [North; South; East; West]
    let validate_position = legal_position board
    let consider_action (accum : (State * float) list) (action : Action) = 
        if are_opposite action state.generated_by then accum else
        let state' = move_state state action
        if validate_position state'.position then
                (state', 1.) :: accum
            else
                accum
    List.fold consider_action [] possible_actions

let key (board : Board) (state : State) =
    let width = board.GetLength 1
    state.position.x + state.position.y * width 

let make_initial_state (problem : Problem) = { 
    position = problem.start; 
    generated_by = Noop 
}

let validate_solution (problem : Problem) (solution : Solution) =
    let rec walk_solution (current_state : State) = function
        | ([] : Solution) -> failwith "Empty Solution is not valid"
        | [ singleton ] -> problem.finish = singleton.position && current_state.position = singleton.position
        | hd::next::tl -> 
            if current_state <> hd then false else
            walk_solution (move_state current_state next.generated_by) (next::tl)
        in
    walk_solution (make_initial_state problem) solution

let manhattan_distance (problem : Problem) (state : State) =
    abs(problem.finish.x - state.position.x) + abs(problem.finish.y - state.position.y)