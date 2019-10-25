module GridNavigation

type Action =
| North
| South
| East
| West
| Noop

type Position = {
    x : int
    y: int
}

type State = {
    position : Position;
    generated_by : Action;
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

let legal_position (board : Board) (position : Position) =
    position.x > 0 && 
    position.y > 0 && 
    position.x < (board.GetLength 0) && 
    position.y < (board.GetLength 1) &&
    board.[position.y, position.x]

let cell_type (problem : Problem) (position : Position) =
    assert legal_position problem.board position
    if problem.start = position then InitialState
    else if problem.finish = position then GoalState
    else if problem.board.[position.y, position.x] then Free
    else Blocked

let char_of_cell = function
| InitialState -> 'S'
| GoalState -> 'G'
| Free -> ' '
| Blocked -> '#'

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

let cell_of_char = function
| 'S'
| 's' -> InitialState
| 'G'
| 'g' -> GoalState
| ' ' -> Free
| '#' -> Blocked
| c -> failwith (Printf.sprintf "Saw unexpected character %c while interpreting board from string" c)

let goal_test (problem : Problem) (position : Position) =
    problem.finish = position

let move (position : Position) = function
| North -> { x = position.x; y = position.y - 1 }
| South -> { x = position.x; y = position.y + 1 }
| East -> { x = position.x + 1; y = position.y }
| West -> { x = position.x - 1; y = position.y }
| Noop -> position


let make_problem (board : Board) (initial : Position) (goal : Position) =
    if legal_position board initial |> not then 
        failwith "Starting state not a legal board position"
    if legal_position board goal |> not then
        failwith "Goal state not a legal board position"
    { start = initial; finish = goal; board = board }
  
let expand (board : Board) (position : Position) = 
    let possible_actions = [North; South; East; West]
    let validate_position = legal_position board
    let consider_action (accum : (Position * float) list) (action : Action) = 
        let position' = move position action
        if validate_position position' then
                (position', 1.) :: accum
            else
                accum

    List.fold consider_action [] possible_actions

// todo: perfect hash by linearizing based on board size
let key (position : Position) =
    position.x, position.y

//(initial_state : 'state)
let make_initial_state (problem : Problem) = { 
    position = problem.start; 
    generated_by = Noop 
}

