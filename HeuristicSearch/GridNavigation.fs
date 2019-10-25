module GridNavigation

type Action =
| North
| South
| East
| West

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

let goal_test (problem : Problem) (position : Position) =
    problem.finish = position

let move (position : Position) = function
| North -> { x = position.x; y = position.y - 1 }
| South -> { x = position.x; y = position.y + 1 }
| East -> { x = position.x + 1; y = position.y }
| West -> { x = position.x - 1; y = position.y }
 
let legal_position (board : Board) (position : Position) =
    position.x > 0 && 
    position.y > 0 && 
    position.x < (board.GetLength 0) && 
    position.y < (board.GetLength 1) &&
    board.[position.x, position.y]

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


