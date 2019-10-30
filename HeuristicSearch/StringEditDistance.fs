﻿module StringEditDistance

// No punctuation, ignore capitalization
type Character =
|Whitespace
|A
|B
|C
|D
|E
|F 
|G 
|H 
|I 
|J 
|K 
|L 
|M 
|N 
|O 
|P 
|Q 
|R 
|S 
|T 
|U 
|V 
|W 
|X 
|Y 
|Z

type Element = 
| Alphabetical of Character
| Null

type Update = {
    index: int
    character: Character
}

type Action = 
| Remove of int
| Add of Update
| Replace of Update
| ShiftLeft
| ShiftRight
| Noop

type State = {
    state: Element array
    generated_by : Action
}

type Problem = {
    start: Element array
    finish: Element array
}

type Solution = State list

let ADD_COST = 1.5
let DEL_COST = 1.0
let NO_COST = 0.
let SHIFT_COST = 0.

let pad_elements (ar : Element array) desired_length =
    Array.init desired_length (fun i -> if i < desired_length then ar.[i] else Null)

let normalize_instance (raw_instance : Problem) =
    let len_start = raw_instance.start.Length
    let len_finish = raw_instance.finish.Length
    if len_start > len_finish then
        { raw_instance with finish = pad_elements raw_instance.finish len_start }
    else if len_finish > len_start then
        { raw_instance with start = pad_elements raw_instance.finish len_finish }
    else raw_instance

let alphabetical_number = function
|Whitespace -> 0
|A -> 1
|B -> 2
|C -> 3
|D -> 4
|E -> 5
|F -> 6
|G -> 7
|H -> 8
|I -> 9
|J -> 10
|K -> 11
|L -> 12
|M -> 13
|N -> 14
|O -> 15
|P -> 16
|Q -> 17
|R -> 18
|S -> 19
|T -> 20
|U -> 21
|V -> 22
|W -> 23
|X -> 24
|Y -> 25
|Z -> 26

let alphabetical_string = function
|Whitespace -> " "
|A -> "A"
|B -> "B"
|C -> "C"
|D -> "D"
|E -> "E"
|F -> "F"
|G -> "G"
|H -> "H"
|I -> "I"
|J -> "J"
|K -> "K"
|L -> "L"
|M -> "M"
|N -> "N"
|O -> "O"
|P -> "P"
|Q -> "Q"
|R -> "R"
|S -> "S"
|T -> "T"
|U -> "U"
|V -> "V"
|W -> "W"
|X -> "X"
|Y -> "Y"
|Z -> "Z"

let WHITESPACE = " "
let NULL_STRING = "_"

let string_of_element = function
| Null -> "_"
| Alphabetical char -> alphabetical_string char

let string_of_element_array (elements : Element array) =
    Array.fold (fun accum element -> sprintf "%s%s" accum (string_of_element element)) "" elements

let string_of_action = function
| Noop -> "Noop"
| Add update -> sprintf "Add %s @ %i" (alphabetical_string update.character) update.index
| Remove index -> sprintf "Remove @ %i" index
| ShiftLeft -> "Shift Left"
| ShiftRight -> "Shift Right"
| Replace update -> sprintf "Replacing character @%i with %s" update.index (alphabetical_string update.character)

let string_of_state (state : State) =
    match state.generated_by with
    | Noop -> string_of_element_array state.state
    | _ -> sprintf "%s generated by way of %s" (string_of_element_array state.state) (string_of_action state.generated_by)

let element_of_char = function
    | '_' -> Null
    | ' '  -> Alphabetical Whitespace
    | 'A' -> Alphabetical A  
    | 'B' -> Alphabetical B 
    | 'C' -> Alphabetical C 
    | 'D' -> Alphabetical D 
    | 'E' -> Alphabetical E 
    | 'F' -> Alphabetical F 
    | 'G' -> Alphabetical G 
    | 'H' -> Alphabetical H 
    | 'I' -> Alphabetical I 
    | 'J' -> Alphabetical J 
    | 'K' -> Alphabetical K 
    | 'L' -> Alphabetical L 
    | 'M' -> Alphabetical M 
    | 'N' -> Alphabetical N 
    | 'O' -> Alphabetical O 
    | 'P' -> Alphabetical P 
    | 'Q' -> Alphabetical Q 
    | 'R' -> Alphabetical R 
    | 'S' -> Alphabetical S 
    | 'T' -> Alphabetical T 
    | 'U' -> Alphabetical U 
    | 'V' -> Alphabetical V 
    | 'W' -> Alphabetical W 
    | 'X' -> Alphabetical X 
    | 'Y' -> Alphabetical Y 
    | 'Z' -> Alphabetical Z 
    | character -> character |> sprintf "%c isn't a valid Element" |> failwith 

let element_array_of_string (input : string) =
    let ar = Array.create (String.length input) Null
    String.iteri (fun index character -> ar.[index] <- element_of_char character) input;
    ar

let alphabetical_distance (alpha1 : Character) (alpha2 : Character) =
    alphabetical_number alpha1 - alphabetical_number alpha2 |> abs |> float

let distance_between (element1 : Element) (element2 : Element) =
    match element1, element2 with
    | Alphabetical c1, Alphabetical c2 -> alphabetical_distance c1 c2
    | Alphabetical c1, Null -> DEL_COST + (float (alphabetical_number c1 ))
    | Null, Alphabetical c1 -> ADD_COST + (float (alphabetical_number c1))
    | Null, Null -> NO_COST

let cost (state : State) (action : Action) =
    match action with
    | Noop -> 0.0
    | Remove _ -> 1.0
    | Add _ -> 1.5
    | Replace update -> distance_between state.state.[update.index] (Alphabetical update.character)
    | ShiftLeft
    | ShiftRight -> SHIFT_COST

let valid_sequence (action1 : Action) (action2 : Action) = 
    match action1, action2 with
    | Add update, Remove index 
    | Remove index, Add update -> update.index = index
    | ShiftLeft, ShiftRight
    | ShiftRight, ShiftLeft -> false
    | _ -> true

let shift_left_at_index (array : Element array) (index : int) =
        for i in index..(array.Length-2) do 
            array.[i] <- array.[i+1]
        done
        array.[array.Length - 1] <- Null

let shift_right_at_index (array : Element array) (index : int) =
    for i in List.rev [index..(array.Length - 2) ] do //TODO: Clean this up
        array.[i + 1] <- array.[i] 
    done
    array.[index] <- Null 

let can_shift (state : State ) = function
    | ShiftLeft -> 
        begin
            state.state.[0] = Null && 
            state.state.Length > 1 && 
            match state.generated_by with 
            | Noop 
            | ShiftLeft -> true 
            | _ -> false
        end
    | ShiftRight -> 
    begin
        state.state.[state.state.Length - 1] = Null && 
        state.state.Length > 1 && 
        match state.generated_by with 
        | Noop 
        | ShiftRight -> true 
        | _ -> false
    end
    | _ -> failwith "can_shift called on an action that isn't a shift"

let valid_move (state : State) (planned_action : Action) =
    valid_sequence state.generated_by planned_action &&
        match planned_action with
        | Noop -> false
        | Remove index -> match state.state.[index] with Null -> false | _ -> true
        | Add _ -> state.state.[state.state.Length - 1] = Null
        | ShiftRight
        | ShiftLeft -> can_shift state planned_action &&
                        match state.generated_by with
                        | Noop
                        | ShiftLeft
                        | ShiftRight -> true
                        | Remove _
                        | Add _
                        | Replace _ -> false
        | Replace _ -> true

let apply_nondestructive (state : State) (action : Action) =
    if not (valid_move state action) then failwith (sprintf "Invalid action %s" (string_of_action action))

    let next_array = Array.copy state.state in
    begin
        match action with
        | Noop -> failwith "Applying Noop"
        | Remove index -> shift_left_at_index next_array index
        | Add update -> shift_right_at_index next_array update.index; next_array.[update.index] <- Alphabetical update.character
        | ShiftRight -> shift_right_at_index next_array 0
        | ShiftLeft -> shift_left_at_index next_array 0
        | Replace update -> next_array.[update.index] <- Alphabetical update.character
    end
    { state = next_array; generated_by = action }

let expand (state : State) = 
    let possible_indices = [0..state.state.Length - 1]
    let possible_characters = 
        [ Whitespace; A; B; C; D; E; F; G; H; I; J; K; L; M; N; O; P; Q; R; S; T; U; V; W; X; Y; Z ]
    let possible_updates = 
        List.fold (fun accum index -> 
            List.fold (fun accum2 character -> { index = index; character = character } :: accum2) accum possible_characters) [] possible_indices
    
    let possible_shifts = [ShiftLeft; ShiftRight]
    let possible_removes = List.map Remove possible_indices
    let possible_adds = List.map Add possible_updates
    let possible_replaces = List.map Replace possible_updates

    let generate_successor (accum : (State*float) list) (action : Action) =
        if valid_move state action then
            (apply_nondestructive state action, cost state action) :: accum
        else
            accum
    let possible_actions = possible_shifts @ possible_removes @ possible_adds @ possible_replaces
    List.fold generate_successor [] possible_actions

let goal_test (instance : Problem) (state : State) = 
    state.state = instance.finish

let key (state : State) = 
    string_of_element_array state.state

let initial_state (problem : Problem) = 
    {
        state = problem.start
        generated_by = Noop
    }