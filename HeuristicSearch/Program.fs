module UniformCostSearch

open System

type 'a SolutionNode = {
    solution : 'a
    found_at_time : DateTime
}

type 'a SearchMetrics = {
    mutable nodes_generated : int
    mutable nodes_expanded : int
    mutable duplicates : int
    mutable solution_nodes : 'a SolutionNode list
}

type 'a SearchNode = {
    state : 'a
    cost : float
}

let wrap_state_fn fn node =
    fn node.state

let initial_metrics () = {
    nodes_generated = 0
    nodes_expanded = 0
    duplicates = 0
    solution_nodes = []
}


let uniform_cost_search (expand : 'state -> ('state * float) list) (goal_test : 'state -> bool) (key : 'state -> 'hash_value) (initial_state : 'state) =
    let openlist = ref []
    let closedlist = ref Map.empty
    let root = { state = initial_state; cost = 0. }
    let metrics = initial_metrics ()
    let node_key = wrap_state_fn key
    let node_expand = wrap_state_fn expand
    let node_goal_test = wrap_state_fn goal_test
    let enqueue (node : 'state SearchNode) = openlist := node :: !openlist
    let consider_child current_node (state, step_cost) = 
        let child_node = { state = state; cost = current_node.cost + step_cost }
        enqueue child_node
    let pop () =
        match !openlist with
        | [] -> None
        | hd::tl -> 
            begin
                openlist := tl;
                Some hd
            end
    let rec search = function
        | None -> ()
        | Some current_node ->
            if node_goal_test current_node then
                metrics.solution_nodes <- { solution = current_node; found_at_time = DateTime.Now } :: metrics.solution_nodes else
                begin
                    let key_val = node_key current_node
                    if Map.containsKey key_val !closedlist then
                        metrics.duplicates <- metrics.duplicates + 1
                    else begin
                        closedlist := Map.add key_val true !closedlist
                        let child_tuples = node_expand current_node
                        List.iter (consider_child current_node) child_tuples
                    end
                end
            search (pop())
    enqueue root
    search (pop())
    metrics
        

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
