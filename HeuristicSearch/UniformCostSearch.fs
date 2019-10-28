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

[<CustomEquality; CustomComparison>]
type SearchNode<'T when 'T: equality> = 
    {
        parent : SearchNode<'T> Option
        state : 'T
        cost : float
    }
    override x.Equals(yobj) =
        match yobj with
        | :? SearchNode<'T> as y -> x.state = y.state
        | _ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? SearchNode<'T> as y -> compare x.cost y.cost
            | _ -> invalidArg "yobj" "Cannot compare values of different types"
        

let better node1 node2 =
    node1.cost < node2.cost

let wrap_state_fn fn node =
    fn node.state

let initial_metrics () = {
    nodes_generated = 0
    nodes_expanded = 0
    duplicates = 0
    solution_nodes = []
}

let make_root initial_state =
    let root = {
        parent = None
        state = initial_state
        cost = 0.
    }
    root

let generate_solution (goal_node : 'a SearchNode) =
    let rec walk node =
        node.state :: begin
            match node.parent with
            | None -> []
            | Some parent -> walk parent
            end in
        walk goal_node |> List.rev

let generate_solution_of_sol_node (sol_node : ('a SearchNode) SolutionNode) =
    generate_solution sol_node.solution

let uniform_cost_search (expand : 'state -> ('state * float) list) (goal_test : 'state -> bool) (key : 'state -> 'hash_value) (initial_state : 'state) =
    let (openlist : Ref<FSharpx.Collections.IPriorityQueue<'state SearchNode>>) = ref (FSharpx.Collections.PriorityQueue.empty false)
    let closedlist = ref Map.empty
    let root = make_root initial_state
    let metrics = initial_metrics ()
    let node_key = wrap_state_fn key
    let node_expand = wrap_state_fn expand
    let node_goal_test = wrap_state_fn goal_test
    let enqueue (node : 'state SearchNode) = openlist := FSharpx.Collections.PriorityQueue.insert node !openlist
    let consider_child current_node (state, step_cost) = 
        let child_node = { parent = Some current_node; state = state; cost = current_node.cost + step_cost }
        match metrics.solution_nodes with
            | [] -> enqueue child_node
            | hd::_ -> if better child_node hd.solution then enqueue child_node
    let pop () =
        match FSharpx.Collections.PriorityQueue.tryPop !openlist with
        | Some (ret, openlist') -> openlist := openlist'; Some ret
        | _ -> None
    enqueue root
    let finished = ref false
    while not !finished do
        match pop() with
        | None -> finished := true
        | Some current_node -> 
            if node_goal_test current_node then begin
                metrics.solution_nodes <- { solution = current_node; found_at_time = DateTime.Now } :: metrics.solution_nodes;
                finished := true end else
                begin
                    let key_val = node_key current_node
                    if Map.containsKey key_val !closedlist then
                        metrics.duplicates <- metrics.duplicates + 1
                    else begin
                        closedlist := Map.add key_val current_node.cost !closedlist
                        let child_tuples = node_expand current_node
                        List.iter (consider_child current_node) child_tuples
                    end
                end
    metrics
        