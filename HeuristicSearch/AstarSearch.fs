module Astar

open SearchInterface
open System

[<CustomEquality; CustomComparison>]
type SearchNode<'T when 'T: equality> =
    {
        parent : SearchNode<'T> Option
        state : 'T
        cost : float
        f : float
    }
    override x.Equals(yobj) =
        match yobj with
        | :? SearchNode<'T> as y -> x.state = y.state
        | _ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? SearchNode<'T> as y ->
                let heuristic_compare = compare x.f y.f
                if heuristic_compare = 0 then compare x.cost y.cost else heuristic_compare
            | _ -> invalidArg "yobj" "Cannot compare values of different types"

let better node1 node2 =
    node1.cost < node2.cost

let wrap_state_fn fn node =
    fn node.state

let make_root heuristic initial_state =
    let root = {
        parent = None
        state = initial_state
        cost = 0.
        f = heuristic initial_state
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

let astar_search (expand : 'state -> ('state * float) list) (goal_test : 'state -> bool) (key : 'state -> 'hash_value) (heuristic : 'state -> float) (initial_state : 'state) =
    let mutable openlist : FSharpx.Collections.IPriorityQueue<'state SearchNode> = FSharpx.Collections.PriorityQueue.empty false
    let mutable closedlist : FSharpx.Collections.PersistentHashMap<'hash_value, 'state SearchNode> = FSharpx.Collections.PersistentHashMap.empty
    let root = make_root heuristic initial_state
    let metrics = initial_metrics ()
    let node_key = wrap_state_fn key
    let node_expand = wrap_state_fn expand
    let node_goal_test = wrap_state_fn goal_test
    let enqueue (node : 'state SearchNode) = openlist <- FSharpx.Collections.PriorityQueue.insert node openlist
    let consider_child current_node (state, step_cost) =
        let child_node = { parent = Some current_node; state = state; cost = current_node.cost + step_cost; f = current_node.cost + (heuristic state) }
        match metrics.solution_nodes with
            | [] -> enqueue child_node
            | hd::_ -> if better child_node hd.solution then enqueue child_node
    let pop () =
        match FSharpx.Collections.PriorityQueue.tryPop openlist with
        | Some (ret, openlist') -> openlist <- openlist'; Some ret
        | _ -> None
    let consider_node key_val current_node =
        if closedlist.ContainsKey key_val then begin
            metrics.duplicates <- metrics.duplicates + 1
            if better current_node closedlist.[key_val] then begin
                closedlist <- closedlist.Remove(key_val)
                true
            end else false
        end else true
    enqueue root
    let mutable finished = false
    while not finished do
        match pop() with
        | None -> finished <- true
        | Some current_node ->
            if node_goal_test current_node then begin
                metrics.solution_nodes <- { solution = current_node; found_at_time = DateTime.Now } :: metrics.solution_nodes;
                finished <- true end else
                begin
                    let key_val = node_key current_node
                    let should_expand = consider_node key_val current_node
                    if should_expand then begin
                        closedlist <- closedlist.Add(key_val, current_node)
                        let child_tuples = node_expand current_node
                        List.iter (consider_child current_node) child_tuples
                    end
                end
    { metrics with stop_time = Some DateTime.Now }