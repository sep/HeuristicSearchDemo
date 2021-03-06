﻿module UniformCostSearch

open SearchInterface
open System

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

let uniform_cost_search (iface : DomainInterface.DuplicateDomainInterface<float, 'state, 'key>) =
    let mutable openlist : FSharpx.Collections.IPriorityQueue<'state SearchNode> = FSharpx.Collections.PriorityQueue.empty false
    let mutable closedlist : FSharpx.Collections.PersistentHashMap<'hash_value, 'state SearchNode> = FSharpx.Collections.PersistentHashMap.empty
    let root = make_root iface.InitialState
    let metrics = initial_metrics ()
    let node_key = wrap_state_fn iface.Key
    let node_expand = wrap_state_fn iface.Expand
    let node_goal_test = wrap_state_fn iface.GoalP
    let enqueue (node : 'state SearchNode) = openlist <- FSharpx.Collections.PriorityQueue.insert node openlist
    let consider_child current_node (state, step_cost) = 
        metrics.nodes_generated <- metrics.nodes_generated + 1
        let child_node = { parent = Some current_node; state = state; cost = current_node.cost + step_cost }
        match metrics.solution_nodes with
            | [] -> enqueue child_node
            | hd::_ -> if better child_node hd.solution then enqueue child_node
    let pop () =
        match FSharpx.Collections.PriorityQueue.tryPop openlist with
        | Some (ret, openlist') -> openlist <- openlist'; Some ret
        | _ -> None
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
                    if closedlist.ContainsKey key_val then
                        metrics.duplicates <- metrics.duplicates + 1
                    else begin
                        closedlist <- closedlist.Add(key_val, current_node)
                        let child_tuples = node_expand current_node
                        metrics.nodes_expanded <- metrics.nodes_expanded + 1
                        List.iter (consider_child current_node) child_tuples
                    end
                end
    { metrics with stop_time = Some DateTime.Now }
    