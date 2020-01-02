module SearchInterface

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
    start_time : DateTime
    stop_time : DateTime option
}

let initial_metrics () = {
    start_time = DateTime.Now
    nodes_generated = 0
    nodes_expanded = 0
    duplicates = 0
    solution_nodes = []
    stop_time = None
}