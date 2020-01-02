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

let common_display metrics = 
    let total_seconds = match metrics.stop_time with
    | None -> (DateTime.Now - metrics.start_time).TotalSeconds
    | Some end_time -> (end_time - metrics.start_time).TotalSeconds in
    printfn "{"
    printfn "\"total time\" : %f ," total_seconds;
    printfn "\"total nodes generated\" : %i ," metrics.nodes_generated;
    printfn "\"total nodes expanded\" : %i ," metrics.nodes_expanded;
    printfn "\"duplicates detected\" : %i ," metrics.duplicates
    printfn "\"solutions found\" : %i ," metrics.solution_nodes.Length;
    printfn "\"nodes generated per second\" : %f ," ((float (metrics.nodes_generated)) / total_seconds)
    printfn "\"nodes expanded per second\" : %f ," ((float (metrics.nodes_expanded)) / total_seconds)
    printfn "}"