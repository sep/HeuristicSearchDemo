module DomainInterface

type DomainInterface<'cost,  'state> =
    abstract member Expand : 'state -> ('state * 'cost) list
    abstract member GoalP : 'state -> bool
    abstract member InitialState : 'state

type DuplicateDomainInterface<'cost, 'state, 'hashvalue> =
    inherit DomainInterface<'cost, 'state>
    abstract member Key : 'state -> 'hashvalue

type CostHeuristicDuplicateDomainInterface<'cost, 'state, 'hashvalue> =
    inherit DuplicateDomainInterface<'cost, 'state, 'hashvalue>
    abstract member H : 'state -> 'cost

type CostLengthHeuristicDuplicateDomainInterface<'cost, 'state, 'hashvalue> =
    inherit CostHeuristicDuplicateDomainInterface<'cost, 'state, 'hashvalue>
    abstract member D : 'state -> int

type CostHeuristicDomainInterface<'cost, 'state> =
    inherit DomainInterface<'cost, 'state>
    abstract member H : 'state -> 'cost

type CostLengthHeuristicDomainInterface<'cost, 'state> =
    inherit CostHeuristicDomainInterface<'cost, 'state>
    abstract member D : 'state -> int