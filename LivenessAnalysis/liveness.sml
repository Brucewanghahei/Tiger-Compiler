structure Liveness: LIVENESS = struct

type liveSet = unit Temp.Table.table * temp list
type liveMap = liveSet FlowGraph.Table.table

end
