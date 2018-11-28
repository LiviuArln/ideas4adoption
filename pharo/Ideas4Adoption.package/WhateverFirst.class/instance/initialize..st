running
initialize: aGraph
	adiacencies := (aGraph groupedBy: #head ) collect: [ :a | (a head) -> (a value collect: #value) ]