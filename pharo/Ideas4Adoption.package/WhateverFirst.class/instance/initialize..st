running
initialize: aGraph
	adiacencies := (aGraph groupedBy: #head)
		collect: [ :a | a value collect: #value ]