initialization
initialize: aGraph
	super initialize: aGraph.
	nodesToVisit := LIFOQueue new.