running
traverseStep
	| nx |
	nx := self next.
	collectHook preCollect: nx.
	(adiacencies at: nx)
		do: [ :n | self push n ].
	collectHook postCollect: nx