running
traverseStep
	| nx |
	nx := self next.
	self preCollect: nx.
	(adiacencies at: nx) do: [ :n | self push n ].
	self postCollect: nx