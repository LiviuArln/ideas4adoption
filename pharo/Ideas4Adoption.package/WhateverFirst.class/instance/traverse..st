running
traverse: aNode
	self push: aNode.
	[ self isEmpty ]
		whileTrue: [ | nx |
			nx := self next self preCollect: nx.
			(adiacencies at: nx) collect: [ :ads | ads do: [ :n | self push n ] ].
			self postCollect: nx ].
	self processEnd