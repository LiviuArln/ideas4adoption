removing
push: aNode
	(self isEmpty |( visited contains: aNode))
		ifTrue: [ self add: aNode ]