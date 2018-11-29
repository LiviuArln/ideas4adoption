running
traverse: aNode
	self push: aNode.
	[ self isEmpty ] whileTrue:
		[ self traverseStep ]