instance creation
new: aGraph withHooks: aCollectHook
	^ self new initialize: aGraph withHooks: aCollectHook; yourself.