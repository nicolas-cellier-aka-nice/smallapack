"Filed out from Dolphin Smalltalk 7"!

ArrayLibrary subclass: #ArrayZLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArrayZLibrary guid: (GUID fromString: '{17F1D9E8-C669-4445-AB4F-4E2F191F9FEC}')!
ArrayZLibrary comment: ''!
!ArrayZLibrary categoriesForClass!Unclassified! !
!ArrayZLibrary methodsFor!

arraydtosWithn: n source: source result: result 
	^ArrayDLibrary new 
		arraydtosWithn: 2 * n
		source: source
		result: result! !
!ArrayZLibrary categoriesFor: #arraydtosWithn:source:result:!public! !

