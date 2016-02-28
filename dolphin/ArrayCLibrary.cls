"Filed out from Dolphin Smalltalk 7"!

ArrayLibrary subclass: #ArrayCLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArrayCLibrary guid: (GUID fromString: '{6C2FEBF8-5C7C-4A85-9B75-9B969DCB8A9F}')!
ArrayCLibrary comment: ''!
!ArrayCLibrary categoriesForClass!Unclassified! !
!ArrayCLibrary methodsFor!

arraystodWithn: n source: source result: result 
	^ArraySLibrary new 
		arraystodWithn: 2 * n
		source: source
		result: result! !
!ArrayCLibrary categoriesFor: #arraystodWithn:source:result:!public! !

