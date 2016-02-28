"Filed out from Dolphin Smalltalk 7"!

ArrayLibrary subclass: #ArraySLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArraySLibrary guid: (GUID fromString: '{CD194712-AE9A-402D-A3FB-E482C7A411CC}')!
ArraySLibrary comment: ''!
!ArraySLibrary categoriesForClass!Unclassified! !
!ArraySLibrary methodsFor!

arraystodWithn: n source: source result: result 
	<cdecl: void 'array_stod'  SDWORD float * double * >
	^self invalidCall! !
!ArraySLibrary categoriesFor: #arraystodWithn:source:result:!public! !

