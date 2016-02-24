"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Settings
	instanceVariableNames: ''
	classVariableNames: 'Registry'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Settings guid: (GUID fromString: '{F2C649C5-1104-4A6D-935A-5FD8E9E8891F}')!
Settings comment: ''!
!Settings categoriesForClass!Smallapack-Matrix! !
!Settings class methodsFor!

blasLibraryEnabled	^self registry at: #blasLibraryEnabled ifAbsent: [true]!

blasLibraryEnabled: aBoolean 	^self registry at: #blasLibraryEnabled put: aBoolean!

initialize	"Settings initialize"	self initializeRegistry!

initializeRegistry	Registry isNil ifTrue: [ self resetRegistry ].!

registry	^Registry!

resetRegistry	" self resetRegistry "		Registry := Dictionary new.!

useAtlasCBlas	^self registry at: #useAtlasCBlas ifAbsent: [true]!

useAtlasCBlas: aBoolean 	self registry at: #useAtlasCBlas put: aBoolean.	LapackMatrix		resetBlasInterfaces;		resetLapackInterfaces! !
!Settings class categoriesFor: #blasLibraryEnabled!public! !
!Settings class categoriesFor: #blasLibraryEnabled:!public! !
!Settings class categoriesFor: #initialize!public! !
!Settings class categoriesFor: #initializeRegistry!public! !
!Settings class categoriesFor: #registry!public! !
!Settings class categoriesFor: #resetRegistry!public! !
!Settings class categoriesFor: #useAtlasCBlas!public! !
!Settings class categoriesFor: #useAtlasCBlas:!public! !

