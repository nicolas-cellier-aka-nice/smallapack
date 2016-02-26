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

icon
	"Answers an Icon that can be used to represent this class"

	^Icon
		fromFile: 'Lapack.ICO'
		usingLocator: (PackageRelativeFileLocator package: self owningPackage)!

initialize	"Settings initialize"	self initializeRegistry.
	(Smalltalk developmentSystem)
		registerTool: self!

initializeRegistry	Registry isNil ifTrue: [ self resetRegistry ].!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects
		add: (Aspect boolean: #useAtlasCBlas);
		add: (Aspect boolean: #blasLibraryEnabled).
	^aspects!

registry	^Registry!

resetRegistry	" self resetRegistry "		Registry := Dictionary new.!

uninitialize
	(Smalltalk developmentSystem)
		unregisterTool: self!

useAtlasCBlas	^self registry at: #useAtlasCBlas ifAbsent: [true]!

useAtlasCBlas: aBoolean 	self registry at: #useAtlasCBlas put: aBoolean.	LapackMatrix		resetBlasInterfaces;		resetLapackInterfaces! !
!Settings class categoriesFor: #blasLibraryEnabled!public! !
!Settings class categoriesFor: #blasLibraryEnabled:!public! !
!Settings class categoriesFor: #icon!constants!public! !
!Settings class categoriesFor: #initialize!class initialization!public! !
!Settings class categoriesFor: #initializeRegistry!private! !
!Settings class categoriesFor: #publishedAspects!accessing!public! !
!Settings class categoriesFor: #registry!private! !
!Settings class categoriesFor: #resetRegistry!private! !
!Settings class categoriesFor: #uninitialize!class initialization!public! !
!Settings class categoriesFor: #useAtlasCBlas!public! !
!Settings class categoriesFor: #useAtlasCBlas:!public! !

