"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SmallapackSettings
	instanceVariableNames: ''
	classVariableNames: 'Registry'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmallapackSettings guid: (GUID fromString: '{F2C649C5-1104-4A6D-935A-5FD8E9E8891F}')!
SmallapackSettings comment: ''!
!SmallapackSettings categoriesForClass!Smallapack-Matrix! !
!SmallapackSettings class methodsFor!

blasLibraryEnabled	^self registry at: #blasLibraryEnabled ifAbsent: [true]!

blasLibraryEnabled: aBoolean 	^self registry at: #blasLibraryEnabled put: aBoolean!

icon
	"Answers an Icon that can be used to represent this class"

	^Icon
		fromFile: 'Lapack.ICO'
		usingLocator: (PackageRelativeFileLocator package: self owningPackage)!

initialize	"SmallapackSettings initialize"	self initializeRegistry.
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
!SmallapackSettings class categoriesFor: #blasLibraryEnabled!public! !
!SmallapackSettings class categoriesFor: #blasLibraryEnabled:!public! !
!SmallapackSettings class categoriesFor: #icon!constants!public! !
!SmallapackSettings class categoriesFor: #initialize!class initialization!public! !
!SmallapackSettings class categoriesFor: #initializeRegistry!private! !
!SmallapackSettings class categoriesFor: #publishedAspects!accessing!public! !
!SmallapackSettings class categoriesFor: #registry!private! !
!SmallapackSettings class categoriesFor: #resetRegistry!private! !
!SmallapackSettings class categoriesFor: #uninitialize!class initialization!public! !
!SmallapackSettings class categoriesFor: #useAtlasCBlas!public! !
!SmallapackSettings class categoriesFor: #useAtlasCBlas:!public! !

