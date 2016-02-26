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

blasLibraryEnabled

blasLibraryEnabled: aBoolean 

icon
	"Answers an Icon that can be used to represent this class"

	^Icon
		fromFile: 'Lapack.ICO'
		usingLocator: (PackageRelativeFileLocator package: self owningPackage)!

initialize
	(Smalltalk developmentSystem)
		registerTool: self!

initializeRegistry

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects
		add: (Aspect boolean: #useAtlasCBlas);
		add: (Aspect boolean: #blasLibraryEnabled).
	^aspects!

registry

resetRegistry

uninitialize
	(Smalltalk developmentSystem)
		unregisterTool: self!

useAtlasCBlas

useAtlasCBlas: aBoolean 
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
