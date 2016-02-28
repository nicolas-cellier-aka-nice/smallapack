| package |
package := Package name: 'Smallapack-Settings'.
package paxVersion: 1;
	basicComment: 'This class is used to handle the preferences and settings of Smallapack (like the name of DLL to be linked)'.


package classNames
	add: #SmallapackSettings;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Core\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #SmallapackSettings
	instanceVariableNames: ''
	classVariableNames: 'Registry'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

SmallapackSettings guid: (GUID fromString: '{F2C649C5-1104-4A6D-935A-5FD8E9E8891F}')!
SmallapackSettings comment: ''!
!SmallapackSettings categoriesForClass!Smallapack-Matrix! !
!SmallapackSettings class methodsFor!

blasLibraryEnabled	^self registry at: #blasLibraryEnabled ifAbsent: [true]!

blasLibraryEnabled: aBoolean 	self registry at: #blasLibraryEnabled put: aBoolean.
	self trigger: #librariesChanged.!

blasLibraryName	^self registry at: #blasLibraryName ifAbsent: ['blas']!

blasLibraryName: aString 	self registry at: #blasLibraryName put: aString.
	self trigger: #librariesChanged.!

cblasLibraryName	^self registry at: #cblasLibraryName ifAbsent: ['cblas']!

cblasLibraryName: aString 	self registry at: #cblasLibraryName put: aString.
	self trigger: #librariesChanged.!

icon
	"Answers an Icon that can be used to represent this class"

	^Icon
		fromFile: 'Lapack.ICO'
		usingLocator: (PackageRelativeFileLocator package: self owningPackage)!

initialize	"SmallapackSettings initialize"	self initializeRegistry.
	(Smalltalk developmentSystem)
		registerTool: self!

initializeRegistry	Registry isNil ifTrue: [ self resetRegistry ].!

lapackLibraryName	^self registry at: #lapackLibraryName ifAbsent: ['lapack']!

lapackLibraryName: aString 	self registry at: #lapackLibraryName put: aString.
	self trigger: #librariesChanged.!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects
		add: (Aspect boolean: #useAtlasCBlas);
		add: (Aspect boolean: #blasLibraryEnabled);
		add: (Aspect string: #blasLibraryName);
		add: (Aspect string: #cblasLibraryName);
		add: (Aspect string: #lapackLibraryName).
	^aspects!

registry	^Registry!

resetRegistry	" self resetRegistry "		Registry := Dictionary new.!

uninitialize
	(Smalltalk developmentSystem)
		unregisterTool: self!

useAtlasCBlas	^self registry at: #useAtlasCBlas ifAbsent: [true]!

useAtlasCBlas: aBoolean 	self registry at: #useAtlasCBlas put: aBoolean.
	self trigger: #librariesChanged.! !
!SmallapackSettings class categoriesFor: #blasLibraryEnabled!public! !
!SmallapackSettings class categoriesFor: #blasLibraryEnabled:!public! !
!SmallapackSettings class categoriesFor: #blasLibraryName!public! !
!SmallapackSettings class categoriesFor: #blasLibraryName:!public! !
!SmallapackSettings class categoriesFor: #cblasLibraryName!public! !
!SmallapackSettings class categoriesFor: #cblasLibraryName:!public! !
!SmallapackSettings class categoriesFor: #icon!constants!public! !
!SmallapackSettings class categoriesFor: #initialize!class initialization!public! !
!SmallapackSettings class categoriesFor: #initializeRegistry!private! !
!SmallapackSettings class categoriesFor: #lapackLibraryName!public! !
!SmallapackSettings class categoriesFor: #lapackLibraryName:!public! !
!SmallapackSettings class categoriesFor: #publishedAspects!accessing!public! !
!SmallapackSettings class categoriesFor: #registry!private! !
!SmallapackSettings class categoriesFor: #resetRegistry!private! !
!SmallapackSettings class categoriesFor: #uninitialize!class initialization!public! !
!SmallapackSettings class categoriesFor: #useAtlasCBlas!public! !
!SmallapackSettings class categoriesFor: #useAtlasCBlas:!public! !

"Binary Globals"!

