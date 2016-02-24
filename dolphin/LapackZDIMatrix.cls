"Filed out from Dolphin Smalltalk 7"!

LapackDiagonalMatrix subclass: #LapackZDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackZDIMatrix guid: (GUID fromString: '{AE16E502-6681-417B-9E99-8137C0704F6A}')!
LapackZDIMatrix comment: 'LapackZDIMatrix handles Diagonal Matrices composed of double precision complex numbers.
See superclass for more information.'!
!LapackZDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZDIMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
!LapackZDIMatrix class categoriesFor: #initialize!public! !
!LapackZDIMatrix class categoriesFor: #initializeFlags!public! !

