"Filed out from Dolphin Smalltalk 7"!

LapackDiagonalMatrix subclass: #LapackDDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDDIMatrix guid: (GUID fromString: '{FA75D423-D2D3-4338-A1A8-5E62C7649084}')!
LapackDDIMatrix comment: 'LapackDDIMatrix handles Diagonal Matrices composed of double precision real numbers.
See superclass for more information.'!
!LapackDDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDDIMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDDIMatrix class categoriesFor: #initialize!public! !
!LapackDDIMatrix class categoriesFor: #initializeFlags!public! !

