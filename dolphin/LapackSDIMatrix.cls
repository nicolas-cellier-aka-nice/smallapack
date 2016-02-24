"Filed out from Dolphin Smalltalk 7"!

LapackDiagonalMatrix subclass: #LapackSDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackSDIMatrix guid: (GUID fromString: '{81497E87-7EBE-4054-8AC2-63E5685FD155}')!
LapackSDIMatrix comment: 'LapackSDIMatrix handles Diagonal Matrices composed of single precision real numbers.
See superclass for more information.'!
!LapackSDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSDIMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSDIMatrix class categoriesFor: #initialize!public! !
!LapackSDIMatrix class categoriesFor: #initializeFlags!public! !

