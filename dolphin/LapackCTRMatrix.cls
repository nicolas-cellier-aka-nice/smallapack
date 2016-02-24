"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedTriangularMatrix subclass: #LapackCTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackCTRMatrix guid: (GUID fromString: '{48F592C7-F8FB-432D-86F5-512E1F736B24}')!
LapackCTRMatrix comment: 'LapackCTRMatrix handles Triangular matrices composed of single precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackCTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
!LapackCTRMatrix class categoriesFor: #initialize!public! !
!LapackCTRMatrix class categoriesFor: #initializeFlags!public! !

