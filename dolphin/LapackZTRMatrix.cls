"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedTriangularMatrix subclass: #LapackZTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackZTRMatrix guid: (GUID fromString: '{00C44E12-18C2-498F-A82B-E9656E2C28F6}')!
LapackZTRMatrix comment: 'LapackZTRMatrix handles Triangular matrices composed of double precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackZTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
!LapackZTRMatrix class categoriesFor: #initialize!public! !
!LapackZTRMatrix class categoriesFor: #initializeFlags!public! !

