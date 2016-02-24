"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedTriangularMatrix subclass: #LapackDTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDTRMatrix guid: (GUID fromString: '{6D56DE32-4578-4BD5-A569-79D5ED7CDA38}')!
LapackDTRMatrix comment: 'LapackDTRMatrix handles Triangular matrices composed of double precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackDTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDTRMatrix class categoriesFor: #initialize!public! !
!LapackDTRMatrix class categoriesFor: #initializeFlags!public! !

