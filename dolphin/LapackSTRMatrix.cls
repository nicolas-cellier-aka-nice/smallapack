"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedTriangularMatrix subclass: #LapackSTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackSTRMatrix guid: (GUID fromString: '{DDF8C48E-1CDA-4A0B-A114-94006C8ADDF2}')!
LapackSTRMatrix comment: 'LapackSTRMatrix handles Triangular matrices composed of single precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackSTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSTRMatrix class categoriesFor: #initialize!public! !
!LapackSTRMatrix class categoriesFor: #initializeFlags!public! !

