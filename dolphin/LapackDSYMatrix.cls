"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedSymmetricRealMatrix subclass: #LapackDSYMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDSYMatrix guid: (GUID fromString: '{D1E7D673-A960-4FE4-80E0-4D00D6718580}')!
LapackDSYMatrix comment: 'LapackDSYMatrix handles Symmetric matrices composed of double precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackDSYMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDSYMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: HermitianMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDSYMatrix class categoriesFor: #initialize!public! !
!LapackDSYMatrix class categoriesFor: #initializeFlags!public! !

