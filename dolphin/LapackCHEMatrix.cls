"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedHermitianMatrix subclass: #LapackCHEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackCHEMatrix guid: (GUID fromString: '{9369B851-89B8-48E0-B64E-E442856FCFEF}')!
LapackCHEMatrix comment: 'LapackCHEMatrix handles Hermitian matrices composed of single precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackCHEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCHEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags :=  (FullStorageMask bitOr: HermitianMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
!LapackCHEMatrix class categoriesFor: #initialize!public! !
!LapackCHEMatrix class categoriesFor: #initializeFlags!public! !

