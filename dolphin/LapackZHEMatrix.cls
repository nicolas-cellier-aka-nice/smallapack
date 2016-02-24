"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedHermitianMatrix subclass: #LapackZHEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackZHEMatrix guid: (GUID fromString: '{C685C67E-F4E6-49BD-85A7-25CA99E41526}')!
LapackZHEMatrix comment: 'LapackZHEMatrix handles Hermitian matrices composed of double precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackZHEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZHEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: HermitianMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
!LapackZHEMatrix class categoriesFor: #initialize!public! !
!LapackZHEMatrix class categoriesFor: #initializeFlags!public! !

