"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedSymmetricRealMatrix subclass: #LapackSSYMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackSSYMatrix guid: (GUID fromString: '{FB2A8C38-E9ED-4CA6-A61B-7BDB05454AD5}')!
LapackSSYMatrix comment: 'LapackSSYMatrix handles Symmetric matrices composed of single precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackSSYMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSSYMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: HermitianMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSSYMatrix class categoriesFor: #initialize!public! !
!LapackSSYMatrix class categoriesFor: #initializeFlags!public! !

