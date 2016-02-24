"Filed out from Dolphin Smalltalk 7"!

LapackDiagonalMatrix subclass: #LapackCDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackCDIMatrix guid: (GUID fromString: '{A84888F8-F150-41A9-BEBA-D1CC1D6E696B}')!
LapackCDIMatrix comment: 'LapackCDIMatrix handles Diagonal Matrices composed of single precision complex numbers.
See superclass for more information.'!
!LapackCDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCDIMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
!LapackCDIMatrix class categoriesFor: #initialize!public! !
!LapackCDIMatrix class categoriesFor: #initializeFlags!public! !

