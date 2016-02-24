"Filed out from Dolphin Smalltalk 7"!

LapackGeneralMatrix subclass: #LapackSGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackSGEMatrix guid: (GUID fromString: '{BB5A2547-1EFA-4A0A-BEED-0B84B2C9A407}')!
LapackSGEMatrix comment: 'LapackSGEMatrix handles General Matrices (dense storage) composed of single precision real numbers
See superclass for more information.'!
!LapackSGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSGEMatrix methodsFor!

productFromFloat: aFloat 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aFloat!

scale: n elementsBy: alpha increment: incx 
	
	[self blasInterface 
		scalWithN: n
		alpha: alpha asFloat
		X: self asParameter
		incX: incx] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							scale: n
							elementsBy: alpha
							increment: incx)]! !
!LapackSGEMatrix categoriesFor: #productFromFloat:!public! !
!LapackSGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackSGEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: GeneralMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSGEMatrix class categoriesFor: #initialize!public! !
!LapackSGEMatrix class categoriesFor: #initializeFlags!public! !

