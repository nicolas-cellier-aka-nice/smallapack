"Filed out from Dolphin Smalltalk 7"!

LapackGeneralMatrix subclass: #LapackCGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackCGEMatrix guid: (GUID fromString: '{53D6F2C7-A470-4EF1-ADC3-4C64133F46D2}')!
LapackCGEMatrix comment: 'LapackCGEMatrix handles General Matrices (dense storage) composed of single precision complex numbers
See superclass for more information.'!
!LapackCGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCGEMatrix methodsFor!

productFromFloat: aFloat 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aFloat!

scale: n elementsBy: alpha increment: incx 
	
	[| comp |
	comp := alpha asFloatComplex.
	comp imaginaryPart isZero 
		ifTrue: 
			[self blasInterface 
				realScalWithN: n
				alpha: comp realPart
				X: self asParameter
				incX: incx]
		ifFalse: 
			[self blasInterface 
				scalWithN: n
				alpha: comp
				X: self asParameter
				incX: incx]] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							scale: n
							elementsBy: alpha
							increment: incx)]! !
!LapackCGEMatrix categoriesFor: #productFromFloat:!public! !
!LapackCGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackCGEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: GeneralMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
!LapackCGEMatrix class categoriesFor: #initialize!public! !
!LapackCGEMatrix class categoriesFor: #initializeFlags!public! !

