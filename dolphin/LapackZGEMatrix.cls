"Filed out from Dolphin Smalltalk 7"!

LapackGeneralMatrix subclass: #LapackZGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackZGEMatrix guid: (GUID fromString: '{A3FAD776-8863-4B39-8502-30726A321D32}')!
LapackZGEMatrix comment: 'LapackZGEMatrix handles General Matrices (dense storage) composed of double precision complex numbers
See superclass for more information.'!
!LapackZGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZGEMatrix methodsFor!

productFromDouble: aDouble 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aDouble!

scale: n elementsBy: alpha increment: incx 
	
	[| comp |
	comp := alpha asDoubleComplex.
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
!LapackZGEMatrix categoriesFor: #productFromDouble:!public! !
!LapackZGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackZGEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: GeneralMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
!LapackZGEMatrix class categoriesFor: #initialize!public! !
!LapackZGEMatrix class categoriesFor: #initializeFlags!public! !

