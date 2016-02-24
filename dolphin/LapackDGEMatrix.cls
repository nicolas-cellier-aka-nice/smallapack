"Filed out from Dolphin Smalltalk 7"!

LapackGeneralMatrix subclass: #LapackDGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDGEMatrix guid: (GUID fromString: '{0BF9D151-CE10-4D9C-93E6-FFF9A3D86795}')!
LapackDGEMatrix comment: 'LapackDGEMatrix handles General Matrices (dense storage) composed of double precision real numbers
See superclass for more information.'!
!LapackDGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDGEMatrix methodsFor!

productFromDouble: aDouble 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aDouble!

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
!LapackDGEMatrix categoriesFor: #productFromDouble:!public! !
!LapackDGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackDGEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags :=  (FullStorageMask bitOr: GeneralMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDGEMatrix class categoriesFor: #initialize!public! !
!LapackDGEMatrix class categoriesFor: #initializeFlags!public! !

