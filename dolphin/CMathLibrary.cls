"Filed out from Dolphin Smalltalk 7"!

ExternalLibrary subclass: #CMathLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CMathLibrary guid: (GUID fromString: '{2AD5423B-7890-4A4B-BA5F-1148296E867B}')!
CMathLibrary comment: ''!
!CMathLibrary categoriesForClass!Unclassified! !
!CMathLibrary methodsFor!

allocateDoubleArraySize: n
	^(DOUBLEArray new: n) asParameter!

allocateDoubleComplexArraySize: n
	^(DOUBLECOMPLEXArray new: n) asParameter!

allocateFloatArraySize: n
	^(FLOATArray new: n) asParameter!

allocateFloatComplexArraySize: n
	^(FLOATCOMPLEXArray new: n) asParameter!

allocateIntegerArraySize: n
	^(SDWORDArray new: n) asParameter!

allocateLogicalArraySize: n
	^DWORDArray new: n!

cCharPointerOn: aCharacter 	"Usefull for passing a character*1 argument to FORTRAN"	^(String with: aCharacter)	"Shouldn't we encode it ?"!

cDoubleComplexPointerOn: aComplex 
	"Usefull for passing a double complex (complex*16) argument to FORTRAN"
	
	| comp |
	comp := aComplex asComplex.
	^(DOUBLECOMPLEXArray with: comp)
		asParameter!

cDoublePointerOn: aDouble 
	"Usefull for passing a double precision (real*8) argument to FORTRAN"

	^(DOUBLEArray with: aDouble asFloat)
		asParameter!

cFloatComplexPointerOn: aComplex 
	"Usefull for passing a complex (complex*8) argument to FORTRAN"
	
	| comp |
	comp := aComplex asComplex.
	^(FLOATCOMPLEXArray with: comp)
		asParameter!

cFloatPointerOn: aFloat 
	"Usefull for passing a real (real*4) argument to FORTRAN"

	^(FLOATArray with: aFloat)
		asParameter!

cIntegerPointerOn: anInteger 
	"Usefull for passing an integer (integer*4) argument to FORTRAN.
	We do not use WordArray, because they are unsigned"

	^(SDWORDArray with: anInteger) asParameter!

cLogicalPointerOn: aBoolean 
	"Usefull for passing a logical (logical*4) argument to FORTRAN"

	^DWORDArray with: (aBoolean ifTrue: [1] ifFalse: [0])!

free: aVariable
	"free aVariable memory if allocated on external heap"
	
	"(aVariable class isKindOf: ExternalAddress) ifTrue: [aVariable free]"! !
!CMathLibrary categoriesFor: #allocateDoubleArraySize:!memory!public! !
!CMathLibrary categoriesFor: #allocateDoubleComplexArraySize:!public! !
!CMathLibrary categoriesFor: #allocateFloatArraySize:!memory!public! !
!CMathLibrary categoriesFor: #allocateFloatComplexArraySize:!public! !
!CMathLibrary categoriesFor: #allocateIntegerArraySize:!memory!public! !
!CMathLibrary categoriesFor: #allocateLogicalArraySize:!public! !
!CMathLibrary categoriesFor: #cCharPointerOn:!public! !
!CMathLibrary categoriesFor: #cDoubleComplexPointerOn:!public! !
!CMathLibrary categoriesFor: #cDoublePointerOn:!public! !
!CMathLibrary categoriesFor: #cFloatComplexPointerOn:!public! !
!CMathLibrary categoriesFor: #cFloatPointerOn:!public! !
!CMathLibrary categoriesFor: #cIntegerPointerOn:!public! !
!CMathLibrary categoriesFor: #cLogicalPointerOn:!public! !
!CMathLibrary categoriesFor: #free:!public! !

