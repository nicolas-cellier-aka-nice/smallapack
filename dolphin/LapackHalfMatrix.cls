"Filed out from Dolphin Smalltalk 7"!

LapackMatrix subclass: #LapackHalfMatrix
	instanceVariableNames: 'uplo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackHalfMatrix guid: (GUID fromString: '{7BCA3B72-375B-4750-B6DD-E3D7FD401C4E}')!
LapackHalfMatrix comment: 'LapackHalfMatrix has common protocol for those matrices storing only half a triangle.

Subclasses must implement the following messages:
	private
		complementOfRow:column:
		complementOfRow:column:put:

Instance Variables:
	uplo	<SmallInteger>	code indicating if upper or lower triangular matrix is filled
'!
!LapackHalfMatrix categoriesForClass!Smallapack-Matrix! !
!LapackHalfMatrix methodsFor!

asPackedMatrix

asUnpackedMatrix

at: anInteger 

at: anInteger put: aNumber 

beLower

beUpper

coerceToDoublePrecisionComplexMatrix

coerceToDoublePrecisionMatrix

coerceToSinglePrecisionComplexMatrix

coerceToSinglePrecisionMatrix

complementOfRow: i column: j 

complementOfRow: i column: j put: aNumber 

differenceFromMatrix: aMatrix

isLower

isUpper

productFromMatrix: aMatrix

rowAt: i columnAt: j 

rowAt: i columnAt: j put: aNumber 

sumFromMatrix: aMatrix
!LapackHalfMatrix categoriesFor: #asPackedMatrix!public! !
!LapackHalfMatrix categoriesFor: #asUnpackedMatrix!public! !
!LapackHalfMatrix categoriesFor: #at:!public! !
!LapackHalfMatrix categoriesFor: #at:put:!public! !
!LapackHalfMatrix categoriesFor: #beLower!public! !
!LapackHalfMatrix categoriesFor: #beUpper!public! !
!LapackHalfMatrix categoriesFor: #coerceToDoublePrecisionComplexMatrix!public! !
!LapackHalfMatrix categoriesFor: #coerceToDoublePrecisionMatrix!public! !
!LapackHalfMatrix categoriesFor: #coerceToSinglePrecisionComplexMatrix!public! !
!LapackHalfMatrix categoriesFor: #coerceToSinglePrecisionMatrix!public! !
!LapackHalfMatrix categoriesFor: #complementOfRow:column:!public! !
!LapackHalfMatrix categoriesFor: #complementOfRow:column:put:!public! !
!LapackHalfMatrix categoriesFor: #differenceFromMatrix:!public! !
!LapackHalfMatrix categoriesFor: #isLower!public! !
!LapackHalfMatrix categoriesFor: #isUpper!public! !
!LapackHalfMatrix categoriesFor: #productFromMatrix:!public! !
!LapackHalfMatrix categoriesFor: #rowAt:columnAt:!public! !
!LapackHalfMatrix categoriesFor: #rowAt:columnAt:put:!public! !
!LapackHalfMatrix categoriesFor: #sumFromMatrix:!public! !

!LapackHalfMatrix class methodsFor!

columns: anArrayOfColumns 

eye: aShape 

fromGeneralMatrix: aMatrix 

fromSequence: aRawArray nrow: nr ncol: nc 

packedMatrix

rows: anArrayOfRows 

unpackedMatrix
!LapackHalfMatrix class categoriesFor: #columns:!public! !
!LapackHalfMatrix class categoriesFor: #eye:!public! !
!LapackHalfMatrix class categoriesFor: #fromGeneralMatrix:!public! !
!LapackHalfMatrix class categoriesFor: #fromSequence:nrow:ncol:!public! !
!LapackHalfMatrix class categoriesFor: #packedMatrix!public! !
!LapackHalfMatrix class categoriesFor: #rows:!public! !
!LapackHalfMatrix class categoriesFor: #unpackedMatrix!public! !
