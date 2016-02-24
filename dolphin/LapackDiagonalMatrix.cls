"Filed out from Dolphin Smalltalk 7"!

LapackMatrix subclass: #LapackDiagonalMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDiagonalMatrix guid: (GUID fromString: '{420E585E-75C6-4311-A97D-92E7046C5230}')!
LapackDiagonalMatrix comment: 'LapackDiagonalMatrix is square and has zeroes off diagonal by definition.

It stores only diagonal elements in the array (economic).
'!
!LapackDiagonalMatrix categoriesForClass!Unclassified! !
!LapackDiagonalMatrix methodsFor!

at: anInteger 

at: anInteger put: aNumber 

columnAt: columnIndex 

diagonalAt: index 

differenceWithLapackMatrix: aLapackMatrix 

eigenValueDecomposition
	^LapackDiagonalEigenDecomposition decompose: self!

eigenValues

elementwiseProductWithMatrix: aMatrix 

inPlaceScaledByComplex: aComplex 

inPlaceScaledByNumber: aNumber 

isDiagonal

isHermitian

isLowerTriangular

isSymmetric

isUpperTriangular

lowerTriangle: ind 

pluDecomposition
	^LapackDiagonalPLUdecomposition decompose: self!

productMatrixTransposeWithColumnVector: aLapackMatrix 

productMatrixWithColumnVector: aLapackMatrix 

reciprocal

rowAt: rowIndex 

rowAt: i columnAt: j 

rowAt: i columnAt: j put: aNumber 

sumWithLapackMatrix: aLapackMatrix 

transposed
	^self!

upperTriangle: ind 
!LapackDiagonalMatrix categoriesFor: #at:!public! !
!LapackDiagonalMatrix categoriesFor: #at:put:!public! !
!LapackDiagonalMatrix categoriesFor: #columnAt:!public! !
!LapackDiagonalMatrix categoriesFor: #diagonalAt:!public! !
!LapackDiagonalMatrix categoriesFor: #differenceWithLapackMatrix:!public! !
!LapackDiagonalMatrix categoriesFor: #eigenValueDecomposition!public! !
!LapackDiagonalMatrix categoriesFor: #eigenValues!public! !
!LapackDiagonalMatrix categoriesFor: #elementwiseProductWithMatrix:!public! !
!LapackDiagonalMatrix categoriesFor: #inPlaceScaledByComplex:!public! !
!LapackDiagonalMatrix categoriesFor: #inPlaceScaledByNumber:!public! !
!LapackDiagonalMatrix categoriesFor: #isDiagonal!public! !
!LapackDiagonalMatrix categoriesFor: #isHermitian!public! !
!LapackDiagonalMatrix categoriesFor: #isLowerTriangular!public! !
!LapackDiagonalMatrix categoriesFor: #isSymmetric!public! !
!LapackDiagonalMatrix categoriesFor: #isUpperTriangular!public! !
!LapackDiagonalMatrix categoriesFor: #lowerTriangle:!public! !
!LapackDiagonalMatrix categoriesFor: #pluDecomposition!public! !
!LapackDiagonalMatrix categoriesFor: #productMatrixTransposeWithColumnVector:!public! !
!LapackDiagonalMatrix categoriesFor: #productMatrixWithColumnVector:!public! !
!LapackDiagonalMatrix categoriesFor: #reciprocal!public! !
!LapackDiagonalMatrix categoriesFor: #rowAt:!public! !
!LapackDiagonalMatrix categoriesFor: #rowAt:columnAt:!public! !
!LapackDiagonalMatrix categoriesFor: #rowAt:columnAt:put:!public! !
!LapackDiagonalMatrix categoriesFor: #sumWithLapackMatrix:!public! !
!LapackDiagonalMatrix categoriesFor: #transposed!public! !
!LapackDiagonalMatrix categoriesFor: #upperTriangle:!public! !

!LapackDiagonalMatrix class methodsFor!

allocateNrow: nr ncol: nc 
!LapackDiagonalMatrix class categoriesFor: #allocateNrow:ncol:!public! !
