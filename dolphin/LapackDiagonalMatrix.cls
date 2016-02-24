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

at: anInteger 	"prefer double index accessing"	| ir ic |	ir := (anInteger - 1) \\ nrow + 1.	ic := (anInteger - 1) // nrow + 1.	^self rowAt: ir columnAt: ic!

at: anInteger put: aNumber 	"prefer double index accessing"	| ir ic |	ir := (anInteger - 1) \\ nrow + 1.	ic := (anInteger - 1) // nrow + 1.	^self 		rowAt: ir		columnAt: ic		put: aNumber!

columnAt: columnIndex 	| column |	column := self class generalMatrix nrow: nrow ncol: 1.	column at: columnIndex put: (self arrayAt: columnIndex).	^column!

diagonalAt: index 	^index = 0 		ifTrue: [self castToColumn copy]		ifFalse: [self class generalMatrix nrow: (self diagonalSizeAt: index)]!

differenceWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class"	| res |	res := self copy.	res castToColumn 		fill: self arraySize		elementsWithStride: 1		withSelfPlusScalar: -1		timesVector: aLapackMatrix castToColumn		stride: 1.	^res!

eigenValueDecomposition
	^LapackDiagonalEigenDecomposition decompose: self!

eigenValues	^self diagonal!

elementwiseProductWithMatrix: aMatrix 	^(self copy)		setDiagonal: (self castToColumn 					elementwiseProductWithMatrix: aMatrix diagonal);		yourself!

inPlaceScaledByComplex: aComplex 	"only scale diagonal..."	self castToColumn inPlaceScaledByComplex: aComplex!

inPlaceScaledByNumber: aNumber 	"only scale diagonal..."	self castToColumn inPlaceScaledByNumber: aNumber!

isDiagonal	^true!

isHermitian	^self diagonal imaginaryPart isZero!

isLowerTriangular	^true!

isSymmetric	^true!

isUpperTriangular	^true!

lowerTriangle: ind 	"return lower triangle matrix"	^ind >= 0 ifTrue: [self] ifFalse: [self class nrow: nrow ncol: ncol]!

pluDecomposition
	^LapackDiagonalPLUdecomposition decompose: self!

productMatrixTransposeWithColumnVector: aLapackMatrix 	"Product Matrix transposed*vector"	^self productMatrixWithColumnVector: aLapackMatrix!

productMatrixWithColumnVector: aLapackMatrix 	"Product Matrix * vector"	^self castToColumn elementwiseProductWithMatrix: aLapackMatrix!

reciprocal	^self class diagonal: (array collect: [:each | each reciprocal])!

rowAt: rowIndex 	| row |	row := self class generalMatrix nrow: 1 ncol: ncol.	row at: rowIndex put: (self arrayAt: rowIndex).	^row!

rowAt: i columnAt: j 	^i = j ifTrue: [self arrayAt: i] ifFalse: [0]!

rowAt: i columnAt: j put: aNumber 	^i = j 		ifTrue: [self arrayAt: i put: aNumber]		ifFalse: 			[aNumber isZero 				ifTrue: 					["do nothing, only diagonal is stored"					]				ifFalse: 					[| coerced |					coerced := self class generalMatrix diagonal: self diagonal.					coerced 						rowAt: i						columnAt: j						put: aNumber.					self become: coerced.					aNumber]]!

sumWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class"	| res |	res := self copy.	res castToColumn 		fill: self arraySize		elementsWithStride: 1		withSelfPlusScalar: 1		timesVector: aLapackMatrix castToColumn		stride: 1.	^res!

transposed
	^self!

upperTriangle: ind 	"return upper triangle matrix"	^ind <= 0 ifTrue: [self] ifFalse: [self class nrow: nrow ncol: ncol]! !
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

allocateNrow: nr ncol: nc 	"Only allocate storage for the diagonal.	A diagonal matrix should be squared"	nr = nc ifFalse: [^self generalMatrix allocateNrow: nr ncol: nc].	^self basicNew 		setArray: (Array new: nr)		nrow: nr		ncol: nc! !
!LapackDiagonalMatrix class categoriesFor: #allocateNrow:ncol:!public! !

