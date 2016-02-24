"Filed out from Dolphin Smalltalk 7"!

LapackMatrix subclass: #LapackGeneralMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralMatrix guid: (GUID fromString: '{C73D8CB2-CF74-42EB-B94B-3FD88305ED0E}')!
LapackGeneralMatrix comment: 'LapackGeneralMatrix is an abstract class for matrices having no known peculiar property,
as opposed to symmetric, hermitian, triangular or diagonal matrices.
As such, they use a dense storage scheme (elements in contiguous memory stored columnwise).

Subclasses are specialized for containing either real or complex numbers with a specific floating point format.'!
!LapackGeneralMatrix categoriesForClass!Smallapack-Matrix! !
!LapackGeneralMatrix methodsFor!

absMax
	^
	[self lapackInterface 
		langeWithnorm: self lapackInterface maxAbs
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super absMax]!

coerceToGeneralMatrix	^self!

diagonalAt: index 
	| diag |
	diag := self class nrow: (self diagonalSizeAt: index).
	index >= 0 
		ifTrue: 
			[diag 
				copy: diag size
				elementsFrom: (self withArrayOffsetBy: index * nrow)
				sourceIncrement: nrow + 1
				destIncrement: 1]
		ifFalse: 
			[diag 
				copy: diag size
				elementsFrom: (self withArrayOffsetBy: index negated)
				sourceIncrement: nrow + 1
				destIncrement: 1].
	^diag!

eigenValueDecomposition
	^self isRealMatrix 
		ifTrue: [LapackRealEigenDecomposition decompose: self]
		ifFalse: [LapackComplexEigenDecomposition decompose: self]!

eigenValues	"it's safer to get them by Schur algorithm"	^(self schurDecomposition)		wantVectors: false;		eigenValues!

fill: m elementsWithStride: incy withSelfScaledBy: beta plusScalar: alpha timesMatrix: a transposed: trans timesVector: x length: n stride: incx 
	"fill self with a Matrix*Vector product
		alpha*op(a)*x+beta*self
	this is BLAS xGEMV operation"

	
	[self blasInterface 
		gemvWithTransA: (trans 
				ifTrue: [self blasInterface transposed]
				ifFalse: [self blasInterface notTransposed])
		M: m
		N: n
		alpha: alpha
		A: a asParameter
		lda: a nrow
		X: x asParameter
		incX: incx
		beta: beta
		Y: self asParameter
		incY: incy] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							fill: m
							elementsWithStride: incy
							withSelfScaledBy: beta
							plusScalar: alpha
							timesMatrix: a
							transposed: trans
							timesVector: x
							length: n
							stride: incx)]!

fillM: m byN: n withSelfScaledBy: beta plusScalar: alpha timesLeftMatrix: a transposed: transa timesRightMatrix: b transposed: transb length: k 
	"fill self with a Matrix*Matrix product
		alpha*transa(a)*transb(b)+beta*self
	this is BLAS xGEMM operation"

	
	[self blasInterface 
		gemmWithTransA: (transa 
				ifTrue: [self blasInterface transposed]
				ifFalse: [self blasInterface notTransposed])
		TransB: (transb 
				ifTrue: [self blasInterface transposed]
				ifFalse: [self blasInterface notTransposed])
		M: m
		N: n
		K: k
		alpha: alpha
		A: a asParameter
		lda: a nrow
		B: b asParameter
		ldb: b nrow
		beta: beta
		C: self asParameter
		ldc: self nrow] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							fillM: m
							byN: n
							withSelfScaledBy: beta
							plusScalar: alpha
							timesLeftMatrix: a
							transposed: transa
							timesRightMatrix: b
							transposed: transb
							length: k)]!

generalizedEigenValueDecompositionWithRHSMatrix: aLapackMatrix 
	"solve a generalized eigen value problem"

	^aLapackMatrix coercingFromLapackMatrix: self
		do: 
			[:a :b | 
			(a isRealMatrix 
				ifTrue: [LapackRealGeneralizedEigenDecomposition]
				ifFalse: [LapackComplexGeneralizedEigenDecomposition])
					decomposeLeft: a right: b]!

i	| res |	res := self class complexMatrix nrow: nrow ncol: ncol.	^self class isComplexMatrix 		ifTrue: 			[| selfReal selfImag resReal resImag |			selfReal := self castToRealWithArrayOffsetBy: 0.			selfImag := self castToRealWithArrayOffsetBy: 1.			resReal := res castToRealWithArrayOffsetBy: 0.			resImag := res castToRealWithArrayOffsetBy: 1.			resImag 				copy: self arraySize				elementsFrom: selfReal				sourceIncrement: 2				destIncrement: 2.			resReal 				copy: self arraySize				elementsFrom: selfImag				sourceIncrement: 2				destIncrement: 2.			resReal 				scale: self arraySize				elementsBy: -1				increment: 2.			res]		ifFalse: 			[(res castToRealWithArrayOffsetBy: 1) 				copy: self arraySize				elementsFrom: self				sourceIncrement: 1				destIncrement: 2.			res]!

lowerTriangle: ind 
	"return lower triangle matrix"

	^
	[ind <= 0 
		ifTrue: 
			[| b offset |
			b := self class triangularMatrix nrow: nrow ncol: ncol.
			offset := ind negated.
			b beLower.
			self lapackInterface 
				lacpyWithuplo: self lapackInterface lower
				m: b nrow - offset
				n: (b nrow - offset min: b ncol)
				a: (self arrayPointerWithOffset: offset)
				lda: nrow
				b: (b arrayPointerWithOffset: offset)
				ldb: b nrow.
			b]
		ifFalse: 
			[| b offset |
			offset := (ind + 1) * nrow.
			b := self copy.
			b lapackInterface 
				lasetWithuplo: b lapackInterface upper
				m: (b ncol - ind - 1 min: b nrow)
				n: b ncol - ind - 1
				alpha: 0.0
				beta: 0.0
				a: (b arrayPointerWithOffset: offset)
				lda: b nrow.
			b]] 
			on: Error
			do: [:exc | exc return: (super lowerTriangle: ind)]!

norm1
	^
	[self lapackInterface 
		langeWithnorm: self lapackInterface norm1
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super norm1]!

normFrobenius
	^
	[self lapackInterface 
		langeWithnorm: self lapackInterface normF
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super normFrobenius]!

normInfinity
	^
	[self lapackInterface 
		langeWithnorm: self lapackInterface normI
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super normInfinity]!

pluDecomposition
	^LapackPLUdecomposition decompose: self!

productMatrixTransposeWithColumnVector: aLapackMatrix 
	"Product Matrix transpose * vector"

	^
	[| y |
	y := self class nrow: ncol.
	self blasInterface 
		gemvWithTransA: self blasInterface transposed
		M: nrow
		N: ncol
		alpha: 1
		A: self asParameter
		lda: nrow
		X: aLapackMatrix asParameter
		incX: 1
		beta: 0
		Y: y asParameter
		incY: 1.
	y] 
			on: Error
			do: 
				[:exc | 
				exc 
					return: (super productMatrixTransposeWithColumnVector: aLapackMatrix)]!

productMatrixWithColumnVector: aLapackMatrix 
	"Product Matrix * vector"

	^
	[| y |
	y := self class nrow: nrow.
	self blasInterface 
		gemvWithTransA: self blasInterface notTransposed
		M: nrow
		N: ncol
		alpha: 1
		A: self asParameter
		lda: nrow
		X: aLapackMatrix asParameter
		incX: 1
		beta: 0
		Y: y asParameter
		incY: 1.
	y] 
			on: Error
			do: [:exc | exc return: (super productMatrixWithColumnVector: aLapackMatrix)]!

productMatrixWithMatrix: aLapackMatrix 
	"Product Matrix * Matrix"

	^
	[| b c |
	b := aLapackMatrix asGeneralMatrix.
	c := self class nrow: nrow ncol: b ncol.
	self blasInterface 
		gemmWithTransA: self blasInterface notTransposed
		TransB: self blasInterface notTransposed
		M: nrow
		N: b ncol
		K: ncol
		alpha: 1
		A: self asParameter
		lda: nrow
		B: b asParameter
		ldb: b nrow
		beta: 0
		C: c asParameter
		ldc: c nrow.
	c] 
			on: Error
			do: [:exc | exc return: (super productMatrixWithMatrix: aLapackMatrix)]!

reciprocal	nrow = ncol ifFalse: [self error: 'should be square'].	^[self pluDecomposition inverse] on: Error		do: [:exc | exc return: super reciprocal]!

smallLowerTriangle: ind 
	"return lower triangle matrix"

	^
	[ind <= 0 
		ifTrue: 
			[| b |
			b := self class triangularMatrix nrow: (nrow + ind max: 1) ncol: (ncol min: (nrow + ind max: 0)).
			b beLower.
			self lapackInterface 
				lacpyWithuplo: self lapackInterface lower
				m: b nrow
				n: b ncol
				a: (self arrayPointerWithOffset: ind negated)
				lda: nrow
				b: (b arrayPointerWithOffset: 0)
				ldb: b nrow.
			b]
		ifFalse: 
			[| b offset |
			offset := (ind + 1) * nrow.
			b := self class nrow: nrow ncol: (ncol min: nrow + ind).
			b copy: b nrow rowsStartingAt: 1 and: b ncol columnsStartingAt: 1 from: self.
			b lapackInterface 
				lasetWithuplo: b lapackInterface upper
				m: (b ncol - ind - 1 min: b nrow)
				n: b ncol - ind - 1
				alpha: 0.0
				beta: 0.0
				a: (b arrayPointerWithOffset: offset)
				lda: b nrow.
			b]] 
			on: Error
			do: [:exc | exc return: (super smallLowerTriangle: ind)]!

smallUpperTriangle: ind 
	"return upper triangle matrix"

	^
	[ind >= 0 
		ifTrue: 
			[| b offset |
			offset := ind * nrow.
			b := self class triangularMatrix nrow: (nrow min: (ncol - ind max: 1)) ncol: (ncol - ind max: 0).
			b beUpper.
			self lapackInterface 
				lacpyWithuplo: self lapackInterface upper
				m: b nrow
				n: b ncol
				a: (self arrayPointerWithOffset: offset)
				lda: nrow
				b: (b arrayPointerWithOffset: 0)
				ldb: b nrow.
			b]
		ifFalse: 
			[| b offset |
			b := self class nrow: (nrow min: ncol - ind) ncol: ncol.
			b copy: b nrow rowsStartingAt: 1 and: b ncol columnsStartingAt: 1 from: self.
			offset := 1 - ind.
			b lapackInterface
				lasetWithuplo: self lapackInterface lower
				m: b nrow - offset
				n: (b nrow - offset min: b ncol)
				alpha: 0.0
				beta: 0.0
				a: (b arrayPointerWithOffset: offset)
				lda: b nrow.
			b]] 
			on: Error
			do: [:exc | exc return: (super smallUpperTriangle: ind)]!

swapColumn: j1 withColumn: j2 
	((j1 between: 1 and: ncol) and: [j2 between: 1 and: ncol]) 
		ifFalse: [^self error: 'bad column specification'].
	
	[self blasInterface 
		swapWithN: nrow
		X: (self asParameterWithOffset: (j1 - 1) * nrow)
		incX: 1
		Y: (self asParameterWithOffset: (j2 - 1) * nrow)
		incY: 1] 
			on: Error
			do: 
				[:exc | 
					1 to: nrow
						do: 
							[:row | 
								self 
									swapRowAt: row
									columnAt: j1
									withRowAt: row
									columnAt: j2].
					exc return: self]!

swapRow: i1 withRow: i2 
	((i1 between: 1 and: nrow) and: [i2 between: 1 and: nrow]) 
		ifFalse: [^self error: 'bad row specification'].
	
	[self blasInterface 
		swapWithN: ncol
		X: (self asParameterWithOffset: i1 - 1)
		incX: nrow
		Y: (self asParameterWithOffset: i2 - 1)
		incY: nrow] 
			on: Error
			do: 
				[:exc | 
					1 to: ncol
						do: 
							[:col | 
								self 
									swapRowAt: i1
									columnAt: col
									withRowAt: i2
									columnAt: col].
					exc return: self]!

transposed
	| res |
	self isRowMatrix ifTrue: [^self asColumnMatrix].
	self isColumnMatrix ifTrue: [^self asRowMatrix].
	res := self class allocateNrow: ncol ncol: nrow.
	nrow <= ncol
		ifTrue: [0 to: nrow - 1 do: [:i |
				res copy: ncol elementsFrom: self sourceOffset: i sourceIncrement: nrow destOffset: i * ncol destIncrement: 1 ] ]
		ifFalse: [0 to: ncol - 1 do: [:j |
				res copy: nrow elementsFrom: self sourceOffset: j * nrow sourceIncrement: 1 destOffset: j destIncrement: ncol ] ].
	^res!

upperTriangle: ind 
	"return upper triangle matrix"

	^
	[ind >= 0 
		ifTrue: 
			[| b offset |
			offset := ind * nrow.
			b := self class triangularMatrix nrow: nrow ncol: ncol.
			b beUpper.
			self lapackInterface 
				lacpyWithuplo: self lapackInterface upper
				m: (b ncol - ind min: b nrow)
				n: b nrow - ind
				a: (self arrayPointerWithOffset: offset)
				lda: nrow
				b: (b arrayPointerWithOffset: offset)
				ldb: b nrow.
			b]
		ifFalse: 
			[| b offset |
			b := self copy.
			offset := 1 - ind.
			b lapackInterface
				lasetWithuplo: self lapackInterface lower
				m: b nrow - offset
				n: (b nrow - offset min: b ncol)
				alpha: 0.0
				beta: 0.0
				a: (b arrayPointerWithOffset: offset)
				lda: b nrow.
			b]] 
			on: Error
			do: [:exc | exc return: (super upperTriangle: ind)]! !
!LapackGeneralMatrix categoriesFor: #absMax!arithmetic!norm!public! !
!LapackGeneralMatrix categoriesFor: #coerceToGeneralMatrix!arithmetic-internal!public! !
!LapackGeneralMatrix categoriesFor: #diagonalAt:!accessing-submatrix!public! !
!LapackGeneralMatrix categoriesFor: #eigenValueDecomposition!decomposition!public! !
!LapackGeneralMatrix categoriesFor: #eigenValues!decomposition!public! !
!LapackGeneralMatrix categoriesFor: #fill:elementsWithStride:withSelfScaledBy:plusScalar:timesMatrix:transposed:timesVector:length:stride:!blas!public! !
!LapackGeneralMatrix categoriesFor: #fillM:byN:withSelfScaledBy:plusScalar:timesLeftMatrix:transposed:timesRightMatrix:transposed:length:!blas!public! !
!LapackGeneralMatrix categoriesFor: #generalizedEigenValueDecompositionWithRHSMatrix:!decomposition!public! !
!LapackGeneralMatrix categoriesFor: #i!arithmetic-complex!public! !
!LapackGeneralMatrix categoriesFor: #lowerTriangle:!accessing-submatrix!public! !
!LapackGeneralMatrix categoriesFor: #norm1!norm!public! !
!LapackGeneralMatrix categoriesFor: #normFrobenius!norm!public! !
!LapackGeneralMatrix categoriesFor: #normInfinity!norm!public! !
!LapackGeneralMatrix categoriesFor: #pluDecomposition!decomposition!public! !
!LapackGeneralMatrix categoriesFor: #productMatrixTransposeWithColumnVector:!arithmetic-internal!public! !
!LapackGeneralMatrix categoriesFor: #productMatrixWithColumnVector:!arithmetic-internal!public! !
!LapackGeneralMatrix categoriesFor: #productMatrixWithMatrix:!arithmetic-internal!public! !
!LapackGeneralMatrix categoriesFor: #reciprocal!arithmetic!public! !
!LapackGeneralMatrix categoriesFor: #smallLowerTriangle:!accessing-submatrix!public! !
!LapackGeneralMatrix categoriesFor: #smallUpperTriangle:!accessing-submatrix!public! !
!LapackGeneralMatrix categoriesFor: #swapColumn:withColumn:!public! !
!LapackGeneralMatrix categoriesFor: #swapRow:withRow:!public! !
!LapackGeneralMatrix categoriesFor: #transposed!norm!public! !
!LapackGeneralMatrix categoriesFor: #upperTriangle:!accessing-submatrix!public! !

