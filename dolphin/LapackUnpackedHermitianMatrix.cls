"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedMatrix subclass: #LapackUnpackedHermitianMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedHermitianMatrix guid: (GUID fromString: '{8E8FE2AC-E925-4370-8223-CF26D36C3D35}')!
LapackUnpackedHermitianMatrix comment: 'LapackUnpackedHermitianMatrix is an abstract class for matrices having hermitian property:
	(M rowAt: i columnAt: j) = (M rowAt: j columnAt: i) conjugated

Diagonal of Hermitian Matrices must have zero imaginary part.

Storage is allocated for the n*n elements.
But only half triangle and main diagonal need being stored.
Variable uplo does indicate which one, upper, lower or both.

Subclasses are specialized for containing either real or complex numbers with a specific floating point format.

Note that real symmetric matrices are also hermitian...


'!
!LapackUnpackedHermitianMatrix categoriesForClass!Smallapack-Matrix! !
!LapackUnpackedHermitianMatrix methodsFor!

absMax
	^self class isComplexMatrix 
		ifTrue: 
			[
			[self lapackInterface 
				lanheWithnorm: self lapackInterface maxAbs
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super absMax]]
		ifFalse: 
			[
			[self lapackInterface 
				lansyWithnorm: self lapackInterface maxAbs
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super absMax]]!

beBothUpperLower

coerceToGeneralMatrix

coercingFromComplexNumber: aComplex do: aBlock 

complementOfRow: i column: j 

diagonalAt: index 

differenceWithLapackMatrix: aLapackMatrix 

eigenValueDecomposition
	^LapackHermitianEigenDecomposition decompose: self!

fillLowerTriangleLapack
	"Try using BLAS/LAPACK primitives for larger matrices"

	
	[| blas |
	blas := self blasInterface.
	self class isComplexMatrix 
		ifTrue: 
			[| lapack |
			lapack := self lapackInterface.
			1 to: nrow - 1
				do: 
					[:i | 
					"Copy a row to a column"
					blas 
						copyWithN: nrow - i
						X: (self asParameterWithOffset: (i * nrow) + i - 1)
						incX: nrow
						Y: (self asParameterWithOffset: (i - 1) * nrow + i)
						incY: 1.
					"Conjugate the column"
					lapack 
						lacgvWithn: nrow - i
						x: (self asParameterWithOffset: (i - 1) * nrow + i)
						incx: 1]]
		ifFalse: 
			[1 to: nrow - 1
				do: 
					[:i | 
					blas 
						copyWithN: nrow - i
						X: (self asParameterWithOffset: (i * nrow) + i - 1)
						incX: nrow
						Y: (self asParameterWithOffset: (i - 1) * nrow + i)
						incY: 1]]] 
			on: Error
			do: [:exc | exc return: self fillLowerTriangleNaive].
	self beBothUpperLower!

fillLowerTriangleNaive

fillOtherTriangle

fillRandNormalWithSeed: seedArray 

fillRandUniformWithSeed: seedArray 

fillUpperTriangleLapack
	"Try using BLAS/LAPACK primitives for larger matrices"

	
	[| blas |
	blas := self blasInterface.
	self class isComplexMatrix 
		ifTrue: 
			[| lapack |
			lapack := self lapackInterface.
			1 to: nrow - 1
				do: 
					[:i | 
					"Copy a column to a row"
					blas 
						copyWithN: nrow - i
						X: (self asParameterWithOffset: (i - 1) * nrow + i)
						incX: 1
						Y: (self asParameterWithOffset: (i * nrow) + i - 1)
						incY: nrow.
					"Conjugate the row"
					lapack 
						lacgvWithn: nrow - i
						x: (self asParameterWithOffset: (i * nrow) + i - 1)
						incx: nrow]]
		ifFalse: 
			[1 to: nrow - 1
				do: 
					[:i | 
					blas 
						copyWithN: nrow - i
						X: (self asParameterWithOffset: (i - 1) * nrow + i)
						incX: 1
						Y: (self asParameterWithOffset: (i * nrow) + i - 1)
						incY: nrow]]] 
			on: Error
			do: [:exc | exc return: self fillUpperTriangleNaive].
	self beBothUpperLower!

fillUpperTriangleNaive

inPlaceSolve: b 
	"find x such that self*x=b, that is solve a linear system of equations
	BEWARE: this will destroy matrices self and b
	on return:
		x is stored in b on return
		a contains L and U of P*L*U decomposition (diagonal 1 of L not included)
		ipiv contains permutations of rows P of P*L*U decomposition
		info contains error code if not 0"

	| ipiv info |
	ipiv := SDWORDArray new: nrow.
	info := self lapackInterface 
				hesvWithuplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: nrow
				nrhs: b ncol
				a: self asParameter
				lda: nrow
				ipiv: ipiv asParameter
				b: b asParameter
				ldb: b nrow.
	info = 0 ifFalse: [self error: 'inversion failed'].
	^b!

isBothUpperLower

isHermitian

isSymmetric

lowerTriangle: ind 

norm1
	^self class isComplexMatrix 
		ifTrue: 
			[
			[self lapackInterface 
				lanheWithnorm: self lapackInterface norm1
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super norm1]]
		ifFalse: 
			[
			[self lapackInterface 
				lansyWithnorm: self lapackInterface norm1
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super norm1]]!

normFrobenius
	^self class isComplexMatrix 
		ifTrue: 
			[
			[self lapackInterface 
				lanheWithnorm: self lapackInterface normF
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super normFrobenius]]
		ifFalse: 
			[
			[self lapackInterface 
				lansyWithnorm: self lapackInterface normF
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super normFrobenius]]!

normInfinity
	^self class isComplexMatrix 
		ifTrue: 
			[
			[self lapackInterface 
				lanheWithnorm: self lapackInterface normI
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super normInfinity]]
		ifFalse: 
			[
			[self lapackInterface 
				lansyWithnorm: self lapackInterface normI
				uplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				n: ncol
				a: self asParameter
				lda: nrow] 
					on: Error
					do: [:exc | exc return: super normInfinity]]!

pluDecomposition
	^LapackHermitianPLUdecomposition decompose: self!

productMatrixAtRightWithMatrix: aLapackMatrix 
	"the other matrix must have conforming precision and complexity"

	^
	[| b c blas |
	self class = aLapackMatrix class 
		ifTrue: 
			["product of two hermitian is hermitian"

			aLapackMatrix fillOtherTriangle.
			b := aLapackMatrix.
			c := aLapackMatrix zero]
		ifFalse: 
			["else, general"

			b := aLapackMatrix asGeneralMatrix.
			c := aLapackMatrix class generalMatrix nrow: aLapackMatrix nrow ncol: ncol].
	blas := self blasInterface.
	blas 
		hemmWithSide: blas right
		Uplo: (self isUpper 
				ifTrue: [blas upper]
				ifFalse: [blas lower])
		M: b nrow
		N: b ncol
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
			do: [:exc | exc return: (super productMatrixAtRightWithMatrix: aLapackMatrix)]!

productMatrixTransposeWithColumnVector: aLapackMatrix 

productMatrixWithColumnVector: aLapackMatrix 
	"the vector must have conforming precision and complexity"

	^
	[| y |
	y := aLapackMatrix class nrow: nrow.
	self blasInterface 
		hemvWithUplo: (self isUpper 
				ifTrue: [self blasInterface upper]
				ifFalse: [self blasInterface lower])
		N: nrow
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
	"the other matrix must have conforming precision and complexity
	NOTE : product of two hermitian is not hermitian"

	^
	[| b c blas |
	self class = aLapackMatrix class ifTrue: [aLapackMatrix fillOtherTriangle].
	b := aLapackMatrix asGeneralMatrix.
	c := b class allocateNrow: nrow ncol: aLapackMatrix ncol.
	blas := self blasInterface.
	blas 
		hemmWithSide: blas left
		Uplo: (self isUpper ifTrue: [blas upper] ifFalse: [blas lower])
		M: b nrow
		N: b ncol
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

reciprocal

scaledByComplex: aComplex 

setArray: anArray nrow: nr ncol: nc 

singularValueDecomposition

smallLowerTriangle: ind 
	"return lower triangle matrix"

	(ind <= 0 and: [self isLower]) ifFalse: [self fillOtherTriangle].
	^(self castTo: self class generalMatrix) smallLowerTriangle: ind!

smallUpperTriangle: ind 
	"return upper triangle matrix"

	(ind >= 0 and: [self isUpper]) ifFalse: [self fillOtherTriangle].
	^(self castTo: self class generalMatrix) smallUpperTriangle: ind!

sumWithLapackMatrix: aLapackMatrix 

transposeConjugated

transposed

upperTriangle: ind 
!LapackUnpackedHermitianMatrix categoriesFor: #absMax!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #beBothUpperLower!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #coerceToGeneralMatrix!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #coercingFromComplexNumber:do:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #complementOfRow:column:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #diagonalAt:!accessing-submatrix!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #differenceWithLapackMatrix:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #eigenValueDecomposition!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #fillLowerTriangleLapack!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #fillLowerTriangleNaive!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #fillOtherTriangle!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #fillRandNormalWithSeed:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #fillRandUniformWithSeed:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #fillUpperTriangleLapack!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #fillUpperTriangleNaive!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #inPlaceSolve:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #isBothUpperLower!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #isHermitian!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #isSymmetric!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #lowerTriangle:!accessing-submatrix!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #norm1!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #normFrobenius!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #normInfinity!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #pluDecomposition!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #productMatrixAtRightWithMatrix:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #productMatrixTransposeWithColumnVector:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #productMatrixWithColumnVector:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #productMatrixWithMatrix:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #reciprocal!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #scaledByComplex:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #setArray:nrow:ncol:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #singularValueDecomposition!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #smallLowerTriangle:!accessing-submatrix!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #smallUpperTriangle:!accessing-submatrix!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #sumWithLapackMatrix:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #transposeConjugated!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #transposed!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #upperTriangle:!accessing-submatrix!public! !

!LapackUnpackedHermitianMatrix class methodsFor!

fromGeneralMatrix: aMatrix 

nrow: nr ncol: nc withAll: aNumber 
!LapackUnpackedHermitianMatrix class categoriesFor: #fromGeneralMatrix:!public! !
!LapackUnpackedHermitianMatrix class categoriesFor: #nrow:ncol:withAll:!public! !
