"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedMatrix subclass: #LapackUnpackedTriangularMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedTriangularMatrix guid: (GUID fromString: '{67E2F288-EAC8-4EA7-9B35-DC48C15391C1}')!
LapackUnpackedTriangularMatrix comment: 'LapackUnpackedTriangularMatrix is an abstract class for matrices having triangular property:
	(M rowAt: i columnAt: j) = 0 for j > i for a lower triangular.
	(M rowAt: i columnAt: j) = 0 for i > j for a upper triangular.

Only half triangle and diagonal need being stored.
Variable uplo does indicate which one, upper, lower.

Subclasses are specialized for containing either real or complex numbers with a specific floating point format.
'!
!LapackUnpackedTriangularMatrix categoriesForClass!Smallapack-Matrix! !
!LapackUnpackedTriangularMatrix methodsFor!

absMax
	^
	[self lapackInterface 
		lantrWithnorm: self lapackInterface maxAbs
		uplo: (self isUpper 
				ifTrue: [self lapackInterface upper]
				ifFalse: [self lapackInterface lower])
		diag: self lapackInterface nonUnit
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super absMax]!

coerceToGeneralMatrix
	"Implementation Notes: a is already set to zero, we need only updating upper or lower"

	^
	[| b |
	b := self class generalMatrix nrow: nrow ncol: ncol.
	self lapackInterface 
		lacpyWithuplo: (self isUpper 
				ifTrue: [self lapackInterface upper]
				ifFalse: [self lapackInterface lower])
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow
		b: b asParameter
		ldb: b nrow.
	b] 
			on: Error
			do: [:exc | exc return: super coerceToGeneralMatrix]!

complementOfRow: i column: j 	^0!

diagonalAt: index 	| diag |	diag := self class nrow: (self diagonalSizeAt: index).	(index >= 0 and: [self isUpper]) 		ifTrue: 			[diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index * nrow)				sourceIncrement: nrow + 1				destIncrement: 1].	(index <= 0 and: [self isLower]) 		ifTrue: 			[diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index negated)				sourceIncrement: nrow + 1				destIncrement: 1].	^diag!

differenceWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class.	but are they both upper / lower ?"	| res |	res := self copy.	res 		fill: self size		elementsWithStride: 1		withSelfPlusScalar: -1		timesVector: aLapackMatrix		stride: 1.	(self isUpper xor: aLapackMatrix isUpper) 		ifTrue: [res := res castTo: self class generalMatrix].	^res!

eigenValues	^self diagonal!

fillOtherTriangle	"nothing to do, because the other triangle is set to zero by default"	^self!

inPlaceSolve: b 
	"find x such that self*x=b, that is solve a linear system of equations
	BEWARE: this will destroy matrices self and b
	on return:
		x is stored in b on return
		a contains L and U of P*L*U decomposition (diagonal 1 of L not included)
		info contains error code if not 0"

	| info |
	info := self lapackInterface 
				trtrsWithuplo: (self isUpper 
						ifTrue: [self lapackInterface upper]
						ifFalse: [self lapackInterface lower])
				trans: self lapackInterface notTransposed
				diag: self lapackInterface nonUnit
				n: nrow
				nrhs: b ncol
				a: self asParameter
				lda: nrow
				b: b asParameter
				ldb: b nrow.
	info = 0 ifFalse: [self error: 'inversion failed'].
	^b!

isLowerTriangular	^self isLower or: [self isDiagonal]!

isTriangular	^true!

isUpperTriangular	^self isUpper or: [self isDiagonal]!

lowerTriangle: ind 
	"return lower triangle matrix"

	^
	[self isLower 
		ifTrue: 
			[ind >= 0 
				ifTrue: [self]
				ifFalse: 
					[| b offset |
					offset := ind negated.
					b := self class triangularMatrix nrow: nrow ncol: ncol.
					b beLower.
					self lapackInterface 
						lacpyWithuplo: self lapackInterface lower
						m: nrow + ind
						n: ncol + ind
						a: (self asParameterWithOffset: offset)
						lda: nrow
						b: (b asParameterWithOffset: offset)
						ldb: b nrow.
					b]]
		ifFalse: 
			[ind = 0 
				ifTrue: 
					[| b |
					b := self class triangularMatrix nrow: nrow ncol: ncol.
					b beLower.
					b setDiagonal: self diagonal.
					b]
				ifFalse: [self zero]]] 
			on: Error
			do: [:exc | exc return: (super lowerTriangle: ind)]!

norm1
	^
	[self lapackInterface 
		lantrWithnorm: self lapackInterface norm1
		uplo: (self isUpper 
				ifTrue: [self lapackInterface upper]
				ifFalse: [self lapackInterface lower])
		diag: self lapackInterface nonUnit
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super norm1]!

normFrobenius
	^
	[self lapackInterface 
		lantrWithnorm: self lapackInterface normF
		uplo: (self isUpper 
				ifTrue: [self lapackInterface upper]
				ifFalse: [self lapackInterface lower])
		diag: self lapackInterface nonUnit
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super normFrobenius]!

normInfinity
	^
	[self lapackInterface 
		lantrWithnorm: self lapackInterface normI
		uplo: (self isUpper 
				ifTrue: [self lapackInterface upper]
				ifFalse: [self lapackInterface lower])
		diag: self lapackInterface nonUnit
		m: nrow
		n: ncol
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super normInfinity]!

pluDecomposition
	^LapackTriangularPLUdecomposition decompose: self!

productMatrixAtRightWithMatrix: aLapackMatrix 
	"the other matrix must have conforming precision and complexity"

	^
	[| b blas |
	b := (aLapackMatrix class = self class 
				and: [aLapackMatrix isUpper == self isUpper]) 
					ifTrue: 
						["Product of two upper triangular is upper triangular.
						Same for two lower."

						aLapackMatrix copy]
					ifFalse: 
						["Else, no known property, answer a generalMatrix"

						aLapackMatrix copy castTo: aLapackMatrix class generalMatrix].
	blas := self blasInterface.
	blas 
		trmmWithSide: blas right
		Uplo: (self isUpper ifTrue: [blas upper] ifFalse: [blas lower])
		TransA: blas notTransposed
		Diag: blas nonUnit
		M: b nrow
		N: b ncol
		alpha: 1
		A: self asParameter
		lda: nrow
		B: b asParameter
		ldb: b nrow.
	b] 
			on: Error
			do: [:exc | exc return: (super productMatrixAtRightWithMatrix: aLapackMatrix)]!

productMatrixTransposeWithColumnVector: aLapackMatrix 
	"the vector must have conforming precision and complexity"

	^
	[| x blas |
	x := aLapackMatrix copy.
	blas := self blasInterface.
	blas 
		trmvWithUplo: (self isUpper 
				ifTrue: [blas upper]
				ifFalse: [blas lower])
		TransA: blas transposed
		Diag: blas nonUnit
		N: x size
		A: self asParameter
		lda: nrow
		X: x asParameter
		incX: 1.
	x] 
			on: Error
			do: [:exc | exc return: (super productMatrixTransposeWithColumnVector: aLapackMatrix)]!

productMatrixWithColumnVector: aLapackMatrix 
	"the vector must have conforming precision and complexity"

	^
	[| x blas |
	x := aLapackMatrix copy.
	blas := self blasInterface.
	blas 
		trmvWithUplo: (self isUpper ifTrue: [blas upper] ifFalse: [blas lower])
		TransA: blas notTransposed
		Diag: blas nonUnit
		N: x size
		A: self asParameter
		lda: nrow
		X: x asParameter
		incX: 1.
	x] 
			on: Error
			do: [:exc | exc return: (super productMatrixWithColumnVector: aLapackMatrix)]!

productMatrixWithMatrix: aLapackMatrix 
	"the other matrix must have conforming precision and complexity"

	^
	[| b blas |
	b := (aLapackMatrix class = self class 
				and: [aLapackMatrix isUpper == self isUpper]) 
					ifTrue: 
						["Product of two upper triangular is upper triangular.
						Same for two lower."

						aLapackMatrix copy]
					ifFalse: 
						["Else, no known property, answer a generalMatrix"

						aLapackMatrix copy castTo: aLapackMatrix class generalMatrix].
	blas := self blasInterface.
	blas 
		trmmWithSide: blas left
		Uplo: (self isUpper ifTrue: [blas upper] ifFalse: [blas lower])
		TransA: blas notTransposed
		Diag: blas nonUnit
		M: b nrow
		N: b ncol
		alpha: 1
		A: self asParameter
		lda: nrow
		B: b asParameter
		ldb: b nrow.
	b] 
			on: Error
			do: [:exc | exc return: (super productMatrixWithMatrix: aLapackMatrix)]!

reciprocal
	^
	[| inv |
	inv := self copy.
	self lapackInterface 
		trtriWithuplo: (self isUpper 
				ifTrue: [self lapackInterface upper]
				ifFalse: [self lapackInterface lower])
		diag: self lapackInterface nonUnit
		n: ncol
		a: inv asParameter
		lda: nrow.
	inv] 
			on: Error
			do: [:exc | exc return: super reciprocal]!

sumWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class.	but are they both upper / lower ?"	| res |	res := self copy.	res 		fill: self size		elementsWithStride: 1		withSelfPlusScalar: 1		timesVector: aLapackMatrix		stride: 1.	(self isUpper xor: aLapackMatrix isUpper) 		ifTrue: [res := res castTo: self class generalMatrix].	^res!

transposed
	| res |
	nrow = 1 ifTrue: [^self].
	res := self class allocateNrow: ncol ncol: nrow.
	self isUpper
		ifTrue:
			[res beLower.
			0 to: nrow - 1 do: [:i |
				res copy: ncol - i elementsFrom: self sourceOffset: i * nrow + i sourceIncrement: nrow destOffset: i * ncol + i destIncrement: 1 ] ]
		ifFalse: 
			[res beUpper.
			0 to: ncol - 1 do: [:i |
				res copy: nrow - i elementsFrom: self sourceOffset: i * nrow + i sourceIncrement: 1 destOffset: i * ncol + i destIncrement: ncol ] ].
	^res!

upperTriangle: ind 
	"return upper triangle matrix"

	^
	[self isUpper
		ifTrue: 
			[ind <= 0 
				ifTrue: [self]
				ifFalse: 
					[| b offset |
					offset := ind * nrow.
					b := self class triangularMatrix nrow: nrow ncol: ncol.
					b beUpper.
					self lapackInterface 
						lacpyWithuplo: self lapackInterface upper
						m: nrow - ind
						n: ncol - ind
						a: (self asParameterWithOffset: offset)
						lda: nrow
						b: (b asParameterWithOffset: offset)
						ldb: b nrow.
					b]]
		ifFalse: 
			[ind = 0 
				ifTrue: 
					[| b |
					b := self class triangularMatrix nrow: nrow ncol: ncol.
					b beUpper.
					b setDiagonal: self diagonal.
					b]
				ifFalse: [self zero]]] 
			on: Error
			do: [:exc | exc return: (super upperTriangle: ind)]! !
!LapackUnpackedTriangularMatrix categoriesFor: #absMax!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #coerceToGeneralMatrix!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #complementOfRow:column:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #diagonalAt:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #differenceWithLapackMatrix:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #eigenValues!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #fillOtherTriangle!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #inPlaceSolve:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #isLowerTriangular!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #isTriangular!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #isUpperTriangular!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #lowerTriangle:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #norm1!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #normFrobenius!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #normInfinity!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #pluDecomposition!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #productMatrixAtRightWithMatrix:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #productMatrixTransposeWithColumnVector:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #productMatrixWithColumnVector:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #productMatrixWithMatrix:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #reciprocal!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #sumWithLapackMatrix:!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #transposed!public! !
!LapackUnpackedTriangularMatrix categoriesFor: #upperTriangle:!public! !

!LapackUnpackedTriangularMatrix class methodsFor!

fromGeneralMatrix: aMatrix 	| new |	^aMatrix isLowerTriangular 		ifTrue: 			[new := aMatrix castTo: self.			new beLower.			new]		ifFalse: 			[aMatrix isUpperTriangular 				ifTrue: 					[new := aMatrix castTo: self.					new beUpper.					new]				ifFalse: [aMatrix]]!

nrow: nr ncol: nc withAll: aNumber 	"cannot be triangular if not all zero"	| aMatrix |	aMatrix := self generalMatrix 				nrow: nr				ncol: nc				withAll: aNumber.	(aNumber isZero and: [aMatrix isSquare])		ifTrue: 			[aMatrix := aMatrix castTo: self.			aMatrix beUpper].	^aMatrix! !
!LapackUnpackedTriangularMatrix class categoriesFor: #fromGeneralMatrix:!public! !
!LapackUnpackedTriangularMatrix class categoriesFor: #nrow:ncol:withAll:!public! !

