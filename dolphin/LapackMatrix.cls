"Filed out from Dolphin Smalltalk 7"!

AbstractMatrix subclass: #LapackMatrix
	instanceVariableNames: ''
	classVariableNames: 'ArrayInterfaces BandStorageMask BlasInterfaces BothUpperLowerMask CArrayClasses ComplexityMask ComplexMask DefaultSeedArray DiagonalMask DoublePrecisionMask FlagsToClassDictionary FullStorageMask GeneralMask HermitianMask LapackInterfaces LowerMask PackedStorageMask PrecisionMask PropertyMask RealMask SDCZMask SinglePrecisionMask SmalltalkArrayClasses StorageMask TriangularMask UpperMask'
	poolDictionaries: ''
	classInstanceVariableNames: 'libraryErrorCollection smalltalkAccessError cAccessError flags'!
LapackMatrix guid: (GUID fromString: '{D905FA3C-CE43-451B-ADC0-5771DDFB0B7E}')!
LapackMatrix comment: 'LapackMatrix can use special CArrayAccessor operating on an underlying CPointer in place of regular array.
This is in order to access fast external libraries (BLAS LAPACK)

In order to survive through image snapshots, array can also be stored in the Smalltalk space (a RawArray) .
Commutations of arrays are done through protocol #cArray, #storeInSmalltalkSpace.
There is a small risk in multi-process that the Matrix no more point to its array (if snapshot occurs at critical point).

Flags for handling generallity and coercing :
We want to be able to add or multiply a double complex hermitian matrix with a triangular single precision real matrix.
Double dispatching is not the right solution due to numerous subclasses involved !!
Generality is not well fitted either, because there are 4 fields of generality :
	Precision (double , single)
	Complexity (complex , real)
	Property (general , triangular , hermitian)
	Storage (dense, packed , banded)

Solution is to use bit flags for each field.
The more general class has flags set to 1, the less general has flags set to 0.
For property, this is less clear because adding a triangular and a hermitian should result in general matrices...
The flags are arranged so that they should be bitOr:''ed to get flags of resulting class for most operations...

The flags are cached in a class instance variable. Current implementation is:
	bit 0: floating point precision (single=0 double=1)
	bit 1: complexity (real=0 complex=1)
	bit 2,3,4 : property (general=111 triangular=101 hermitian=011) there is room for 110...
	bit 5,6: storage (11=full 10=band 01=packed) 00 not attributed... room for tri-diagonal?

External DLL functions of BLAS/LAPACK library are called for performing arithmetic and algebra algorithm.
For instance, the blas function to copy a vector times a scalar is named AXPY.
Depending on the precision and complexity, either the CAXPY, DAXPY, SAXPY or ZAXPY function must be called.
These functions take the same arguments.
So an easy way to factor code is to create 4 subclasses of the blas library, one for each precision/complexity pair.
Then, the correct subclass must be used depending on Matrix class.
This is implemented by caching the interfaces into a shared variable accessed with #sdczIndex indirection.

Another possibility is to call CBLAS, a version of BLAS with C-like interface (passing values instead of pointers).
If one changes the Settings (using CBLAS or not), then the (LapackMatrix resetBlasInterfaces) must be called.

Some Exception can occur in case one try to store a Double into a SinglePrecisionMatrix.
Some other if a library is not found.
These exceptions are protected with exception handling.
The class of these exceptions are cached into some class instance variable for efficiency.
'!
!LapackMatrix categoriesForClass!Unclassified! !
!LapackMatrix methodsFor!

- aMatrix

* aMatrix

, aMatrix 

,, aMatrix 

/ aMatrix

\ aMatrix

+ aMatrix

absMax

arrayPointer

arrayPointerWithOffset: anOffset

asAbstractMatrix

asColumnMatrix

asComplexMatrix

asDoublePrecisionComplexMatrix

asDoublePrecisionMatrix

asGeneralMatrix

asPackedMatrix

asParameter
	"Answer a pointer on an array directly usable in C primitives..."

	^self cArray asParameter!

asParameterWithOffset: anOffset
	"Answer a pointer on an array directly usable in C primitives...
	The offset is given in number of elements, not in bytes."

	^(self cArray withArrayOffsetBy: anOffset) asParameter!

asRowMatrix

asSinglePrecisionComplexMatrix

asSinglePrecisionMatrix

asUnpackedMatrix

at: anInteger put: aNumber 

at: anInteger put: aNumber handle: exceptionBlock 

atAllPut: aNumber 

atIntervalFrom: interStart to: interStop by: interStep 

blasInterface

cArray
	"Make sure the array is member of CArrayAccessor,
	and answer this array.
	This transfer is needed before calling an external library... "


	| theArray cArray  |

	"Implementation Note: this is an atomic multi thread safe implementation
	use a temporary theArray, do not use inst.var. array, nor other isInCSpace utility function
	because it might have changed before return in a mult-threaded environment"
	^(theArray := array) class == self class cArrayClass 
		ifTrue: [theArray]
		ifFalse: 
			[theArray class = self class smalltalkArrayClass 
				ifTrue: 
					["we can use optimized primitive in this case"

					cArray := self class cArrayClass new: theArray size.
					cArray replaceFrom: 1 to: theArray size  with: theArray startingAt: 1]
				ifFalse: 
					["one did not use the optimized smalltalkArrayClass
					Fall back to slower elementwise copy"

					cArray := self class cArrayClass withAll: theArray].
			array := cArray]!

castTo: aMatrixClass 

castToColumn

castToRealWithArrayOffsetBy: aPositiveInteger 

castToRow

coerceFlags: destFlags 

coerceFlagsButProperty: destFlags 

coerceToComplexMatrix
	Fast implementation: copy every two reals"

coerceToDoublePrecisionComplexMatrix

coerceToDoublePrecisionMatrix
	"Try FAST primitive and if fail revert to SLOW naive implementation"

	^
	[| res |
	res := self class doublePrecisionMatrix allocateNrow: nrow ncol: ncol.
	res class isComplexMatrix 
		ifTrue: 
			[self class realMatrix arrayInterface 
				arraystodWithn: self arraySize * 2
				source: (self cArray castToRealWithArrayOffsetBy: 0) asParameter
				result: (res cArray castToRealWithArrayOffsetBy: 0) asParameter]
		ifFalse: 
			[self class arrayInterface 
				arraystodWithn: self arraySize
				source: self asParameter
				result: res asParameter].
	res] 
			on: Error
			do: [:exc | exc return: self naiveCoerceToDoublePrecisionMatrix]!

coerceToGeneralMatrix

coerceToSinglePrecisionComplexMatrix

coerceToSinglePrecisionMatrix
	"Try FAST primitive and if fail revert to SLOW naive implementation"

	^
	[| res |
	res := self class singlePrecisionMatrix allocateNrow: nrow ncol: ncol.
	res class isComplexMatrix 
		ifTrue: 
			[self class realMatrix arrayInterface 
				arraydtosWithn: self arraySize * 2
				source: (self cArray castToRealWithArrayOffsetBy: 0) asParameter
				result: (res cArray castToRealWithArrayOffsetBy: 0) asParameter]
		ifFalse: 
			[self class arrayInterface 
				arraydtosWithn: self arraySize
				source: self asParameter
				result: res asParameter].
	res] 
			on: Error
			do: [:exc | exc return: self naiveCoerceToSinglePrecisionMatrix]!

coercing: aMatrix do: aBlock 

coercingButPropertyFromLapackMatrix: aLapackMatrix do: aBlock 

coercingFromComplexNumber: aComplex do: aBlock 

coercingFromLapackMatrix: aLapackMatrix do: aBlock 

coercingFromNumber: aNumber do: aBlock 

coercingFromRealNumber: aFloat do: aBlock 

concatColumnsFromLapackMatrix: aLapackMatrix

concatColumnsWithLapackMatrix: aMatrix 

concatRowsFromLapackMatrix: aLapackMatrix

concatRowsWithLapackMatrix: aMatrix 

conjugated
	"Try lapack or revert to super slow naive implementation.
	Note: this should also work with packed forms thanks to arraySize"

	^self class isComplexMatrix 
		ifTrue: 
			[
			[| res |
			res := self copy.
			self lapackInterface 
				lacgvWithn: self arraySize
				x: res asParameter
				incx: 1.
			res] 
					on: Error
					do: [:exc | exc return: super conjugated]]
		ifFalse: [self]!

copy

copy: n elementsFrom: aLapackMatrix sourceOffset: offx sourceIncrement: incx destOffset: offy destIncrement: incy 
	(offx >= 0 and: [n - 1 * incx abs + offx < aLapackMatrix size]) 
		ifFalse: [^self error: 'matrix access out of bounds'].
	(offy >= 0 and: [n - 1 * incy abs + offy < self size]) 
		ifFalse: [^self error: 'matrix access out of bounds'].
	
	[self blasInterface 
		copyWithN: n
		X: (aLapackMatrix asParameterWithOffset: offx)
		incX: incx
		Y: (self asParameterWithOffset: offy)
		incY: incy] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							copy: n
							elementsFrom: aLapackMatrix
							sourceOffset: offx
							sourceIncrement: incx
							destOffset: offy
							destIncrement: incy)]!

copy: m rowsStartingAt: ir and: n columnsStartingAt: jc from: a 
	
	[self lapackInterface 
		lacpyWithuplo: self lapackInterface notTransposed
		m: m
		n: n
		a: a asParameter
		lda: a nrow
		b: (self asParameterWithOffset: (self arrayOffsetAtRow: ir atColumn: jc))
		ldb: nrow] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							copy: m
							rowsStartingAt: ir
							and: n
							columnsStartingAt: jc
							from: a)]!

defaultTolerance

determinant

differenceFromLapackMatrix: aLapackMatrix 

differenceWithLapackMatrix: aLapackMatrix 

dotProduct: n elementsIncrement: incx with: aMatrix increment: incy 
	
	^[self blasInterface 
		dotWithN: n
		X: self asParameter
		incX: incx
		Y: aMatrix asParameter
		incY: incy] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							dotProduct: n
							elementsIncrement: incx
							with: aMatrix
							increment: incy)]!

eigenValueDecomposition

eigenValues

fill: n elementsWithStride: incy withSelfPlusScalar: alpha timesVector: aMatrix stride: incx 
	"Fill n elements of self with a Scalar*Vector product
		alpha*aMatrix + self
	This is the xAXPY BLAS operation"

	
	[self blasInterface 
		axpyWithN: n
		alpha: alpha
		X: aMatrix asParameter
		incX: incx
		Y: self asParameter
		incY: incy] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							fill: n
							elementsWithStride: incy
							withSelfPlusScalar: alpha
							timesVector: aMatrix
							stride: incx)]!

fillM: m byN: n withScalar: alpha timesColumnVector: x stride: incx timesRowVector: y stride: incy 
	"fill self with a Vector*transpose(Vector) product
		alpha*x*transpose(y)"
 
	[self blasInterface 
		gerWithM: m
		N: n
		alpha: alpha
		X: x asParameter
		incX: incx
		Y: y asParameter
		incY: incy
		A: self asParameter
		lda: nrow] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							fillM: m
							byN: n
							withScalar: alpha
							timesColumnVector: x
							stride: incx
							timesRowVector: y
							stride: incy)]!

fillRandNormal

fillRandNormalWithSeed: seedArray 
	"On return, the seed array is changed"

	| directAccess iseed |
	directAccess := seedArray class = SDWORDArray and: 
					[seedArray size = 4].
	directAccess 
		ifTrue: [iseed := seedArray]
		ifFalse: 
			[(iseed := SDWORDArray new: 4) 
				replaceFrom: 1
				to: 4
				with: seedArray].
	self lapackInterface 
		larnvWithidist: self lapackInterface normal01
		iseed: iseed "asParameter"
		n: self arraySize
		x: self asParameter.
	directAccess 
		ifFalse: 
			[seedArray 
				replaceFrom: 1
				to: 4
				with: iseed]!

fillRandUniform

fillRandUniformWithSeed: seedArray 
	"On return, the seed array is changed"

	| directAccess iseed |
	directAccess := seedArray class = SDWORDArray and: 
					[seedArray size = 4].
	directAccess 
		ifTrue: [iseed := seedArray]
		ifFalse: 
			[(iseed := SDWORDArray new: 4) 
				replaceFrom: 1
				to: 4
				with: seedArray].
	self lapackInterface 
		larnvWithidist: self lapackInterface uniform01
		iseed: iseed "asParameter"
		n: self arraySize
		x: self asParameter.
	directAccess 
		ifFalse: 
			[seedArray 
				replaceFrom: 1
				to: 4
				with: iseed]!

fromColumn: jStart toColumn: jStop by: jStep 
	"Trick: use lapack auxiliary matrix copy with leading dimension augmented"

	jStep positive 
		ifFalse: 
			[^super 
				fromColumn: jStart
				toColumn: jStop
				by: jStep].
	^
	[| nc res |
	nc := (jStart to: jStop by: jStep) size.
	res := self class allocateNrow: nrow ncol: nc.
	self lapackInterface 
		lacpyWithuplo: self lapackInterface notTransposed
		m: nrow
		n: nc
		a: (self asParameterWithOffset: (jStart - 1) * nrow)
		lda: nrow * jStep
		b: res asParameter
		ldb: nrow.
	res] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							fromColumn: jStart
							toColumn: jStop
							by: jStep)]!

generalizedEigenValueDecompositionWithRHSMatrix: aMatrix 
	"solve a generalized eigenvalue problem"

	^self asGeneralMatrix 
		generalizedEigenValueDecompositionWithRHSMatrix: aMatrix!

hessenbergDecomposition
	"hessenberg decomposition"

	nrow = ncol 
		ifFalse: 
			[^self error: 'hessenberg decomposition apply only on square Matrices'].
	^LapackHessenbergDecomposition decompose: self asGeneralMatrix!

imaginaryPart

inPlaceScaledByComplex: aComplex 
	"Scale self in place (modify self, not a copy).
	aComplex and self should agree on precision before sending this message."

	
	[self blasInterface 
		scalWithN: self arraySize
		alpha: aComplex
		X: self asParameter
		incX: 1] 
			on: Error
			do: 
				[:exc | 
				exc retryUsing: 
						[self 
							scale: self size
							elementsBy: aComplex
							increment: 1]]!

inPlaceScaledByNumber: aNumber 
	"Scale self in place (modify self, not a copy).
	aNumber and self should agree on precision before sending this message."

	
	[self class isComplexMatrix 
		ifTrue: 
			[self blasInterface 
				realScalWithN: self arraySize
				alpha: aNumber
				X: self asParameter
				incX: 1]
		ifFalse: 
			[self blasInterface 
				scalWithN: self arraySize
				alpha: aNumber
				X: self asParameter
				incX: 1]] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(self 
							scale: self size
							elementsBy: aNumber
							increment: 1)]!

inPlaceSolve: b 
	"find x such that self*x=b, that is solve a linear system of equations
	BEWARE: this will destroy matrices self and b
	on return:
		x is stored in b on return
		self contains L and U of P*L*U decomposition (diagonal 1 of L not included)
		ipiv contains permutations of rows P of P*L*U decomposition
		info contains error code if not 0"

	| ipiv info |
	ipiv := SDWORDArray new: nrow.
	info := self lapackInterface 
				gesvWithn: nrow
				nrhs: b ncol
				a: self asParameter
				lda: nrow
				ipiv: ipiv asParameter
				b: b asParameter
				ldb: b nrow.
	info = 0 ifFalse: [self error: 'inversion failed'].
	^b!

isBandMatrix

isComplexMatrix

isDiagonalMatrix

isDoublePrecisionMatrix

isGeneralMatrix

isHermitianMatrix

isInCSpace

isPackedMatrix

isRealMatrix

isSinglePrecisionMatrix

isSymmetricMatrix

isTriangularMatrix

isUnpackedMatrix

lapackInterface

leftDivideFromLapackMatrix: aLapackMatrix 

leftDivideWithLapackMatrix: aLapackMatrix 
	"Answer the result of division
		self \ aLapackMatrix
	NAIVE implementation.
	Should be coded more efficiently with direct DGESV or DGELSS calls ..."

	^self isSquare 
		ifTrue: [self reciprocal * aLapackMatrix]
		ifFalse: 
			["self pseudoInverse * aLapackMatrix"

			| solver |
			solver := LapackLeastSquareProblem new matrix: self asGeneralMatrix
						rhsMatrix: aLapackMatrix asGeneralMatrix.
			solver solution]!

multiplyByFloat: aFloat 

multiplyByFraction: aFraction

multiplyByInteger: anInteger 

multiplyByScaledDecimal: aFixedPoint 
	^aFixedPoint asFloat coercing: self
		do: [:num :mat | mat scaledByNumber: num]!

naiveCoerceToDoublePrecisionMatrix

naiveCoerceToSinglePrecisionMatrix

negated

norm1

normFrobenius

normInfinity

pluDecomposition

postCoercingFromLapackMatrix: aLapackMatrix do: aBlock 

postCopy

productColumnVectorWithRowVector: aLapackMatrix 

productFromComplex: aComplex 

productFromDouble: aDouble 

productFromFixedPoint: aFixedPoint 

productFromFloat: aFloat 

productFromFraction: aFraction 

productFromInteger: anInteger 

productFromLapackMatrix: aLapackMatrix 

productRowVectorWithColumnVector: aLapackMatrix 

productRowVectorWithMatrix: aMatrix 

productWithLapackMatrix: aLapackMatrix 

pseudoInverse

pseudoInverseTolerance: tol 

qrDecomposition
	"QR factorization"

	^LapackQRdecomposition decompose: self asGeneralMatrix!

qrpDecomposition
	"QR factorization with column pivoting"

	^LapackQRPdecomposition decompose: self asGeneralMatrix!

quotientFromLapackMatrix: aLapackMatrix 

rank

rankTolerance: tol 

realPart

reciprocal

reduceGeneralityIfPossible

rightDivideWithLapackMatrix: aLapackMatrix 
	"Answer the result of division
		self / aLapackMatrix
	NAIVE implementation.
	Should be coded more efficiently with direct DGESV or DGELSS calls ..."

	self isSquare 
		ifTrue: [self * aLapackMatrix reciprocal]
		ifFalse: 
			["self * aLapackMatrix pseudoInverse"

			| solver |
			solver := LapackLeastSquareProblem new 
						matrix: aLapackMatrix asGeneralMatrix transposed
						rhsMatrix: self asGeneralMatrix transposed.
			solver solution transposed]!

scaledByComplex: aComplex 

scaledByNumber: aNumber 

schurDecomposition
	^self isRealMatrix 
		ifTrue: [LapackRealSchurDecomposition decompose: self asGeneralMatrix]
		ifFalse: [LapackComplexSchurDecomposition decompose: self asGeneralMatrix]!

setOffDiagonal: alpha diagonal: beta 
	
	[self lapackInterface 
		lasetWithuplo: self lapackInterface notTransposed
		m: nrow
		n: ncol
		alpha: alpha
		beta: beta
		a: self asParameter
		lda: nrow] 
			on: Error
			do: [:exc | exc return: (self naiveSetOffDiagonal: alpha diagonal: beta)]!

singularValueDecomposition
	^LapackSVDecomposition decompose: self asGeneralMatrix!

singularValues

solve: aMatrix 

storeInSmalltalkSpace

sumFromLapackMatrix: aLapackMatrix 

sumWithLapackMatrix: aLapackMatrix 

swapColumn: j1 withColumn: j2 

swapRow: i1 withRow: i2 

transposeConjugated

withArrayOffsetBy: aPositiveInteger 
!LapackMatrix categoriesFor: #-!arithmetic!public! !
!LapackMatrix categoriesFor: #*!arithmetic!public! !
!LapackMatrix categoriesFor: #,!concatenating!public! !
!LapackMatrix categoriesFor: #,,!concatenating!public! !
!LapackMatrix categoriesFor: #/!arithmetic!public! !
!LapackMatrix categoriesFor: #\!arithmetic!public! !
!LapackMatrix categoriesFor: #+!arithmetic!public! !
!LapackMatrix categoriesFor: #absMax!norm!public! !
!LapackMatrix categoriesFor: #arrayPointer!accessing-storage!public! !
!LapackMatrix categoriesFor: #arrayPointerWithOffset:!accessing-storage!public! !
!LapackMatrix categoriesFor: #asAbstractMatrix!converting!public! !
!LapackMatrix categoriesFor: #asColumnMatrix!converting!public! !
!LapackMatrix categoriesFor: #asComplexMatrix!converting!public! !
!LapackMatrix categoriesFor: #asDoublePrecisionComplexMatrix!converting!public! !
!LapackMatrix categoriesFor: #asDoublePrecisionMatrix!converting!public! !
!LapackMatrix categoriesFor: #asGeneralMatrix!converting!public! !
!LapackMatrix categoriesFor: #asPackedMatrix!converting!public! !
!LapackMatrix categoriesFor: #asParameter!converting!public! !
!LapackMatrix categoriesFor: #asParameterWithOffset:!converting!public! !
!LapackMatrix categoriesFor: #asRowMatrix!converting!public! !
!LapackMatrix categoriesFor: #asSinglePrecisionComplexMatrix!converting!public! !
!LapackMatrix categoriesFor: #asSinglePrecisionMatrix!converting!public! !
!LapackMatrix categoriesFor: #asUnpackedMatrix!converting!public! !
!LapackMatrix categoriesFor: #at:put:!accessing-elements!public! !
!LapackMatrix categoriesFor: #at:put:handle:!accessing-elements!public! !
!LapackMatrix categoriesFor: #atAllPut:!accessing-elements!public! !
!LapackMatrix categoriesFor: #atIntervalFrom:to:by:!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #blasInterface!accessing!public! !
!LapackMatrix categoriesFor: #cArray!accessing-storage!public! !
!LapackMatrix categoriesFor: #castTo:!coercing!public! !
!LapackMatrix categoriesFor: #castToColumn!coercing!public! !
!LapackMatrix categoriesFor: #castToRealWithArrayOffsetBy:!coercing!public! !
!LapackMatrix categoriesFor: #castToRow!coercing!public! !
!LapackMatrix categoriesFor: #coerceFlags:!coercing!public! !
!LapackMatrix categoriesFor: #coerceFlagsButProperty:!coercing!public! !
!LapackMatrix categoriesFor: #coerceToComplexMatrix!coercing!public! !
!LapackMatrix categoriesFor: #coerceToDoublePrecisionComplexMatrix!coercing!public! !
!LapackMatrix categoriesFor: #coerceToDoublePrecisionMatrix!coercing!public! !
!LapackMatrix categoriesFor: #coerceToGeneralMatrix!coercing!public! !
!LapackMatrix categoriesFor: #coerceToSinglePrecisionComplexMatrix!coercing!public! !
!LapackMatrix categoriesFor: #coerceToSinglePrecisionMatrix!coercing!public! !
!LapackMatrix categoriesFor: #coercing:do:!coercing!public! !
!LapackMatrix categoriesFor: #coercingButPropertyFromLapackMatrix:do:!coercing!public! !
!LapackMatrix categoriesFor: #coercingFromComplexNumber:do:!coercing!public! !
!LapackMatrix categoriesFor: #coercingFromLapackMatrix:do:!coercing!public! !
!LapackMatrix categoriesFor: #coercingFromNumber:do:!coercing!public! !
!LapackMatrix categoriesFor: #coercingFromRealNumber:do:!coercing!public! !
!LapackMatrix categoriesFor: #concatColumnsFromLapackMatrix:!concatenating!public! !
!LapackMatrix categoriesFor: #concatColumnsWithLapackMatrix:!concatenating!public! !
!LapackMatrix categoriesFor: #concatRowsFromLapackMatrix:!concatenating!public! !
!LapackMatrix categoriesFor: #concatRowsWithLapackMatrix:!concatenating!public! !
!LapackMatrix categoriesFor: #conjugated!arithmetic-complex!public! !
!LapackMatrix categoriesFor: #copy!copying!public! !
!LapackMatrix categoriesFor: #copy:elementsFrom:sourceOffset:sourceIncrement:destOffset:destIncrement:!blas!public! !
!LapackMatrix categoriesFor: #copy:rowsStartingAt:and:columnsStartingAt:from:!blas!public! !
!LapackMatrix categoriesFor: #defaultTolerance!accessing!public! !
!LapackMatrix categoriesFor: #determinant!arithmetic!public! !
!LapackMatrix categoriesFor: #differenceFromLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #differenceWithLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #dotProduct:elementsIncrement:with:increment:!blas!public! !
!LapackMatrix categoriesFor: #eigenValueDecomposition!decomposition!public! !
!LapackMatrix categoriesFor: #eigenValues!decomposition!public! !
!LapackMatrix categoriesFor: #fill:elementsWithStride:withSelfPlusScalar:timesVector:stride:!blas!public! !
!LapackMatrix categoriesFor: #fillM:byN:withScalar:timesColumnVector:stride:timesRowVector:stride:!blas!public! !
!LapackMatrix categoriesFor: #fillRandNormal!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #fillRandNormalWithSeed:!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #fillRandUniform!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #fillRandUniformWithSeed:!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #fromColumn:toColumn:by:!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #generalizedEigenValueDecompositionWithRHSMatrix:!decomposition!public! !
!LapackMatrix categoriesFor: #hessenbergDecomposition!decomposition!public! !
!LapackMatrix categoriesFor: #imaginaryPart!arithmetic-complex!public! !
!LapackMatrix categoriesFor: #inPlaceScaledByComplex:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #inPlaceScaledByNumber:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #inPlaceSolve:!arithmetic!public! !
!LapackMatrix categoriesFor: #isBandMatrix!public!testing! !
!LapackMatrix categoriesFor: #isComplexMatrix!public!testing! !
!LapackMatrix categoriesFor: #isDiagonalMatrix!public!testing! !
!LapackMatrix categoriesFor: #isDoublePrecisionMatrix!public!testing! !
!LapackMatrix categoriesFor: #isGeneralMatrix!public!testing! !
!LapackMatrix categoriesFor: #isHermitianMatrix!public!testing! !
!LapackMatrix categoriesFor: #isInCSpace!public!testing! !
!LapackMatrix categoriesFor: #isPackedMatrix!public!testing! !
!LapackMatrix categoriesFor: #isRealMatrix!public!testing! !
!LapackMatrix categoriesFor: #isSinglePrecisionMatrix!public!testing! !
!LapackMatrix categoriesFor: #isSymmetricMatrix!public!testing! !
!LapackMatrix categoriesFor: #isTriangularMatrix!public!testing! !
!LapackMatrix categoriesFor: #isUnpackedMatrix!public!testing! !
!LapackMatrix categoriesFor: #lapackInterface!accessing!public! !
!LapackMatrix categoriesFor: #leftDivideFromLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #leftDivideWithLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #multiplyByFloat:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #multiplyByFraction:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #multiplyByInteger:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #multiplyByScaledDecimal:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #naiveCoerceToDoublePrecisionMatrix!coercing!public! !
!LapackMatrix categoriesFor: #naiveCoerceToSinglePrecisionMatrix!coercing!public! !
!LapackMatrix categoriesFor: #negated!arithmetic!public! !
!LapackMatrix categoriesFor: #norm1!norm!public! !
!LapackMatrix categoriesFor: #normFrobenius!norm!public! !
!LapackMatrix categoriesFor: #normInfinity!norm!public! !
!LapackMatrix categoriesFor: #pluDecomposition!decomposition!public! !
!LapackMatrix categoriesFor: #postCoercingFromLapackMatrix:do:!coercing!public! !
!LapackMatrix categoriesFor: #postCopy!copying!public! !
!LapackMatrix categoriesFor: #productColumnVectorWithRowVector:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productFromComplex:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productFromDouble:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productFromFixedPoint:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productFromFloat:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productFromFraction:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productFromInteger:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productFromLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productRowVectorWithColumnVector:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productRowVectorWithMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #productWithLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #pseudoInverse!arithmetic!public! !
!LapackMatrix categoriesFor: #pseudoInverseTolerance:!arithmetic!public! !
!LapackMatrix categoriesFor: #qrDecomposition!decomposition!public! !
!LapackMatrix categoriesFor: #qrpDecomposition!decomposition!public! !
!LapackMatrix categoriesFor: #quotientFromLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #rank!accessing!public! !
!LapackMatrix categoriesFor: #rankTolerance:!accessing!public! !
!LapackMatrix categoriesFor: #realPart!arithmetic-complex!public! !
!LapackMatrix categoriesFor: #reciprocal!arithmetic!public! !
!LapackMatrix categoriesFor: #reduceGeneralityIfPossible!coercing!public! !
!LapackMatrix categoriesFor: #rightDivideWithLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #scaledByComplex:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #scaledByNumber:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #schurDecomposition!decomposition!public! !
!LapackMatrix categoriesFor: #setOffDiagonal:diagonal:!accessing-elements!public! !
!LapackMatrix categoriesFor: #singularValueDecomposition!decomposition!public! !
!LapackMatrix categoriesFor: #singularValues!decomposition!public! !
!LapackMatrix categoriesFor: #solve:!arithmetic!public! !
!LapackMatrix categoriesFor: #storeInSmalltalkSpace!accessing-storage!public! !
!LapackMatrix categoriesFor: #sumFromLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #sumWithLapackMatrix:!arithmetic-internal!public! !
!LapackMatrix categoriesFor: #swapColumn:withColumn:!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #swapRow:withRow:!accessing-submatrix!public! !
!LapackMatrix categoriesFor: #transposeConjugated!arithmetic-complex!public! !
!LapackMatrix categoriesFor: #withArrayOffsetBy:!accessing-storage!public! !

!LapackMatrix class methodsFor!

allocateNrow: nr ncol: nc 

arrayInterface

blasInterface

cAccessError

cArrayClass

columnMatrix

complexMatrix

diagonalMatrix

doublePrecisionMatrix

eye: anIntegerOrPointOrArrayOfInteger 

findClassWithFlags: someFlags 

flags

generalMatrix

hermitianMatrix

initialize
	"I have to be warned whenever a snapshot is going to take place.
	This is why i register myself as a dependent of ObjectMemory."

	"ObjectMemory
		removeDependent: LapackMatrix;
		addDependent: LapackMatrix."

	self initializeMasks.
	self initializeSeed.
	self initializeArrayClasses.
	[self resetBlasInterfaces] on: Win32Error do: [:exc |exc return: nil].
	[self resetLapackInterfaces] on: Win32Error do: [:exc |exc return: nil].
	[self resetArrayInterfaces] on: Win32Error do: [:exc |exc return: nil].
	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self!

initializeArrayClasses
	"Initialize some variables depending on precision/complexity.
	These variables are to be used with (self sdczIndex) indirection"

	CArrayClasses := Array new: 4.
	CArrayClasses at: 1 + SinglePrecisionMask + RealMask put: FLOATArray.
	CArrayClasses at: 1 + DoublePrecisionMask + RealMask put: DOUBLEArray.
	CArrayClasses at: 1 + SinglePrecisionMask + ComplexMask put: FLOATCOMPLEXArray.
	CArrayClasses at: 1 + DoublePrecisionMask + ComplexMask put: DOUBLECOMPLEXArray.

"Squeak implementation note: we use the same class to hold either C-space or Smalltalk-space arrays"
	SmalltalkArrayClasses := Array new: 4.
	SmalltalkArrayClasses at: 1 + SinglePrecisionMask + RealMask put: FLOATArray.
	SmalltalkArrayClasses at: 1 + DoublePrecisionMask + RealMask put: DOUBLEArray.
	SmalltalkArrayClasses at: 1 + SinglePrecisionMask + ComplexMask put: FLOATCOMPLEXArray.
	SmalltalkArrayClasses at: 1 + DoublePrecisionMask + ComplexMask put: DOUBLECOMPLEXArray.!

initializeClassInstVars

initializeFlags

initializeMasks

initializeSeed
	"This seed is used by LAPACK pseudo random generator
	It must contain four 12bit integers, the last should be even."

	| n |
	DefaultSeedArray := SWORDArray new: 4.
	n := Time millisecondClockValue.
	1 to: 4
		do: 
			[:i | 
			DefaultSeedArray at: i put: (n \\ (2 raisedTo: 12) bitOr: 1).
			n := n // (2 raisedTo: 12)]!

isBandMatrix

isComplexMatrix

isDiagonalMatrix

isDoublePrecisionMatrix

isFullMatrix

isGeneralMatrix

isHermitianMatrix

isPackedMatrix

isRealMatrix

isSinglePrecisionMatrix

isSymmetricMatrix

isTriangularMatrix

isUnpackedMatrix

lapackInterface

libraryError

nrow: nr ncol: nc withAll: aNumber 

onStartup
	"reset the ExternalLibrary nterfaces on image startup"

	[self resetBlasInterfaces] on: Win32Error do: [:exc | exc return: nil].
	[self resetLapackInterfaces] on: Win32Error do: [:exc | exc return: nil].
	[self resetArrayInterfaces] on: Win32Error do: [:exc | exc return: nil].!

packedMatrix

preSnapshot

randNormal: anIntegerOrPointOrArrayOfInteger 

randNormal: anIntegerOrPointOrArrayOfInteger withSeed: seedArray 

randUniform: anIntegerOrPointOrArrayOfInteger 

randUniform: anIntegerOrPointOrArrayOfInteger withSeed: seedArray 

realMatrix

resetArrayInterfaces
	ArrayInterfaces := Array new: 4.
	ArrayInterfaces at: 1 + SinglePrecisionMask + RealMask put: ArraySLibrary default.
	ArrayInterfaces at: 1 + DoublePrecisionMask + RealMask put: ArrayDLibrary default.
	ArrayInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: ArrayCLibrary default.
	ArrayInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: ArrayZLibrary default!

resetBlasInterfaces

resetLapackInterfaces

rowMatrix

sdczIndex

singlePrecisionMatrix

smalltalkAccessError

smalltalkArrayClass

storeInstancesInSmalltalkSpace

triangularMatrix

uninitialize
	SessionManager current removeEventsTriggeredFor: self!

uninitializeBeforeRemove
	"Note: this cannot be uninitialize, because we want it to be performed by each removed subclass"
	self unregisterFlags.

unpackedMatrix

unregisterFlags
!LapackMatrix class categoriesFor: #allocateNrow:ncol:!public! !
!LapackMatrix class categoriesFor: #arrayInterface!public! !
!LapackMatrix class categoriesFor: #blasInterface!public! !
!LapackMatrix class categoriesFor: #cAccessError!public! !
!LapackMatrix class categoriesFor: #cArrayClass!public! !
!LapackMatrix class categoriesFor: #columnMatrix!public! !
!LapackMatrix class categoriesFor: #complexMatrix!public! !
!LapackMatrix class categoriesFor: #diagonalMatrix!public! !
!LapackMatrix class categoriesFor: #doublePrecisionMatrix!public! !
!LapackMatrix class categoriesFor: #eye:!public! !
!LapackMatrix class categoriesFor: #findClassWithFlags:!public! !
!LapackMatrix class categoriesFor: #flags!public! !
!LapackMatrix class categoriesFor: #generalMatrix!public! !
!LapackMatrix class categoriesFor: #hermitianMatrix!public! !
!LapackMatrix class categoriesFor: #initialize!public! !
!LapackMatrix class categoriesFor: #initializeArrayClasses!public! !
!LapackMatrix class categoriesFor: #initializeClassInstVars!public! !
!LapackMatrix class categoriesFor: #initializeFlags!public! !
!LapackMatrix class categoriesFor: #initializeMasks!public! !
!LapackMatrix class categoriesFor: #initializeSeed!public! !
!LapackMatrix class categoriesFor: #isBandMatrix!public! !
!LapackMatrix class categoriesFor: #isComplexMatrix!public! !
!LapackMatrix class categoriesFor: #isDiagonalMatrix!public! !
!LapackMatrix class categoriesFor: #isDoublePrecisionMatrix!public! !
!LapackMatrix class categoriesFor: #isFullMatrix!public! !
!LapackMatrix class categoriesFor: #isGeneralMatrix!public! !
!LapackMatrix class categoriesFor: #isHermitianMatrix!public! !
!LapackMatrix class categoriesFor: #isPackedMatrix!public! !
!LapackMatrix class categoriesFor: #isRealMatrix!public! !
!LapackMatrix class categoriesFor: #isSinglePrecisionMatrix!public! !
!LapackMatrix class categoriesFor: #isSymmetricMatrix!public! !
!LapackMatrix class categoriesFor: #isTriangularMatrix!public! !
!LapackMatrix class categoriesFor: #isUnpackedMatrix!public! !
!LapackMatrix class categoriesFor: #lapackInterface!public! !
!LapackMatrix class categoriesFor: #libraryError!public! !
!LapackMatrix class categoriesFor: #nrow:ncol:withAll:!public! !
!LapackMatrix class categoriesFor: #onStartup!public! !
!LapackMatrix class categoriesFor: #packedMatrix!public! !
!LapackMatrix class categoriesFor: #preSnapshot!public! !
!LapackMatrix class categoriesFor: #randNormal:!public! !
!LapackMatrix class categoriesFor: #randNormal:withSeed:!public! !
!LapackMatrix class categoriesFor: #randUniform:!public! !
!LapackMatrix class categoriesFor: #randUniform:withSeed:!public! !
!LapackMatrix class categoriesFor: #realMatrix!public! !
!LapackMatrix class categoriesFor: #resetArrayInterfaces!public! !
!LapackMatrix class categoriesFor: #resetBlasInterfaces!public! !
!LapackMatrix class categoriesFor: #resetLapackInterfaces!public! !
!LapackMatrix class categoriesFor: #rowMatrix!public! !
!LapackMatrix class categoriesFor: #sdczIndex!public! !
!LapackMatrix class categoriesFor: #singlePrecisionMatrix!public! !
!LapackMatrix class categoriesFor: #smalltalkAccessError!public! !
!LapackMatrix class categoriesFor: #smalltalkArrayClass!public! !
!LapackMatrix class categoriesFor: #storeInstancesInSmalltalkSpace!public! !
!LapackMatrix class categoriesFor: #triangularMatrix!public! !
!LapackMatrix class categoriesFor: #uninitialize!public! !
!LapackMatrix class categoriesFor: #uninitializeBeforeRemove!public! !
!LapackMatrix class categoriesFor: #unpackedMatrix!public! !
!LapackMatrix class categoriesFor: #unregisterFlags!public! !
