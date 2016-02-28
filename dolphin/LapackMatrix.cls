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

- aMatrix	^aMatrix differenceFromLapackMatrix: self!

* aMatrix	^aMatrix productFromLapackMatrix: self!

, aMatrix 	^aMatrix concatColumnsFromLapackMatrix: self!

,, aMatrix 	^aMatrix concatRowsFromLapackMatrix: self!

/ aMatrix	^aMatrix quotientFromLapackMatrix: self!

\ aMatrix	^aMatrix leftDivideFromLapackMatrix: self!

+ aMatrix	^aMatrix sumFromLapackMatrix: self!

absMax	^self class isGeneralMatrix 		ifTrue: [super absMax]		ifFalse: [self asGeneralMatrix absMax]!

arrayPointer	"Answer a pointer on an array directly usable in C primitives..."	^self cArray asParameter!

arrayPointerWithOffset: anOffset	"Answer a pointer on an array directly usable in C primitives...	The offset is given in number of elements, not in bytes."	^(self cArray withArrayOffsetBy: anOffset)!

asAbstractMatrix	^ AbstractMatrix basicNew		setArray: (self isInCSpace				ifTrue: [array copyInSmalltalkSpace]				ifFalse: [array copy])		nrow: nrow		ncol: ncol!

asColumnMatrix	^ncol = 1 		ifTrue: [self]		ifFalse: 			[| res |			res := self class nrow: self size.			res 				copy: self size				elementsFrom: self				sourceIncrement: 1				destIncrement: 1]!

asComplexMatrix	^self class isComplexMatrix 		ifTrue: [self]		ifFalse: [self coerceToComplexMatrix]!

asDoublePrecisionComplexMatrix	^self class isDoublePrecisionMatrix 		ifTrue: [self asComplexMatrix]		ifFalse: [self coerceToDoublePrecisionComplexMatrix]!

asDoublePrecisionMatrix	^self class isDoublePrecisionMatrix 		ifTrue: [self]		ifFalse: [self coerceToDoublePrecisionMatrix]!

asGeneralMatrix	^self class isGeneralMatrix 		ifTrue: [self]		ifFalse: [self coerceToGeneralMatrix]!

asPackedMatrix	"Implementation notes: this does not make much sense for general matrix.	But this work as a stub. Should be overloaded in subclasses where it make sense."	^self!

asParameter
	"Answer a pointer on an array directly usable in C primitives..."

	^self cArray asParameter!

asParameterWithOffset: anOffset
	"Answer a pointer on an array directly usable in C primitives...
	The offset is given in number of elements, not in bytes."

	^(self cArray withArrayOffsetBy: anOffset) asParameter!

asRowMatrix	^nrow = 1 		ifTrue: [self]		ifFalse: 			[| res |			res := self class ncol: self size.			res 				copy: self size				elementsFrom: self				sourceIncrement: 1				destIncrement: 1]!

asSinglePrecisionComplexMatrix	^self class isSinglePrecisionMatrix 		ifTrue: [self asComplexMatrix]		ifFalse: [self coerceToSinglePrecisionComplexMatrix]!

asSinglePrecisionMatrix	^self class isSinglePrecisionMatrix 		ifTrue: [self]		ifFalse: [self coerceToSinglePrecisionMatrix]!

asUnpackedMatrix	"Default for LapackMatrices is to be unpacked, return self.	This message should be overloaded in subclass where it make sense"	^self!

at: anInteger put: aNumber 	"This method does handle error due to impossible conversion"	^self 		at: anInteger		put: aNumber		handle: 			[:exc | 			exc return: (aNumber coercing: self						do: 							[:num :mat | 							(mat == self ifTrue: [self] ifFalse: [self become: mat])								arrayAt: anInteger put: num;								yourself])]!

at: anInteger put: aNumber handle: exceptionBlock 	"This method does handle error due to	impossible conversion"		^ [array at: anInteger put: aNumber]		on: Error		do: [:ex | (anInteger between: 1 and: array size)				ifTrue: [exceptionBlock value: ex]				ifFalse: [ex pass]]!

atAllPut: aNumber 	self setOffDiagonal: aNumber diagonal: aNumber!

atIntervalFrom: interStart to: interStop by: interStep 	"fast method : bounds checking must be done externally"	| sz |	sz := (interStop - interStart) // interStep + 1.	^(nrow = 1 		ifTrue: [self class allocateNrow: 1 ncol: sz]		ifFalse: [self class allocateNrow: sz ncol: 1]) 			copy: sz			elementsFrom: (self withArrayOffsetBy: interStart - 1)			sourceIncrement: interStep			destIncrement: 1!

blasInterface	^self class blasInterface!

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

castTo: aMatrixClass 	"Answer a Matrix sharing the same array and that can be used in place of me.	This should not be used to cast type (real/complex) or precision (single/double),	because the underlying arrays will not be compatible.	This can be used only to cast the properties.	This message avoids a copy and is efficient, but should be used with care.	It is as ugly as a C-code cast"	^aMatrixClass basicNew 		setArray: self cArray		nrow: nrow		ncol: ncol!

castToColumn	^self class generalMatrix basicNew 		setArray: array		nrow: self arraySize		ncol: 1!

castToRealWithArrayOffsetBy: aPositiveInteger 	"Return a kind of shallow copy of self pointing on the same C underlying object, but as if it was a sequence of reals with an offset.	This is ugly like C code but maybe more efficient"	^self isComplexMatrix 		ifTrue: 			[| castToReal |			castToReal := self cArray castToRealWithArrayOffsetBy: aPositiveInteger.			self class realMatrix generalMatrix basicNew 				setArray: castToReal				nrow: castToReal size				ncol: 1]		ifFalse: [self withArrayOffsetBy: aPositiveInteger]!

castToRow	^self class generalMatrix basicNew 		setArray: array		nrow: 1		ncol: self arraySize!

coerceFlags: destFlags 	| flags m |	(flags := self class flags) = destFlags ifTrue: [^self].	m := self.	(flags bitAnd: PropertyMask) = (destFlags bitAnd: PropertyMask) 		ifFalse: 			[m := m asGeneralMatrix.			flags := m class flags].	(flags bitAnd: ComplexityMask) = (destFlags bitAnd: ComplexityMask) 		ifFalse: 			[m := m asComplexMatrix.			flags := m class flags].	(flags bitAnd: PrecisionMask) = (destFlags bitAnd: PrecisionMask) 		ifFalse: 			[m := m asDoublePrecisionMatrix.			flags := m class flags].	^m!

coerceFlagsButProperty: destFlags 	| flags m |	(flags := self class flags) = destFlags ifTrue: [^self].	m := self.	(flags bitAnd: ComplexityMask) = (destFlags bitAnd: ComplexityMask) 		ifFalse: 			[m := m asComplexMatrix.			flags := m class flags].	(flags bitAnd: PrecisionMask) = (destFlags bitAnd: PrecisionMask) 		ifFalse: 			[m := m asDoublePrecisionMatrix.			flags := m class flags].	^m!

coerceToComplexMatrix	"Warning: we get here only in case we are a real matrix
	Fast implementation: copy every two reals"	| dst |	dst := self class complexMatrix nrow: nrow ncol: ncol.	(dst castToRealWithArrayOffsetBy: 0) 		copy: self arraySize		elementsFrom: self		sourceIncrement: 1		destIncrement: 2.	^dst!

coerceToDoublePrecisionComplexMatrix	"SLOW naive implementation"	| res |	res := self class doublePrecisionMatrix complexMatrix allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (self rowAt: ir columnAt: jc) asDoubleComplex]].	^res!

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

coerceToGeneralMatrix	"SLOW naive implementation"	| res |	res := self class generalMatrix allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (self rowAt: ir columnAt: jc)]].	^res!

coerceToSinglePrecisionComplexMatrix	"SLOW naive implementation"	| res |	res := self class singlePrecisionMatrix complexMatrix allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (self rowAt: ir columnAt: jc) asFloatComplex]].	^res!

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

coercing: aMatrix do: aBlock 	^aMatrix coercingFromLapackMatrix: self do: aBlock!

coercingButPropertyFromLapackMatrix: aLapackMatrix do: aBlock 	"Convert self and aLapackMatrix to a minimum generality common Matrix class.	Resulting matrices can have different property flags	Do a cheap test on class first in order to handle fast what should be the more frequent case	Then evaluate aBlock with: aLapackMatrix with: self in that order"	^aLapackMatrix class = self class 		ifTrue: [self postCoercingFromLapackMatrix: aLapackMatrix do: aBlock]		ifFalse: 			[| resultFlags |			resultFlags := aLapackMatrix class flags bitOr: self class flags.			(self coerceFlagsButProperty: resultFlags) 				postCoercingFromLapackMatrix: (aLapackMatrix coerceFlagsButProperty: resultFlags)				do: aBlock]!

coercingFromComplexNumber: aComplex do: aBlock 	^aBlock value: aComplex		value: self asComplexMatrix!

coercingFromLapackMatrix: aLapackMatrix do: aBlock 	"Convert self and aMatrix to a minimum generality common Matrix class.	Do a cheap test on class first in order to handle fast what should be the more frequent case	Then evaluate aBlock with: aLapackMatrix with: self in that order"	^aLapackMatrix class = self class 		ifTrue: [self postCoercingFromLapackMatrix: aLapackMatrix do: aBlock]		ifFalse: 			[| resultFlags |			resultFlags := aLapackMatrix class flags bitOr: self class flags.			(self coerceFlags: resultFlags) 				postCoercingFromLapackMatrix: (aLapackMatrix coerceFlags: resultFlags)				do: aBlock]!

coercingFromNumber: aNumber do: aBlock 	^aNumber asFloat coercing: self do: aBlock!

coercingFromRealNumber: aFloat do: aBlock 	^aBlock value: (self class isComplexMatrix 				ifTrue:  [aFloat asComplex]				ifFalse: [aFloat])		value: self!

concatColumnsFromLapackMatrix: aLapackMatrix	"Answer a Matrix whose columns are those of a aLapackMatrix concatenated with selve's.	Must first coerce matrices to same class"	^self coercingFromLapackMatrix: aLapackMatrix do: [:a :b | a concatColumnsWithLapackMatrix: b]!

concatColumnsWithLapackMatrix: aMatrix 	"we know that aMatrix and self have same class	use blas primitive to do the job"	| res |	self size = 0 ifTrue: [^aMatrix copy].	aMatrix size = 0 ifTrue: [^self copy].	aMatrix nrow = nrow 		ifFalse: [self error: 'cannot concatenate columns if not same number of rows'].	res := self class allocateNrow: nrow ncol: ncol + aMatrix ncol.	res 		copy: self size		elementsFrom: self		sourceIncrement: 1		destIncrement: 1.	(res withArrayOffsetBy: self size) 		copy: aMatrix size		elementsFrom: aMatrix		sourceIncrement: 1		destIncrement: 1.	^res!

concatRowsFromLapackMatrix: aLapackMatrix	"Answer a Matrix whose rows are those of a aLapackMatrix concatenated with selve's.	Must first coerce matrices to same class"	^self coercingFromLapackMatrix: aLapackMatrix do: [:a :b | a concatRowsWithLapackMatrix: b]!

concatRowsWithLapackMatrix: aMatrix 	"we know that aMatrix and self have same class	use lapack primitive to do the job"	| res |	self size = 0 ifTrue: [^aMatrix copy].	aMatrix size = 0 ifTrue: [^self copy].	aMatrix ncol = ncol 		ifFalse: [self error: 'cannot concatenate rows if not same number of columns'].	res := self class allocateNrow: nrow + aMatrix nrow ncol: ncol.	res 		copy: nrow		rowsStartingAt: 1		and: ncol		columnsStartingAt: 1		from: self.	res 		copy: aMatrix nrow		rowsStartingAt: nrow + 1		and: ncol		columnsStartingAt: 1		from: aMatrix.	^res!

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

copy	"Implementation notes: this code should be more efficient than doing the super postCopy (array := array copy)	postCopy must be overloaded to just create the array"	^super copy 		copy: self arraySize		elementsFrom: self		sourceIncrement: 1		destIncrement: 1!

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

defaultTolerance	"Answer a tolerance used by default for several algorithm"	^(nrow max: ncol) * self norm2 * self lapackInterface eps!

determinant	nrow = ncol ifFalse: [self error: 'should be square'].	^self pluDecomposition determinant!

differenceFromLapackMatrix: aLapackMatrix 	(aLapackMatrix nrow = nrow and: [aLapackMatrix ncol = ncol]) 		ifFalse: [self error: 'unconsistent matrix dimensions'].	^self coercingFromLapackMatrix: aLapackMatrix do: [:a :b | a differenceWithLapackMatrix: b]!

differenceWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class	use BLAS xAXPY (aMatrix * -1 + self)"	| res |	res := self copy.	res 		fill: self arraySize		elementsWithStride: 1		withSelfPlusScalar: -1		timesVector: aLapackMatrix		stride: 1.	^res!

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

eigenValueDecomposition	^self asGeneralMatrix eigenValueDecomposition!

eigenValues	^(self eigenValueDecomposition)		wantLeftEigenVectors: false;		wantRightEigenVectors: false;		eigenValues!

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

fillRandNormal	^self fillRandNormalWithSeed: DefaultSeedArray!

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

fillRandUniform	^self fillRandUniformWithSeed: DefaultSeedArray!

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

imaginaryPart	"Fast implementation: copy every two reals"	^self class isComplexMatrix 		ifTrue: 			[| res |			(res := self class realMatrix nrow: nrow ncol: ncol) 				copy: self arraySize				elementsFrom: (self castToRealWithArrayOffsetBy: 1)				sourceIncrement: 2				destIncrement: 1.			res]		ifFalse: [self zero]!

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

isBandMatrix	^self class isBandMatrix!

isComplexMatrix	^self class isComplexMatrix!

isDiagonalMatrix	^self class isDiagonalMatrix!

isDoublePrecisionMatrix	^self class isDoublePrecisionMatrix!

isGeneralMatrix	^self class isGeneralMatrix!

isHermitianMatrix	^self class isHermitianMatrix!

isInCSpace	^array class == self class cArrayClass!

isPackedMatrix	^self class isPackedMatrix!

isRealMatrix	^self class isRealMatrix!

isSinglePrecisionMatrix	^self class isSinglePrecisionMatrix!

isSymmetricMatrix	^self class isSymmetricMatrix!

isTriangularMatrix	^self class isTriangularMatrix!

isUnpackedMatrix	^self class isUnpackedMatrix!

lapackInterface	^self class lapackInterface!

leftDivideFromLapackMatrix: aLapackMatrix 	"More or less equivalent to :		aLapackMatrixf pseudoInverse * sel"	aLapackMatrix ncol = ncol 		ifFalse: [^self error: 'matrix dimensions are not compatible'].	^(self coercingButPropertyFromLapackMatrix: aLapackMatrix				do: [:a :b | a leftDivideWithLapackMatrix: b])!

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

multiplyByFloat: aFloat 	^aFloat coercing: self do: [:num :mat | mat scaledByNumber: num]!

multiplyByFraction: aFraction	^aFraction asFloat coercing: self		do: [:num :mat | mat scaledByNumber: num]!

multiplyByInteger: anInteger 	^anInteger asFloat coercing: self		do: [:num :mat | mat scaledByNumber: num]!

multiplyByScaledDecimal: aFixedPoint 
	^aFixedPoint asFloat coercing: self
		do: [:num :mat | mat scaledByNumber: num]!

naiveCoerceToDoublePrecisionMatrix	"SLOW naive implementation"	| res |	res := self class doublePrecisionMatrix allocateNrow: nrow ncol: ncol.	res class isComplexMatrix 		ifTrue: 			[1 to: ncol do: [:jc | 					1 to: nrow do: [:ir | 							res 								rowAt: ir								columnAt: jc								put: (self rowAt: ir columnAt: jc) asFloatComplex]]]		ifFalse: 			[1 to: ncol do: [:jc | 					1 to: nrow do: [:ir | 							res 								rowAt: ir								columnAt: jc								put: (self rowAt: ir columnAt: jc) asFloat]]].	^res!

naiveCoerceToSinglePrecisionMatrix	"SLOW naive implementation"	| res |	res := self class singlePrecisionMatrix allocateNrow: nrow ncol: ncol.	res class isComplexMatrix 		ifTrue: 			[1 to: ncol do: [:jc | 					1 to: nrow do: [:ir | 							res 								rowAt: ir								columnAt: jc								put: (self rowAt: ir columnAt: jc) asFloatComplex]]]		ifFalse: 			[1 to: ncol do: [:jc | 					1 to: nrow do: [:ir | 							res 								rowAt: ir								columnAt: jc								put: (self rowAt: ir columnAt: jc) asFloat]]].	^res!

negated	^self scaledByNumber: -1!

norm1	^self class isGeneralMatrix 		ifTrue: [super norm1]		ifFalse: [self asGeneralMatrix norm1]!

normFrobenius	^self class isGeneralMatrix 		ifTrue: [super normFrobenius]		ifFalse: [self asGeneralMatrix normFrobenius]!

normInfinity	^self class isGeneralMatrix 		ifTrue: [super normInfinity]		ifFalse: [self asGeneralMatrix normInfinity]!

pluDecomposition	^self asGeneralMatrix pluDecomposition!

postCoercingFromLapackMatrix: aLapackMatrix do: aBlock 	"Default implementation is to evaluate a block.	Some subclasses might have to perform further checking (like uplo conformance)"	^aBlock value: aLapackMatrix value: self!

postCopy	"Implementation notes: only allocate here,	DO NOT COPY THE ARRAY AT THIS LEVEL	(hence do NOT call super postCopy)		copy will actually do the copy of array contents with optimized BLAS"	array := array class new: array size.!

productColumnVectorWithRowVector: aLapackMatrix 	"We know that aPrimitiveMatrix has a compatible class with self	Branch on more efficient BLAS routine according to dimensions"	| res |	res := self class generalMatrix nrow: nrow ncol: aLapackMatrix ncol.	res 		fillM: nrow		byN: aLapackMatrix ncol		withScalar: 1		timesColumnVector: self		stride: 1		timesRowVector: aLapackMatrix		stride: 1.	^res!

productFromComplex: aComplex 	^aComplex coercing: self do: [:num :mat | mat scaledByComplex: num]!

productFromDouble: aDouble 	^aDouble coercing: self do: [:num :mat | mat scaledByNumber: num]!

productFromFixedPoint: aFixedPoint 	^aFixedPoint asFloat coercing: self		do: [:num :mat | mat scaledByNumber: num]!

productFromFloat: aFloat 	^aFloat coercing: self do: [:num :mat | mat scaledByNumber: num]!

productFromFraction: aFraction 	^aFraction asFloat coercing: self		do: [:num :mat | mat scaledByNumber: num]!

productFromInteger: anInteger 	^anInteger asFloat coercing: self		do: [:num :mat | mat scaledByNumber: num]!

productFromLapackMatrix: aLapackMatrix 	"Answer the result of matrix product		aLapackMatrix * self	Implementation Notes: 	1) Do not coerce the properties (Hermitian, Triangular, ...)	   because LAPACK/BLAS functions will handle heterogeneous case.	   (as Matrix*Vector for example)	2) Dimension testing is done here only.	   Won't be done in lower #product* methods"	aLapackMatrix ncol = nrow 		ifFalse: [^self error: 'matrix dimensions are not compatible'].	^self coercingButPropertyFromLapackMatrix: aLapackMatrix		do: [:a :b | a productWithLapackMatrix: b]!

productRowVectorWithColumnVector: aLapackMatrix 
	"Implementation notes:	We must not use DOT in complex case because BLAS return a complex by value,	and VW Smalltalk does not seem to handle the case correctly (at least with g77)"

	| res |
	res := self class nrow: 1 ncol: 1.
	(self isComplexMatrix not or: [SmallapackSettings useAtlasCBlas]) 
		ifTrue: 
			["Case when we can use DOT"
			| dotProduct |
			dotProduct := self 
						dotProduct: self size
						elementsIncrement: 1
						with: aLapackMatrix
						increment: 1.
			res at: 1 put: dotProduct]
		ifFalse: 
			["Fall back to matrix*vector product"
			self productMatrixWithColumnVector: aLapackMatrix].
	^res!

productRowVectorWithMatrix: aMatrix 	"Vector * Matrix	naive algorithm"	^aMatrix productMatrixTransposeWithColumnVector: self asColumnMatrix!

productWithLapackMatrix: aLapackMatrix 	"We know that aLapackMatrix has a compatible class with self	(in term of complexity and precision)	Branch on more efficient BLAS routine according to dimensions	Implementation notes:	BLAS give functions for multiplying Matrix*Matrix :	- HE * GE or GE * HE	- SY * GE or GE * SY	- TR * GE or GE * TR	- GE * GE	Then, one of the two matrices will be considered as General and have all elements accessed (full storage).	In case of triangular Matrix, the other triangle is always set to zero (by construction)	In case of symmetric/hermitian Matrix, this should be asserted using message #fillOtherTriangle.	Note: there is no HP * GE nor SP * GE nor TP * GE BLAS routine. We have to use unpacked forms.	BECAUSE LapackHermitianMatrix has the lowest propertyFlags,	GeneralMask > TriangularMask > HermitianMask	then a Matrix*Matrix product involving a Symmetric/Hermitian Matrix	will always perform a message with a LapackHermitianMatrix receiver.	Thus the fillOtherTriangle operation need only be handled in LapackHermitianMatrix subclass	This trick will not work if one changes property ordering.		When adding a new subclass, one should consider rewriting this code.	In particular, something is needed for band matrices"	^aLapackMatrix isColumnVector 		ifTrue: 			[self isRowVector 				ifTrue: 					["Case (1,n)*(n,1)"					self productRowVectorWithColumnVector: aLapackMatrix]				ifFalse: 					["Case (m,n)*(n,1)"					self productMatrixWithColumnVector: aLapackMatrix]]		ifFalse: 			[self isColumnVector 				ifTrue: 					["Case (m,1)*(1,n)"					self productColumnVectorWithRowVector: aLapackMatrix]				ifFalse: 					[self isRowVector 						ifTrue: 							["Case (1,m)*(m,n) : transpose the product							with a little trick to save a copy : use self castToCulumn instead of self transposed"							(aLapackMatrix productMatrixTransposeWithColumnVector: self castToColumn) transposed]						ifFalse: 							["Case (m,p)*(p,n)"							| leftFlags rightFlags |							leftFlags := self class flags bitAnd: PropertyMask.							rightFlags := aLapackMatrix class flags bitAnd: PropertyMask.							leftFlags <= rightFlags 								ifTrue: 									[self asUnpackedMatrix 										productMatrixWithMatrix: aLapackMatrix asUnpackedMatrix]								ifFalse: 									[aLapackMatrix asUnpackedMatrix 										productMatrixAtRightWithMatrix: self asUnpackedMatrix]]]]!

pseudoInverse	"Compute pseudo inverse of self with default tolerance"	^self pseudoInverseTolerance: self defaultTolerance!

pseudoInverseTolerance: tol 	"Let A = (U*S*V transposeConjugated)	Then A pseudoInverse = (V*S reciprocal*U transposeConjugated)	If some singular values are smaller than the tolerance they are not taken into account"	| svd sigma |	svd := self singularValueDecomposition.	svd wantSomeSingularVector.	sigma := svd s class diagonal: (svd s 						collect: [:each | each <= tol ifTrue: [0] ifFalse: [each reciprocal]]).	^svd v * sigma * svd ut!

qrDecomposition
	"QR factorization"

	^LapackQRdecomposition decompose: self asGeneralMatrix!

qrpDecomposition
	"QR factorization with column pivoting"

	^LapackQRPdecomposition decompose: self asGeneralMatrix!

quotientFromLapackMatrix: aLapackMatrix 	"More or less equivalent to :		aLapackMatrix * self pseudoInverse"	aLapackMatrix ncol = ncol 		ifFalse: [^self error: 'matrix dimensions are not compatible'].	^(self coercingButPropertyFromLapackMatrix: aLapackMatrix				do: [:a :b | a rightDivideWithLapackMatrix: b])!

rank	"Compute rank of self with default tolerance"	^self rankTolerance: self defaultTolerance!

rankTolerance: tol 	"Answer the number of linearly independant rows or columns of self.	count non null singular values with a tolerance "	^self singularValues count: [:each | each > tol ]!

realPart	"Fast implementation: copy every two reals"	^self class isComplexMatrix 		ifTrue: 			[| res |			(res := self class realMatrix nrow: nrow ncol: ncol)				copy: self arraySize				elementsFrom: (self castToRealWithArrayOffsetBy: 0)				sourceIncrement: 2				destIncrement: 1.			res]		ifFalse: [self]!

reciprocal	nrow = ncol ifFalse: [self error: 'should be square'].	^self copy inPlaceSolve: (self class eye: nrow)!

reduceGeneralityIfPossible	^(self class isComplexMatrix and: [self imaginaryPart isZero]) 		ifTrue: [self realPart]		ifFalse: [self]!

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

scaledByComplex: aComplex 	"Answer a copy of self scaled.	aComplex and self should agree on precision before sending this message."	aComplex imaginaryPart isZero ifTrue: [^self scaledByNumber: aComplex realPart].	^self asComplexMatrix copy inPlaceScaledByComplex: aComplex!

scaledByNumber: aNumber 	"Answer a copy of self scaled.	aNumber and self should agree on precision before sending this message."	^self copy inPlaceScaledByNumber: aNumber!

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

singularValues	^(self singularValueDecomposition)		wantNoSingularVector;		singularValues!

solve: aMatrix 	"Solve the linear equations self * X = aMatrix	In other words, find X such that..."	^aMatrix coercingButPropertyFromLapackMatrix: self		do: [:a :b | a copy inPlaceSolve: b copy]!

storeInSmalltalkSpace	"This transfer is needed before saving an image..."		self isInCSpace		ifTrue: [array storeInSmalltalkSpace]!

sumFromLapackMatrix: aLapackMatrix 	(aLapackMatrix nrow = nrow and: [aLapackMatrix ncol = ncol]) 		ifFalse: [self error: 'unconsistent matrix dimensions'].	^self coercingFromLapackMatrix: aLapackMatrix		do: [:a :b | a sumWithLapackMatrix: b]!

sumWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class and compatible dimensions"	| res |	res := self copy.	^res 		fill: self arraySize		elementsWithStride: 1		withSelfPlusScalar: 1		timesVector: aLapackMatrix		stride: 1!

swapColumn: j1 withColumn: j2 	"Most matrices loose their properties when swapping columns"	^(self become: self asGeneralMatrix) swapColumn: j1 withColumn: j2!

swapRow: i1 withRow: i2 	"Most matrices loose their properties when swapping rows"	^(self become: self asGeneralMatrix) swapRow: i1 withRow: i2!

transposeConjugated	^self class isComplexMatrix 		ifTrue: [super transposeConjugated]		ifFalse: [self transposed]!

withArrayOffsetBy: aPositiveInteger 	"Return a kind of shallow copy of self pointing on the same C underlying object, but with an offset.	This is ugly like C code but maybe more efficient.	This also has the side effect of hiding properties while storing elements	(because property could be lost due to temporary disagreement when filling)"	| newArray |	newArray := self cArray withArrayOffsetBy: aPositiveInteger.	^self class generalMatrix basicNew 		setArray: newArray		nrow: newArray size		ncol: 1! !
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

allocateNrow: nr ncol: nc 	"Allocate the array, but do not initialize its contents"	^self basicNew 		setArray: (self smalltalkArrayClass new: nr * nc)		nrow: nr		ncol: nc!

arrayInterface	^ArrayInterfaces at: self sdczIndex!

blasInterface	^BlasInterfaces at: self sdczIndex!

cArrayClass	"answer the array suitable for storing data in C space	can eventually be optimized in subclasses"	^CArrayClasses at: self sdczIndex!

columnMatrix	^self generalMatrix!

complexMatrix	^self findClassWithFlags: (self flags bitOr: ComplexityMask)!

diagonalMatrix	^self 		findClassWithFlags: ((self flags maskClear: PropertyMask) bitOr: DiagonalMask)!

doublePrecisionMatrix	^self findClassWithFlags: (self flags bitOr: PrecisionMask)!

eye: anIntegerOrPointOrArrayOfInteger 	"Create eye matrix. Smae as super but do not initialize the array twice"	^(self allocateShape: anIntegerOrPointOrArrayOfInteger)		setToEye;		yourself!

findClassWithFlags: someFlags 	"PrimitiveLapackMatrix 		allSubclassesDo: [:aClass | aClass flags = someFlags ifTrue: [^aClass]]."	^FlagsToClassDictionary at: someFlags		ifAbsent: 			[self error: 'No Lapack Matrix class were found wih requested properties']!

flags	^flags!

generalMatrix	^self findClassWithFlags: (self flags bitOr: PropertyMask)!

hermitianMatrix	^self 		findClassWithFlags: ((self flags maskClear: PropertyMask) bitOr: HermitianMask)!

initialize
	"I have to be warned whenever a snapshot is going to take place.
	This is why i register myself as a dependent of ObjectMemory."

	"ObjectMemory
		removeDependent: LapackMatrix;
		addDependent: LapackMatrix."

	self initializeMasks.
	self initializeSeed.
	self initializeArrayClasses.
	SmallapackSettings
		when: #librariesChanged
		send: #resetInterfaces
		to: self.
	self resetInterfaces.
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

initializeClassInstVars	self		initializeFlags";		initializeSignals".	FlagsToClassDictionary at: self flags put: self!

initializeFlags	self subclassResponsibility!

initializeMasks	SDCZMask := 2r11.	PrecisionMask := 2r1.	ComplexityMask := 2r10.	SinglePrecisionMask := 2r0.	DoublePrecisionMask := 2r1.	ComplexMask := 2r10.	RealMask := 2r00.	PropertyMask := 2r11100.	GeneralMask := 2r11100.	TriangularMask := 2r10100.	HermitianMask := 2r01100.	DiagonalMask := 2r00100.	StorageMask := 2r1100000.	BandStorageMask := 2r1000000.	FullStorageMask := 2r1100000.	PackedStorageMask := 2r0100000.	LowerMask := 2r01.	UpperMask := 2r10.	BothUpperLowerMask := 2r11.	FlagsToClassDictionary := Dictionary new.!

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

isBandMatrix	^(flags bitAnd: StorageMask) = BandStorageMask!

isComplexMatrix	^flags allMask: ComplexityMask!

isDiagonalMatrix	^(flags bitAnd: PropertyMask) = DiagonalMask!

isDoublePrecisionMatrix	^flags allMask: PrecisionMask!

isFullMatrix	^(flags bitAnd: StorageMask) = FullStorageMask!

isGeneralMatrix	^flags allMask: PropertyMask!

isHermitianMatrix	^(flags bitAnd: PropertyMask) = HermitianMask!

isPackedMatrix	^(flags bitAnd: StorageMask) = PackedStorageMask!

isRealMatrix	^(flags allMask: ComplexityMask) not!

isSinglePrecisionMatrix	^(flags allMask: PrecisionMask) not!

isSymmetricMatrix	" we are not interested by Complex Symmetric matrices"	^self isRealMatrix and: [self isHermitianMatrix]!

isTriangularMatrix	^(flags bitAnd: PropertyMask) = TriangularMask!

isUnpackedMatrix	^(flags bitAnd: StorageMask) ~= PackedStorageMask!

lapackInterface	^LapackInterfaces at: self sdczIndex!

nrow: nr ncol: nc withAll: aNumber 	"Create a matrix filled with aNumber"	^(self allocateNrow: nr ncol: nc) atAllPut: aNumber!

onStartup
	"reset the ExternalLibrary interfaces on image startup"

	self resetInterfaces!

packedMatrix	"This must be overloaded where it make sense"	^self!

preSnapshot	"The system is going to snapshot...	objects pointing onto C heap memory will not survive across the snapshot...	it is necessary to transfer them in Smalltalk memory before snapshot..."	self allSubclassesDo: [:subclass | subclass storeInstancesInSmalltalkSpace].!

randNormal: anIntegerOrPointOrArrayOfInteger 	"Create a matirx with given shape and initialized randomly (normal distributed mean 0 deviation 1)"	^(self allocateShape: anIntegerOrPointOrArrayOfInteger)		fillRandNormal;		yourself!

randNormal: anIntegerOrPointOrArrayOfInteger withSeed: seedArray 	"Create a matirx with given shape and initialized randomly (normal distributed mean 0 deviation 1)"	^(self allocateShape: anIntegerOrPointOrArrayOfInteger)		fillRandNormalWithSeed: seedArray;		yourself!

randUniform: anIntegerOrPointOrArrayOfInteger 	"Create a matirx with given shape and initialized randomly (uniform distributed between 0 and 1)"	^(self allocateShape: anIntegerOrPointOrArrayOfInteger)		fillRandUniform;		yourself!

randUniform: anIntegerOrPointOrArrayOfInteger withSeed: seedArray 	"Create a matirx with given shape and initialized randomly (uniform distributed between 0 and 1)"	^(self allocateShape: anIntegerOrPointOrArrayOfInteger)		fillRandUniformWithSeed: seedArray;		yourself!

realMatrix	^self findClassWithFlags: (self flags maskClear: ComplexityMask)!

resetArrayInterfaces
	ArrayInterfaces := Array new: 4.
	ArrayInterfaces at: 1 + SinglePrecisionMask + RealMask put: ArraySLibrary default.
	ArrayInterfaces at: 1 + DoublePrecisionMask + RealMask put: ArrayDLibrary default.
	ArrayInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: ArrayCLibrary default.
	ArrayInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: ArrayZLibrary default!

resetBlasInterfaces	BlasInterfaces := Array new: 4.	SmallapackSettings useAtlasCBlas 		ifTrue: 			["BlasInterfaces at: 1 + SinglePrecisionMask + RealMask put: CBlasSLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + RealMask put: CBlasDLibrary default.			BlasInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: CBlasCLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: CBlasZLibrary default"]		ifFalse: 			[BlasInterfaces at: 1 + SinglePrecisionMask + RealMask put: BlasSLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + RealMask put: BlasDLibrary default.			BlasInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: BlasCLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: BlasZLibrary default]!

resetInterfaces
	[self resetBlasInterfaces] on: Win32Error do: [:exc |exc return: nil].
	[self resetLapackInterfaces] on: Win32Error do: [:exc |exc return: nil].
	[self resetArrayInterfaces] on: Win32Error do: [:exc |exc return: nil].!

resetLapackInterfaces	LapackInterfaces := Array new: 4.	LapackInterfaces at: 1 + SinglePrecisionMask + RealMask put: LapackSLibrary default.	LapackInterfaces at: 1 + DoublePrecisionMask + RealMask put: LapackDLibrary default.	LapackInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: LapackCLibrary default.	LapackInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: LapackZLibrary default!

rowMatrix	^self generalMatrix!

sdczIndex	"Answer an index used for indirection according to precision and complexity"	^1 + (self flags bitAnd: SDCZMask)!

singlePrecisionMatrix	^self findClassWithFlags: (self flags maskClear: PrecisionMask)!

smalltalkArrayClass	"answer the array suitable for storing data in Smalltalk space"	^SmalltalkArrayClasses at: self sdczIndex!

storeInstancesInSmalltalkSpace	self allInstancesDo: [:e | e storeInSmalltalkSpace]!

triangularMatrix	^self 		findClassWithFlags: ((self flags maskClear: PropertyMask) bitOr: TriangularMask)!

uninitialize
	SmallapackSettings removeEventsTriggeredFor: self.
	SessionManager current removeEventsTriggeredFor: self!

uninitializeBeforeRemove
	"Note: this cannot be uninitialize, because we want it to be performed by each removed subclass"
	self unregisterFlags.	^super uninitializeBeforeRemove.!

unpackedMatrix	"This must be overloaded where it make sense"	^self!

unregisterFlags	"remove reference to a self before being unloaded"	FlagsToClassDictionary removeKey: self flags ifAbsent: []! !
!LapackMatrix class categoriesFor: #allocateNrow:ncol:!instance creation!public! !
!LapackMatrix class categoriesFor: #arrayInterface!accessing!public! !
!LapackMatrix class categoriesFor: #blasInterface!accessing!public! !
!LapackMatrix class categoriesFor: #cArrayClass!accessing!private! !
!LapackMatrix class categoriesFor: #columnMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #complexMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #diagonalMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #doublePrecisionMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #eye:!instance creation!public! !
!LapackMatrix class categoriesFor: #findClassWithFlags:!accessing!private! !
!LapackMatrix class categoriesFor: #flags!accessing!public! !
!LapackMatrix class categoriesFor: #generalMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #hermitianMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #initialize!class initialization!public! !
!LapackMatrix class categoriesFor: #initializeArrayClasses!class initialization!public! !
!LapackMatrix class categoriesFor: #initializeClassInstVars!class initialization!public! !
!LapackMatrix class categoriesFor: #initializeFlags!class initialization!public! !
!LapackMatrix class categoriesFor: #initializeMasks!class initialization!public! !
!LapackMatrix class categoriesFor: #initializeSeed!class initialization!public! !
!LapackMatrix class categoriesFor: #isBandMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isComplexMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isDiagonalMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isDoublePrecisionMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isFullMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isGeneralMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isHermitianMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isPackedMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isRealMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isSinglePrecisionMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isSymmetricMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isTriangularMatrix!public!testing! !
!LapackMatrix class categoriesFor: #isUnpackedMatrix!public!testing! !
!LapackMatrix class categoriesFor: #lapackInterface!accessing!public! !
!LapackMatrix class categoriesFor: #nrow:ncol:withAll:!instance creation!public! !
!LapackMatrix class categoriesFor: #onStartup!class initialization!public! !
!LapackMatrix class categoriesFor: #packedMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #preSnapshot!public! !
!LapackMatrix class categoriesFor: #randNormal:!instance creation!public! !
!LapackMatrix class categoriesFor: #randNormal:withSeed:!instance creation!public! !
!LapackMatrix class categoriesFor: #randUniform:!instance creation!public! !
!LapackMatrix class categoriesFor: #randUniform:withSeed:!instance creation!public! !
!LapackMatrix class categoriesFor: #realMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #resetArrayInterfaces!class initialization!public! !
!LapackMatrix class categoriesFor: #resetBlasInterfaces!class initialization!public! !
!LapackMatrix class categoriesFor: #resetInterfaces!class initialization!public! !
!LapackMatrix class categoriesFor: #resetLapackInterfaces!class initialization!public! !
!LapackMatrix class categoriesFor: #rowMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #sdczIndex!accessing!private! !
!LapackMatrix class categoriesFor: #singlePrecisionMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #smalltalkArrayClass!accessing!private! !
!LapackMatrix class categoriesFor: #storeInstancesInSmalltalkSpace!public! !
!LapackMatrix class categoriesFor: #triangularMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #uninitialize!class initialization!public! !
!LapackMatrix class categoriesFor: #uninitializeBeforeRemove!class initialization!public! !
!LapackMatrix class categoriesFor: #unpackedMatrix!accessing!public! !
!LapackMatrix class categoriesFor: #unregisterFlags!class initialization!public! !

