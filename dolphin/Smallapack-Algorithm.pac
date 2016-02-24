| package |
package := Package name: 'Smallapack-Algorithm'.
package paxVersion: 1;
	basicComment: '
Smallapack-Algorithm provides algorithms for several linear algebra problems

- solving linear system A*X=B
- finding eigen value and eigen vector (diagonalisation)
- Singular Value Decomposition
- QR decomposition
- Schur decomposition (triangularisation)

Algorithms are not implemented in Smalltalk.
Smalltalk provides only wrappers to Lapack library.

Using an algorithm class instead of a function has several advantages:
- enable combination of several options
- provide several outputs (unlike message who would return an array)
- do not rely on positional input or output argument but on clearly named messages
- use lazy evaluation (computations are deferred until an output message is sent)'.


package classNames
	add: #LapackComplexEigenDecomposition;
	add: #LapackComplexGeneralizedEigenDecomposition;
	add: #LapackComplexSchurDecomposition;
	add: #LapackContinuousRiccatiProblem;
	add: #LapackDecomposition;
	add: #LapackDiagonalEigenDecomposition;
	add: #LapackDiagonalPLUdecomposition;
	add: #LapackDiscreteRiccatiProblem;
	add: #LapackEigenDecomposition;
	add: #LapackGeneralizedEigenDecomposition;
	add: #LapackGeneralizedSVDecomposition;
	add: #LapackHermitianEigenDecomposition;
	add: #LapackHermitianGeneralizedEigenDecomposition;
	add: #LapackHermitianPackedGeneralizedEigenDecomposition;
	add: #LapackHermitianPLUdecomposition;
	add: #LapackHessenbergDecomposition;
	add: #LapackLeastSquareProblem;
	add: #LapackLeastSquareProblemWithEqualityConstraints;
	add: #LapackPLUdecomposition;
	add: #LapackQRdecomposition;
	add: #LapackQRPdecomposition;
	add: #LapackRealEigenDecomposition;
	add: #LapackRealGeneralizedEigenDecomposition;
	add: #LapackRealSchurDecomposition;
	add: #LapackRiccatiProblem;
	add: #LapackSchurDecomposition;
	add: #LapackSVDecomposition;
	add: #LapackTriangularPLUdecomposition;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #LapackDecomposition
	instanceVariableNames: 'sourceMatrix isComputed info'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackEigenDecomposition
	instanceVariableNames: 'eigenValues leftEigenVectors rightEigenVectors wantLeft wantRight'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackGeneralizedSVDecomposition
	instanceVariableNames: 'otherMatrix u v q r alpha beta x k l wantU wantV wantQ xIsComputed'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackHessenbergDecomposition
	instanceVariableNames: 'q h tau ilo ihi'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackLeastSquareProblem
	instanceVariableNames: 'algorithm rhsMatrix solution rcond rank'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackPLUdecomposition
	instanceVariableNames: 'p l u a ipiv inverse'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackQRdecomposition
	instanceVariableNames: 'q r tau qfull rfull'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackRiccatiProblem
	instanceVariableNames: 'sourceMatrix1 sourceMatrix2 sourceMatrix3 algorithm postActionBlock epsilon solution'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackSchurDecomposition
	instanceVariableNames: 'eigenValues schurTriangular schurVectors wantVectors shouldScale selectFunction sdim a ilo ihi scale'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDecomposition subclass: #LapackSVDecomposition
	instanceVariableNames: 'u s vt whichLeftVector whichRightVector'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackEigenDecomposition subclass: #LapackComplexEigenDecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackEigenDecomposition subclass: #LapackGeneralizedEigenDecomposition
	instanceVariableNames: 'rhsMatrix alpha beta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackEigenDecomposition subclass: #LapackHermitianEigenDecomposition
	instanceVariableNames: 'algorithm'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackEigenDecomposition subclass: #LapackRealEigenDecomposition
	instanceVariableNames: 'wr wi vl vr'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralizedEigenDecomposition subclass: #LapackComplexGeneralizedEigenDecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralizedEigenDecomposition subclass: #LapackHermitianGeneralizedEigenDecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralizedEigenDecomposition subclass: #LapackHermitianPackedGeneralizedEigenDecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralizedEigenDecomposition subclass: #LapackRealGeneralizedEigenDecomposition
	instanceVariableNames: 'alphar alphai vl vr'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackHermitianEigenDecomposition subclass: #LapackDiagonalEigenDecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackLeastSquareProblem subclass: #LapackLeastSquareProblemWithEqualityConstraints
	instanceVariableNames: 'equalityLHSmatrix equalityRHSmatrix'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackPLUdecomposition subclass: #LapackHermitianPLUdecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackPLUdecomposition subclass: #LapackTriangularPLUdecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackTriangularPLUdecomposition subclass: #LapackDiagonalPLUdecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackQRdecomposition subclass: #LapackQRPdecomposition
	instanceVariableNames: 'p jpvt'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackRiccatiProblem subclass: #LapackContinuousRiccatiProblem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackRiccatiProblem subclass: #LapackDiscreteRiccatiProblem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackSchurDecomposition subclass: #LapackComplexSchurDecomposition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackSchurDecomposition subclass: #LapackRealSchurDecomposition
	instanceVariableNames: 'wr wi'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LapackDecomposition guid: (GUID fromString: '{4D479A22-6F93-43C3-BB2C-BE0B70005350}')!
LapackDecomposition comment: 'LapackDecomposition is an abstract class holding some common protocol for decomposing LapackMatrices

It is equipped for lazy computations.
Nothing is computed before an instance variable is accessed...
Doing this enable setting various algorithm parameter before doing actual computations.

Subclasses must implement the following messages:
	processing
		decompose

Instance Variables:
	info	<Integer>	information code on return from Lapack function. 0 if OK
	isComputed	<Boolean>	indicating whether decomposition is computed or not yet
	sourceMatrix	<LapackMatrix>	hold the Matrix which should be decomposed
'!
!LapackDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackDecomposition methodsFor!

checkDecomposition	isComputed ifFalse: [self decompose]!

decompose	^self subclassResponsibility!

decompose: aMatrix	sourceMatrix := aMatrix.	self reset!

info	^info!

initialize	"This message is used at creation.	It should initialize the default options"	^self!

reset	isComputed := false! !
!LapackDecomposition categoriesFor: #checkDecomposition!public! !
!LapackDecomposition categoriesFor: #decompose!public! !
!LapackDecomposition categoriesFor: #decompose:!public! !
!LapackDecomposition categoriesFor: #info!public! !
!LapackDecomposition categoriesFor: #initialize!public! !
!LapackDecomposition categoriesFor: #reset!public! !

!LapackDecomposition class methodsFor!

decompose: aMatrix 	^self new decompose: aMatrix!

new	^super new initialize! !
!LapackDecomposition class categoriesFor: #decompose:!public! !
!LapackDecomposition class categoriesFor: #new!public! !

LapackEigenDecomposition guid: (GUID fromString: '{B7DDF558-DEE3-4C62-B252-FDB6622D906C}')!
LapackEigenDecomposition comment: 'LapackEigenDecomposition decompose a square matrix into eigenvalues (diagonal) and eigenvectors.
Subclass will implement the kernel Lapack call according to Matrix property (general, hermitian, ...).

right eigenvectors problem: given A, find P and D diagonal such that:
	A * P = P * D
left eigenvectors problem:
	P transposeConjugated * A = D * P transposeConjugated

Diagonal elements of D are eigenvalues (off-diagonal are 0)

Subclasses must implement the following messages:
	processing
		decompose:

Instance Variables:
	eigenValues	<LapackMatrix>	contains the eigenValues
	leftEigenVectors	<LapackMatrix>	contains the left eigenVectors
	rightEigenVectors	<LapackMatrix>	contains the right eigenVectors
	wantLeft	<Boolean>	indicating if left eigen vectors are to be computed
	wantRight	<Boolean>	indicating if right eigen vectors are to be computed
'!
!LapackEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackEigenDecomposition methodsFor!

checkEigenValues	self checkDecomposition!

checkLeftEigenVectors	wantLeft 		ifFalse: 			[self wantLeftEigenVectors: true.			isComputed := false].	self checkDecomposition!

checkRightEigenVectors	wantRight 		ifFalse: 			[self wantRightEigenVectors: true.			isComputed := false].	self checkDecomposition!

eigenValues	self checkEigenValues.	^eigenValues!

initialize	wantLeft := false.	wantRight := false.	isComputed := false.	super initialize!

leftEigenVectors	self checkLeftEigenVectors.	^leftEigenVectors!

reset	eigenValues := nil.	leftEigenVectors := nil.	rightEigenVectors := nil.	super reset.!

rightEigenVectors	self checkRightEigenVectors.	^rightEigenVectors!

solveByDivideAndConquer	"this is a stub."	self 		error: 'divide and conquer algorithm is for complex hermitian or real symmetric matrices only'!

solveByRelativelyRobustRepresentation	"this is a stub."	self 		error: 'relatively robust representation algorithm is for complex hermitian or real symmetric matrices only'!

solveByStandardAlgorithm	"this is a stub.	by default, we use standard algorithm, nothing more to do"!

wantLeftEigenVectors: aBoolean 	wantLeft := aBoolean!

wantRightEigenVectors: aBoolean 	wantRight := aBoolean! !
!LapackEigenDecomposition categoriesFor: #checkEigenValues!public! !
!LapackEigenDecomposition categoriesFor: #checkLeftEigenVectors!public! !
!LapackEigenDecomposition categoriesFor: #checkRightEigenVectors!public! !
!LapackEigenDecomposition categoriesFor: #eigenValues!public! !
!LapackEigenDecomposition categoriesFor: #initialize!public! !
!LapackEigenDecomposition categoriesFor: #leftEigenVectors!public! !
!LapackEigenDecomposition categoriesFor: #reset!public! !
!LapackEigenDecomposition categoriesFor: #rightEigenVectors!public! !
!LapackEigenDecomposition categoriesFor: #solveByDivideAndConquer!public! !
!LapackEigenDecomposition categoriesFor: #solveByRelativelyRobustRepresentation!public! !
!LapackEigenDecomposition categoriesFor: #solveByStandardAlgorithm!public! !
!LapackEigenDecomposition categoriesFor: #wantLeftEigenVectors:!public! !
!LapackEigenDecomposition categoriesFor: #wantRightEigenVectors:!public! !

LapackGeneralizedSVDecomposition guid: (GUID fromString: '{E254F0E9-F5A6-404A-A6E5-B184227E7D47}')!
LapackGeneralizedSVDecomposition comment: ''!
!LapackGeneralizedSVDecomposition categoriesForClass!Unclassified! !
!LapackGeneralizedSVDecomposition methodsFor!

A
	"answer with input matrix A of the decomposition (see class comment)"
	
	^sourceMatrix!

alpha
	"answer with diagonal elements of the decomposition (see class comment)"
	
	self checkDecomposition.
	^alpha!

B
	"answer with input matrix B of the decomposition (see class comment)"
	
	^otherMatrix!

beta
	"answer with diagonal elements of the decomposition (see class comment)"
	
	self checkDecomposition.
	^beta!

checkX
	xIsComputed
		ifFalse:
			[self computeX.
			xIsComputed := true]!

computeX
	| n irinv |
	self checkDecomposition.
	n := sourceMatrix ncol.
	irinv := ((sourceMatrix class identity: n - k - l) , (sourceMatrix class rows: n - k - l columns: k + l)) ,,
		((sourceMatrix class rows: k + l columns: n - k - l) , r reciprocal).
	x := q * irinv.!

decompose
	| a b m p n kHolder lHolder |
	a := sourceMatrix copy.
	b := otherMatrix copy.
	a isComplexMatrix ifTrue: [b := b asComplexMatrix].
	b isComplexMatrix ifTrue: [a := a asComplexMatrix].
	a isDoublePrecisionMatrix ifTrue: [b := b asDoublePrecisionMatrix].
	b isDoublePrecisionMatrix ifTrue: [a := a asDoublePrecisionMatrix].
	m := a nrow.
	p := b nrow.
	n := a ncol.
	alpha := a class realMatrix allocateNrow: n ncol: 1.
	beta := a class realMatrix allocateNrow: n ncol: 1.
	u := wantU 
				ifTrue: [a class allocateNrow: m ncol: m]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	v := wantV 
				ifTrue: [a class allocateNrow: p ncol: p]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	q := wantQ 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	kHolder := SDWORDArray new: 1.
	lHolder := SDWORDArray new: 1.
	info := a lapackInterface 
				ggsvdWithjobu: (a lapackInterface wantU: wantU)
				jobv: (a lapackInterface wantV: wantV)
				jobq: (a lapackInterface wantQ: wantQ)
				m: m
				n: n
				p: p
				k: kHolder asParameter
				l: lHolder asParameter
				a: a asParameter
				lda: a nrow
				b: b asParameter
				ldb: b nrow
				alpha: alpha asParameter
				beta: beta asParameter
				u: u asParameter
				ldu: u nrow
				v: v asParameter
				ldv: v nrow
				q: q asParameter
				ldq: q nrow.
	info = 0 ifFalse: [self error: 'A Generalized Singular Value Decomposition failed'].
	k := kHolder at: 1.
	l := lHolder at: 1.
	m >= (k + l)
		ifTrue: [r := a smallUpperTriangle: n - k - l]
		ifFalse: [
			r := a ,, (b atRows: (m - k + 1 to: l))
				smallUpperTriangle: n - k - l].
	isComputed := true!

decompose: aLapackMatrix
	"The generalized SVD problem deals with two matrices, not one"

	self shouldNotImplement!

decomposeA: aMatrix B: bMatrix 
	sourceMatrix := aMatrix.
	otherMatrix := bMatrix.
	self reset!

initialize
	super initialize.
	wantU := wantV := wantQ := true!

k
	"answer with dimension k of the decomposition (see class comment)"
	
	self checkDecomposition.
	^k!

l
	"answer with dimension l of the decomposition (see class comment)"
	
	self checkDecomposition.
	^l!

Q
	"answer with unitary matrix Q of the decomposition (see class comment)"
	
	self wantQ: true.
	self checkDecomposition.
	^q!

R
	"answer with triangular matrix R of the decomposition (see class comment)"
	
	self checkDecomposition.
	^r!

reset
	xIsComputed := false.
	u := v := q := r := x := alpha := beta := nil.
	k := l := 0.
	super reset.!

U
	"answer with unitary matrix U of the decomposition (see class comment)"
	
	self wantU: true.
	self checkDecomposition.
	^u!

V
	"answer with unitary matrix V of the decomposition (see class comment)"
	
	self wantV: true.
	self checkDecomposition.
	^v!

wantQ: aBoolean
	"Tell whether unitary matrix Q is to be computed or not (see class comment)."
	
	(aBoolean and: [wantQ not]) ifTrue: [
		"Q is wanted but were not computed: we have to reset processing"
		self reset].
	wantQ := aBoolean!

wantU: aBoolean
	"Tell whether unitary matrix U is to be computed or not (see class comment)."
	
	(aBoolean and: [wantU not]) ifTrue: [
		"U is wanted but were not computed: we have to reset processing"
		self reset].
	wantU := aBoolean!

wantV: aBoolean
	"Tell whether unitary matrix V is to be computed or not (see class comment)."
	
	(aBoolean and: [wantV not]) ifTrue: [
		"V is wanted but were not computed: we have to reset processing"
		self reset].
	wantV := aBoolean!

X
	"answer with non singular matrix X of the decomposition in diagonal form (see class comment)"
	
	self checkX.
	^x! !
!LapackGeneralizedSVDecomposition categoriesFor: #A!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #alpha!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #B!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #beta!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #checkX!private! !
!LapackGeneralizedSVDecomposition categoriesFor: #computeX!private! !
!LapackGeneralizedSVDecomposition categoriesFor: #decompose!processing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #decompose:!initialize/release!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #decomposeA:B:!initialize/release!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #initialize!initialize/release!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #k!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #l!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #Q!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #R!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #reset!initialize/release!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #U!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #V!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #wantQ:!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #wantU:!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #wantV:!accessing!public! !
!LapackGeneralizedSVDecomposition categoriesFor: #X!accessing!public! !

!LapackGeneralizedSVDecomposition class methodsFor!

decomposeA: aMatrix B: bMatrix
	"The generalized singular value decomposition problem deals with two matrices, not one"

	^self new decomposeA: aMatrix B: bMatrix! !
!LapackGeneralizedSVDecomposition class categoriesFor: #decomposeA:B:!instance creation!public! !

LapackHessenbergDecomposition guid: (GUID fromString: '{CF0B7545-A71A-495E-A948-75F4E96336BB}')!
LapackHessenbergDecomposition comment: ''!
!LapackHessenbergDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackHessenbergDecomposition methodsFor!

decompose
	| a lapack n scale |
	a := sourceMatrix copy.
	n := a nrow.
	lapack := sourceMatrix lapackInterface.
"Find the non triangular part"
	ilo := SDWORDArray new: 1.
	ihi := SDWORDArray new: 1.
	scale := a class realMatrix nrow: n.
	info := lapack 
				gebalWithjob: lapack balanceDoNothing
				n: n
				a: a asParameter
				lda: a nrow
				ilo: ilo asParameter
				ihi: ihi asParameter
				scale: scale asParameter.
	info = 0 ifFalse: [self error: 'Balancing a matrix failed'].
"Decompose the matrix a"
	tau := a class nrow: n.
	info := lapack 
				gehrdWithn: n
				ilo: ilo asParameter
				ihi: ihi asParameter
				a: a asParameter
				lda: a nrow
				tau: tau asParameter.
	info = 0 
		ifFalse: [self error: 'Hessenberg decomposition of a matrix failed'].
	h := a copy upperTriangle: -1.
"Generate the orthogonal matrix"
	info := a isComplexMatrix 
				ifTrue: 
					[lapack 
						unghrWithn: n
						ilo: ilo asParameter
						ihi: ihi asParameter
						a: a asParameter
						lda: a nrow
						tau: tau asParameter]
				ifFalse: 
					[lapack 
						orghrWithn: n
						ilo: ilo asParameter
						ihi: ihi asParameter
						a: a asParameter
						lda: a nrow
						tau: tau asParameter].
	info = 0 ifFalse: [self error: 'generation of orthogonal matrix failed'].
	q := a.
	isComputed := true.!

h	self checkDecomposition.	^h!

q	self checkDecomposition.	^q!

reset	q := h := tau := nil.	super reset! !
!LapackHessenbergDecomposition categoriesFor: #decompose!processing!public! !
!LapackHessenbergDecomposition categoriesFor: #h!accessing!public! !
!LapackHessenbergDecomposition categoriesFor: #q!accessing!public! !
!LapackHessenbergDecomposition categoriesFor: #reset!initialize/release!public! !

!LapackHessenbergDecomposition class methodsFor!

minimizeTimesX: aMatrix minus: bMatrix subjectToTimesX: cMatrix equal: dMatrix
	^self new minimizeTimesX: aMatrix minus: bMatrix subjectToTimesX: cMatrix equal: dMatrix! !
!LapackHessenbergDecomposition class categoriesFor: #minimizeTimesX:minus:subjectToTimesX:equal:!instance creation!public! !

LapackLeastSquareProblem guid: (GUID fromString: '{25605C59-E0F5-468B-9235-DCB94CEA40BE}')!
LapackLeastSquareProblem comment: 'LapackLeastSquareProblem solves a least square problem:

solution = arg min over X of || A*X - B ||

where A is the sourceMatrix, B the rhsMatrix.

Several algorithms can be used:
	- a QR factorization (see LAPACK dgelsy)
	- a SVD decomposition (see LAPACK dgelss)
	- a divide and conquer SVD decomposition (see LAPACK dgelsd)

Instance Variables:
	algorithm	<ByteSymbol>	the algorithm to be performed
	rank	<CArrayAccessor>	the rank of A matrix
	rcond	<Number>	condition number
	rhsMatrix	<LapackMatrix>	the matrix B above
	solution	<LapackMatrix>	the matrix A above

'!
!LapackLeastSquareProblem categoriesForClass!Smallapack-Algorithm! !
!LapackLeastSquareProblem methodsFor!

decompose	self perform: algorithm!

decompose: aLapackMatrix	"This message is not appropriated because	the least square problem deal with two matrices, not one"	self shouldNotImplement!

initialize	super initialize.	self solveBySVD!

matrix: aLapackMatrix rhsMatrix: bMatrix 	"This will find x minimizing :	( aLapackMatrix * x -  bMatrix ) norm2"	sourceMatrix := aLapackMatrix.	rhsMatrix := bMatrix.	self reset!

processDivideAndConquerSVD
	"solve by SVD using faster divide and conquer algorithm"

	| a b lapack s m n nrhs |
	isComputed := false.
	a := sourceMatrix copy.
	m := a nrow.
	n := a ncol.
	nrhs := rhsMatrix ncol.
	b := m >= n 
				ifTrue: [rhsMatrix copy]
				ifFalse: [rhsMatrix copy ,, (rhsMatrix class nrow: n - m ncol: nrhs)].
	lapack := sourceMatrix lapackInterface.
	s := sourceMatrix class allocateNrow: (a nrow min: a ncol) ncol: 1.
	rank := SDWORDArray new: 1.
	lapack 
		gelsdWithm: m
		n: n
		nrhs: nrhs
		a: a asParameter
		lda: a nrow
		b: b asParameter
		ldb: b nrow
		s: s asParameter
		rcond: (rcond isNil ifTrue: [sourceMatrix defaultTolerance] ifFalse: [rcond])
		rank: rank asParameter.
	info = 0 
		ifFalse: [self error: 'solving a least square problem by divide and conquer SVD faled'].
	n < m 
		ifTrue: 
			[solution := b class allocateNrow: n ncol: nrhs.
			solution 
				copy: n
				rowsStartingAt: 1
				and: nrhs
				columnsStartingAt: 1
				from: b]
		ifFalse: [solution := b].
	isComputed := true!

processOrthogonalFactorization
	"solve by OrthogonalFactorization"

	| a b lapack jpvt m n nrhs |
	isComputed := false.
	a := sourceMatrix copy.
	m := a nrow.
	n := a ncol.
	nrhs := rhsMatrix ncol.
	b := m >= n 
				ifTrue: [rhsMatrix copy]
				ifFalse: [rhsMatrix copy ,, (rhsMatrix class nrow: n - m ncol: nrhs)].
	lapack := sourceMatrix lapackInterface.
	jpvt := SDWORDArray new: n.
	rank := SDWORDArray new: 1.
	lapack 
		gelsyWithm: m
		n: n
		nrhs: nrhs
		a: a asParameter
		lda: a nrow
		b: b asParameter
		ldb: b nrow
		jpvt: jpvt asParameter
		rcond: (rcond isNil ifTrue: [sourceMatrix defaultTolerance] ifFalse: [rcond])
		rank: rank asParameter.
	info = 0 
		ifFalse: 
			[self 
				error: 'solving a least square problem by Orthogonal Factorization faled'].
	n < m 
		ifTrue: 
			[solution := b class allocateNrow: n ncol: nrhs.
			solution 
				copy: n
				rowsStartingAt: 1
				and: nrhs
				columnsStartingAt: 1
				from: b]
		ifFalse: [solution := b].
	isComputed := true!

processSVD
	"solve by SVD"

	| a b lapack s m n nrhs |
	isComputed := false.
	a := sourceMatrix copy.
	m := a nrow.
	n := a ncol.
	nrhs := rhsMatrix ncol.
	b := m >= n 
				ifTrue: [rhsMatrix copy]
				ifFalse: [rhsMatrix copy ,, (rhsMatrix class nrow: n - m ncol: nrhs)].
	lapack := sourceMatrix lapackInterface.
	s := sourceMatrix class allocateNrow: (a nrow min: a ncol) ncol: 1.
	rank := SDWORDArray new: 1.
	lapack 
		gelssWithm: m
		n: n
		nrhs: nrhs
		a: a asParameter
		lda: a nrow
		b: b asParameter
		ldb: b nrow
		s: s asParameter
		rcond: (rcond isNil ifTrue: [sourceMatrix defaultTolerance] ifFalse: [rcond])
		rank: rank asParameter.
	info = 0 
		ifFalse: [self error: 'solving a least square problem by SVD faled'].
	n < m 
		ifTrue: 
			[solution := b class allocateNrow: n ncol: nrhs.
			solution 
				copy: n
				rowsStartingAt: 1
				and: nrhs
				columnsStartingAt: 1
				from: b]
		ifFalse: [solution := b].
	isComputed := true!

rank	self checkDecomposition.	^rank at: 1!

rankConditionNumber: aNumber 	rcond := aNumber!

residuals
	^rhsMatrix - (sourceMatrix*self solution)!

solution	self checkDecomposition.	^solution!

solveByDivideAndConquerSVD	algorithm := #processDivideAndConquerSVD!

solveByOrthogonalFactorization	algorithm := #processOrthogonalFactorization!

solveBySVD	algorithm := #processSVD! !
!LapackLeastSquareProblem categoriesFor: #decompose!processing!public! !
!LapackLeastSquareProblem categoriesFor: #decompose:!initialize/release!public! !
!LapackLeastSquareProblem categoriesFor: #initialize!initialize/release!public! !
!LapackLeastSquareProblem categoriesFor: #matrix:rhsMatrix:!initialize/release!public! !
!LapackLeastSquareProblem categoriesFor: #processDivideAndConquerSVD!processing!public! !
!LapackLeastSquareProblem categoriesFor: #processOrthogonalFactorization!processing!public! !
!LapackLeastSquareProblem categoriesFor: #processSVD!processing!public! !
!LapackLeastSquareProblem categoriesFor: #rank!accessing!public! !
!LapackLeastSquareProblem categoriesFor: #rankConditionNumber:!accessing!public! !
!LapackLeastSquareProblem categoriesFor: #residuals!accessing!public! !
!LapackLeastSquareProblem categoriesFor: #solution!accessing!public! !
!LapackLeastSquareProblem categoriesFor: #solveByDivideAndConquerSVD!accessing!public! !
!LapackLeastSquareProblem categoriesFor: #solveByOrthogonalFactorization!accessing!public! !
!LapackLeastSquareProblem categoriesFor: #solveBySVD!accessing!public! !

!LapackLeastSquareProblem class methodsFor!

matrix: aLapackMatrix rhsMatrix: bMatrix 	^self new matrix: aLapackMatrix rhsMatrix: bMatrix ! !
!LapackLeastSquareProblem class categoriesFor: #matrix:rhsMatrix:!public! !

LapackPLUdecomposition guid: (GUID fromString: '{86E38315-298A-4A65-95F7-051D75DA774E}')!
LapackPLUdecomposition comment: 'LapackPLUdecomposition perform a Lower Upper (LU) decomposition of a LapackMatrix using LAPACK interface
	A = p*l*u

Instance Variables:
	l	<LapackMatrix>	lower triangle unitary matrix
	p	<LapackMatrix>	permutation matrix
	u	<LapackMatrix>	upper triangle matrix
	a	<LapackMatrix>	packed lu decomposition
	ipiv	<CArrayAccessor> hold permutations
	inverse	<LapackMatrix>	inverse of sourceMatrix
'!
!LapackPLUdecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackPLUdecomposition methodsFor!

computeInverse
	"compute the inverse from the packed lu decomposition"

	| lapack |
	sourceMatrix isSquare 
		ifFalse: [self error: 'inverse is for square matrix'].
	self checkDecomposition.
	inverse := a copy.
	lapack := sourceMatrix lapackInterface.
	info := lapack 
				getriWithn: a nrow
				a: inverse asParameter
				lda: a nrow
				ipiv: ipiv asParameter.
	info = 0 ifFalse: [self error: 'inverse of a general matrix failed']!

decompose
	| min permut lapack |
	min := sourceMatrix nrow min: sourceMatrix ncol.
	a := sourceMatrix copy.
	lapack := sourceMatrix lapackInterface.
	ipiv := SDWORDArray new: a nrow.
	info := lapack 
				getrfWithm: a nrow
				n: a ncol
				a: a asParameter
				lda: a nrow
				ipiv: ipiv asParameter.
	info = 0 ifFalse: [self error: 'PLU decomposition of a matrix failed'].
	l := a lowerTriangle: -1.
	u := a upperTriangle.
	a ncol > min ifTrue: [l := l atColumns: (1 to: min)].
	a nrow > min ifTrue: [u := u atRows: (1 to: min)].
	1 to: min
		do: 
			[:i | 
			l 
				rowAt: i
				columnAt: i
				put: 1].
	permut := (1 to: a nrow) asArray.
	min to: 1
		by: -1
		do: 
			[:i | 
			| k temp |
			k := ipiv at: i.
			temp := permut at: i.
			permut at: i put: (permut at: k).
			permut at: k put: temp].
	p := a class nrow: a nrow ncol: a nrow.
	1 to: a nrow
		do: 
			[:i | 
			p 
				rowAt: i
				columnAt: (permut at: i)
				put: 1].
	isComputed := true!

determinant	| signature |	signature := 1.	1 to: ipiv size		do: [:i | (ipiv at: i) = i ifFalse: [signature := signature negated]].	^(1 to: u nrow) inject: signature		into: [:det :index | det * (u rowAt: index columnAt: index)]!

inverse	inverse isNil ifTrue: [self computeInverse].	^inverse!

ipiv	^ipiv!

l	^l!

p	^p!

plu	^Array with: p with: l with: u!

reset	"Do job immediately"	p := l := u := a := ipiv := inverse := nil.	self decompose!

u	^u! !
!LapackPLUdecomposition categoriesFor: #computeInverse!public! !
!LapackPLUdecomposition categoriesFor: #decompose!public! !
!LapackPLUdecomposition categoriesFor: #determinant!public! !
!LapackPLUdecomposition categoriesFor: #inverse!public! !
!LapackPLUdecomposition categoriesFor: #ipiv!public! !
!LapackPLUdecomposition categoriesFor: #l!public! !
!LapackPLUdecomposition categoriesFor: #p!public! !
!LapackPLUdecomposition categoriesFor: #plu!public! !
!LapackPLUdecomposition categoriesFor: #reset!public! !
!LapackPLUdecomposition categoriesFor: #u!public! !

LapackQRdecomposition guid: (GUID fromString: '{F004BC83-0D81-47AF-9EBE-E757E7B14326}')!
LapackQRdecomposition comment: 'LapackQRdecomposition will perform a Q*R factorisation of a matrix A
	A=Q*R

Q = H1 * H2 * ... * Hk
H(i) = Id - tau(i) * v(i) * v(i) ''

Instance Variables:
	q	<LapackMatrix>	orthonormal matrix
	r	<LapackMatrix>	triangular/trapezoidal matrix
	tau	<LapackMatrix>	elementary reflectors factors forming the matrix q
	qfull	<LapackMatrix>	orthonormal matrix
	rfull	<LapackMatrix>	triangular/trapezoidal matrix

'!
!LapackQRdecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackQRdecomposition methodsFor!

decompose
	| a lapack m n k |
	a := sourceMatrix copy.
	m := a nrow.
	n := a ncol.
	k := m min: n.
	lapack := sourceMatrix lapackInterface.
	tau := a class nrow: k.
	info := lapack 
				geqrfWithm: m
				n: n
				a: a asParameter
				lda: a nrow
				tau: tau asParameter.
	info = 0 ifFalse: [self error: 'QR factorization of a matrix failed'].
	qfull := a copy.
	rfull := a.
	info := a isComplexMatrix 
				ifTrue: 
					[lapack 
						ungqrWithm: m
						n: n
						k: k
						a: qfull asParameter
						lda: qfull nrow
						tau: tau asParameter]
				ifFalse: 
					[lapack 
						orgqrWithm: m
						n: n
						k: k
						a: qfull asParameter
						lda: qfull nrow
						tau: tau asParameter].
	info = 0 ifFalse: [self error: 'generation of orthogonal matrix failed'].
	q := qfull atColumns: (1 to: k).
	r := (rfull atRows: (1 to: k)) upperTriangle.
	rfull := rfull upperTriangle.
	isComputed := true.!

q	self checkDecomposition.	^q!

r	self checkDecomposition.	^r!

reset	q := r := tau := nil.	super reset!

tau	self checkDecomposition.	^tau! !
!LapackQRdecomposition categoriesFor: #decompose!public! !
!LapackQRdecomposition categoriesFor: #q!public! !
!LapackQRdecomposition categoriesFor: #r!public! !
!LapackQRdecomposition categoriesFor: #reset!public! !
!LapackQRdecomposition categoriesFor: #tau!public! !

LapackRiccatiProblem guid: (GUID fromString: '{A585A695-8195-4B9A-9C7C-1838B59985ED}')!
LapackRiccatiProblem comment: 'LapackRiccatiProblem is an abstract class for solving Riccati equations problem.
Each subclass should solve a form of Riccati equation.

Instance Variables:
	algorithm	<ByteSymbol>	name of the algorithm (hold a selector to be performed by this class or subclass)
	epsilon	<Number | nil>	a tolerance for asserting assumptions
	postActionBlock	<BlockClosure>	action to be taken after algortihm has been performed
	solution	<LapackMatrix>	hold the solution of Riccati equation
	sourceMatrix1	<LapacktMatrix>	hold one of the matrices forming Riccati equation (see subclass for details)
	sourceMatrix2	<LapacktMatrix>	hold one of the matrices forming Riccati equation (see subclass for details)
	sourceMatrix3	<LapacktMatrix>	hold one of the matrices forming Riccati equation (see subclass for details)

'!
!LapackRiccatiProblem categoriesForClass!Unclassified! !
!LapackRiccatiProblem methodsFor!

decompose: aLapackMatrix
	"The Riccati problem deal with many matrices, not just one"

	self shouldNotImplement!

initialize
	"set default algorithm to be used"

	super initialize.
	self solveBySchurDecomposition!

postAction: aBlock 
	"set the action to be performed after the algorithm"

	postActionBlock := aBlock!

solution
	self checkDecomposition.
	^solution!

solveA: A B: B Q: Q R: R  
	"Solve the Riccati equation under the classical form of LQR/LQE in Control Theory"

	self subclassResponsibility!

solveA: aMatrix B: bMatrix Q: qMatrix R: rMatrix S: sMatrix
	"Solve the problem under the classical form of LQR/LQE in Control Theory"

	self subclassResponsibility!

solveByDiagonalization
	algorithm := #processDiagonalizationAlgorithm.!

solveBySchurDecomposition
	algorithm := #processSchurDecompositionAlgorithm.!

tolerance
	^epsilon isNil
		ifTrue: [sourceMatrix defaultTolerance]
		ifFalse: [epsilon]!

tolerance: aNumberOrNil 
	epsilon := aNumberOrNil! !
!LapackRiccatiProblem categoriesFor: #decompose:!initialize/release!public! !
!LapackRiccatiProblem categoriesFor: #initialize!initialize/release!public! !
!LapackRiccatiProblem categoriesFor: #postAction:!accessing!public! !
!LapackRiccatiProblem categoriesFor: #solution!accessing!public! !
!LapackRiccatiProblem categoriesFor: #solveA:B:Q:R:!initialize/release!public! !
!LapackRiccatiProblem categoriesFor: #solveA:B:Q:R:S:!initialize/release!public! !
!LapackRiccatiProblem categoriesFor: #solveByDiagonalization!accessing!public! !
!LapackRiccatiProblem categoriesFor: #solveBySchurDecomposition!accessing!public! !
!LapackRiccatiProblem categoriesFor: #tolerance!accessing!public! !
!LapackRiccatiProblem categoriesFor: #tolerance:!accessing!public! !

LapackSchurDecomposition guid: (GUID fromString: '{7C737D59-6ECD-4064-8371-182AE1DCE9D1}')!
LapackSchurDecomposition comment: 'LapackSchurDecomposition does decompose a square matrix in its Schur form

	A = Z * T * Z transposeConjugated

where Z is an orthogonal matrix and T is an upper triangular matrix

LAPACK algorithm can select some eigenValues in the upper diagonal according to a logical select function.

Instance Variables:
	eigenValues	<LapackMatrix>	contain the eigenValues
	schurTriangular	<LapackMatrix>	contain the Schur Triangular Form (T)
	schurVectors	<LapackMatrix>	contain the orthogonal vectors (Z)
	sdim	<CArrayAccessor>	contains number of selected eigenValues
	selectFunction	<ByteSymbol>	the select function usedd to select eigenValues (must be understood by the lapackInterface)
	wantVectors	<Boolean>	indicates whether Schur vectors should be computed or not
'!
!LapackSchurDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackSchurDecomposition methodsFor!

checkEigenValues	self checkDecomposition!

checkSchurVectors	wantVectors 		ifFalse: 			[self wantVectors: true.			isComputed := false].	self checkDecomposition!

eigenValues	self checkEigenValues.	^eigenValues!

initialize	super initialize.	wantVectors := false.
	shouldScale := false.	selectFunction := #selectNone!

numberOfSelectedEigenValues	self checkDecomposition.	^sdim first!

postprocess
	"balance back if necessary"

	(wantVectors and: [shouldScale]) 
		ifTrue: 
			[| lapack |
			lapack := a lapackInterface.
			info := lapack 
						gebakWithjob: lapack balanceScale
						side: lapack right
						n: schurVectors nrow
						ilo: ilo arrayPointer
						ihi: ihi arrayPointer
						scale: scale arrayPointer
						m: schurVectors ncol
						v: schurVectors arrayPointer
						ldv: schurVectors nrow.
			info = 0 ifFalse: [self error: 'Balancing back a matrix failed']]!

preprocess
	"balance if necessary"

	shouldScale 
		ifTrue: 
			[| lapack |
			lapack := a lapackInterface.
			ilo := SDWORDArray new: 1.
			ihi := SDWORDArray new: 1.
			scale := a class realMatrix nrow: a nrow.
			info := lapack 
						gebalWithjob: lapack balanceScale
						n: a nrow
						a: a arrayPointer
						lda: a nrow
						ilo: ilo arrayPointer
						ihi: ihi arrayPointer
						scale: scale arrayPointer.
			info = 0 ifFalse: [self error: 'Balancing a matrix failed']]!

reset	eigenValues := schurTriangular := schurVectors := nil.	super reset!

schurTriangular	self checkDecomposition.	^schurTriangular!

schurVectors	self checkSchurVectors.	^schurVectors!

selectAbsLessThanUnity	selectFunction := #selectAbsLessThanUnity!

selectAbsStriclyLessThanUnity	selectFunction := #selectAbsStriclyLessThanUnity!

selectNegativeReal	selectFunction := #selectNegativeReal!

selectNone	selectFunction := #selectNone!

selectStrictlyNegativeReal	selectFunction := #selectStrictlyNegativeReal!

shouldScale: aBoolean 
	shouldScale := aBoolean!

wantVectors: aBoolean 	wantVectors := aBoolean! !
!LapackSchurDecomposition categoriesFor: #checkEigenValues!private! !
!LapackSchurDecomposition categoriesFor: #checkSchurVectors!private! !
!LapackSchurDecomposition categoriesFor: #eigenValues!accessing!public! !
!LapackSchurDecomposition categoriesFor: #initialize!initialize/release!public! !
!LapackSchurDecomposition categoriesFor: #numberOfSelectedEigenValues!accessing!public! !
!LapackSchurDecomposition categoriesFor: #postprocess!processing!public! !
!LapackSchurDecomposition categoriesFor: #preprocess!processing!public! !
!LapackSchurDecomposition categoriesFor: #reset!initialize/release!public! !
!LapackSchurDecomposition categoriesFor: #schurTriangular!accessing!public! !
!LapackSchurDecomposition categoriesFor: #schurVectors!accessing!public! !
!LapackSchurDecomposition categoriesFor: #selectAbsLessThanUnity!accessing!public! !
!LapackSchurDecomposition categoriesFor: #selectAbsStriclyLessThanUnity!accessing!public! !
!LapackSchurDecomposition categoriesFor: #selectNegativeReal!accessing!public! !
!LapackSchurDecomposition categoriesFor: #selectNone!accessing!public! !
!LapackSchurDecomposition categoriesFor: #selectStrictlyNegativeReal!accessing!public! !
!LapackSchurDecomposition categoriesFor: #shouldScale:!accessing!public! !
!LapackSchurDecomposition categoriesFor: #wantVectors:!accessing!public! !

LapackSVDecomposition guid: (GUID fromString: '{87C45F13-41A1-41D8-8EB2-10945C37039B}')!
LapackSVDecomposition comment: 'LapackSVDdecomposition perform a Singular Value Decomposition (SVD) of a LapackMatrix using LAPACK interface
	A =U*sigma*V transposeConjugated

if A is (m,n) Matrix then
sigma is a (m,n) matrix null off diagonal
U is a (m,m) orthogonal Matrix
and V is a (n,n) orthogonal Matrix

Instance Variables:
	u	<LapackMatrix>	left orthogonal matrix, may have only first min(m,n) columns
	s	<LapackMatrix>	singular values (diagonal of sigma)
	vt	<LapackMatrix>	right orthogonal matrix (transposeConjugated), may have only first min(m,n) rows
	whichLeftVector	<Symbol> indicate which left vectors should be computed, either #allSingularVector #someSingularVector #noSingularVector
	whichRightVector	<Symbol> idem for right vectors'!
!LapackSVDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackSVDecomposition methodsFor!

checkLeftSingularVectors	whichLeftVector == #noSingularVector 		ifTrue: 			[whichLeftVector := #allSingularVector.			isComputed := false].	self checkDecomposition!

checkRightSingularVectors	whichRightVector == #noSingularVector 		ifTrue: 			[whichRightVector := #allSingularVector.			isComputed := false].	self checkDecomposition!

checkSingularValues	self checkDecomposition!

decompose
	| a k |
	a := sourceMatrix copy.
	k := a nrow min: a ncol.
	s := a class realMatrix allocateNrow: k ncol: 1.
	u := whichLeftVector == #allSingularVector 
				ifTrue: [a class allocateNrow: a nrow ncol: a nrow]
				ifFalse: 
					[whichLeftVector := #someSingularVector. "DOLPHIN HAS A PROBLEM DETECTING A ZERO DIVIDE when noSingularVector"
					whichLeftVector == #someSingularVector
						ifTrue: [a class allocateNrow: a nrow ncol: k]
						ifFalse: [a class allocateNrow: 1 ncol: 1]].
	vt := whichRightVector == #allSingularVector
				ifTrue: [a class allocateNrow: a ncol ncol: a ncol]
				ifFalse: 
					[whichRightVector := #someSingularVector. "DOLPHIN HAS A PROBLEM DETECTING A ZERO DIVIDE when noSingularVector"
					whichRightVector == #someSingularVector 
						ifTrue: [a class allocateNrow: k ncol: a ncol]
						ifFalse: [a class allocateNrow: 1 ncol: 1]].
	info := a lapackInterface 
				gesvdWithjobu: (a lapackInterface perform: whichLeftVector)
				jobvt: (a lapackInterface perform: whichRightVector)
				m: a nrow
				n: a ncol
				a: a asParameter
				lda: a nrow
				s: s asParameter
				u: u asParameter
				ldu: u nrow
				vt: vt asParameter
				ldvt: vt nrow.
	info = 0 ifFalse: [self error: 'A Singular Value Decomposition failed'].
	isComputed := true!

initialize	whichLeftVector := #someSingularVector.	whichRightVector := #someSingularVector.	super initialize!

leftSingularVectors	^self u!

reset	u := nil.	s := nil.	vt := nil.	super reset.!

rightSingularVectors	^self v!

s	self checkSingularValues.	^s!

singularValues	^self s!

u	self checkLeftSingularVectors.	^u!

ut	^self u transposeConjugated!

v	^self vt transposeConjugated!

vt	self checkRightSingularVectors.	^vt!

wantAllLeftSingularVector	whichLeftVector := #allSingularVector!

wantAllRightSingularVector	whichRightVector := #allSingularVector!

wantAllSingularVector	self		wantAllLeftSingularVector;		wantAllRightSingularVector!

wantNoLeftSingularVector	whichLeftVector := #noSingularVector!

wantNoRightSingularVector	whichRightVector := #noSingularVector!

wantNoSingularVector	self		wantNoLeftSingularVector;		wantNoRightSingularVector!

wantSomeLeftSingularVector	whichLeftVector := #someSingularVector!

wantSomeRightSingularVector	whichRightVector := #someSingularVector!

wantSomeSingularVector	self		wantSomeLeftSingularVector;		wantSomeRightSingularVector! !
!LapackSVDecomposition categoriesFor: #checkLeftSingularVectors!public! !
!LapackSVDecomposition categoriesFor: #checkRightSingularVectors!public! !
!LapackSVDecomposition categoriesFor: #checkSingularValues!public! !
!LapackSVDecomposition categoriesFor: #decompose!public! !
!LapackSVDecomposition categoriesFor: #initialize!public! !
!LapackSVDecomposition categoriesFor: #leftSingularVectors!public! !
!LapackSVDecomposition categoriesFor: #reset!public! !
!LapackSVDecomposition categoriesFor: #rightSingularVectors!public! !
!LapackSVDecomposition categoriesFor: #s!public! !
!LapackSVDecomposition categoriesFor: #singularValues!public! !
!LapackSVDecomposition categoriesFor: #u!public! !
!LapackSVDecomposition categoriesFor: #ut!public! !
!LapackSVDecomposition categoriesFor: #v!public! !
!LapackSVDecomposition categoriesFor: #vt!public! !
!LapackSVDecomposition categoriesFor: #wantAllLeftSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantAllRightSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantAllSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantNoLeftSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantNoRightSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantNoSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantSomeLeftSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantSomeRightSingularVector!public! !
!LapackSVDecomposition categoriesFor: #wantSomeSingularVector!public! !

LapackComplexEigenDecomposition guid: (GUID fromString: '{9C718DDB-0882-4E6B-AB0B-808349FDA327}')!
LapackComplexEigenDecomposition comment: 'LapackComplexEigenDecomposition is specialized for complex matrices.
'!
!LapackComplexEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackComplexEigenDecomposition methodsFor!

decompose
	| a n lapack |
	isComputed := false.
	a := sourceMatrix copy.
	n := a nrow.
	eigenValues := a class allocateNrow: n ncol: 1.
	leftEigenVectors := wantLeft 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	rightEigenVectors := wantRight 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	lapack := a lapackInterface.
	info := lapack 
				geevWithjobvl: (wantLeft 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				jobvr: (wantRight 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				n: n
				a: a asParameter
				lda: a nrow
				w: eigenValues asParameter
				vl: leftEigenVectors asParameter
				ldvl: leftEigenVectors nrow
				vr: rightEigenVectors asParameter
				ldvr: rightEigenVectors nrow.
	info = 0 ifFalse: [self error: 'eigen value decomposition failed'].
	isComputed := true! !
!LapackComplexEigenDecomposition categoriesFor: #decompose!public! !

LapackGeneralizedEigenDecomposition guid: (GUID fromString: '{D06E90A5-5F55-49DD-B055-38420D089703}')!
LapackGeneralizedEigenDecomposition comment: ''!
!LapackGeneralizedEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackGeneralizedEigenDecomposition methodsFor!

alpha
	self checkAlpha.
	^alpha!

beta
	self checkBeta.
	^beta!

checkAlpha
	self checkDecomposition!

checkBeta
	self checkDecomposition!

decompose: aLapackMatrix	"The generalized eigen value problem deal with two matrices, not one"	self shouldNotImplement!

decomposeLeft: aMatrix right: bMatrix 	sourceMatrix := aMatrix.	rhsMatrix := bMatrix.	self reset!

reset
	alpha := nil.
	beta := nil.
	super reset.! !
!LapackGeneralizedEigenDecomposition categoriesFor: #alpha!accessing!public! !
!LapackGeneralizedEigenDecomposition categoriesFor: #beta!accessing!public! !
!LapackGeneralizedEigenDecomposition categoriesFor: #checkAlpha!private! !
!LapackGeneralizedEigenDecomposition categoriesFor: #checkBeta!private! !
!LapackGeneralizedEigenDecomposition categoriesFor: #decompose:!initialize/release!public! !
!LapackGeneralizedEigenDecomposition categoriesFor: #decomposeLeft:right:!initialize/release!public! !
!LapackGeneralizedEigenDecomposition categoriesFor: #reset!initialize/release!public! !

!LapackGeneralizedEigenDecomposition class methodsFor!

decomposeLeft: leftMatrix right: rightMatrix	"The generalized eigen value problem deal with two matrices, not one"	^self new decomposeLeft: leftMatrix right: rightMatrix! !
!LapackGeneralizedEigenDecomposition class categoriesFor: #decomposeLeft:right:!public! !

LapackHermitianEigenDecomposition guid: (GUID fromString: '{9C5A6F4A-6458-437D-A079-AB8931FA6F69}')!
LapackHermitianEigenDecomposition comment: 'LapackHermitianEigenDecomposition is specialized for complex hermitian / real symmetric matrices

Instance Variables:
	algorithm	<Symbol>	hold the name of a method implemented in self to process decomposition'!
!LapackHermitianEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackHermitianEigenDecomposition methodsFor!

decompose	self perform: algorithm!

initialize	super initialize.	self solveByStandardAlgorithm!

processDivideAndConquer
	| a n lapack |
	isComputed := false.
	n := sourceMatrix nrow.
	eigenValues := sourceMatrix class realMatrix nrow: n.
	a := sourceMatrix copy.
	rightEigenVectors := leftEigenVectors := a castTo: a class generalMatrix.
	a isBothUpperLower ifTrue: [a beUpper].
	lapack := a lapackInterface.
	info := a isComplexMatrix 
				ifTrue: 
					[lapack 
						heevdWithjobz: (wantLeft 
								ifTrue: [lapack doComputeVector]
								ifFalse: [lapack dontComputeVector])
						uplo: (a isUpper ifTrue: [lapack upper] ifFalse: [lapack lower])
						n: n
						a: a asParameter
						lda: a nrow
						w: eigenValues asParameter]
				ifFalse: 
					[lapack 
						syevdWithjobz: (wantLeft 
								ifTrue: [lapack doComputeVector]
								ifFalse: [lapack dontComputeVector])
						uplo: (a isUpper ifTrue: [lapack upper] ifFalse: [lapack lower])
						n: n
						a: a asParameter
						lda: a nrow
						w: eigenValues asParameter].
	info = 0 ifFalse: [self error: 'eigen value decomposition failed'].
	isComputed := true!

processRelativelyRobustRepresentation	self error: 'not implemented yet'!

processStandardAlgorithm
	| a n lapack |
	isComputed := false.
	n := sourceMatrix nrow.
	eigenValues := sourceMatrix class realMatrix nrow: n.
	a := sourceMatrix copy.
	rightEigenVectors := leftEigenVectors := a castTo: a class generalMatrix.
	a isBothUpperLower ifTrue: [a beUpper].
	lapack := a lapackInterface.
	info := a isComplexMatrix 
				ifTrue: 
					[lapack 
						heevWithjobz: (wantLeft 
								ifTrue: [lapack doComputeVector]
								ifFalse: [lapack dontComputeVector])
						uplo: (a isUpper ifTrue: [lapack upper] ifFalse: [lapack lower])
						n: n
						a: a asParameter
						lda: a nrow
						w: eigenValues asParameter]
				ifFalse: 
					[lapack 
						syevWithjobz: (wantLeft 
								ifTrue: [lapack doComputeVector]
								ifFalse: [lapack dontComputeVector])
						uplo: (a isUpper ifTrue: [lapack upper] ifFalse: [lapack lower])
						n: n
						a: a asParameter
						lda: a nrow
						w: eigenValues asParameter].
	info = 0 ifFalse: [self error: 'eigen value decomposition failed'].
	isComputed := true!

solveByDivideAndConquer	algorithm := #processDivideAndConquer.	isComputed := false!

solveByRelativelyRobustRepresentation	algorithm := #processRelativelyRobustRepresentation.	isComputed := false!

solveByStandardAlgorithm	algorithm := #processStandardAlgorithm.	isComputed := false!

wantLeftEigenVectors: aBoolean 	wantLeft := wantRight := aBoolean!

wantRightEigenVectors: aBoolean 	wantLeft := wantRight := aBoolean! !
!LapackHermitianEigenDecomposition categoriesFor: #decompose!public! !
!LapackHermitianEigenDecomposition categoriesFor: #initialize!public! !
!LapackHermitianEigenDecomposition categoriesFor: #processDivideAndConquer!public! !
!LapackHermitianEigenDecomposition categoriesFor: #processRelativelyRobustRepresentation!public! !
!LapackHermitianEigenDecomposition categoriesFor: #processStandardAlgorithm!public! !
!LapackHermitianEigenDecomposition categoriesFor: #solveByDivideAndConquer!public! !
!LapackHermitianEigenDecomposition categoriesFor: #solveByRelativelyRobustRepresentation!public! !
!LapackHermitianEigenDecomposition categoriesFor: #solveByStandardAlgorithm!public! !
!LapackHermitianEigenDecomposition categoriesFor: #wantLeftEigenVectors:!public! !
!LapackHermitianEigenDecomposition categoriesFor: #wantRightEigenVectors:!public! !

LapackRealEigenDecomposition guid: (GUID fromString: '{1CB13561-6CFA-4D38-A655-153DAD7F5E86}')!
LapackRealEigenDecomposition comment: 'LapackRealEigenDecomposition is specialized for real matrices.
In this case, underlying lapack routine does handle only real matrix.
It separates eigenvalue real part from imaginary parts (thus two arguments wr and wi are needed)
The storing of eigenvectors is also tricky: in case of complex eigen value,
there are two associated eigen vectors (one is conjugated of the other).
One column will contain realPart, the following will contain the imaginaryPart.

Instance Variables:
	wr	<GeneralLapackMatrix>	realPart of eigenvalues
	wi	<GeneralLapackMatrix>	imaginaryPart of eigenvalues
	vl	<GeneralLapackMatrix>	left eigenvectors in raw format
	vr	<GeneralLapackMatrix>	right eigenvectors in raw format

'!
!LapackRealEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackRealEigenDecomposition methodsFor!

checkEigenValues	super checkEigenValues.	eigenValues isNil 		ifTrue: [eigenValues := wi isZero ifTrue: [wr] ifFalse: [wr i: wi]]!

checkLeftEigenVectors	super checkLeftEigenVectors.	leftEigenVectors isNil 		ifTrue: [leftEigenVectors := self postConjugate: vl]!

checkRightEigenVectors	super checkRightEigenVectors.	rightEigenVectors isNil 		ifTrue: [rightEigenVectors := self postConjugate: vr]!

checkVl	wantLeft 		ifFalse: 			[wantLeft := true.			isComputed := false].	self checkDecomposition!

checkVr	wantRight 		ifFalse: 			[wantRight := true.			isComputed := false].	self checkDecomposition!

checkWi	self checkDecomposition!

checkWr	self checkDecomposition!

decompose
	| a n lapack |
	isComputed := false.
	a := sourceMatrix copy.
	n := a nrow.
	wr := a class allocateNrow: n ncol: 1.
	wi := a class allocateNrow: n ncol: 1.
	vl := wantLeft 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	vr := wantRight 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	lapack := a lapackInterface.
	info := lapack 
				geevWithjobvl: (wantLeft 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				jobvr: (wantRight 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				n: n
				a: a asParameter
				lda: a nrow
				wr: wr asParameter
				wi: wi asParameter
				vl: vl asParameter
				ldvl: vl nrow
				vr: vr asParameter
				ldvr: vr nrow.
	info = 0 ifFalse: [self error: 'eigen value decomposition failed'].
	isComputed := true!

eigenvalues	eigenValues isNil 		ifTrue: [eigenValues := wi isZero ifTrue: [wr] ifFalse: [wr i: wi]].	^eigenValues!

postConjugate: aMatrix 	"convert from raw format to complex matrix.	This routine is not fully optimized (one loop in Smalltalk)"	| res iCol comp |	res := aMatrix asComplexMatrix.	iCol := 0.	[iCol < aMatrix ncol] whileTrue: 			[iCol := iCol + 1.			(wi at: iCol) isZero 				ifFalse: 					[comp := (aMatrix columnAt: iCol) i: (aMatrix columnAt: iCol + 1).					res columnAt: iCol putSequence: comp.					iCol := iCol + 1.					res columnAt: iCol putSequence: comp conjugated]].	^comp isNil ifTrue: [aMatrix] ifFalse: [res]!

reset	wr := wi := vl := vr := nil.	super reset!

vl	self checkVl.	^vl!

vr	self checkVr.	^vr!

wi	self checkWi.	^wi!

wr	self checkWr.	^wr! !
!LapackRealEigenDecomposition categoriesFor: #checkEigenValues!public! !
!LapackRealEigenDecomposition categoriesFor: #checkLeftEigenVectors!public! !
!LapackRealEigenDecomposition categoriesFor: #checkRightEigenVectors!public! !
!LapackRealEigenDecomposition categoriesFor: #checkVl!public! !
!LapackRealEigenDecomposition categoriesFor: #checkVr!public! !
!LapackRealEigenDecomposition categoriesFor: #checkWi!public! !
!LapackRealEigenDecomposition categoriesFor: #checkWr!public! !
!LapackRealEigenDecomposition categoriesFor: #decompose!public! !
!LapackRealEigenDecomposition categoriesFor: #eigenvalues!public! !
!LapackRealEigenDecomposition categoriesFor: #postConjugate:!public! !
!LapackRealEigenDecomposition categoriesFor: #reset!public! !
!LapackRealEigenDecomposition categoriesFor: #vl!public! !
!LapackRealEigenDecomposition categoriesFor: #vr!public! !
!LapackRealEigenDecomposition categoriesFor: #wi!public! !
!LapackRealEigenDecomposition categoriesFor: #wr!public! !

LapackComplexGeneralizedEigenDecomposition guid: (GUID fromString: '{03D947B1-7FE1-481E-B6F9-1B8C571079D1}')!
LapackComplexGeneralizedEigenDecomposition comment: ''!
!LapackComplexGeneralizedEigenDecomposition categoriesForClass!Unclassified! !
!LapackComplexGeneralizedEigenDecomposition methodsFor!

decompose
	| a b n lapack |
	isComputed := false.
	a := sourceMatrix copy.
	b := rhsMatrix copy.
	n := a nrow.
	alpha := a class allocateNrow: n ncol: 1.
	beta := a class allocateNrow: n ncol: 1.
	leftEigenVectors := wantLeft 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	rightEigenVectors := wantRight 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	lapack := a lapackInterface.
	info := lapack 
				ggevWithjobvl: (wantLeft 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				jobvr: (wantRight 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				n: n
				a: a arrayPointer
				lda: a nrow
				b: b arrayPointer
				ldb: b nrow
				alpha: alpha arrayPointer
				beta: beta arrayPointer
				vl: leftEigenVectors arrayPointer
				ldvl: leftEigenVectors nrow
				vr: rightEigenVectors arrayPointer
				ldvr: rightEigenVectors nrow.
	info = 0 
		ifFalse: [self error: 'generalized eigen value decomposition failed'].
	isComputed := true! !
!LapackComplexGeneralizedEigenDecomposition categoriesFor: #decompose!processing!public! !

LapackHermitianGeneralizedEigenDecomposition guid: (GUID fromString: '{1F8F1C61-8FAA-4196-8E3E-702E81E9BF51}')!
LapackHermitianGeneralizedEigenDecomposition comment: ''!
!LapackHermitianGeneralizedEigenDecomposition categoriesForClass!Unclassified! !
LapackHermitianPackedGeneralizedEigenDecomposition guid: (GUID fromString: '{67CEDF48-4079-4103-93D9-F8983F35CB33}')!
LapackHermitianPackedGeneralizedEigenDecomposition comment: ''!
!LapackHermitianPackedGeneralizedEigenDecomposition categoriesForClass!Unclassified! !
LapackRealGeneralizedEigenDecomposition guid: (GUID fromString: '{C03D87DB-D4EA-4ECE-AC8A-8768B1EC46DD}')!
LapackRealGeneralizedEigenDecomposition comment: ''!
!LapackRealGeneralizedEigenDecomposition categoriesForClass!Unclassified! !
!LapackRealGeneralizedEigenDecomposition methodsFor!

checkAlpha
	self
		checkAlphar;
		checkAlphai.
	alpha isNil 
		ifTrue: [alpha := alphai isZero ifTrue: [alphar] ifFalse: [alphar i: alphai]]!

checkAlphai
	self checkDecomposition!

checkAlphar
	self checkDecomposition!

checkLeftEigenVectors
	super checkLeftEigenVectors.
	leftEigenVectors isNil 
		ifTrue: [leftEigenVectors := self postConjugate: vl]!

checkRightEigenVectors
	super checkRightEigenVectors.
	rightEigenVectors isNil 
		ifTrue: [rightEigenVectors := self postConjugate: vr]!

checkVl
	wantLeft 
		ifFalse: 
			[wantLeft := true.
			isComputed := false].
	self checkDecomposition!

checkVr
	wantRight 
		ifFalse: 
			[wantRight := true.
			isComputed := false].
	self checkDecomposition!

decompose
	| a b n lapack |
	isComputed := false.
	a := sourceMatrix copy.
	b := rhsMatrix copy.
	n := a nrow.
	alphar := a class allocateNrow: n ncol: 1.
	alphai := a class allocateNrow: n ncol: 1.
	beta := a class allocateNrow: n ncol: 1.
	vl := wantLeft 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	vr := wantRight 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	lapack := a lapackInterface.
	info := lapack 
				ggevWithjobvl: (wantLeft 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				jobvr: (wantRight 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				n: n
				a: a arrayPointer
				lda: a nrow
				b: b arrayPointer
				ldb: b nrow
				alphar: alphar arrayPointer
				alphai: alphai arrayPointer
				beta: beta arrayPointer
				vl: vl arrayPointer
				ldvl: vl nrow
				vr: vr arrayPointer
				ldvr: vr nrow.
	info = 0 
		ifFalse: [self error: 'generalized eigen value decomposition failed'].
	isComputed := true!

postConjugate: aMatrix 
	"convert from raw format to complex matrix.
	This routine is not fully optimized (one loop in Smalltalk)"

	| res iCol comp |
	res := aMatrix asComplexMatrix.
	iCol := 0.
	[iCol < aMatrix ncol] whileTrue: 
			[iCol := iCol + 1.
			(alphai at: iCol) isZero 
				ifFalse: 
					[comp := (aMatrix columnAt: iCol) i: (aMatrix columnAt: iCol + 1).
					res columnAt: iCol putSequence: comp.
					iCol := iCol + 1.
					res columnAt: iCol putSequence: comp conjugated]].
	^comp isNil ifTrue: [aMatrix] ifFalse: [res]!

reset
	alphar := alphai := vl := vr := nil.
	super reset! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #checkAlpha!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #checkAlphai!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #checkAlphar!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #checkLeftEigenVectors!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #checkRightEigenVectors!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #checkVl!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #checkVr!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #decompose!processing!public! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #postConjugate:!private! !
!LapackRealGeneralizedEigenDecomposition categoriesFor: #reset!initialize/release!public! !

LapackDiagonalEigenDecomposition guid: (GUID fromString: '{48D331A3-B284-4E99-8E1F-D7BC4ED2A26C}')!
LapackDiagonalEigenDecomposition comment: 'LapackDiagonalEigenDecomposition is for the trivial case of diagonal matrix'!
!LapackDiagonalEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackDiagonalEigenDecomposition methodsFor!

decompose	eigenValues := sourceMatrix diagonal.	rightEigenVectors := leftEigenVectors := wantLeft 						ifTrue: [sourceMatrix class eye: sourceMatrix nrow]						ifFalse: [nil].	isComputed := true! !
!LapackDiagonalEigenDecomposition categoriesFor: #decompose!public! !

LapackLeastSquareProblemWithEqualityConstraints guid: (GUID fromString: '{CFB73F29-75E8-41DE-9C94-6B1A774ACEB9}')!
LapackLeastSquareProblemWithEqualityConstraints comment: 'LapackLeastSquareProblemWithEqualityConstraints solves a least square problem subject to linear equality constraints:

solution = arg min over x of || A*x - b || subject to C*x=d

where
	A of size(m,n) is the sourceMatrix,
	b of size (m,1) is the rhsMatrix,
	C of size (p,n) is the equalityLHSmatrix
	d of size (p,1) is the equalityRHSmatrix.
	|| M || denotes the L2 norm of M.

Several algorithms can be used:
	- a QR factorization (see LAPACK dgglse)
	- a generalized SVD decomposition (see LAPACK dggsvd)

BEWARE: In case of QR algorithm, LAPACK routine dgglse requires
- p <= n <= m+p
- a matrix C of full rank rank(C)=p
- a matrix [A ; C] of full rank([A;C])=n

In case of a generalized SVD algorithm, no such assumption is made. Algorithm is based on ideas from http://kom.aau.dk/~borre/kalman/lecture2/p403.pdf
System is written under diagonal form
	Ut*A*X=[0 D1]
	Vt*C*X=[0 D2]
since Ut is orthogonal, the L2 norm is unchanged:
	|| Ut*A*x-Ut*b || = || A*x-b ||
We thus solve a diagonal system versus y=inv(X)*x:
	min || Ut*A*X*y-Ut*b || subject to Vt*C*X*y=Vt*d
which is hopefully diagonal
	|| [0 D1]*y-Ut*b || subject to [0 D2]*y=Vt*d
This leads to a trivial system to solve in y,
	l terms are solved from equality constraints (l = rank(B))
	either k or m-k terms are solved from minimization problem (m=nrow(A), k=rank([A;C])-rank(C))
remaining terms are set to zero.
x can then be retrieved easily:
	x=X*y


Instance Variables:
	equalityLHSmatrix	<LapackMatrix>	the C matrix in left hand side of equality constraints equation above
	equalityRHSmatrix	<LapackMatrix>	the d matrix in right hand side of equality constraints equation above

'!
!LapackLeastSquareProblemWithEqualityConstraints categoriesForClass!Unclassified! !
!LapackLeastSquareProblemWithEqualityConstraints methodsFor!

equalityResiduals
	"answer the residuals from equality constraints"
	
	^equalityRHSmatrix - (equalityLHSmatrix * self solution)!

matrix: aLapackMatrix rhsMatrix: bMatrix 
	self shouldNotImplement!

minimizeTimesX: aMatrix minus: bMatrix subjectToTimesX: cMatrix equal: dMatrix
	"This will solve min ||a*x-b|| subject to c*x=d"
	
	sourceMatrix := aMatrix.
	rhsMatrix := bMatrix.
	equalityLHSmatrix := cMatrix.
	equalityRHSmatrix := dMatrix.
	self reset!

processDivideAndConquerSVD
	self error: 'Not implemented'!

processOrthogonalFactorization
	"Beware: this applies to restricted cases. See the assumptions in class comment."
	
	| a b c d x m n p |
	isComputed := false.
	a := sourceMatrix copy.
	b := equalityLHSmatrix copy.
	c := rhsMatrix copy.
	d := equalityRHSmatrix copy.
	
	a isComplexMatrix ifTrue: [b := b asComplexMatrix. c := c asComplexMatrix. d := d asComplexMatrix].
	b isComplexMatrix ifTrue: [a := a asComplexMatrix. c := c asComplexMatrix. d := d asComplexMatrix].
	c isComplexMatrix ifTrue: [a := a asComplexMatrix. b := b asComplexMatrix. d := d asComplexMatrix].
	d isComplexMatrix ifTrue: [a := a asComplexMatrix. b := b asComplexMatrix. c := c asComplexMatrix].
	a isDoublePrecisionMatrix ifTrue: [b := b asDoublePrecisionMatrix. c := c asDoublePrecisionMatrix. d := d asDoublePrecisionMatrix].
	b isDoublePrecisionMatrix ifTrue: [a := a asDoublePrecisionMatrix. c := c asDoublePrecisionMatrix. d := d asDoublePrecisionMatrix].
	c isDoublePrecisionMatrix ifTrue: [a := a asDoublePrecisionMatrix. b := b asDoublePrecisionMatrix. d := d asDoublePrecisionMatrix].
	d isDoublePrecisionMatrix ifTrue: [a := a asDoublePrecisionMatrix. b := b asDoublePrecisionMatrix. c := c asDoublePrecisionMatrix].
	
	m := a nrow.
	n := a ncol.
	p := b nrow.
	x := a class nrow: n ncol: 1.
	info := a lapackInterface 
		gglseWithm: m
		n: n
		p: p
		a: a asParameter
		lda: a nrow
		b: b asParameter
		ldb: b nrow
		c: c asParameter
		d: d asParameter
		x: x asParameter.
	info = 0 
		ifFalse: 
			[self 
				error: 'solving a generalized least square problem by Orthogonal Factorization failed'].
	solution := x.
	isComputed := true
	!

processSVD
	"solve by generalized SVD"

	| gsvd ut vt x alpha beta k l n temp |
	gsvd := LapackGeneralizedSVDecomposition decomposeA: sourceMatrix B: equalityLHSmatrix.
	ut := gsvd U transposeConjugated.
	vt := gsvd V transposeConjugated.
	x := gsvd X.
	alpha := gsvd alpha.
	beta := gsvd beta.
	k := gsvd k.
	l := gsvd l.
	n := sourceMatrix ncol.
	solution := sourceMatrix class nrow: n ncol: 1.
	1 to: l do: [:i |
		temp := ((vt rowAt: i) * equalityRHSmatrix at: 1) / (beta at: k + i).
		solution := solution + (temp * (x columnAt: n - l + i))].
	1 to: k do: [:i |
			temp := ((ut rowAt: i) * rhsMatrix at: 1) / (alpha at: i).
			solution := solution + (temp * (x columnAt: n - k - l + i))].
	isComputed := true! !
!LapackLeastSquareProblemWithEqualityConstraints categoriesFor: #equalityResiduals!accessing!public! !
!LapackLeastSquareProblemWithEqualityConstraints categoriesFor: #matrix:rhsMatrix:!initialize/release!public! !
!LapackLeastSquareProblemWithEqualityConstraints categoriesFor: #minimizeTimesX:minus:subjectToTimesX:equal:!initialize/release!public! !
!LapackLeastSquareProblemWithEqualityConstraints categoriesFor: #processDivideAndConquerSVD!public! !
!LapackLeastSquareProblemWithEqualityConstraints categoriesFor: #processOrthogonalFactorization!public! !
!LapackLeastSquareProblemWithEqualityConstraints categoriesFor: #processSVD!public! !

!LapackLeastSquareProblemWithEqualityConstraints class methodsFor!

minimizeTimesX: aMatrix minus: bMatrix subjectToTimesX: cMatrix equal: dMatrix
	^self new minimizeTimesX: aMatrix minus: bMatrix subjectToTimesX: cMatrix equal: dMatrix! !
!LapackLeastSquareProblemWithEqualityConstraints class categoriesFor: #minimizeTimesX:minus:subjectToTimesX:equal:!instance creation!public! !

LapackHermitianPLUdecomposition guid: (GUID fromString: '{23EBA227-F472-42F2-8510-73C49793BD12}')!
LapackHermitianPLUdecomposition comment: 'LapackHermitianPLUdecomposition is LapackPLUDecomposition specialized in HermitianMatrix

Bunch Kaufman pivoting'!
!LapackHermitianPLUdecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackHermitianPLUdecomposition methodsFor!

computeInverse
	"compute the inverse from the packed lu decomposition"

	| lapack |
	self checkDecomposition.
	inverse := a copy.
	lapack := sourceMatrix lapackInterface.
	sourceMatrix isComplexMatrix 
		ifTrue: 
			[info := lapack 
						hetriWithuplo: (sourceMatrix isUpper 
								ifTrue: [lapack upper]
								ifFalse: [lapack lower])
						n: a nrow
						a: inverse asParameter
						lda: a nrow
						ipiv: ipiv asParameter.
			info = 0 
				ifFalse: 
					[inverse := nil.
					self error: 'inverse of a hermitian matrix failed']]
		ifFalse: 
			[info := lapack 
						sytriWithuplo: (sourceMatrix isUpper 
								ifTrue: [lapack upper]
								ifFalse: [lapack lower])
						n: a nrow
						a: inverse asParameter
						lda: a nrow
						ipiv: ipiv asParameter.
			info = 0 
				ifFalse: 
					[inverse := nil.
					self error: 'inverse of a hermitian matrix failed']]!

decompose
	| lapack |
	a := sourceMatrix copy.
	sourceMatrix isUpper ifTrue: [a beUpper] ifFalse: [a beLower].
	lapack := sourceMatrix lapackInterface.
	ipiv := SDWORDArray new: a nrow.
	info := lapack 
				hetrfWithuplo: (a isUpper ifTrue: [lapack upper] ifFalse: [lapack lower])
				n: a nrow
				a: a asParameter
				lda: a nrow
				ipiv: ipiv asParameter.
	info = 0 
		ifFalse: [self error: 'PLU decomposition of a hermitian matrix failed'].
	isComputed := true! !
!LapackHermitianPLUdecomposition categoriesFor: #computeInverse!public! !
!LapackHermitianPLUdecomposition categoriesFor: #decompose!public! !

LapackTriangularPLUdecomposition guid: (GUID fromString: '{77E5825E-7388-4FA2-B584-656BB379F011}')!
LapackTriangularPLUdecomposition comment: 'LapackTriangularPLUdecomposition  is LapackPLUDecomposition specialized in TriangularMatrix

If Matrix is already upper, then nothing to do.'!
!LapackTriangularPLUdecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackTriangularPLUdecomposition methodsFor!

computeInverse	"compute the inverse from the packed lu decomposition"	sourceMatrix isTriangularMatrix 		ifTrue: [inverse := sourceMatrix reciprocal]		ifFalse: [super computeInverse]!

decompose
	sourceMatrix isUpper 
		ifTrue: 
			["When the matrix is already upper, then nothing to do"

			| n |
			n := sourceMatrix nrow.
			ipiv := SDWORDArray new: n.
			1 to: n do: [:i | ipiv at: i put: i].
			info := 0.
			p := sourceMatrix class eye: n.
			l := sourceMatrix class eye: n.
			a := u := sourceMatrix.
			isComputed := true]
		ifFalse: 
			["Fall back to general algorithm.
			There should be smarter solution..."

			sourceMatrix := sourceMatrix castTo: sourceMatrix class generalMatrix.
			super decompose]!

determinant	^sourceMatrix diagonal product! !
!LapackTriangularPLUdecomposition categoriesFor: #computeInverse!public! !
!LapackTriangularPLUdecomposition categoriesFor: #decompose!public! !
!LapackTriangularPLUdecomposition categoriesFor: #determinant!public! !

LapackDiagonalPLUdecomposition guid: (GUID fromString: '{8CA23584-CBAE-4CBA-BB51-C4A936BAB48F}')!
LapackDiagonalPLUdecomposition comment: 'LapackDiagonalPLUdecomposition is for trivial case of diagonal matrix'!
!LapackDiagonalPLUdecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackDiagonalPLUdecomposition methodsFor!

computeInverse	inverse := sourceMatrix reciprocal!

decompose
	| n |
	n := sourceMatrix nrow.
	ipiv := SDWORDArray new: n.
	1 to: n do: [:i | ipiv at: i put: i].
	info := 0.
	p := sourceMatrix class eye: n.
	l := sourceMatrix class eye: n.
	a := u := sourceMatrix.
	isComputed := true! !
!LapackDiagonalPLUdecomposition categoriesFor: #computeInverse!public! !
!LapackDiagonalPLUdecomposition categoriesFor: #decompose!public! !

LapackQRPdecomposition guid: (GUID fromString: '{757A2393-BDDF-4CDA-A1FE-16DB80226EED}')!
LapackQRPdecomposition comment: 'LapackQRPdecomposition will perform a Q*R factorisation of a matrix A with column pivoting
	A*P=Q*R

Q = H1 * H2 * ... * Hk
H(i) = Id - tau(i) * v(i) * v(i) ''

Instance Variables:
	jpvt	<ArrayedCollection>	permutation of columns
	p	<LapackMatrix>	the permutation matrix
'!
!LapackQRPdecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackQRPdecomposition methodsFor!

checkP	self checkDecomposition!

decompose
	| a lapack m n k |
	a := sourceMatrix copy.
	m := a nrow.
	n := a ncol.
	k := m min: n.
	lapack := sourceMatrix lapackInterface.
	jpvt := SDWORDArray new: a nrow.
	tau := a class nrow: k.
	info := lapack 
				geqp3Withm: m
				n: n
				a: a asParameter
				lda: a nrow
				jpvt: jpvt asParameter
				tau: tau asParameter.
	info = 0 ifFalse: [self error: 'QR factorization of a matrix failed'].
	qfull := a copy.
	rfull := a.
	info := a isComplexMatrix 
				ifTrue: 
					[lapack 
						ungqrWithm: m
						n: n
						k: k
						a: qfull asParameter
						lda: qfull nrow
						tau: tau asParameter]
				ifFalse: 
					[lapack 
						orgqrWithm: m
						n: n
						k: k
						a: qfull asParameter
						lda: qfull nrow
						tau: tau asParameter].
	info = 0 ifFalse: [self error: 'generation of orthogonal matrix failed'].
	p := a class realMatrix nrow: n ncol: n.
	1 to: k
		do: 
			[:i | 
			(jpvt at: i) = 0 
				ifFalse: 
					[p 
						rowAt: (jpvt at: i)
						columnAt: i
						put: 1]].
	q := qfull atColumns: (1 to: k).
	r := (rfull atRows: (1 to: k)) upperTriangle.
	rfull := rfull upperTriangle.
	isComputed := true.!

jpvt	self checkDecomposition.	^jpvt!

p	self checkP.	^p!

pseudoInverse	"Answer a pseudo inverse of sourceMatrix using my QR factorisation"	^self pseudoInverseTolerance: sourceMatrix defaultTolerance!

pseudoInverseTolerance: tol 	"Answer a pseudo inverse of sourceMatrix using my QR factorisation"	| rr |	self checkDecomposition.	rr := sourceMatrix class shape: sourceMatrix ncol.	rr 		copy: (rfull now min: sourceMatrix ncol)		rowsStartingAt: 1		and: (rfull ncol min: sourceMatrix ncol)		columnsStartingAt: 1		from: rfull.	^p * (rr upperTriangle pseudoInverseTolerance: tol) 		* qfull transposeConjugated! !
!LapackQRPdecomposition categoriesFor: #checkP!public! !
!LapackQRPdecomposition categoriesFor: #decompose!public! !
!LapackQRPdecomposition categoriesFor: #jpvt!public! !
!LapackQRPdecomposition categoriesFor: #p!public! !
!LapackQRPdecomposition categoriesFor: #pseudoInverse!public! !
!LapackQRPdecomposition categoriesFor: #pseudoInverseTolerance:!public! !

LapackContinuousRiccatiProblem guid: (GUID fromString: '{3FF71859-E501-4739-8986-58E631F8C906}')!
LapackContinuousRiccatiProblem comment: 'LapackContinuousRiccatiProblem is for solving Riccati equations for continuous system, that is:

find X such that:

	(X*sourceMatrix2*X) + (X*sourceMatrix1) + (sourceMatrix1 transposed*X) + sourceMatrix = 0

'!
!LapackContinuousRiccatiProblem categoriesForClass!Unclassified! !
!LapackContinuousRiccatiProblem methodsFor!

decompose
	self perform: algorithm.
	postActionBlock value!

processDiagonalizationAlgorithm

	| HamiltonianMatrix eig W i j W12 W22 |

	"Form Hamiltonian matrix"
	HamiltonianMatrix := (sourceMatrix1 , sourceMatrix2 negated) ,,
			(sourceMatrix negated , sourceMatrix1 transpose negated).

	"Use diagonalization"
	eig := HamiltonianMatrix eigenValueDecomposition.
	eig wantRightEigenVectors: true.

	"Check assertion that no eigen value should lie on imaginary axis"
	(eig wr anySatisfy: [:e | e abs <= self tolerance]) ifTrue: [self 
				error: 'This algorithm requires no eigenvalue should lie on imaginary axis'].

	"Sort stable subspace"
	W := eig vr.
	i := 0.  j := 0.
	W12 := sourceMatrix class nrow: sourceMatrix nrow ncol: sourceMatrix ncol.
	W22 := sourceMatrix class nrow: sourceMatrix nrow ncol: sourceMatrix ncol.
	eig wr do: [:eigir | 
		i := i + 1.
		eigir < 0 ifTrue: [
				 j := j + 1.
				 W12 columnAt: j putSequence: (W fromRow: 1 toRow: sourceMatrix nrow fromColumn: i toColumn: i).
				 W22 columnAt: j putSequence: (W fromRow: sourceMatrix nrow + 1 toRow: sourceMatrix nrow * 2 fromColumn: i toColumn: i)]].

	"Note that eigenvectors of complex eigenvalues are in two columns of W under the form (Real, Imag)
	We can write W=Wcomplex*J, but then we would compute W22 * J*J inv * W12 inv. 
	the complex matrix J would simplify, thus we can use raw form from LAPACK algorithm"

	"Form solution"
	solution := W22 * W12 reciprocal.
	isComputed := true!

processSchurDecompositionAlgorithm

	| n HamiltonianMatrix schur U X1 X2 |

	"Form Hamiltonian matrix"
	n := sourceMatrix nrow.
	HamiltonianMatrix := (sourceMatrix1 , sourceMatrix2 negated) ,,
			(sourceMatrix negated , sourceMatrix1 transpose negated).

	"Use Schur decomposition"
	schur := HamiltonianMatrix schurDecomposition.
	schur
		wantVectors: true;
		shouldScale: true;
		selectNegativeReal.

	"Check assertion that no eigen value should lie on imaginary axis"
	(schur eigenValues anySatisfy: [:e | e abs <= self tolerance]) 
		ifTrue: 
			[self 
				error: 'This algorithm requires no eigenvalue should lie on imaginary axis'].

	"Form solution"
	U := schur schurVectors.
	X2 := U 
				fromRow: 1
				toRow: n
				fromColumn: n + 1
				toColumn: n * 2.
	X1 := U 
				fromRow: 1
				toRow: n
				fromColumn: 1
				toColumn: n.
	solution := X2 * X1 reciprocal.
	isComputed := true!

solveA: A B: B Q: Q R: R 
	"Solve the Riccati equation under the classical form of LQR/LQE in Control Theory"

	| Rinv |
	Rinv := R reciprocal.
	sourceMatrix := Q.
	sourceMatrix1 := B * Rinv * B transposed.
	sourceMatrix2 := A.
	self reset.
	self postAction: nil!

solveA: A B: B Q: Q R: R S: S 	"Solve the feedback gain problem under the classical form of LQR/LQE in Control Theory"	| Rinv |	Rinv := R reciprocal.	sourceMatrix := Q - (S * Rinv * S transposed).	sourceMatrix1 := B * Rinv * B transposed.	sourceMatrix2 := A - (B * Rinv * S transposed).	self reset.	self postAction: 			[solution := Rinv * (B transposed negated * solution - S transposed)]! !
!LapackContinuousRiccatiProblem categoriesFor: #decompose!processing!public! !
!LapackContinuousRiccatiProblem categoriesFor: #processDiagonalizationAlgorithm!processing!public! !
!LapackContinuousRiccatiProblem categoriesFor: #processSchurDecompositionAlgorithm!processing!public! !
!LapackContinuousRiccatiProblem categoriesFor: #solveA:B:Q:R:!initialize/release!public! !
!LapackContinuousRiccatiProblem categoriesFor: #solveA:B:Q:R:S:!public! !

LapackDiscreteRiccatiProblem guid: (GUID fromString: '{D68C6B61-DDFA-4C1D-A8CC-14E08BD0B53F}')!
LapackDiscreteRiccatiProblem comment: 'LapackDiscreteRiccatiProblem is for solving Riccati equations for discrete system, that is:

find X such that:

	X = Q + (A transposed*X*A) - (A transposed*X*B * (B transposed*X*B+R) reciprocal * B transposed*X*A)

where
	sourceMatrix is Q
	sourceMatrix1 is A
	sourceMatrix2 is B
	sourceMatrix3 is R
	solution is X

'!
!LapackDiscreteRiccatiProblem categoriesForClass!Unclassified! !
!LapackDiscreteRiccatiProblem methodsFor!

decompose
	self perform: algorithm!

processDiagonalizationAlgorithm

	| n A B Q R Rinv Id Zero H1 H2 eig W i j W12 W22 |

	"Form Hamiltonian matrices"
	n := sourceMatrix nrow.
	Q := sourceMatrix.
	A := sourceMatrix1.
	B := sourceMatrix2.
	R := sourceMatrix3.
	Rinv := R reciprocal.
	Id := Q class eye: n.
	Zero := Q class nrow: n ncol: n.
	H1 := (Id , B * Rinv * B transposed) ,,
			(Zero , A transposed).
	H2 := (A , Zero) ,,
			(Q negated , Id).

	"Use diagonalization"
	eig := H1 generalizedEigenValueDecompositionWithRHSMatrix: H2.
	eig wantRightEigenVectors: true.

	"Check assertion that no eigen value should lie on unit circle"
	(eig eigenValues anySatisfy: [:e | (e abs - 1) abs <= self tolerance]) ifTrue: [self 
				error: 'This algorithm requires no eigenvalue should lie on unit circle'].

	"Sort stable subspace"
	(eig eigenValues count: [:e | e abs <= 1]) = n ifFalse: [self 
				error: 'There should be as many eigenvalues inside unit circle as outside'].
	W := eig vr.
	i := 0.  j := 0.
	W12 := sourceMatrix class nrow: sourceMatrix nrow ncol: sourceMatrix ncol.
	W22 := sourceMatrix class nrow: sourceMatrix nrow ncol: sourceMatrix ncol.
	eig eigenValues do: [:eigi | 
		i := i + 1.
		eigi abs < 1 ifTrue: [
				 j := j + 1.
				 W12 columnAt: j putSequence: (W fromRow: 1 toRow: sourceMatrix nrow fromColumn: i toColumn: i).
				 W22 columnAt: j putSequence: (W fromRow: sourceMatrix nrow + 1 toRow: sourceMatrix nrow * 2 fromColumn: i toColumn: i)]].

	"Note that eigenvectors of complex eigenvalues are in two columns of W under the form (Real, Imag)
	We can write W=Wcomplex*J, but then we would compute W22 * J*J inv * W12 inv. 
	the complex matrix J would simplify, thus we can use raw form from LAPACK algorithm"

	"Form solution"
	solution := W22 * W12 reciprocal.
	isComputed := true!

processSchurDecompositionAlgorithm

	| n A B Q R Rinv Id Zero H1 H2 schur U X1 X2 |

	"Form Hamiltonian matrices"
	n := sourceMatrix nrow.
	Q := sourceMatrix.
	A := sourceMatrix1.
	B := sourceMatrix2.
	R := sourceMatrix3.
	Rinv := R reciprocal.
	Id := Q class eye: n.
	Zero := Q class nrow: n ncol: n.
	H1 := (Id , B * Rinv * B transposed) ,,
			(Zero , A transposed).
	H2 := (A , Zero) ,,
			(Q negated , Id).

	"Use Schur decomposition - should reaaly use generalized schur decomposition..."
	schur := (H1 reciprocal * H2) schurDecomposition.
	schur
		wantVectors: true;
		shouldScale: true;
		selectAbsLessThanUnity.

	"Check assertion that no eigen value should lie on unit circle"
	(schur eigenValues anySatisfy: [:e | (e abs - 1) abs <= self tolerance]) ifTrue: [self 
				error: 'This algorithm requires no eigenvalue should lie on unit circle'].
	
	"Form solution"
	U := schur schurVectors.
	X2 := U 
				fromRow: 1
				toRow: n
				fromColumn: n + 1
				toColumn: n * 2.
	X1 := U 
				fromRow: 1
				toRow: n
				fromColumn: 1
				toColumn: n.
	solution := X2 * X1 reciprocal.
	isComputed := true!

solveA: A B: B Q: Q R: R  
	"Solve the Riccati equation under the classical form of LQR/LQE in Control Theory"

	sourceMatrix := Q.
	sourceMatrix1 := B.
	sourceMatrix2 := A.
	sourceMatrix3 := R.
	self reset.
	self postAction: nil!

solveA: A B: B Q: Q R: R S: S 
	"Solve the feedback gain problem under the classical form of LQR/LQE in Control Theory"

	| R1St |
	R1St := R reciprocal * S transpose.
	sourceMatrix := Q - (S * R1St).
	sourceMatrix1 := B.
	sourceMatrix2 := A - (B * R1St).
	self reset.
	self postAction: 
			[| BtX |
			BtX := B transposed * solution.
			solution := ((BtX * B + R) reciprocal* (BtX * A + S transposed)) negated]! !
!LapackDiscreteRiccatiProblem categoriesFor: #decompose!processing!public! !
!LapackDiscreteRiccatiProblem categoriesFor: #processDiagonalizationAlgorithm!processing!public! !
!LapackDiscreteRiccatiProblem categoriesFor: #processSchurDecompositionAlgorithm!processing!public! !
!LapackDiscreteRiccatiProblem categoriesFor: #solveA:B:Q:R:!initialize/release!public! !
!LapackDiscreteRiccatiProblem categoriesFor: #solveA:B:Q:R:S:!initialize/release!public! !

LapackComplexSchurDecomposition guid: (GUID fromString: '{5D0D125F-A71C-4B62-9B0F-EDC0F71D61BF}')!
LapackComplexSchurDecomposition comment: 'LapackComplexSchurDecomposition will produce a triangular complex Schur form
'!
!LapackComplexSchurDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackComplexSchurDecomposition methodsFor!

decompose
	| n lapack w vs |
	isComputed := false.
	a := sourceMatrix copy.
	self preprocess.
	n := a nrow.
	w := a class allocateNrow: n ncol: 1.
	lapack := a lapackInterface.
	sdim := SDWORDArray new: 1.
	vs := wantVectors 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	info := lapack 
				geesWithjobvs: (wantVectors 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				sort: (selectFunction == #selectNone 
						ifTrue: [lapack schurDoNotSort]
						ifFalse: [lapack schurDoSort])
				select: (lapack perform: selectFunction)
				n: n
				a: a asParameter
				lda: a nrow
				sdim: sdim asParameter
				w: w asParameter
				vs: vs asParameter
				ldvs: vs nrow.
	info = 0 ifFalse: [self error: 'eigen value decomposition failed'].
	eigenValues := w.
	schurVectors := vs.
	self postprocess.
	schurTriangular := a upperTriangle.
	isComputed := true! !
!LapackComplexSchurDecomposition categoriesFor: #decompose!public! !

LapackRealSchurDecomposition guid: (GUID fromString: '{7BF77B4B-4FD7-416D-B0A6-E333CACD6004}')!
LapackRealSchurDecomposition comment: 'LapackRealSchurDecomposition use a LAPACK algorithm that does not produce complex matrices:

- the Schur form is quasi-triangular with 2x2 blocks in case of complex eigen values.
- the real and imaginary parts of eigenvalues are stored in two real vectors

Instance Variables:
	wi	<LapackMatrix>	eigenValues imaginaryPart
	wr	<LapackMatrix>	eigenValues realPart 

'!
!LapackRealSchurDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackRealSchurDecomposition methodsFor!

checkEigenValues	super checkEigenValues.	eigenValues isNil 		ifTrue: [eigenValues := wi isZero ifTrue: [wr] ifFalse: [wr i: wi]]!

decompose
	| n lapack vs |
	isComputed := false.
	a := sourceMatrix copy.
	self preprocess.
	n := a nrow.
	wr := a class allocateNrow: n ncol: 1.
	wi := a class allocateNrow: n ncol: 1.
	lapack := a lapackInterface.
	sdim := SDWORDArray new: 1.
	vs := wantVectors 
				ifTrue: [a class allocateNrow: n ncol: n]
				ifFalse: [a class allocateNrow: 1 ncol: 1].
	info := lapack 
				geesWithjobvs: (wantVectors 
						ifTrue: [lapack doComputeVector]
						ifFalse: [lapack dontComputeVector])
				sort: (selectFunction == #selectNone 
						ifTrue: [lapack schurDoNotSort]
						ifFalse: [lapack schurDoSort])
				select: (lapack perform: selectFunction)
				n: n
				a: a asParameter
				lda: a nrow
				sdim: sdim asParameter
				wr: wr asParameter
				wi: wi asParameter
				vs: vs asParameter
				ldvs: vs nrow.
	info = 0 ifFalse: [self error: 'eigen value decomposition failed'].
	schurVectors := vs.
	self postprocess.
	schurTriangular := a.
	isComputed := true!

reset	wr := wi := nil.	super reset! !
!LapackRealSchurDecomposition categoriesFor: #checkEigenValues!public! !
!LapackRealSchurDecomposition categoriesFor: #decompose!public! !
!LapackRealSchurDecomposition categoriesFor: #reset!public! !

"Binary Globals"!

