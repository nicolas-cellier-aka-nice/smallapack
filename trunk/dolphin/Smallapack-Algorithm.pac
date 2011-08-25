| package |
package := Package name: 'Smallapack-Algorithm'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #LapackComplexEigenDecomposition;
	add: #LapackComplexSchurDecomposition;
	add: #LapackDecomposition;
	add: #LapackDiagonalEigenDecomposition;
	add: #LapackDiagonalPLUdecomposition;
	add: #LapackEigenDecomposition;
	add: #LapackGeneralizedEigenDecomposition;
	add: #LapackHermitianEigenDecomposition;
	add: #LapackHermitianPLUdecomposition;
	add: #LapackHessenbergDecomposition;
	add: #LapackLeastSquareProblem;
	add: #LapackPLUdecomposition;
	add: #LapackQRdecomposition;
	add: #LapackQRPdecomposition;
	add: #LapackRealEigenDecomposition;
	add: #LapackRealSchurDecomposition;
	add: #LapackSchurDecomposition;
	add: #LapackSVDecomposition;
	add: #LapackTriangularPLUdecomposition;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'Object Arts\Dolphin\Base\Dolphin';
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
LapackDecomposition subclass: #LapackSchurDecomposition
	instanceVariableNames: 'eigenValues schurTriangular schurVectors wantVectors selectFunction sdim'
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
	instanceVariableNames: 'rhsMatrix'
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
LapackHermitianEigenDecomposition subclass: #LapackDiagonalEigenDecomposition
	instanceVariableNames: ''
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

checkDecomposition

decompose

decompose: aMatrix

info

initialize

reset
!LapackDecomposition categoriesFor: #checkDecomposition!public! !
!LapackDecomposition categoriesFor: #decompose!public! !
!LapackDecomposition categoriesFor: #decompose:!public! !
!LapackDecomposition categoriesFor: #info!public! !
!LapackDecomposition categoriesFor: #initialize!public! !
!LapackDecomposition categoriesFor: #reset!public! !

!LapackDecomposition class methodsFor!

decompose: aMatrix 

new
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

checkEigenValues

checkLeftEigenVectors

checkRightEigenVectors

eigenValues

initialize

leftEigenVectors

reset

rightEigenVectors

solveByDivideAndConquer

solveByRelativelyRobustRepresentation

solveByStandardAlgorithm

wantLeftEigenVectors: aBoolean 

wantRightEigenVectors: aBoolean 
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

h

q

reset
!LapackHessenbergDecomposition categoriesFor: #decompose!public! !
!LapackHessenbergDecomposition categoriesFor: #h!public! !
!LapackHessenbergDecomposition categoriesFor: #q!public! !
!LapackHessenbergDecomposition categoriesFor: #reset!public! !

LapackLeastSquareProblem guid: (GUID fromString: '{25605C59-E0F5-468B-9235-DCB94CEA40BE}')!
LapackLeastSquareProblem comment: ''!
!LapackLeastSquareProblem categoriesForClass!Smallapack-Algorithm! !
!LapackLeastSquareProblem methodsFor!

decompose

decompose: aLapackMatrix

initialize

matrix: aLapackMatrix rhsMatrix: bMatrix 

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

rank

rankConditionNumber: aNumber 

solution

solveByDivideAndConquerSVD

solveByOrthogonalFactorization

solveBySVD
!LapackLeastSquareProblem categoriesFor: #decompose!public! !
!LapackLeastSquareProblem categoriesFor: #decompose:!public! !
!LapackLeastSquareProblem categoriesFor: #initialize!public! !
!LapackLeastSquareProblem categoriesFor: #matrix:rhsMatrix:!public! !
!LapackLeastSquareProblem categoriesFor: #processDivideAndConquerSVD!public! !
!LapackLeastSquareProblem categoriesFor: #processOrthogonalFactorization!public! !
!LapackLeastSquareProblem categoriesFor: #processSVD!public! !
!LapackLeastSquareProblem categoriesFor: #rank!public! !
!LapackLeastSquareProblem categoriesFor: #rankConditionNumber:!public! !
!LapackLeastSquareProblem categoriesFor: #solution!public! !
!LapackLeastSquareProblem categoriesFor: #solveByDivideAndConquerSVD!public! !
!LapackLeastSquareProblem categoriesFor: #solveByOrthogonalFactorization!public! !
!LapackLeastSquareProblem categoriesFor: #solveBySVD!public! !

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

determinant

inverse

ipiv

l

p

plu

reset

u
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

q

r

reset

tau
!LapackQRdecomposition categoriesFor: #decompose!public! !
!LapackQRdecomposition categoriesFor: #q!public! !
!LapackQRdecomposition categoriesFor: #r!public! !
!LapackQRdecomposition categoriesFor: #reset!public! !
!LapackQRdecomposition categoriesFor: #tau!public! !

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

checkEigenValues

checkSchurVectors

eigenValues

initialize

numberOfSelectedEigenValues

reset

schurTriangular

schurVectors

selectAbsLessThanUnity

selectAbsStriclyLessThanUnity

selectNegativeReal

selectNone

selectStrictlyNegativeReal

wantVectors: aBoolean 
!LapackSchurDecomposition categoriesFor: #checkEigenValues!public! !
!LapackSchurDecomposition categoriesFor: #checkSchurVectors!public! !
!LapackSchurDecomposition categoriesFor: #eigenValues!public! !
!LapackSchurDecomposition categoriesFor: #initialize!public! !
!LapackSchurDecomposition categoriesFor: #numberOfSelectedEigenValues!public! !
!LapackSchurDecomposition categoriesFor: #reset!public! !
!LapackSchurDecomposition categoriesFor: #schurTriangular!public! !
!LapackSchurDecomposition categoriesFor: #schurVectors!public! !
!LapackSchurDecomposition categoriesFor: #selectAbsLessThanUnity!public! !
!LapackSchurDecomposition categoriesFor: #selectAbsStriclyLessThanUnity!public! !
!LapackSchurDecomposition categoriesFor: #selectNegativeReal!public! !
!LapackSchurDecomposition categoriesFor: #selectNone!public! !
!LapackSchurDecomposition categoriesFor: #selectStrictlyNegativeReal!public! !
!LapackSchurDecomposition categoriesFor: #wantVectors:!public! !

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

checkLeftSingularVectors

checkRightSingularVectors

checkSingularValues

decompose
	| a k |
	a := sourceMatrix copy.
	k := a nrow min: a ncol.
	s := a class realMatrix allocateNrow: k ncol: 1.
	u := whichLeftVector == #allSingularVector 
				ifTrue: [a class allocateNrow: a nrow ncol: a nrow]
				ifFalse: 
					[whichLeftVector == #someSingularVector 
						ifTrue: [a class allocateNrow: a nrow ncol: k]
						ifFalse: [a class allocateNrow: 1 ncol: 1]].
	vt := whichRightVector == #allSingularVector 
				ifTrue: [a class allocateNrow: a ncol ncol: a ncol]
				ifFalse: 
					[whichRightVector == #someSingularVector 
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

initialize

leftSingularVectors

reset

rightSingularVectors

s

singularValues

u

ut

v

vt

wantAllLeftSingularVector

wantAllRightSingularVector

wantAllSingularVector

wantNoLeftSingularVector

wantNoRightSingularVector

wantNoSingularVector

wantSomeLeftSingularVector

wantSomeRightSingularVector

wantSomeSingularVector
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

decompose: aLapackMatrix

decomposeLeft: aMatrix right: bMatrix 
!LapackGeneralizedEigenDecomposition categoriesFor: #decompose:!public! !
!LapackGeneralizedEigenDecomposition categoriesFor: #decomposeLeft:right:!public! !

!LapackGeneralizedEigenDecomposition class methodsFor!

decomposeLeft: leftMatrix right: rightMatrix
!LapackGeneralizedEigenDecomposition class categoriesFor: #decomposeLeft:right:!public! !

LapackHermitianEigenDecomposition guid: (GUID fromString: '{9C5A6F4A-6458-437D-A079-AB8931FA6F69}')!
LapackHermitianEigenDecomposition comment: 'LapackHermitianEigenDecomposition is specialized for complex hermitian / real symmetric matrices

Instance Variables:
	algorithm	<Symbol>	hold the name of a method implemented in self to process decomposition'!
!LapackHermitianEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackHermitianEigenDecomposition methodsFor!

decompose

initialize

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

processRelativelyRobustRepresentation

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

solveByDivideAndConquer

solveByRelativelyRobustRepresentation

solveByStandardAlgorithm

wantLeftEigenVectors: aBoolean 

wantRightEigenVectors: aBoolean 
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

checkEigenValues

checkLeftEigenVectors

checkRightEigenVectors

checkVl

checkVr

checkWi

checkWr

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

eigenvalues

postConjugate: aMatrix 

reset

vl

vr

wi

wr
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

LapackDiagonalEigenDecomposition guid: (GUID fromString: '{48D331A3-B284-4E99-8E1F-D7BC4ED2A26C}')!
LapackDiagonalEigenDecomposition comment: 'LapackDiagonalEigenDecomposition is for the trivial case of diagonal matrix'!
!LapackDiagonalEigenDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackDiagonalEigenDecomposition methodsFor!

decompose
!LapackDiagonalEigenDecomposition categoriesFor: #decompose!public! !

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

computeInverse

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

determinant
!LapackTriangularPLUdecomposition categoriesFor: #computeInverse!public! !
!LapackTriangularPLUdecomposition categoriesFor: #decompose!public! !
!LapackTriangularPLUdecomposition categoriesFor: #determinant!public! !

LapackDiagonalPLUdecomposition guid: (GUID fromString: '{8CA23584-CBAE-4CBA-BB51-C4A936BAB48F}')!
LapackDiagonalPLUdecomposition comment: 'LapackDiagonalPLUdecomposition is for trivial case of diagonal matrix'!
!LapackDiagonalPLUdecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackDiagonalPLUdecomposition methodsFor!

computeInverse

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

checkP

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

jpvt

p

pseudoInverse

pseudoInverseTolerance: tol 
!LapackQRPdecomposition categoriesFor: #checkP!public! !
!LapackQRPdecomposition categoriesFor: #decompose!public! !
!LapackQRPdecomposition categoriesFor: #jpvt!public! !
!LapackQRPdecomposition categoriesFor: #p!public! !
!LapackQRPdecomposition categoriesFor: #pseudoInverse!public! !
!LapackQRPdecomposition categoriesFor: #pseudoInverseTolerance:!public! !

LapackComplexSchurDecomposition guid: (GUID fromString: '{5D0D125F-A71C-4B62-9B0F-EDC0F71D61BF}')!
LapackComplexSchurDecomposition comment: 'LapackComplexSchurDecomposition will produce a triangular complex Schur form
'!
!LapackComplexSchurDecomposition categoriesForClass!Smallapack-Algorithm! !
!LapackComplexSchurDecomposition methodsFor!

decompose
	| a n lapack w vs |
	isComputed := false.
	a := sourceMatrix copy.
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

checkEigenValues

decompose
	| a n lapack vs |
	isComputed := false.
	a := sourceMatrix copy.
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
	schurTriangular := a.
	isComputed := true!

reset
!LapackRealSchurDecomposition categoriesFor: #checkEigenValues!public! !
!LapackRealSchurDecomposition categoriesFor: #decompose!public! !
!LapackRealSchurDecomposition categoriesFor: #reset!public! !

"Binary Globals"!
