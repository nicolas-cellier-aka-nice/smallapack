| package |
package := Package name: 'Smallapack-SUnitTests'.
package paxVersion: 1;
	basicComment: 'Smallapack-SUnitTests implement some tests for the Smallapack library'.


package classNames
	add: #TestLapackMatrix;
	add: #TestRandMatrix;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Documents and Settings\cellier\Mes documents\Dolphin Smalltalk X6\Object Arts\Dolphin\Base\Dolphin';
	add: 'Smallapack-Algorithm';
	add: 'Smallapack-Matrix';
	add: '..\..\Documents and Settings\cellier\Mes documents\Dolphin Smalltalk X6\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #TestLapackMatrix
	instanceVariableNames: 'vec3 i3 ones34 zeros43 seq34'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #TestRandMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

TestLapackMatrix guid: (GUID fromString: '{8B7A3D15-0C1D-4DB3-91F1-0E7A56043F2D}')!
TestLapackMatrix comment: ''!
!TestLapackMatrix categoriesForClass!Unclassified! !
!TestLapackMatrix methodsFor!

setUp
	"Borrowed to Smallpack"

	vec3 := LapackDGEMatrix columns: (Array with: (1 to: 3)).
	i3 := LapackDGEMatrix eye: 3.
	ones34 := LapackDGEMatrix 
				nrow: 3
				ncol: 4
				withAll: 1.
	zeros43 := LapackDGEMatrix nrow: 4 ncol: 3.
	seq34 := LapackDGEMatrix nrow: 3 ncol: 4.
	1 to: 12 do: [:i | seq34 at: i put: i]!

testComplex
	| zge zge3 zge3i4 zgeId idZge |
	zge := seq34 i: ones34.

	self assert: zge isComplexMatrix.
	self assert: zge realPart = seq34.
	self assert: zge imaginaryPart = ones34.

	zge3 := zge * 3.
	self assert: zge3 realPart = (seq34 * 3).
	self assert: zge3 imaginaryPart = (ones34 * 3).

	zge3i4 := zge * (3 + 4 i).
	self assert: zge3i4 realPart = (seq34 * 3 - (4 * ones34)).
	self assert: zge3i4 imaginaryPart = (ones34 * 3 + (4 * seq34)).

	zgeId := zge * (LapackDGEMatrix eye: 4).
	self assert: zgeId = zge.

	idZge := (LapackDGEMatrix eye: 3) * zge.
	self assert: idZge = zge.!

testConcatenation
	| md1 md2 md3 md12 md13 mrow mcol |
	md1 := LapackDGEMatrix rows: #( (3 2 4) (2 -5 -1) ( 1 -2 2) (7 -5 3)).
	md2 := LapackDGEMatrix rows: #( (1 -2 ) (11 9 ) (-4 2) (0 1)).
	md3 := LapackDGEMatrix rows: #( (13 17 19)).

	md12 := md1 , md2.

	self assert: md12 isDoublePrecisionMatrix.
	self assert: md12 ncol = (md1 ncol + md2 ncol).
	self assert: md12 nrow = (md1 nrow).

	md13 := md1 ,, md3.

	self assert: md13 isDoublePrecisionMatrix.
	self assert: md13 nrow = (md1 nrow + md3 nrow).
	self assert: md13 ncol = (md1 ncol).

	mrow := LapackDGEMatrix rows: #( (3 2 4) ).
	mcol := LapackDGEMatrix columns: #( (3 2 4) ).
	(#( 21 21.0e0 21.0d0  ) copyWith: 1/2) do: [:aNumber |
		| mr mc |
		mr := mrow , aNumber.
		self assert: mr isDoublePrecisionMatrix.
		self assert: mr ncol = (mrow ncol + 1).
		self assert: mr nrow = (mrow nrow).

		mc := mcol ,, aNumber.
		self assert: mc isDoublePrecisionMatrix.
		self assert: mc nrow = (mcol nrow + 1).
		self assert: mc ncol = (mcol ncol).
	].!

testCreation
	| aSmallapackDoubleArray aMatrix1 aMatrix2 aMatrix3 aMatrix4 aMatrix5 |
	aSmallapackDoubleArray := DOUBLEArray new: 3 * 2.
	1 to: 6 do: [:i | aSmallapackDoubleArray at: i put: i asFloat].
	aMatrix1 := aSmallapackDoubleArray asLapackMatrixNrow: 3 ncol: 2.
	aMatrix2 := LapackDGEMatrix columns: #(#(1 2 3) #(4 5 6)).
	aMatrix3 := LapackDGEMatrix rows: #(#(1 4) #(2 5) #(3 6)).
	self assert: aMatrix1 isMatrix.
	self assert: aMatrix2 isMatrix.
	self assert: aMatrix3 isMatrix.
	self assert: ((1 to: 6) allSatisfy: [:i | (aMatrix1 at: i) = i]).
	self assert: ((1 to: 6) allSatisfy: [:i | (aMatrix2 at: i) = i]).
	self assert: ((1 to: 6) allSatisfy: [:i | (aMatrix3 at: i) = i]).

	aMatrix4 := LapackDGEMatrix diagonal: #(6 4 5).
	self assert: aMatrix4 isDiagonal.
	self assert: (aMatrix4 diagonal isSameSequenceAs: #(6 4 5)).

	aMatrix5 := LapackDGEMatrix shape: 3.
	self assert: aMatrix5 nrow = 3.
	self assert: aMatrix5 ncol = 3.
	self assert: aMatrix5 isZero.

	aMatrix5 := LapackDGEMatrix shape: 3 @ 4.
	self assert: aMatrix5 nrow = 3.
	self assert: aMatrix5 ncol = 4.
	self assert: aMatrix5 isZero.

	aMatrix5 := LapackDGEMatrix shape: #(3 4).
	self assert: aMatrix5 nrow = 3.
	self assert: aMatrix5 ncol = 4.
	self assert: aMatrix5 isZero.!

testDeterminant

	| m |
	m := LapackDGEMatrix rows: #( (3 2 4) (2 -5 -1) ( 1 -2 2)).
	self should: [ (m determinant - -42) abs < 1.0d-12].!

testEigenValues

	| p p1 d a eva |
	p := LapackDGEMatrix rows: #((1 2 3) (0 4 5) (1 0 6)).
	p1 := p reciprocal.
	d := LapackDGEMatrix diagonal: #( 7 11 13 ).
	a := p*d*p1.

	eva := a eigenValueDecomposition.
	eva wantLeftEigenVectors: true.
	eva wantRightEigenVectors: true.
	
	self assert: ((LapackDGEMatrix diagonal: eva eigenValues asSortedCollection ) - d) absMax < 1.0d-12.
	self assert: (a * eva rightEigenVectors - ( eva rightEigenVectors * (LapackDGEMatrix diagonal: eva eigenValues) ) ) absMax < 1.0d-12.!

testHessenberg
	| m hess |
	m := LapackDGEMatrix rows: #(#(3 2 4) #(2 -5 -1) #(1 -2 2)).
	hess := m hessenbergDecomposition.
	self assert: (hess q * hess h * hess q transposeConjugated - m) absMax 	< 1.0d-12!

testLeastSquareWithEqualityConstraints

	| a b c d x dx lsqec minimum cInv iseed x1 |
	a := LapackDGEMatrix rows: #(#(3 2 4) #(2 -5 -1) #(1 -2 2)).
	b := LapackDGEMatrix rows: #(#(1) (2) (3)).
	c := LapackDGEMatrix rows: #(#(1 1 0) #(0 1 -1)).
	d := LapackDGEMatrix rows: #(#(0) #(2)).
	
	lsqec := LapackLeastSquareProblemWithEqualityConstraints
		minimizeTimesX: a
		minus: b
		subjectToTimesX: c
		equal: d.
	x := lsqec solution.
	minimum := lsqec residuals norm2.
	
	self assert: lsqec equalityResiduals norm2 < 1.0e-12 description: 'equality constraints should be feasible'.
	cInv := c pseudoInverse.
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	10 timesRepeat: [
		dx := 0.1*(LapackDGEMatrix randUniform: x dimensions withSeed: iseed).
		"displacement with respect to constraints c*dx=0"
		dx := dx - (cInv*(c*dx)).
		x1 := x + dx.
		self assert: (c*x1-d) norm2 < 1.0e-12 description: 'equality constraints should be feasible'.
		self assert: (a*x1-b) norm2 >= minimum description: 'least square solution should be minimum'].
	!

testLowerTriangle
	"Unlike #upperTriangle: , #smallLowerTriangle: can be of smaller size than receiver"
	
	(LapackGeneralMatrix allSubclasses copyWith: AbstractMatrix) do: [:matrixClass |
		| m33 m36 m63 |
		m33 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33)).
		m36 := matrixClass rows: #(#(11 12 13 14 15 16) #(21 22 23 24 25 26) #(31 32 33 34 35 36)).
		m63 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63)).
	
		self assert: (m33 lowerTriangle: -2) = (matrixClass rows: #(#(0 0 0) #(0 0 0) #(31 0 0))).
		self assert: (m33 lowerTriangle: -1) = (matrixClass rows: #(#(0 0 0) #(21 0 0) #(31 32 0))).
		self assert: (m33 lowerTriangle: 0) = (matrixClass rows: #(#(11 0 0) #(21 22 0) #(31 32 33))).
		self assert: (m33 lowerTriangle: 1) = (matrixClass rows: #(#(11 12 0) #(21 22 23) #(31 32 33))).
		self assert: (m33 lowerTriangle: 2) = m33.
	
		self assert: (m36 lowerTriangle: -2) = (matrixClass rows: #(#(0 0 0 0 0 0) #(0 0 0 0 0 0) #(31 0 0 0 0 0))).
		self assert: (m36 lowerTriangle: -1) = (matrixClass rows: #(#(0 0 0 0 0 0) #(21 0 0 0 0 0) #(31 32 0 0 0 0))).
		self assert: (m36 lowerTriangle: 0) = (matrixClass rows: #(#(11 0 0 0 0 0) #(21 22 0 0 0 0) #(31 32 33 0 0 0))).
		self assert: (m36 lowerTriangle: 1) = (matrixClass rows: #(#(11 12 0 0 0 0) #(21 22 23 0 0 0) #(31 32 33 34 0 0))).
		self assert: (m36 lowerTriangle: 2) = (matrixClass rows: #(#(11 12 13 0 0 0) #(21 22 23 24 0 0) #(31 32 33 34 35 0))).
		self assert: (m36 lowerTriangle: 5) = m36.
	
		self assert: (m63 lowerTriangle: 2) = m63.
		self assert: (m63 lowerTriangle: 1) = (matrixClass rows: #(#(11 12 0) #(21 22 23) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63))).
		self assert: (m63 lowerTriangle: 0) = (matrixClass rows: #(#(11 0 0) #(21 22 0) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63))).
		self assert: (m63 lowerTriangle: -1) = (matrixClass rows: #(#(0 0 0) #(21 0 0) #(31 32 0) #(41 42 43) #(51 52 53) #(61 62 63))).
		self assert: (m63 lowerTriangle: -2) = (matrixClass rows: #(#(0 0 0) #(0 0 0) #(31 0 0) #(41 42 0) #(51 52 53) #(61 62 63))).
		self assert: (m63 lowerTriangle: -3) = (matrixClass rows: #(#(0 0 0) #(0 0 0) #(0 0 0) #(41 0 0) #(51 52 0) #(61 62 63))).
		self assert: (m63 lowerTriangle: -4) = (matrixClass rows: #(#(0 0 0) #(0 0 0) #(0 0 0) #(0 0 0) #(51 0 0) #(61 62 0)))]!

testMatrixDiagonal

	| m33 m34 m43 |
	m33 := LapackDGEMatrix rows: #(
		#(3  2  4)
		#(7 -5 -1)
		#(1 -2  8)).
	self assert: (m33 diagonal isSameSequenceAs: #(3 -5 8)).
	self assert: ((m33 diagonalAt: 1) isSameSequenceAs: #(2 -1)).
	self assert: ((m33 diagonalAt: -1) isSameSequenceAs: #(7 -2)).

	m34 := LapackDGEMatrix rows: #(
		#(3  2  4 11)
		#(7 -5 -1 -6)
		#(1 -2  8 -7)).
	self assert: (m34 diagonal isSameSequenceAs: #(3 -5 8)).
	self assert: ((m34 diagonalAt: 1) isSameSequenceAs: #(2 -1 -7)).
	self assert: ((m34 diagonalAt: -1) isSameSequenceAs: #(7 -2)).

	m43 := LapackDGEMatrix rows: #(
		#(3  2  4)
		#(7 -5 -1)
		#(1 -2  8)
		 #(6 11 -7)).
	self assert: (m43 diagonal isSameSequenceAs: #(3 -5 8)).
	self assert: ((m43 diagonalAt: 1) isSameSequenceAs: #(2 -1)).
	self assert: ((m43 diagonalAt: -1) isSameSequenceAs: #(7 -2 -7)).!

testMatrixOperation
	"Code Example 8.1"
	| a b c |
	a := LapackDGEMatrix rows: #( ( 1 0 1) (-1 -2 3)).
	b := LapackDGEMatrix rows: #( ( 1 2 3) (-2 1 7) (5 6 7)).
	c := a * b.
	self should: [ c numberOfRows = 2].
	self should: [ c numberOfColumns = 3].
	self should: [ ((c rowAt: 1) at: 1) = 6].
	self should: [ ((c rowAt: 1) at: 2) = 8].
	self should: [ ((c rowAt: 1) at: 3) = 10].
	self should: [ ((c rowAt: 2) at: 1) = 18].
	self should: [ ((c rowAt: 2) at: 2) = 14].
	self should: [ ((c rowAt: 2) at: 3) = 4].!

testMatrixProduct
	| dic matrixClasses |
	dic := Dictionary new.
	dic at: 33 put: #(#(3 2 4) #(2 -5 -1) #(1 -2 2)).
	dic at: 31 put: #(#(-7) #(11) #(19)).
	dic at: 13 put: #(#(2 -5 8)).
	dic at: 34 put: #(#(1 3 -2 4) #(-5 1 -5 -1) #(-1 2 4 2)).
	matrixClasses := Array with: LapackDGEMatrix with: LapackSGEMatrix.
	matrixClasses do: 
			[:mclass1 | 
			dic keysDo: 
					[:k1 | 
					| m1 m1t |
					m1 := mclass1 rows: (dic at: k1).
					m1t := m1 lowerTriangle: 0.
					matrixClasses do: 
							[:mclass2 | 
							dic keysDo: 
									[:k2 | 
									| m2 m2t |
									m2 := mclass2 rows: (dic at: k2).
									m2t := m2 lowerTriangle: 0.
									k1 \\ 10 = (k2 // 10) 
										ifTrue: 
											[(Array with: m1 with: m1t) do: 
													[:mm1 | 
													(Array with: m2 with: m2t) do: 
															[:mm2 | 
															| m1m2 |
															m1m2 := mm1 * mm2.
															self assert: m1m2 nrow = m1 nrow.
															self assert: m1m2 ncol = m2 ncol.
															self assert: (m1m2 transposed - (mm2 transposed * mm1 transposed)) absMax < 1.0d-15]]]
										ifFalse: [self should: [m1t * m2] raise: TestResult error]]]]]!

testMatrixReplicate
	| m1 o34 m23 m1012 m1012b |
	m1 := LapackDGEMatrix rows: #(#(1)).
	o34 := m1 replicateNrow: 3 timesNcol: 4.
	self assert: o34 = (LapackDGEMatrix ones: #(3 4)).

	m23 := LapackDGEMatrix rows: #(#(1 4 7) #(-2 -1 -3)).
	m1012 := m23 replicateNrow: 5 timesNcol: 4.
	m1012b :=  LapackDGEMatrix rows: #(
		#( 1  4  7   1  4  7   1  4  7   1  4  7 )
		#(-2 -1 -3 -2 -1 -3 -2 -1 -3 -2 -1 -3 )
		#( 1  4  7   1  4  7   1  4  7   1  4  7 )
		#(-2 -1 -3 -2 -1 -3 -2 -1 -3 -2 -1 -3 )
		#( 1  4  7   1  4  7   1  4  7   1  4  7 )
		#(-2 -1 -3 -2 -1 -3 -2 -1 -3 -2 -1 -3 )
		#( 1  4  7   1  4  7   1  4  7   1  4  7 )
		#(-2 -1 -3 -2 -1 -3 -2 -1 -3 -2 -1 -3 )
		#( 1  4  7   1  4  7   1  4  7   1  4  7 )
		#(-2 -1 -3 -2 -1 -3 -2 -1 -3 -2 -1 -3 )
	).
	self assert: m1012 = m1012b!

testMatrixSymmetric
	| m1 m1s m2l m2lb m2u m2ub |
	m1 := LapackDGEMatrix rows: #(#(3 2 4) #(2 -5 -1) #(1 -2 2)).
	m1s := (m1 + m1 transposed) / 2.
	m1s := m1s class hermitianMatrix rows: m1s rows.

	self assert: m1s class isSymmetricMatrix.
	self assert: (m1s * m1) class isGeneralMatrix.
	self assert: (m1s * m1s reciprocal - m1s identity) absMax < 1.0d-12.

	m2l := LapackDSYMatrix rows: #(#(1) #(2 3) #(4 5 6)).
	self assert: m2l class isSymmetricMatrix.
	self assert: m2l isLower.

	m2lb := LapackDSYMatrix columns: #(#(1 2 4) #(3 5) #(6)).
	self assert: m2lb class isSymmetricMatrix.
	self assert: m2lb isLower.
	self assert: m2lb = m2l.

	m2u := LapackDSYMatrix columns: #(#(1) #(2 3) #(4 5 6)).
	self assert: m2u class isSymmetricMatrix.
	self assert: m2u isUpper.

	m2ub := LapackDSYMatrix rows: #(#(1 2 4) #(3 5) #(6)).
	self assert: m2ub class isSymmetricMatrix.
	self assert: m2ub isUpper.
	self assert: m2ub = m2u.

	self assert: m2l = m2u.

	m2l fillOtherTriangle.
	self assert: (m2l castTo: m2l class generalMatrix) = (LapackDGEMatrix rows: #(#(1 2 4) #(2 3 5) #(4 5 6))).

	m2u fillOtherTriangle.
	self assert: (m2u castTo: m2u class generalMatrix) = (LapackDGEMatrix rows: #(#(1 2 4) #(2 3 5) #(4 5 6))).!

testMatrixTriangular
	| m1 m1up m1lo m1up2 m2u m2l m2lb m2ub |
	m1 := LapackDGEMatrix rows: #(#(3 2 4) #(2 -5 -1) #(1 -2 2)).
	m1up := m1 upperTriangle: 0.
	m1lo := m1 lowerTriangle: -1.

	self assert: m1lo diagonal isZero.
	self assert: m1up + m1lo = m1.
	self assert: m1up isUpperTriangular.
	self assert: m1lo isLowerTriangular.

	m1up2 := m1up*m1up.
	self assert: m1up2 isUpperTriangular.

	m2l := LapackDTRMatrix rows: #(#(1) #(2 3) #(4 5 6)).
	self assert: m2l class isTriangularMatrix.
	self assert: m2l isLower.

	m2lb := LapackDTRMatrix columns: #(#(1 2 4) #(3 5) #(6)).
	self assert: m2lb class isTriangularMatrix.
	self assert: m2lb isLower.
	self assert: m2lb = m2l.

	m2u := LapackDTRMatrix columns: #(#(1) #(2 3) #(4 5 6)).
	self assert: m2u class isTriangularMatrix.
	self assert: m2u isUpper.

	m2ub := LapackDTRMatrix rows: #(#(1 2 4) #(3 5) #(6)).
	self assert: m2ub class isTriangularMatrix.
	self assert: m2ub isUpper.
	self assert: m2ub = m2u.!

testQR
	| m qr qrp |
	m := LapackDGEMatrix rows: #(#(3 2 4) #(2 -5 -1) #(1 -2 2) #(5 2 -1)).
	qr := m qrDecomposition.
	self assert: (qr q * qr r - m) absMax < 1.0d-12.
	qrp := m qrpDecomposition.
	self assert: (qrp q * qrp r - (m*qrp p)) absMax < 1.0d-12.!

testReciprocal
	| aSmallapackArray aMatrix  |
	aSmallapackArray := DOUBLEArray new: 3 * 3.
	1 to: 9 do: [:i | aSmallapackArray at: i put: i asFloat squared].
	aMatrix := aSmallapackArray asLapackMatrixNrow: 3 ncol: 3.
	self assert: (aMatrix * aMatrix reciprocal - aMatrix identity) absMax < 1.0d-12.!

testSingularValues
	| a svd sigma k |
	a := LapackDGEMatrix rows: #(#(1 2 3) #(0 4 5) #(1 0 6) #(4 7 9)).

"ECONOMIC"
	svd := a singularValueDecomposition.
	svd wantSomeSingularVector.
	k := a nrow min: a ncol.
	sigma := a class diagonal: svd s.
	self assert: (svd ut * svd u - (a class eye: k)) absMax < 1.0d-12.
	self assert: (svd vt * svd v - (a class eye: k)) absMax < 1.0d-12.
	self assert: (svd u * sigma * svd vt - a) absMax < 1.0d-12.

"FULL"
	svd reset; wantAllSingularVector.
	sigma := a class nrow: a nrow ncol: a ncol.
	sigma setDiagonal: svd s.
	self assert: (svd u * sigma * svd vt - a) absMax < 1.0d-12.!

testSmallLowerTriangle
	"Unlike #upperTriangle: , #smallLowerTriangle: can be of smaller size than receiver"
	
	(LapackGeneralMatrix allSubclasses copyWith: AbstractMatrix) do: [:matrixClass |
		| m33 m36 m63 |
		m33 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33)).
		m36 := matrixClass rows: #(#(11 12 13 14 15 16) #(21 22 23 24 25 26) #(31 32 33 34 35 36)).
		m63 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63)).
	
		self assert: (m33 smallLowerTriangle: -1) = (matrixClass rows: #(#(21 0) #(31 32 ))).
		self assert: (m33 smallLowerTriangle: 0) = (matrixClass rows: #(#(11 0 0) #(21 22 0) #(31 32 33))).
		self assert: (m33 smallLowerTriangle: 1) = (matrixClass rows: #(#(11 12 0) #(21 22 23) #(31 32 33))).
		self assert: (m33 smallLowerTriangle: 2) = m33.
	
		self assert: (m36 smallLowerTriangle: -1) = (matrixClass rows: #(#(21 0) #(31 32))).
		self assert: (m36 smallLowerTriangle: 0) = (matrixClass rows: #(#(11 0 0) #(21 22 0) #(31 32 33))).
		self assert: (m36 smallLowerTriangle: 1) = (matrixClass rows: #(#(11 12 0 0) #(21 22 23 0) #(31 32 33 34))).
		self assert: (m36 smallLowerTriangle: 5) = m36.
	
		self assert: (m63 smallLowerTriangle: 2) = m63.
		self assert: (m63 smallLowerTriangle: 1) = (matrixClass rows: #(#(11 12 0) #(21 22 23) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63))).
		self assert: (m63 smallLowerTriangle: 0) = (matrixClass rows: #(#(11 0 0) #(21 22 0) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63))).
		self assert: (m63 smallLowerTriangle: -1) = (matrixClass rows: #(#(21 0 0) #(31 32 0) #(41 42 43) #(51 52 53) #(61 62 63))).
		self assert: (m63 smallLowerTriangle: -2) = (matrixClass rows: #(#(31 0 0) #(41 42 0) #(51 52 53) #(61 62 63))).
		self assert: (m63 smallLowerTriangle: -3) = (matrixClass rows: #(#(41 0 0) #(51 52 0) #(61 62 63))).
		self assert: (m63 smallLowerTriangle: -4) = (matrixClass rows: #(#(51 0) #(61 62)))]!

testSmallPackAccessing
	"Borrowed to Smallpack"

	| seqRows seqCols |
	self assert: ones34 size = 12.
	self assert: ((i3 at: 4)	= 0 and: [(i3 at: 5)	= 1]).
	i3 at: 6 put: 7.
	self assert: (i3 atRow:3  column: 2) = 7.
	i3 atRow: 3 column: 2 put: 0.
	self assert: (i3 at: 6) = 0.
	self assert: ((seq34 atColumn: 2) isSameSequenceAs: 4 asLapackMatrix, 5, 6).
	self assert: ((seq34 atRow: 3) isSameSequenceAs: 3 asLapackMatrix, 6, 9, 12).
	zeros43 atRow:3  putSequence: vec3.
	zeros43 atColumn: 2 putSequence: (1 asLapackMatrix, 2, 3, 4).
	self assert: (zeros43 asColumnMatrix isSameSequenceAs: 0 asLapackMatrix, 0, 1, 0, 1, 2, 3, 4, 0, 0, 3, 0).
	self assert: (zeros43 nrow = 4 and: [ zeros43 ncol = 3]).
	seqRows := seq34 rows.
	self assert: ((seqRows isKindOf: Array) and: [ seqRows size = 3]).
	self assert: (((seqRows at: 2) isKindOf: LapackMatrix) and: [ (seqRows at: 2) isSameSequenceAs: 2 asLapackMatrix, 5, 8, 11]).
	seqCols := seq34 columns.
	self assert: ((seqCols isKindOf: Array) and: [ seqCols size = 4]).
	self assert: (((seqCols at: 3) isKindOf: LapackMatrix) and: [ (seqCols at: 3) isSameSequenceAs: 7 asLapackMatrix, 8, 9]).
	self assert: (i3  diagonal isSameSequenceAs: 1 asLapackMatrix, 1, 1).
	i3 setDiagonal: vec3.
	self assert: (i3 diagonal isSameSequenceAs: 1 asLapackMatrix, 2, 3).
	ones34 atRow: 1 putAll: 0.
	ones34 atColumn: 2 putAll: 2.
	self assert: (ones34 isSameSequenceAs: 0 asLapackMatrix, 1, 1, 2, 2, 2, 0, 1, 1, 0, 1, 1).!

testSmallPackArithmetic
	"Borrowed to Smallpack"

	"matrix op number"
	self assert: ((i3 + 2 hasSameShapeAs: i3) and: [ i3 + 2 isSameSequenceAs: 3 asLapackMatrix, 2 , 2 , 2 , 3 , 2 , 2 , 2 , 3]).
	self assert: (i3 - 2 + 2 = i3).
	self assert: ((i3 * 3  hasSameShapeAs: i3) and: [ i3 * 3 isSameSequenceAs: 3 asLapackMatrix, 0, 0, 0, 3, 0, 0, 0, 3]).
	self assert: (i3 * 3 / 3 = i3).
"	self assert: (i3 *~ 2 = (i3 * 2))."
"	self assert: ((i3 * -5 // 2) = (i3 * -3))."
"	self assert: ((i3 * -5 quo: 2) = (i3 * -2))."
"	self assert: ((i3 * -5 \\ 2) = i3)."
"	self assert: ((i3 * -5 rem: 2) = (i3 * -1))."
	
	"number op matrix"
	self assert: (5 + ones34 = (ones34 + 5)).
	self assert: (5 - ones34 = (ones34 * 4)).
	self assert: (32 * ones34 = (ones34 * 32)).
"	self assert: (5 / (ones34 * 5) = ones34)."
"	self assert: (4 *~ ones34 = (4 * ones34)."
"	self assert: ( -5 // (2 * ones34) = (-5 // 2 * ones34))."
"	self assert: ((-5 quo: (2 * ones34)) = (-2 * ones34))."
"	self assert: ((i3 * -5 \\ 2) = i3)."
"	self assert: ((i3 * -5 rem: 2) = i3 * -1)."

	"matrix op vector"
	self shouldnt: [ vec3 asMatrix + vec3 ] raise: TestResult error.
	self shouldnt: [ vec3 asMatrix - vec3 ] raise: TestResult error.
	self should: [ vec3 asMatrix * vec3 ] raise: TestResult error.
"	self should: [ vec3 asMatrix / vec3 ] raise: TestResult error."
"	self assert: ( i3 *~ vec3 equalsVector: vec3)."
"	self should: [ vec3 asMatrix // vec3 ] raise: TestResult error."
"	self should: [ vec3 asMatrix quo: vec3 ] raise: TestResult error."
"	self should: [ vec3 asMatrix \\ vec3 ] raise: TestResult error."
"	self should: [ vec3 asMatrix rem: vec3 ] raise: TestResult error."

	"vector op matrix"
	self shouldnt: [ vec3 + (vec3 asMatrix)] raise: TestResult error.
	self shouldnt: [ vec3 - (vec3 asMatrix)] raise: TestResult error.
	self should: [ vec3  * (vec3 asMatrix)] raise: TestResult error.
"	self should: [ vec3  / (vec3  asMatrix)] raise: TestResult error."
"	self should: [ vec3 *~ (vec3 asMatrix)] raise: TestResult error."
"	self assert: ((vec3 *~ (vec3 asRowMatrix) hasSameShapeAs: i3) and: [
		vec3 *~ (vec3 asRowMatrix) isSameSequenceAs: 1 asLapackMatrix,2,3,2,4,6,3,6,9]).
""	self should: [ vec3 // (vec3 asMatrix)] raise: TestResult error."
"	self should: [ vec3 quo: (vec3 asMatrix)] raise: TestResult error."
"	self should: [ vec3 \\ (vec3 asMatrix)] raise: TestResult error."
"	self should: [ vec3 rem: (vec3 asMatrix)] raise: TestResult error."

	"matrix op matrix"
	self assert: ( i3 + i3 = (2 * i3)).
	self assert: (( 2*ones34) - ones34 = ones34).
"	self assert: (( seq34 * seq34 hasSameShapeAs: seq34) and: [
		seq34*seq34 isSameSequenceAs: ((1 to: 12) asVector squared)])."
	self assert: ( ones34 / 5 * 5 = ones34).
	self assert: ( i3 * seq34 = seq34).
	self assert: ( seq34 * zeros43 = (i3 - i3)).
	self assert: ( zeros43 * ones34 hasShape: 4 by: 4 ).
"	self assert: ((-5 * ones34) // (2 * ones34) = -5 // 2 * ones34)."
"	self assert: (((-5 * ones34) quo: (2 * ones34)) = (-5 quo: 2) * ones34)."
"	self assert: ((-5 * ones34) \\ (2 * ones34) = -5\\2 * ones34). "
"	self assert: (((-5 * ones34) rem: (2 * ones34)) = (-5 rem: 2) * ones34)."

	"mismatched matrices"
	self should: [ ones34 + zeros43] raise: TestResult error.
	self should: [ i3 - ones34] raise: TestResult error.
	self should: [ ones34 * seq34 ] raise: TestResult error.
"	self should: [ ones34 / i3 ] raise: TestResult error."
"	self should: [ ones34 *~ ones34] raise: TestResult error."!

testSmallPackAugmenting
	"Borrowed to Smallpack"

	| mat vec03 |
	mat := i3 appendColumns: 2.
	self assert: (mat hasShape: 3 by: 5).
	self assert: (mat isSameSequenceAs: 1 asLapackMatrix, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0).

	mat := ones34 appendRows: 5.
	self assert: (mat hasShape: 8 by: 4).
	self assert: mat abs sum = 12.

	mat := i3 prependColumns: 2.
	self assert: (mat hasShape: 3 by: 5).
	self assert: (mat isSameSequenceAs: 0 asLapackMatrix, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1).

	mat := ones34 prependRows: 5.
	self assert: (mat hasShape: 8 by: 4).
	self assert: mat abs sum = 12.
	self assert: ((mat atColumn: 2) = (0 asLapackMatrix ,, 0,, 0,, 0,, 0,, 1,, 1,, 1)).
	
	vec03 := 0 asLapackMatrix, 0, 0.
	self assert: ((i3 , vec03 transposed) = (i3 appendColumns: 1)).
	self assert: ((vec03 transposed , seq34) = (seq34 prependColumns: 1)).
	self assert: ((i3 ,, vec03) = (i3 appendRows: 1)).
	self assert: ((vec03 ,, i3) = (i3 prependRows: 1)).
	
	self assert: ((vec03 transposed , vec03 transposed) hasShape: 3 by: 2).
	self assert: ((i3 , ones34) hasShape: 3 by: 7).
	self assert: ((i3 ,, zeros43) hasShape: 7 by: 3).
	self assert: ((((vec03 ,, vec03) ,, vec03) ,, vec03) = zeros43).!

testSmallUpperTriangle
	"Unlike #upperTriangle: , #smallUpperTriangle: can be of smaller size than receiver"
	
	(LapackGeneralMatrix allSubclasses copyWith: AbstractMatrix) do: [:matrixClass |
		| m33 m36 m63 |
		m33 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33)).
		m36 := matrixClass rows: #(#(11 12 13 14 15 16) #(21 22 23 24 25 26) #(31 32 33 34 35 36)).
		m63 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63)).
	
		self assert: (m33 smallUpperTriangle: -2) = m33.
		self assert: (m33 smallUpperTriangle: -1) = (matrixClass rows: #(#(11 12 13 ) #(21 22 23 ) #(0 32 33 ))).
		self assert: (m33 smallUpperTriangle: 0) = (matrixClass rows: #(#(11 12 13) #(0 22 23) #(0 0 33))).
		self assert: (m33 smallUpperTriangle: 1) = (matrixClass rows: #(#(12 13) #(0 23))).
	
		self assert: (m36 smallUpperTriangle: -2) = m36.
		self assert: (m36 smallUpperTriangle: -1) = (matrixClass rows: #(#(11 12 13 14 15 16 ) #(21 22 23 24 25 26 ) #(0 32 33 34 35 36 ))).
		self assert: (m36 smallUpperTriangle: 0) = (matrixClass rows: #(#(11 12 13 14 15 16 ) #(0 22 23 24 25 26 ) #(0 0 33 34 35 36 ))).
		self assert: (m36 smallUpperTriangle: 1) = (matrixClass rows: #(#(12 13 14 15 16 ) #(0 23 24 25 26 ) #(0 0 34 35 36 ))).
		self assert: (m36 smallUpperTriangle: 4) = (matrixClass rows: #(#(15 16 ) #(0 26 ))).
	
		self assert: (m63 smallUpperTriangle: -5) = m63.
		self assert: (m63 smallUpperTriangle: -1) = (matrixClass rows: #(#(11 12 13 ) #(21 22 23 ) #(0 32 33 ) #(0 0 43 ))).
		self assert: (m63 smallUpperTriangle: 0) = (matrixClass rows: #(#(11 12 13) #(0 22 23) #(0 0 33))).
		self assert: (m63 smallUpperTriangle: 1) = (matrixClass rows: #(#(12 13) #(0 23)))]!

testSum
	| aSmallapackArray aMatrix bMatrix md ms mdmd mdms msmd msms |
	aSmallapackArray := DOUBLEArray new: 3 * 2.
	1 to: 6 do: [:i | aSmallapackArray at: i put: i asFloat].
	aMatrix := aSmallapackArray asLapackMatrixNrow: 3 ncol: 2.
	bMatrix := aMatrix + aMatrix.
	1 to: 6 do: [:i | self assert: 2 * i = (bMatrix at: i)].
	self assert: (aMatrix - aMatrix) isZero.
	self assert: aMatrix + aMatrix zero = aMatrix.

	"Automatic conversion"
	md := LapackDGEMatrix rows: #( (3 2 4) (2 -5 -1) ( 1 -2 2)).
	ms := LapackSGEMatrix rows: #( (3 2 4) (2 -5 -1) ( 1 -2 2)).

	mdmd := md+md.
	mdms := md+ms.
	msmd := ms+md.
	msms := ms+ms.

	self assert: mdmd isDoublePrecisionMatrix.
	self assert: msmd isDoublePrecisionMatrix.
	self assert: mdms isDoublePrecisionMatrix.
	self assert: msms isSinglePrecisionMatrix.
	self assert: ((1 to: mdmd size) allSatisfy: [:i | (mdmd at: i) = (mdms at: i)]).
	self assert: ((1 to: mdmd size) allSatisfy: [:i | (mdmd at: i) = (msmd at: i)]).
	self assert: ((1 to: mdmd size) allSatisfy: [:i | (mdmd at: i) asFloat = (msms at: i)]).!

testTransposed

	(Array with: LapackDGEMatrix with: LapackSGEMatrix with: LapackCGEMatrix with: LapackZGEMatrix) do: [:mclass |
		| v vt ge get  |
		v := mclass rows: #( (7  11  13) ).
		vt := v transposed.
		self assert: v nrow = vt ncol.
		self assert: v ncol = vt nrow.
		self assert: (vt = (mclass columns: #( (7  11  13) ))).
		self assert: vt transposed = v.
		
		ge := mclass rows: #( (5  7  11)  (13  17  19) ).
		get := ge transposed.
		self assert: (get = (mclass columns: #( (5  7  11)  (13  17  19) ))).
		self assert: get transposed = ge].
	
	(Array with: LapackDTRMatrix with: LapackSTRMatrix with: LapackCTRMatrix with: LapackZTRMatrix) do: [:mclass |
		| tr trt |
		tr := mclass rows: #( (5  7  11)  (0  13  17)  (0  0  19) ).
		trt := tr transposed.
		self assert: trt class = tr class.
		self assert: trt isUpper = tr isLower.
		self assert: trt transposed class = tr class.
		self assert: trt transposed = tr].
	
	(Array with: LapackDSYMatrix with: LapackSSYMatrix with: LapackCHEMatrix with: LapackZHEMatrix) do: [:mclass |
		| sy syt |
		sy := mclass rows: #( (5  7  11)  (7  13  17)  (11  17  19) ).
		syt := sy transposed.
		self assert: syt class = sy class.
		self assert: syt = sy].!

testUpperTriangle
	"Unlike #upperTriangle: , #smallUpperTriangle: can be of smaller size than receiver"
	
	(LapackGeneralMatrix allSubclasses copyWith: AbstractMatrix) do: [:matrixClass |
		| m33 m35 m37 m53 m73 |
		m33 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33)).
		m35 := matrixClass rows: #(#(11 12 13 14 15 ) #(21 22 23 24 25 ) #(31 32 33 34 35 )).
		m37 := matrixClass rows: #(#(11 12 13 14 15 16 17) #(21 22 23 24 25 26 27) #(31 32 33 34 35 36 37)).
		m53 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33) #(41 42 43) #(51 52 53) ).
		m73 := matrixClass rows: #(#(11 12 13) #(21 22 23) #(31 32 33) #(41 42 43) #(51 52 53) #(61 62 63) #(71 72 73)).
	
		self assert: (m33 upperTriangle: -2) = m33.
		self assert: (m33 upperTriangle: -1) = (matrixClass rows: #(#(11 12 13 ) #(21 22 23 ) #(0 32 33 ))).
		self assert: (m33 upperTriangle: 0) = (matrixClass rows: #(#(11 12 13) #(0 22 23) #(0 0 33))).
		self assert: (m33 upperTriangle: 1) = (matrixClass rows: #(#(0 12 13) #(0 0 23) #(0 0 0))).
	
		self assert: (m35 upperTriangle: -2) = m35.
		self assert: (m35 upperTriangle: -1) = (matrixClass rows: #(#(11 12 13 14 15) #(21 22 23 24 25) #(0 32 33 34 35))).
		self assert: (m35 upperTriangle: 0) = (matrixClass rows: #(#(11 12 13 14 15) #(0 22 23 24 25) #(0 0 33 34 35))).
		self assert: (m35 upperTriangle: 1) = (matrixClass rows: #(#(0 12 13 14 15) #(0 0 23 24 25) #(0 0 0 34 35))).
		self assert: (m35 upperTriangle: 4) = (matrixClass rows: #(#(0 0 0 0 15) #(0 0 0 0 0) #(0 0 0 0 0))).

		self assert: (m37 upperTriangle: -2) = m37.
		self assert: (m37 upperTriangle: -1) = (matrixClass rows: #(#(11 12 13 14 15 16 17) #(21 22 23 24 25 26 27) #(0 32 33 34 35 36 37))).
		self assert: (m37 upperTriangle: 0) = (matrixClass rows: #(#(11 12 13 14 15 16 17) #(0 22 23 24 25 26 27) #(0 0 33 34 35 36 37))).
		self assert: (m37 upperTriangle: 1) = (matrixClass rows: #(#(0 12 13 14 15 16 17) #(0 0 23 24 25 26 27) #(0 0 0 34 35 36 37))).
		self assert: (m37 upperTriangle: 4) = (matrixClass rows: #(#(0 0 0 0 15 16 17) #(0 0 0 0 0 26 27) #(0 0 0 0 0 0 37))).
	
		self assert: (m53 upperTriangle: -5) = m53.
		self assert: (m53 upperTriangle: -1) = (matrixClass rows: #(#(11 12 13) #(21 22 23) #(0 32 33) #(0 0 43) #(0 0 0))).
		self assert: (m53 upperTriangle: 0) = (matrixClass rows: #(#(11 12 13) #(0 22 23) #(0 0 33) #(0 0 0) #(0 0 0))).
		self assert: (m53 upperTriangle: 1) = (matrixClass rows: #(#(0 12 13) #(0 0 23) #(0 0 0) #(0 0 0) #(0 0 0))).
	
		self assert: (m73 upperTriangle: -6) = m73.
		self assert: (m73 upperTriangle: -1) = (matrixClass rows: #(#(11 12 13) #(21 22 23) #(0 32 33) #(0 0 43) #(0 0 0) #(0 0 0) #(0 0 0))).
		self assert: (m73 upperTriangle: 0) = (matrixClass rows: #(#(11 12 13) #(0 22 23) #(0 0 33) #(0 0 0) #(0 0 0) #(0 0 0) #(0 0 0))).
		self assert: (m73 upperTriangle: 1) = (matrixClass rows: #(#(0 12 13) #(0 0 23) #(0 0 0) #(0 0 0) #(0 0 0) #(0 0 0) #(0 0 0)))]! !
!TestLapackMatrix categoriesFor: #setUp!public! !
!TestLapackMatrix categoriesFor: #testComplex!public! !
!TestLapackMatrix categoriesFor: #testConcatenation!public! !
!TestLapackMatrix categoriesFor: #testCreation!public! !
!TestLapackMatrix categoriesFor: #testDeterminant!public! !
!TestLapackMatrix categoriesFor: #testEigenValues!public! !
!TestLapackMatrix categoriesFor: #testHessenberg!public! !
!TestLapackMatrix categoriesFor: #testLeastSquareWithEqualityConstraints!public!running! !
!TestLapackMatrix categoriesFor: #testLowerTriangle!public!running! !
!TestLapackMatrix categoriesFor: #testMatrixDiagonal!public! !
!TestLapackMatrix categoriesFor: #testMatrixOperation!public! !
!TestLapackMatrix categoriesFor: #testMatrixProduct!public! !
!TestLapackMatrix categoriesFor: #testMatrixReplicate!public! !
!TestLapackMatrix categoriesFor: #testMatrixSymmetric!public! !
!TestLapackMatrix categoriesFor: #testMatrixTriangular!public! !
!TestLapackMatrix categoriesFor: #testQR!public! !
!TestLapackMatrix categoriesFor: #testReciprocal!public! !
!TestLapackMatrix categoriesFor: #testSingularValues!public! !
!TestLapackMatrix categoriesFor: #testSmallLowerTriangle!public!running! !
!TestLapackMatrix categoriesFor: #testSmallPackAccessing!public! !
!TestLapackMatrix categoriesFor: #testSmallPackArithmetic!public! !
!TestLapackMatrix categoriesFor: #testSmallPackAugmenting!public! !
!TestLapackMatrix categoriesFor: #testSmallUpperTriangle!public!running! !
!TestLapackMatrix categoriesFor: #testSum!public! !
!TestLapackMatrix categoriesFor: #testTransposed!public!running! !
!TestLapackMatrix categoriesFor: #testUpperTriangle!public!running! !

TestRandMatrix guid: (GUID fromString: '{30B2F9AD-6C51-4ACD-9067-99977CF6031C}')!
TestRandMatrix comment: ''!
!TestRandMatrix categoriesForClass!Unclassified! !
!TestRandMatrix methodsFor!

testComplexTiming
	| dcm1 dcm2 t1 t2 iseed |
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm1 := LapackSGEMatrix randNormal: 1000 @ 1000 withSeed: iseed.
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm2 := LapackDGEMatrix randNormal: 1000 @ 1000 withSeed: iseed.
	t1 := Time millisecondsToRun: [(dcm1 i: dcm1) imaginaryPart].
	MemoryManager current collectGarbage.
	t2 := Time millisecondsToRun: [(dcm2 i: dcm2) imaginaryPart].
	Transcript
		cr;
		print: (Array 
					with: t1
					with: t2
					with: (t1 / t2) asFloat
					with: dcm2 size / (1000.0d0 * t2));
		flush!

testHermitianEigenValues
	| dcm1 dcm2 iseed ev1 ev2 |
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm1 := LapackCHEMatrix randNormal: 50 @ 50 withSeed: iseed.
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm2 := LapackZHEMatrix randNormal: 50 @ 50 withSeed: iseed.

	ev1 := dcm1 eigenValueDecomposition.
	ev1 wantLeftEigenVectors: true.
	self assert: ev1 eigenValues class isRealMatrix.
	self 
		assert: (dcm1 * ev1 rightEigenVectors 
				- (ev1 rightEigenVectors * (LapackSSYMatrix diagonal: ev1 eigenValues))) 
					absMax < 50.0d-6.

	ev1 solveByDivideAndConquer.
	self assert: ev1 eigenValues class isRealMatrix.
	self 
		assert: (dcm1 * ev1 rightEigenVectors 
				- (ev1 rightEigenVectors * (LapackSSYMatrix diagonal: ev1 eigenValues))) 
					absMax < 50.0d-6.

	ev2 := dcm2 eigenValueDecomposition.
	ev2 wantLeftEigenVectors: true.
	self assert: ev2 eigenValues class isRealMatrix.
	self 
		assert: (dcm2 * ev2 rightEigenVectors 
				- (ev2 rightEigenVectors * (LapackDSYMatrix diagonal: ev2 eigenValues))) 
					absMax < 50.0d-12.

	ev2 solveByDivideAndConquer.
	self assert: ev2 eigenValues class isRealMatrix.
	self 
		assert: (dcm2 * ev2 rightEigenVectors 
				- (ev2 rightEigenVectors * (LapackDSYMatrix diagonal: ev2 eigenValues))) 
					absMax < 50.0d-12!

testOperationTiming
	| a |
	a := Array new: 13.
	a at: 1 put: (LapackDGEMatrix eye: 33).
	a at: 2 put: (AbstractMatrix eye: 33).
	a at: 3 put: (LapackZGEMatrix eye: 33).
	a at: 4 put: (LapackSGEMatrix eye: 33).
	a at: 5 put: (LapackCGEMatrix eye: 33).
	a at: 6 put: (LapackDTRMatrix eye: 33).
	a at: 7 put: (LapackZTRMatrix eye: 33).
	a at: 8 put: (LapackSTRMatrix eye: 33).
	a at: 9 put: (LapackCTRMatrix eye: 33).
	a at: 10 put: (LapackDSYMatrix eye: 33).
	a at: 11 put: (LapackZHEMatrix eye: 33).
	a at: 12 put: (LapackSSYMatrix eye: 33).
	a at: 13 put: (LapackCHEMatrix eye: 33).
	"#elementProduct: ?"
	#(#+ #- #* #, #,,) do: 
			[:op | 
			| ref error |
			ref := a first perform: op with: a first.
			Transcript
				nextPutAll: 'testing : ' , op;
				cr;
				flush.
			error := AbstractMatrix nrow: a size ncol: a size.
			1 to: a size
				do: 
					[:i | 
					Transcript
						print: (a at: i) class;
						space;
						flush.
					1 to: a size
						do: 
							[:j | 
							self 
								assert: (error 
										rowAt: i
										columnAt: j
										put: (((a at: i) perform: op with: (a at: j)) - ref) normInfinity) < 1.0e-6].
					Transcript
						nextPutAll: 'max error=';
						print: error max;
						cr;
						flush]]!

testReciprocal
	| iseed baseClass testCases |
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	baseClass := LapackDGEMatrix.
	testCases := #(#(#yourself) #(#complexMatrix) #(#singlePrecisionMatrix) #(#singlePrecisionMatrix #complexMatrix)).
	testCases do: 
			[:array | 
			| tmp |
			tmp := (array inject: baseClass into: [:class :msg | class perform: msg]) 
						randNormal: 30 @ 30
						withSeed: iseed.
			self assert: (tmp reciprocal * tmp - tmp identity) absMax < (30 * tmp singularValues max)]!

testReciprocalTiming
	| dcm1 dcm2 t1 t2 iseed |
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm1 := LapackSGEMatrix randNormal: 300 @ 300 withSeed: iseed.
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm2 := LapackDGEMatrix randNormal: 300 @ 300 withSeed: iseed.
	MemoryManager current collectGarbage.
	t1 := Time millisecondsToRun: [dcm1 reciprocal].
	MemoryManager current collectGarbage.
	t2 := Time millisecondsToRun: [dcm2 reciprocal].
	Transcript
		cr;
		print: (Array 
					with: t1
					with: t2
					with: (t1 / t2) asFloat
					with: dcm2 size / (1000.0 * t2));
		flush!

testSymmetricEigenValues
	| dcm1 dcm2 iseed ev1 ev2 |
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm1 := LapackSSYMatrix randNormal: 50 @ 50 withSeed: iseed.
	iseed := Array 
				with: 3
				with: 5
				with: 7
				with: 11.
	dcm2 := LapackDSYMatrix randNormal: 50 @ 50 withSeed: iseed.

	ev1 := dcm1 eigenValueDecomposition.
	ev1 wantLeftEigenVectors: true.
	self assert: ev1 eigenValues class isRealMatrix.
	self 
		assert: (dcm1 * ev1 rightEigenVectors 
				- (ev1 rightEigenVectors * (LapackSSYMatrix diagonal: ev1 eigenValues))) 
					absMax < 50.0d-6.

	ev1 solveByDivideAndConquer.
	self assert: ev1 eigenValues class isRealMatrix.
	self 
		assert: (dcm1 * ev1 rightEigenVectors 
				- (ev1 rightEigenVectors * (LapackSSYMatrix diagonal: ev1 eigenValues))) 
					absMax < 50.0d-6.

	ev2 := dcm2 eigenValueDecomposition.
	ev2 wantLeftEigenVectors: true.
	self assert: ev2 eigenValues class isRealMatrix.
	self 
		assert: (dcm2 * ev2 rightEigenVectors 
				- (ev2 rightEigenVectors * (LapackDSYMatrix diagonal: ev2 eigenValues))) 
					absMax < 50.0d-12.

	ev2 solveByDivideAndConquer.
	self assert: ev2 eigenValues class isRealMatrix.
	self 
		assert: (dcm2 * ev2 rightEigenVectors 
				- (ev2 rightEigenVectors * (LapackDSYMatrix diagonal: ev2 eigenValues))) 
					absMax < 50.0d-12! !
!TestRandMatrix categoriesFor: #testComplexTiming!public! !
!TestRandMatrix categoriesFor: #testHermitianEigenValues!public! !
!TestRandMatrix categoriesFor: #testOperationTiming!public! !
!TestRandMatrix categoriesFor: #testReciprocal!public! !
!TestRandMatrix categoriesFor: #testReciprocalTiming!public! !
!TestRandMatrix categoriesFor: #testSymmetricEigenValues!public! !

"Binary Globals"!

