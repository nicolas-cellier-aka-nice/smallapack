"Filed out from Dolphin Smalltalk 7"!

Object subclass: #AbstractMatrix
	instanceVariableNames: 'array nrow ncol'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractMatrix guid: (GUID fromString: '{914C7792-6775-471F-AE37-DA3CB50F863D}')!
AbstractMatrix comment: 'AbstractMatrix is the abstract superclass for common Matrix protocol
AbstractMatrix have a number of elements equal to its number of rows times its number of columns.
The elements are stored in a flat (Array of Number) rather than in an (Array of: (Array of: Number)).
Elements are ordered with column-major order (like FORTRAN and Matlab).
This arbitrary tyrannic implementation choice isn''t mine;
It comes from the fact that original LAPACK BLAS libraries are written in FORTRAN.

Instance Variables:
	array	<SequenceableCollection>	hold elements of the matrix
	ncol	<Integer>	number of columns
	nrow	<Integer>	number of rows'!
!AbstractMatrix categoriesForClass!Unclassified! !
!AbstractMatrix methodsFor!

- aMatrix 

* aMatrix 

, aMatrix 

,, aMatrix 

+ aMatrix 

= aMatrix 

abs

absMax

adaptToComplex: rcvr andSend: selector 

adaptToFloat: rcvr andCompare: selector 


adaptToFloat: rcvr andSend: selector 

adaptToFraction: rcvr andSend: selector 

adaptToInteger: rcvr andCompare: selector 
	"If I am involved in comparison with an Integer."

	^self adaptToNumber: rcvr andSend: selector!

adaptToInteger: rcvr andSend: selector 

adaptToNumber: rcvr andSend: selector 

adaptToScaledDecimal: rcvr andSend: selector 

addToFloat: aFloat 

addToFraction: aFraction 

addToInteger: anInteger 

addToScaledDecimal: aScaledDecimal

allSatisfy: aBlock 

anySatisfy: aBlock 

appendColumns: nColumns 

appendRows: nRows 

arrayAt: anInteger

arrayAt: anInteger put: aNumber

arrayOffsetAtRow: rowIndex atColumn: columnIndex 

arraySize

asAbstractMatrix

asArray

asBag

asColumnMatrix
			res]!

asDoubleComplexMatrix

asDoubleMatrix

asDoublePrecisionComplexMatrix

asDoublePrecisionMatrix

asFloatComplexMatrix

asFloatMatrix

asMatrix

asOrderedCollection

asRowMatrix
			res]!

asSet

asSinglePrecisionComplexMatrix

asSinglePrecisionMatrix

asSortedCollection

asSortedCollection: aBlock 

at: anInteger

at: rowIndex at: columnIndex 

at: r at: c ifInvalid: v

at: row at: column incrementBy: value 

at: rowIndex at: columnIndex put: aNumber

at: anInteger put: aNumber 

atAllPut: aNumber 

atColumn: columnIndex 

atColumn: colIndex put: aCollection 

atColumn: columnIndex putAll: aNumber 

atColumn: colIndex putSequence: aCollection 

atColumns: columnsIndexCollection 

atInteger: anInteger

atInteger: rowIndex andInteger: columnIndex 

atInteger: rowIndex andInteger: columnIndex put: aNumber

atInteger: anInteger put: aNumber

atIntervalFrom: interStart to: interStop by: interStep 

atPoint: aPoint 

atPoint: aPoint put: aNumber

atRow: rowIndex 

atRow: rowIndex column: columnIndex 

atRow: rowIndex column: columnIndex put: aNumber 

atRow: colIndex put: aCollection 

atRow: rowIndex putAll: aNumber 

atRow: rowIndex putSequence: aCollection 

atRows: rowsIndexCollection 

atRows: rowsIndexCollection atColumns: columnIndexCollection
	| res |
	res := self class allocateNrow: rowsIndexCollection size ncol: columnIndexCollection size.
	columnIndexCollection keysAndValuesDo: [:j :jCol |
		rowsIndexCollection  keysAndValuesDo: [:i :iRow |
			res rowAt: i columnAt: j put: (self rowAt: iRow columnAt: jCol)]].
	^res!

capacity

collect: aBlock 

columnAt: columnIndex 

columnAt: columnIndex putAll: aNumber 

columnAt: colIndex putSequence: aCollection 

columnCount

columns

columns: anArrayOfColumns 

columnsDo: aBlock 

concatColumnsFromLapackMatrix: aLapackMatrix

concatColumnsFromMatrix: aMatrix 

concatColumnsWithMatrix: aMatrix 

concatRowsFromLapackMatrix: aLapackMatrix

concatRowsFromMatrix: aMatrix 

concatRowsWithMatrix: aMatrix 

conjugated

copy: n elementsFrom: aMatrix sourceIncrement: incx destIncrement: incy 

copy: n elementsFrom: aMatrix sourceOffset: offx sourceIncrement: incx destOffset: offy destIncrement: incy 
	"BLAS primitve xCOPY - naive implementation"

	| sx sy |
	sx := incx positive ifTrue: [offx + 1] ifFalse: [(1 - n) * incx + offx + 1].
	sy := incy positive ifTrue: [offy + 1] ifFalse: [(1 - n) * incy + offy + 1].
	0 to: n - 1
		do: 
			[:i | 
			self at: i * incy + sy
				put: (aMatrix at: i * incx + sx)]!

copy: m rowsStartingAt: i and: n columnsStartingAt: j from: a 

count: aBlock 

cumulativeProduct: aBlock dimension: aDimension 

cumulativeSum: aBlock dimension: aDimension 

diagonal

diagonalAt: index 

diagonalSizeAt: index 

differenceFromComplex: aComplex 

differenceFromDouble: aDouble 

differenceFromFixedPoint: aFixedPoint 

differenceFromFloat: aFloat 

differenceFromFraction: aFraction 

differenceFromInteger: anInteger 

differenceFromLapackMatrix: aLapackMatrix

differenceFromMatrix: aMatrix 

dimensions

do: aBlock 

dotProduct: n elementsIncrement: incx with: aMatrix increment: incy 
	"BLAS primitve xDOT - naive implementation"

	| sx sy |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - n) * incx + 1].
	sy := incy positive ifTrue: [ 1] ifFalse: [(1 - n) * incy +1].
	^(1 to: n) sum: 
			[:i | 
			(self at: (i - 1) * incx + sx) 
				* (aMatrix at: (i - 1) * incy + sy)]!

elementwisePowerFromNumber: aNumber 

elementwisePowerWithMatrix: aMatrix 

elementwisePowerWithNumber: aNumber 

elementwiseProductFromNumber: aNumber 

elementwiseProductWithMatrix: aMatrix 

elementwiseProductWithNumber: aNumber 

elementwiseQuotientFromNumber: aNumber 

elementwiseQuotientWithMatrix: aMatrix 

elementwiseQuotientWithNumber: aNumber 

fill: n elementsWithStride: incy withSelfPlusScalar: alpha timesVector: aMatrix stride: incx 
	"BLAS primitve xAXPY - naive implementation"

	| sx sy |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - n) * incx + 1].
	sy := incy positive ifTrue: [1] ifFalse: [(1 - n) * incy + 1].
	1 to: n
		do: 
			[:i | 
			self at: (i - 1) * incy + sy
				put: (aMatrix at: (i - 1) * incx + sx) 
						+ (alpha * (self at: (i - 1) * incy + sy))]!

fill: m elementsWithStride: incy withSelfScaledBy: beta plusScalar: alpha timesMatrix: a transposed: trans timesVector: x length: n stride: incx 
	"BLAS primitve xGEMV - naive implementation"

	| sx sy |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - n) * incx + 1].
	sy := incy positive ifTrue: [1] ifFalse: [(1 - m) * incy + 1].
	trans 
		ifTrue: 
			[1 to: m
				do: 
					[:i | 
					self at: (i - 1) * incy + sy
						put: alpha * ((1 to: n) 
										sum: [:j | (a rowAt: j columnAt: i) * (x at: (j - 1) * incx + sx)]) 
								+ (beta * (self at: (i - 1) * incy + sy))]]
		ifFalse: 
			[1 to: m
				do: 
					[:i | 
					self at: (i - 1) * incy + sy
						put: alpha * ((1 to: n) 
										sum: [:j | (a rowAt: i columnAt: j) * (x at: (j - 1) * incx + sx)]) 
								+ (beta * (self at: (i - 1) * incy + sy))]]!

fillM: m byN: n withScalar: alpha timesColumnVector: x stride: incx timesRowVector: y stride: incy 
	"Blas library xGERx 
	fill mxn elements of self from following m-length x and a n-length y vector product
		alpha*x*transpose(y)"

	| sx sy |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - m) * incx + 1].
	sy := incy positive ifTrue: [1] ifFalse: [(1 - n) * incy + 1].
	1 to: n
		do: 
			[:j | 
			1 to: m
				do: 
					[:i | 
					self 
						rowAt: i
						columnAt: j
						put: alpha * (x at: (i - 1) * incx + sx) 
								* (y at: (j - 1) * incy + sy)]]!

fillM: m byN: n withSelfScaledBy: beta plusScalar: alpha timesLeftMatrix: a transposed: transa timesRightMatrix: b transposed: transb length: k 
	"BLAS primitve xGEMM - naive implementation"

	transa 
		ifTrue: 
			[transb 
				ifTrue: 
					[1 to: n do: [:j | 
						1 to: m do: [:i | 
							self 
								rowAt: i
								columnAt: j
								put: alpha * ((1 to: k) 
												sum: [:kk | (a rowAt: kk columnAt: i) * (b rowAt: j columnAt: kk)]) 
										+ (beta * (self rowAt: i columnAt: j))]]]
				ifFalse: 
					[1 to: n do: [:j | 
						1 to: m do: [:i | 
							self 
								rowAt: i
								columnAt: j
								put: alpha * ((1 to: k) 
												sum: [:kk | (a rowAt: kk columnAt: i) * (b rowAt: kk columnAt: j)]) 
										+ (beta * (self rowAt: i columnAt: j))]]]]
		ifFalse: 
			[transb 
				ifTrue: 
					[1 to: n do: [:j | 
						1 to: m do: [:i | 
							self 
								rowAt: i
								columnAt: j
								put: alpha * ((1 to: k) 
												sum: [:kk | (a rowAt: i columnAt: kk) * (b rowAt: j columnAt: kk)]) 
										+ (beta * (self rowAt: i columnAt: j))]]]
				ifFalse: 
					[1 to: n do: [:j | 
						1 to: m do: [:i | 
							self 
								rowAt: i
								columnAt: j
								put: alpha * ((1 to: k) 
												sum: [:kk | (a rowAt: i columnAt: kk) * (b rowAt: kk columnAt: j)]) 
										+ (beta * (self rowAt: i columnAt: j))]]]]!

findMax

findMaxOf: aBlock 

findMin

findMinOf: aBlock 

first

fromColumn: jStart toColumn: jStop by: jStep

fromRow: iStart toRow: iStop by: iStep

fromRow: irStart toRow: irStop fromColumn: jcStart toColumn: jcStop 
	"Extract a contiguous sub-matrix"

	| nr nc result |
	nr := irStop - irStart + 1.
	nc := jcStop - jcStart + 1.
	result := self class nrow: nr ncol: nc.
	result 
		copy: nr
		rowsStartingAt: irStart
		and: nc
		columnsStartingAt: jcStart
		from: self.
	^result!

generalizedAt: index 

generalizedAt: index put: anObject 

hash

hasSameShapeAs: aMatrix

hasShape: nRows by: nColumns 

i

i: aMatrix

identity

imaginaryPart

indicesCollect: aBlock 

indicesDo: aBlock 

indicesInject: start into: aBlock 

inject: aValue into: aBlock 

isColumnMatrix

isColumnVector

isComplexMatrix

isDiagonal

isEmpty

isHermitian

isLowerTriangular

isMatrix

isRowMatrix

isRowVector

isSameSequenceAs: otherCollection

isSequenceable

isSquare

isSymmetric

isTriangular

isUpperTriangular

isVector

isZero

last

lowerTriangle

lowerTriangle: ind 

max

maxOf: aBlock 

maxOf: aBlock dimension: aDimension 

min

minOf: aBlock 

minOf: aBlock dimension: aDimension 

multiplyByFloat: aFloat 

multiplyByFraction: aFraction 

multiplyByInteger: anInteger 

multiplyByScaledDecimal: aScaledDecimal 

naiveSetOffDiagonal: alpha diagonal: beta 

ncol

nCols

negated

norm1

norm2
		ifTrue: [self vectorNorm2]
		ifFalse: [self singularValues max]!

normFrobenius

normInfinity

nrow

nRows

numberOfColumns

numberOfRows

postCopy

prependColumns: nColumns 

prependRows: nRows 

printOn: aStream 

product

product: aBlock 
	"backward compatibility - synonym of productOf:"

	^self productOf: aBlock!

product: aBlock dimension: aDimension 

productColumnVectorWithRowVector: aVector 

productFromComplex: aComplex 

productFromDouble: aDouble 

productFromFixedPoint: aFixedPoint 

productFromFloat: aFloat 

productFromFraction: aFraction 

productFromInteger: anInteger 

productFromLapackMatrix: aLapackMatrix

productFromMatrix: aMatrix 

productMatrixAtRightWithMatrix: aMatrix 

productMatrixTransposeWithColumnVector: aVector 

productMatrixWithColumnVector: aVector 

productMatrixWithMatrix: aMatrix 
	| a b c iB iC |
	a := self.
	b := aMatrix.
	c := aMatrix class allocateNrow: a nrow ncol: b ncol.
	iC := 0.
	iB := 1.
	1 to: b ncol
		do: 
			[:jc | 
			1 to: a nrow
				do: 
					[:iA | 
					| cij kA kB |
					cij := (a at: (kA := iA)) * (b at: (kB := iB)).
					2 to: b nrow
						do: [:k | cij := cij + ((a at: (kA := kA + a nrow)) * (b at: (kB := kB + 1)))].
					c at: (iC := iC + 1) put: cij].
			iB := iB + b nrow].
	^c!

productOf: aBlock 
	"Evaluate the product of all elements"

	self isEmpty ifTrue: [^1].

productOf: aBlock dimension: aDimension 

productRowVectorWithColumnVector: aVector 

productRowVectorWithMatrix: aMatrix 

realPart

replicate: newDimensions
	^self class shape: newDimensions do: [:nr :nc | self replicateNrow: nr timesNcol: nc]!

replicateNrow: nr timesNcol: nc 

reshape: newDimensions
	^self class shape: newDimensions do: [:nr :nc | self reshapeNrow: nr ncol: nc]!

reshapeNrow: nr ncol: nc
	nrow * ncol = (nr * nc) ifFalse: [self error: 'cannot reshape with a different size'].
	^(nrow = nr and: [ncol = nc])
		ifTrue: [self]
		ifFalse: 
			[| res |
			res := self class nrow: nr ncol: nc.
			1 to: self size do: [:i | res at: i put: (self at: i)].
			res]!

respondsToArithmetic

rowAt: rowIndex 

rowAt: rowIndex columnAt: columnIndex 

rowAt: rowIndex columnAt: columnIndex  put: aNumber

rowAt: rowIndex putAll: aNumber 

rowAt: rowIndex putSequence: aCollection 

rowCount

rows

rows: anArrayOfRows 

rowsDo: aBlock 

scale: n elementsBy: alpha increment: incx 
	"BLAS primitve xSCAL - naive implementation"

	| sx |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - n) * incx + 1].
	1 to: n
		do: 
			[:i | 
			self at: (i - 1) * incx + sx
				put: (self at: (i - 1) * incx + sx) * alpha]!

scaledByComplex: aComplex 

scaledByNumber: aNumber 

setArray: anArray nrow: nr ncol: nc 

setDiagonal: aMatrix

setOffDiagonal: alpha diagonal: beta 

setToEye

singularValues
	^self subclassResponsibility!

size

smallLowerTriangle: ind 
	"return small lower triangle matrix starting with super-diagonal ind"

	| lower |
	ind <= 0
		ifTrue: [
			lower := self class nrow: (nrow + ind max: 0) ncol: (ncol min: (nrow + ind max: 0)).
			1 to: lower ncol
				do: 
					[:j | 
					1 to: lower nrow
						do: 
							[:i | 
							j <= i 
								ifTrue: 
									[lower 
										rowAt: i
										columnAt: j
										put: (self rowAt: i - ind columnAt: j)]]]]
		ifFalse: [
			lower := self class nrow: nrow ncol: (ncol min: nrow + ind).
			1 to: lower ncol
				do: 
					[:j | 
					1 to: lower nrow
						do: 
							[:i | 
							j - ind <= i 
								ifTrue: 
									[lower 
										rowAt: i
										columnAt: j
										put: (self rowAt: i columnAt: j)]]]].
	^lower	!

smallUpperTriangle: ind 
	"return small upper triangle matrix starting with super-diagonal ind"

	| upper |
	ind >= 0
		ifTrue: [
			upper := self class nrow: (nrow min: (ncol - ind max: 0)) ncol: (ncol - ind max: 0).
			1 to: upper ncol
				do: 
					[:j | 
					1 to: upper nrow
						do: 
							[:i | 
							j >= i 
								ifTrue: 
									[upper 
										rowAt: i
										columnAt: j
										put: (self rowAt: i columnAt: j + ind)]]]]
		ifFalse: [
			upper := self class nrow: (nrow min: ncol - ind) ncol: ncol.
			1 to: upper ncol
				do: 
					[:j | 
					1 to: upper nrow
						do: 
							[:i | 
							j - ind >= i 
								ifTrue: 
									[upper 
										rowAt: i
										columnAt: j
										put: (self rowAt: i columnAt: j)]]]].
	^upper	!

subtractFromFloat: aFloat 

subtractFromFraction: aFraction 

subtractFromInteger: anInteger

subtractFromScaledDecimal: aScaledDecimal

sum

sum: aBlock 
	"backward compatibility - synonym of sumOf:"

	#deprecated.

sum: aBlock dimension: aDimension 

sumFromComplex: aComplex 

sumFromDouble: aDouble 

sumFromFixedPoint: aFixedPoint 

sumFromFloat: aFloat 

sumFromFraction: aFraction 

sumFromInteger: anInteger 

sumFromLapackMatrix: aLapackMatrix

sumFromMatrix: aMatrix 

sumOf: aBlock 

sumOf: aBlock dimension: aDimension 

swap: r1 at: c1 with: r2 at: c2 

swapColumn: c1 withColumn: c2

swapRow: r1 withRow: r2

swapRowAt: r1 columnAt: c1 withRowAt: r2 columnAt: c2 

transpose

transposeConjugated

transposed

transposedConjugated

upperTriangle

upperTriangle: ind 

vectorNorm1

vectorNorm2

vectorNormFrobenius

vectorNormInfinity

with: aCollection collect: aBlock 

with: aCollection do: aBlock 

withArrayOffsetBy: aPositiveInteger 

withIndicesCollect: aBlock 

withIndicesDo: aBlock 

withIndicesInject: start into: aBlock 

zero
!AbstractMatrix categoriesFor: #-!arithmetic!public! !
!AbstractMatrix categoriesFor: #*!arithmetic!public! !
!AbstractMatrix categoriesFor: #,!concatenating!public! !
!AbstractMatrix categoriesFor: #,,!concatenating!public! !
!AbstractMatrix categoriesFor: #+!arithmetic!public! !
!AbstractMatrix categoriesFor: #=!comparing!public! !
!AbstractMatrix categoriesFor: #abs!arithmetic!public! !
!AbstractMatrix categoriesFor: #absMax!arithmetic!norm!public! !
!AbstractMatrix categoriesFor: #adaptToComplex:andSend:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #adaptToFloat:andCompare:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #adaptToFloat:andSend:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #adaptToFraction:andSend:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #adaptToInteger:andCompare:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #adaptToInteger:andSend:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #adaptToNumber:andSend:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #adaptToScaledDecimal:andSend:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #addToFloat:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #addToFraction:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #addToInteger:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #addToScaledDecimal:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #allSatisfy:!enumerating!public! !
!AbstractMatrix categoriesFor: #anySatisfy:!enumerating!public! !
!AbstractMatrix categoriesFor: #appendColumns:!concatenating!public! !
!AbstractMatrix categoriesFor: #appendRows:!concatenating!public! !
!AbstractMatrix categoriesFor: #arrayAt:!accessing-storage!public! !
!AbstractMatrix categoriesFor: #arrayAt:put:!accessing-storage!public! !
!AbstractMatrix categoriesFor: #arrayOffsetAtRow:atColumn:!accessing-storage!public! !
!AbstractMatrix categoriesFor: #arraySize!accessing-size!public! !
!AbstractMatrix categoriesFor: #asAbstractMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asArray!converting!public! !
!AbstractMatrix categoriesFor: #asBag!converting!public! !
!AbstractMatrix categoriesFor: #asColumnMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asDoubleComplexMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asDoubleMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asDoublePrecisionComplexMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asDoublePrecisionMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asFloatComplexMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asFloatMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asOrderedCollection!converting!public! !
!AbstractMatrix categoriesFor: #asRowMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asSet!converting!public! !
!AbstractMatrix categoriesFor: #asSinglePrecisionComplexMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asSinglePrecisionMatrix!converting!public! !
!AbstractMatrix categoriesFor: #asSortedCollection!converting!public! !
!AbstractMatrix categoriesFor: #asSortedCollection:!converting!public! !
!AbstractMatrix categoriesFor: #at:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #at:at:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #at:at:ifInvalid:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #at:at:incrementBy:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #at:at:put:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #at:put:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atAllPut:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atColumn:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atColumn:put:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atColumn:putAll:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atColumn:putSequence:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atColumns:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atInteger:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atInteger:andInteger:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atInteger:andInteger:put:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atInteger:put:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atIntervalFrom:to:by:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atPoint:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atPoint:put:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #atRow:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atRow:column:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atRow:column:put:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atRow:put:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atRow:putAll:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atRow:putSequence:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atRows:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #atRows:atColumns:!public! !
!AbstractMatrix categoriesFor: #capacity!accessing-size!public! !
!AbstractMatrix categoriesFor: #collect:!enumerating!public! !
!AbstractMatrix categoriesFor: #columnAt:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #columnAt:putAll:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #columnAt:putSequence:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #columnCount!accessing-size!public! !
!AbstractMatrix categoriesFor: #columns!enumerating!public! !
!AbstractMatrix categoriesFor: #columns:!enumerating!public! !
!AbstractMatrix categoriesFor: #columnsDo:!enumerating!public! !
!AbstractMatrix categoriesFor: #concatColumnsFromLapackMatrix:!concatenating!public! !
!AbstractMatrix categoriesFor: #concatColumnsFromMatrix:!concatenating!public! !
!AbstractMatrix categoriesFor: #concatColumnsWithMatrix:!concatenating!public! !
!AbstractMatrix categoriesFor: #concatRowsFromLapackMatrix:!concatenating!public! !
!AbstractMatrix categoriesFor: #concatRowsFromMatrix:!concatenating!public! !
!AbstractMatrix categoriesFor: #concatRowsWithMatrix:!concatenating!public! !
!AbstractMatrix categoriesFor: #conjugated!arithmetic!arithmetic-complex!public! !
!AbstractMatrix categoriesFor: #copy:elementsFrom:sourceIncrement:destIncrement:!blas!public! !
!AbstractMatrix categoriesFor: #copy:elementsFrom:sourceOffset:sourceIncrement:destOffset:destIncrement:!blas!public! !
!AbstractMatrix categoriesFor: #copy:rowsStartingAt:and:columnsStartingAt:from:!blas!public! !
!AbstractMatrix categoriesFor: #count:!enumerating!public! !
!AbstractMatrix categoriesFor: #cumulativeProduct:dimension:!enumerating!public! !
!AbstractMatrix categoriesFor: #cumulativeSum:dimension:!enumerating!public! !
!AbstractMatrix categoriesFor: #diagonal!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #diagonalAt:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #diagonalSizeAt:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #differenceFromComplex:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #differenceFromDouble:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #differenceFromFixedPoint:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #differenceFromFloat:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #differenceFromFraction:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #differenceFromInteger:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #differenceFromLapackMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #differenceFromMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #dimensions!accessing-size!public! !
!AbstractMatrix categoriesFor: #do:!enumerating!public! !
!AbstractMatrix categoriesFor: #dotProduct:elementsIncrement:with:increment:!blas!public! !
!AbstractMatrix categoriesFor: #elementwisePowerFromNumber:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwisePowerWithMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwisePowerWithNumber:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwiseProductFromNumber:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwiseProductWithMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwiseProductWithNumber:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwiseQuotientFromNumber:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwiseQuotientWithMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #elementwiseQuotientWithNumber:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #fill:elementsWithStride:withSelfPlusScalar:timesVector:stride:!blas!public! !
!AbstractMatrix categoriesFor: #fill:elementsWithStride:withSelfScaledBy:plusScalar:timesMatrix:transposed:timesVector:length:stride:!blas!public! !
!AbstractMatrix categoriesFor: #fillM:byN:withScalar:timesColumnVector:stride:timesRowVector:stride:!blas!public! !
!AbstractMatrix categoriesFor: #fillM:byN:withSelfScaledBy:plusScalar:timesLeftMatrix:transposed:timesRightMatrix:transposed:length:!blas!public! !
!AbstractMatrix categoriesFor: #findMax!enumerating!public! !
!AbstractMatrix categoriesFor: #findMaxOf:!enumerating!public! !
!AbstractMatrix categoriesFor: #findMin!enumerating!public! !
!AbstractMatrix categoriesFor: #findMinOf:!enumerating!public! !
!AbstractMatrix categoriesFor: #first!accessing-elements!public! !
!AbstractMatrix categoriesFor: #fromColumn:toColumn:by:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #fromRow:toRow:by:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #fromRow:toRow:fromColumn:toColumn:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #generalizedAt:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #generalizedAt:put:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #hash!comparing!public! !
!AbstractMatrix categoriesFor: #hasSameShapeAs:!public!testing! !
!AbstractMatrix categoriesFor: #hasShape:by:!public!testing! !
!AbstractMatrix categoriesFor: #i!arithmetic-complex!public! !
!AbstractMatrix categoriesFor: #i:!arithmetic-complex!public! !
!AbstractMatrix categoriesFor: #identity!arithmetic!public! !
!AbstractMatrix categoriesFor: #imaginaryPart!arithmetic-complex!public! !
!AbstractMatrix categoriesFor: #indicesCollect:!enumerating!public! !
!AbstractMatrix categoriesFor: #indicesDo:!enumerating!public! !
!AbstractMatrix categoriesFor: #indicesInject:into:!enumerating!public! !
!AbstractMatrix categoriesFor: #inject:into:!enumerating!public! !
!AbstractMatrix categoriesFor: #isColumnMatrix!public!testing! !
!AbstractMatrix categoriesFor: #isColumnVector!public!testing! !
!AbstractMatrix categoriesFor: #isComplexMatrix!public!testing! !
!AbstractMatrix categoriesFor: #isDiagonal!public!testing! !
!AbstractMatrix categoriesFor: #isEmpty!public!testing! !
!AbstractMatrix categoriesFor: #isHermitian!public!testing! !
!AbstractMatrix categoriesFor: #isLowerTriangular!public!testing! !
!AbstractMatrix categoriesFor: #isMatrix!public!testing! !
!AbstractMatrix categoriesFor: #isRowMatrix!public!testing! !
!AbstractMatrix categoriesFor: #isRowVector!public!testing! !
!AbstractMatrix categoriesFor: #isSameSequenceAs:!public!testing! !
!AbstractMatrix categoriesFor: #isSequenceable!public!testing! !
!AbstractMatrix categoriesFor: #isSquare!public!testing! !
!AbstractMatrix categoriesFor: #isSymmetric!public!testing! !
!AbstractMatrix categoriesFor: #isTriangular!public!testing! !
!AbstractMatrix categoriesFor: #isUpperTriangular!public!testing! !
!AbstractMatrix categoriesFor: #isVector!public!testing! !
!AbstractMatrix categoriesFor: #isZero!public!testing! !
!AbstractMatrix categoriesFor: #last!accessing-elements!public! !
!AbstractMatrix categoriesFor: #lowerTriangle!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #lowerTriangle:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #max!enumerating!public! !
!AbstractMatrix categoriesFor: #maxOf:!enumerating!public! !
!AbstractMatrix categoriesFor: #maxOf:dimension:!enumerating!public! !
!AbstractMatrix categoriesFor: #min!enumerating!public! !
!AbstractMatrix categoriesFor: #minOf:!enumerating!public! !
!AbstractMatrix categoriesFor: #minOf:dimension:!enumerating!public! !
!AbstractMatrix categoriesFor: #multiplyByFloat:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #multiplyByFraction:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #multiplyByInteger:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #multiplyByScaledDecimal:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #naiveSetOffDiagonal:diagonal:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #ncol!accessing-size!public! !
!AbstractMatrix categoriesFor: #nCols!accessing-size!public! !
!AbstractMatrix categoriesFor: #negated!arithmetic!public! !
!AbstractMatrix categoriesFor: #norm1!norm!public! !
!AbstractMatrix categoriesFor: #norm2!norm!public! !
!AbstractMatrix categoriesFor: #normFrobenius!norm!public! !
!AbstractMatrix categoriesFor: #normInfinity!norm!public! !
!AbstractMatrix categoriesFor: #nrow!accessing-size!public! !
!AbstractMatrix categoriesFor: #nRows!accessing-size!public! !
!AbstractMatrix categoriesFor: #numberOfColumns!accessing-size!public! !
!AbstractMatrix categoriesFor: #numberOfRows!accessing-size!public! !
!AbstractMatrix categoriesFor: #postCopy!copying!public! !
!AbstractMatrix categoriesFor: #prependColumns:!concatenating!public! !
!AbstractMatrix categoriesFor: #prependRows:!concatenating!public! !
!AbstractMatrix categoriesFor: #printOn:!printing!public! !
!AbstractMatrix categoriesFor: #product!enumerating!public! !
!AbstractMatrix categoriesFor: #product:!enumerating!public! !
!AbstractMatrix categoriesFor: #product:dimension:!enumerating!public! !
!AbstractMatrix categoriesFor: #productColumnVectorWithRowVector:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromComplex:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromDouble:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromFixedPoint:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromFloat:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromFraction:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromInteger:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromLapackMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productFromMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productMatrixAtRightWithMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productMatrixTransposeWithColumnVector:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productMatrixWithColumnVector:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productMatrixWithMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productOf:!enumerating!public! !
!AbstractMatrix categoriesFor: #productOf:dimension:!public! !
!AbstractMatrix categoriesFor: #productRowVectorWithColumnVector:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #productRowVectorWithMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #realPart!arithmetic-complex!public! !
!AbstractMatrix categoriesFor: #replicate:!concatenating!public! !
!AbstractMatrix categoriesFor: #replicateNrow:timesNcol:!concatenating!public! !
!AbstractMatrix categoriesFor: #reshape:!converting!public! !
!AbstractMatrix categoriesFor: #reshapeNrow:ncol:!converting!public! !
!AbstractMatrix categoriesFor: #respondsToArithmetic!arithmetic!public!testing! !
!AbstractMatrix categoriesFor: #rowAt:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #rowAt:columnAt:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #rowAt:columnAt:put:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #rowAt:putAll:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #rowAt:putSequence:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #rowCount!accessing-size!public! !
!AbstractMatrix categoriesFor: #rows!enumerating!public! !
!AbstractMatrix categoriesFor: #rows:!enumerating!public! !
!AbstractMatrix categoriesFor: #rowsDo:!enumerating!public! !
!AbstractMatrix categoriesFor: #scale:elementsBy:increment:!blas!public! !
!AbstractMatrix categoriesFor: #scaledByComplex:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #scaledByNumber:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #setArray:nrow:ncol:!accessing-storage!public! !
!AbstractMatrix categoriesFor: #setDiagonal:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #setOffDiagonal:diagonal:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #setToEye!accessing-elements!public! !
!AbstractMatrix categoriesFor: #singularValues!decomposition!public! !
!AbstractMatrix categoriesFor: #size!accessing-size!public! !
!AbstractMatrix categoriesFor: #smallLowerTriangle:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #smallUpperTriangle:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #subtractFromFloat:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #subtractFromFraction:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #subtractFromInteger:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #subtractFromScaledDecimal:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sum!enumerating!public! !
!AbstractMatrix categoriesFor: #sum:!enumerating!public! !
!AbstractMatrix categoriesFor: #sum:dimension:!enumerating!public! !
!AbstractMatrix categoriesFor: #sumFromComplex:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumFromDouble:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumFromFixedPoint:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumFromFloat:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumFromFraction:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumFromInteger:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumFromLapackMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumFromMatrix:!arithmetic-internal!public! !
!AbstractMatrix categoriesFor: #sumOf:!enumerating!public! !
!AbstractMatrix categoriesFor: #sumOf:dimension:!public! !
!AbstractMatrix categoriesFor: #swap:at:with:at:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #swapColumn:withColumn:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #swapRow:withRow:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #swapRowAt:columnAt:withRowAt:columnAt:!accessing-elements!public! !
!AbstractMatrix categoriesFor: #transpose!arithmetic!public! !
!AbstractMatrix categoriesFor: #transposeConjugated!arithmetic!arithmetic-complex!public! !
!AbstractMatrix categoriesFor: #transposed!arithmetic!public! !
!AbstractMatrix categoriesFor: #transposedConjugated!arithmetic!arithmetic-complex!public! !
!AbstractMatrix categoriesFor: #upperTriangle!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #upperTriangle:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #vectorNorm1!norm!public! !
!AbstractMatrix categoriesFor: #vectorNorm2!norm!public! !
!AbstractMatrix categoriesFor: #vectorNormFrobenius!norm!public! !
!AbstractMatrix categoriesFor: #vectorNormInfinity!norm!public! !
!AbstractMatrix categoriesFor: #with:collect:!enumerating!public! !
!AbstractMatrix categoriesFor: #with:do:!enumerating!public! !
!AbstractMatrix categoriesFor: #withArrayOffsetBy:!accessing-submatrix!public! !
!AbstractMatrix categoriesFor: #withIndicesCollect:!enumerating!public! !
!AbstractMatrix categoriesFor: #withIndicesDo:!enumerating!public! !
!AbstractMatrix categoriesFor: #withIndicesInject:into:!enumerating!public! !
!AbstractMatrix categoriesFor: #zero!arithmetic!public! !

!AbstractMatrix class methodsFor!

allocateNrow: nr ncol: nc 

allocateShape: aShape 

column: aSequenceableCollection

columnMatrix

columns: anArrayOfColumns 

diagonal: aSequenceOfNumber 

dimensions: anIntegerOrPointOrArrayOfInteger 

eye: anIntegerOrPointOrArrayOfInteger 

fromSequence: aSequenceableCollection nrow: nr ncol: nc 

identity: anIntegerOrPointOrArrayOfInteger 

ncol: nc

new: anInteger

nrow: nr

nrow: nr ncol: nc 

nrow: nrow ncol: ncol tabulate: aBlock 

nrow: nr ncol: nc withAll: aNumber 

nRows: numRows nCols: numCols 

ones: anIntegerOrPointOrArrayOfInteger 

row: aSequenceableCollection

rowMatrix

rows: anArrayOfRows 

rows: rows columns: columns 

rows: rows columns: columns contents: contents 

rows: rows columns: columns element: element 

rows: rows columns: columns tabulate: aBlock 

shape: anIntegerOrPointOrArrayOfInteger 

shape: anIntegerOrPointOrArrayOfInteger do: aBlock 

zeros: anIntegerOrPointOrArrayOfInteger 
!AbstractMatrix class categoriesFor: #allocateNrow:ncol:!public! !
!AbstractMatrix class categoriesFor: #allocateShape:!public! !
!AbstractMatrix class categoriesFor: #column:!public! !
!AbstractMatrix class categoriesFor: #columnMatrix!public! !
!AbstractMatrix class categoriesFor: #columns:!public! !
!AbstractMatrix class categoriesFor: #diagonal:!public! !
!AbstractMatrix class categoriesFor: #dimensions:!public! !
!AbstractMatrix class categoriesFor: #eye:!public! !
!AbstractMatrix class categoriesFor: #fromSequence:nrow:ncol:!public! !
!AbstractMatrix class categoriesFor: #identity:!public! !
!AbstractMatrix class categoriesFor: #ncol:!public! !
!AbstractMatrix class categoriesFor: #new:!public! !
!AbstractMatrix class categoriesFor: #nrow:!public! !
!AbstractMatrix class categoriesFor: #nrow:ncol:!public! !
!AbstractMatrix class categoriesFor: #nrow:ncol:tabulate:!public! !
!AbstractMatrix class categoriesFor: #nrow:ncol:withAll:!public! !
!AbstractMatrix class categoriesFor: #nRows:nCols:!public! !
!AbstractMatrix class categoriesFor: #ones:!public! !
!AbstractMatrix class categoriesFor: #row:!public! !
!AbstractMatrix class categoriesFor: #rowMatrix!public! !
!AbstractMatrix class categoriesFor: #rows:!public! !
!AbstractMatrix class categoriesFor: #rows:columns:!public! !
!AbstractMatrix class categoriesFor: #rows:columns:contents:!public! !
!AbstractMatrix class categoriesFor: #rows:columns:element:!public! !
!AbstractMatrix class categoriesFor: #rows:columns:tabulate:!public! !
!AbstractMatrix class categoriesFor: #shape:!public! !
!AbstractMatrix class categoriesFor: #shape:do:!public! !
!AbstractMatrix class categoriesFor: #zeros:!public! !
