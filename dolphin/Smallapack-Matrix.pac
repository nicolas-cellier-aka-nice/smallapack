| package |
package := Package name: 'Smallapack-Matrix'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #AbstractMatrix;
	add: #LapackCDIMatrix;
	add: #LapackCGEMatrix;
	add: #LapackCHEMatrix;
	add: #LapackCTRMatrix;
	add: #LapackDDIMatrix;
	add: #LapackDGEMatrix;
	add: #LapackDiagonalMatrix;
	add: #LapackDSYMatrix;
	add: #LapackDTRMatrix;
	add: #LapackGeneralMatrix;
	add: #LapackHalfMatrix;
	add: #LapackMatrix;
	add: #LapackSDIMatrix;
	add: #LapackSGEMatrix;
	add: #LapackSSYMatrix;
	add: #LapackSTRMatrix;
	add: #LapackUnpackedHermitianMatrix;
	add: #LapackUnpackedMatrix;
	add: #LapackUnpackedSymmetricRealMatrix;
	add: #LapackUnpackedTriangularMatrix;
	add: #LapackZDIMatrix;
	add: #LapackZGEMatrix;
	add: #LapackZHEMatrix;
	add: #LapackZTRMatrix;
	add: #Settings;
	yourself.

package methodNames
	add: #Complex -> #asDoubleComplex;
	add: #Complex -> #asFloatComplex;
	add: #Complex -> #asLapackMatrix;
	add: #Complex -> #coercing:do:;
	add: #Complex -> #concatColumnsFromLapackMatrix:;
	add: #Complex -> #concatRowsFromLapackMatrix:;
	add: #Complex -> #differenceFromLapackMatrix:;
	add: #Complex -> #productFromLapackMatrix:;
	add: #Complex -> #quotientFromLapackMatrix:;
	add: #Complex -> #reduce;
	add: #Complex -> #respondsToArithmetic;
	add: #Complex -> #sumFromLapackMatrix:;
	add: #DOUBLEArray -> #asLapackMatrixNrow:ncol:;
	add: #DOUBLECOMPLEXArray -> #asLapackMatrixNrow:ncol:;
	add: #ExternalArray -> #isSequenceable;
	add: #Float -> #asDoubleComplex;
	add: #Float -> #asFloatComplex;
	add: #Float -> #coercing:do:;
	add: #FLOATArray -> #asLapackMatrixNrow:ncol:;
	add: #FLOATCOMPLEXArray -> #asLapackMatrixNrow:ncol:;
	add: #Number -> #asDoubleComplex;
	add: #Number -> #asFloatComplex;
	add: #Number -> #asLapackMatrix;
	add: #Number -> #coercing:do:;
	add: #Number -> #concatColumnsFromLapackMatrix:;
	add: #Number -> #concatRowsFromLapackMatrix:;
	add: #Number -> #differenceFromLapackMatrix:;
	add: #Number -> #productFromLapackMatrix:;
	add: #Number -> #quotientFromLapackMatrix:;
	add: #Number -> #respondsToArithmetic;
	add: #Number -> #sumFromLapackMatrix:;
	add: #Object -> #isSequenceable;
	add: #Object -> #respondsToArithmetic;
	add: #SequenceableCollection -> #isSequenceable;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Dolphin Smalltalk 5.1\Burning River\Complex\Complex';
	add: '..\..\WINDOWS\Profiles\nicolas\Mes Documents\Dolphin Smalltalk X6\Object Arts\Dolphin\Base\Dolphin';
	add: 'Smallapack-Algorithm';
	add: 'Smallapack-External';
	yourself).

package!

"Class Definitions"!

Object subclass: #AbstractMatrix
	instanceVariableNames: 'array nrow ncol'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Settings
	instanceVariableNames: ''
	classVariableNames: 'Registry'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractMatrix subclass: #LapackMatrix
	instanceVariableNames: ''
	classVariableNames: 'ArrayInterfaces BandStorageMask BlasInterfaces BothUpperLowerMask CArrayClasses ComplexityMask ComplexMask DefaultSeedArray DiagonalMask DoublePrecisionMask FlagsToClassDictionary FullStorageMask GeneralMask HermitianMask LapackInterfaces LowerMask PackedStorageMask PrecisionMask PropertyMask RealMask SDCZMask SinglePrecisionMask SmalltalkArrayClasses StorageMask TriangularMask UpperMask'
	poolDictionaries: ''
	classInstanceVariableNames: 'libraryErrorCollection smalltalkAccessError cAccessError flags'!
LapackMatrix subclass: #LapackDiagonalMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackMatrix subclass: #LapackGeneralMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackMatrix subclass: #LapackHalfMatrix
	instanceVariableNames: 'uplo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDiagonalMatrix subclass: #LapackCDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDiagonalMatrix subclass: #LapackDDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDiagonalMatrix subclass: #LapackSDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackDiagonalMatrix subclass: #LapackZDIMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralMatrix subclass: #LapackCGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralMatrix subclass: #LapackDGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralMatrix subclass: #LapackSGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackGeneralMatrix subclass: #LapackZGEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackHalfMatrix subclass: #LapackUnpackedMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedMatrix subclass: #LapackUnpackedHermitianMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedMatrix subclass: #LapackUnpackedTriangularMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedHermitianMatrix subclass: #LapackCHEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedHermitianMatrix subclass: #LapackUnpackedSymmetricRealMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedHermitianMatrix subclass: #LapackZHEMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedSymmetricRealMatrix subclass: #LapackDSYMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedSymmetricRealMatrix subclass: #LapackSSYMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedTriangularMatrix subclass: #LapackCTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedTriangularMatrix subclass: #LapackDTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedTriangularMatrix subclass: #LapackSTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackUnpackedTriangularMatrix subclass: #LapackZTRMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Complex methodsFor!

asDoubleComplex
	^real asFloat i: imag asFloat!

asFloatComplex
	^real asFloat i: imag asFloat!

asLapackMatrix
	^LapackZGEMatrix 
		fromSequence: (DOUBLECOMPLEXArray with: self)
		nrow: 1
		ncol: 1!

coercing: aMatrix do: aBlock 
	imag isZero ifTrue: [^real coercing: aMatrix do: aBlock].
	^aMatrix coercingFromComplexNumber: self asDoubleComplex do: aBlock!

concatColumnsFromLapackMatrix: aMatrix 

concatRowsFromLapackMatrix: aMatrix 

differenceFromLapackMatrix: aMatrix 

productFromLapackMatrix: aMatrix 

quotientFromLapackMatrix: aMatrix 

reduce
	imag isZero ifTrue: [^real reduce]!

respondsToArithmetic

sumFromLapackMatrix: aMatrix 
!Complex categoriesFor: #asDoubleComplex!converting!public! !
!Complex categoriesFor: #asFloatComplex!converting!public! !
!Complex categoriesFor: #asLapackMatrix!public! !
!Complex categoriesFor: #coercing:do:!converting!public! !
!Complex categoriesFor: #concatColumnsFromLapackMatrix:!public! !
!Complex categoriesFor: #concatRowsFromLapackMatrix:!public! !
!Complex categoriesFor: #differenceFromLapackMatrix:!public! !
!Complex categoriesFor: #productFromLapackMatrix:!public! !
!Complex categoriesFor: #quotientFromLapackMatrix:!public! !
!Complex categoriesFor: #reduce!converting!public! !
!Complex categoriesFor: #respondsToArithmetic!public! !
!Complex categoriesFor: #sumFromLapackMatrix:!public! !

!DOUBLEArray methodsFor!

asLapackMatrixNrow: nr ncol: nc 
	^LapackDGEMatrix 
		fromSequence: self copy
		nrow: nr
		ncol: nc! !
!DOUBLEArray categoriesFor: #asLapackMatrixNrow:ncol:!converting!public! !

!DOUBLECOMPLEXArray methodsFor!

asLapackMatrixNrow: nr ncol: nc 
	^LapackZGEMatrix 
		fromSequence: self copy
		nrow: nr
		ncol: nc! !
!DOUBLECOMPLEXArray categoriesFor: #asLapackMatrixNrow:ncol:!converting!public! !

!ExternalArray methodsFor!

isSequenceable
	"Compatibility with ST-80 VW and Squeak"

	^true! !
!ExternalArray categoriesFor: #isSequenceable!public!testing! !

!Float methodsFor!

asDoubleComplex

asFloatComplex

coercing: aMatrix do: aBlock 
!Float categoriesFor: #asDoubleComplex!public! !
!Float categoriesFor: #asFloatComplex!public! !
!Float categoriesFor: #coercing:do:!public! !

!FLOATArray methodsFor!

asLapackMatrixNrow: nr ncol: nc 
	^LapackSGEMatrix 
		fromSequence: self copy
		nrow: nr
		ncol: nc! !
!FLOATArray categoriesFor: #asLapackMatrixNrow:ncol:!converting!public! !

!FLOATCOMPLEXArray methodsFor!

asLapackMatrixNrow: nr ncol: nc 
	^LapackCGEMatrix 
		fromSequence: self copy
		nrow: nr
		ncol: nc! !
!FLOATCOMPLEXArray categoriesFor: #asLapackMatrixNrow:ncol:!public! !

!Number methodsFor!

asDoubleComplex

asFloatComplex

asLapackMatrix 
	^LapackDGEMatrix 
		fromSequence: (DOUBLEArray with: self asFloat)
		nrow: 1
		ncol: 1!

coercing: aMatrix do: aBlock 

concatColumnsFromLapackMatrix: aMatrix 

concatRowsFromLapackMatrix: aMatrix 

differenceFromLapackMatrix: aMatrix 

productFromLapackMatrix: aMatrix 

quotientFromLapackMatrix: aMatrix 

respondsToArithmetic

sumFromLapackMatrix: aMatrix 
!Number categoriesFor: #asDoubleComplex!public! !
!Number categoriesFor: #asFloatComplex!public! !
!Number categoriesFor: #asLapackMatrix!converting!public! !
!Number categoriesFor: #coercing:do:!public! !
!Number categoriesFor: #concatColumnsFromLapackMatrix:!public! !
!Number categoriesFor: #concatRowsFromLapackMatrix:!public! !
!Number categoriesFor: #differenceFromLapackMatrix:!public! !
!Number categoriesFor: #productFromLapackMatrix:!public! !
!Number categoriesFor: #quotientFromLapackMatrix:!public! !
!Number categoriesFor: #respondsToArithmetic!public! !
!Number categoriesFor: #sumFromLapackMatrix:!public! !

!Object methodsFor!

isSequenceable

respondsToArithmetic
!Object categoriesFor: #isSequenceable!public! !
!Object categoriesFor: #respondsToArithmetic!public! !

!SequenceableCollection methodsFor!

isSequenceable
	"Compatibility with ST-80 VW and Squeak"

	^true! !
!SequenceableCollection categoriesFor: #isSequenceable!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

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

asDoubleComplexMatrix

asDoubleMatrix

asDoublePrecisionComplexMatrix

asDoublePrecisionMatrix

asFloatComplexMatrix

asFloatMatrix

asMatrix

asOrderedCollection

asRowMatrix

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

fill: m elementsWithStride: incy withSelfScaledBy: beta plusScalar: alpha timesMatrix: a transposed: trans timesVector: x length: n stride: incx 

fillM: m byN: n withScalar: alpha timesColumnVector: x stride: incx timesRowVector: y stride: incy 

fillM: m byN: n withSelfScaledBy: beta plusScalar: alpha timesLeftMatrix: a transposed: transa timesRightMatrix: b transposed: transb length: k 

findMax

findMaxOf: aBlock 

findMin

findMinOf: aBlock 

first

fromColumn: jStart toColumn: jStop by: jStep

fromRow: iStart toRow: iStop by: iStep

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

productRowVectorWithColumnVector: aVector 

productRowVectorWithMatrix: aMatrix 

realPart

replicateNrow: nr timesNcol: nc 

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

scaledByComplex: aComplex 

scaledByNumber: aNumber 

setArray: anArray nrow: nr ncol: nc 

setDiagonal: aMatrix

setOffDiagonal: alpha diagonal: beta 

setToEye

size

subtractFromFloat: aFloat 

subtractFromFraction: aFraction 

subtractFromInteger: anInteger

subtractFromScaledDecimal: aScaledDecimal

sum

sum: aBlock 

sum: aBlock dimension: aDimension 

sumFromComplex: aComplex 

sumFromDouble: aDouble 

sumFromFixedPoint: aFixedPoint 

sumFromFloat: aFloat 

sumFromFraction: aFraction 

sumFromInteger: anInteger 

sumFromLapackMatrix: aLapackMatrix

sumFromMatrix: aMatrix 

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
!AbstractMatrix categoriesFor: #-!public! !
!AbstractMatrix categoriesFor: #*!public! !
!AbstractMatrix categoriesFor: #,!public! !
!AbstractMatrix categoriesFor: #,,!public! !
!AbstractMatrix categoriesFor: #+!public! !
!AbstractMatrix categoriesFor: #=!public! !
!AbstractMatrix categoriesFor: #abs!public! !
!AbstractMatrix categoriesFor: #absMax!public! !
!AbstractMatrix categoriesFor: #adaptToComplex:andSend:!public! !
!AbstractMatrix categoriesFor: #adaptToFloat:andCompare:!public! !
!AbstractMatrix categoriesFor: #adaptToFloat:andSend:!public! !
!AbstractMatrix categoriesFor: #adaptToFraction:andSend:!public! !
!AbstractMatrix categoriesFor: #adaptToInteger:andCompare:!public! !
!AbstractMatrix categoriesFor: #adaptToInteger:andSend:!public! !
!AbstractMatrix categoriesFor: #adaptToNumber:andSend:!public! !
!AbstractMatrix categoriesFor: #adaptToScaledDecimal:andSend:!public! !
!AbstractMatrix categoriesFor: #addToFloat:!public! !
!AbstractMatrix categoriesFor: #addToFraction:!public! !
!AbstractMatrix categoriesFor: #addToInteger:!public! !
!AbstractMatrix categoriesFor: #addToScaledDecimal:!public! !
!AbstractMatrix categoriesFor: #allSatisfy:!public! !
!AbstractMatrix categoriesFor: #anySatisfy:!public! !
!AbstractMatrix categoriesFor: #appendColumns:!public! !
!AbstractMatrix categoriesFor: #appendRows:!public! !
!AbstractMatrix categoriesFor: #arrayAt:!public! !
!AbstractMatrix categoriesFor: #arrayAt:put:!public! !
!AbstractMatrix categoriesFor: #arrayOffsetAtRow:atColumn:!public! !
!AbstractMatrix categoriesFor: #arraySize!public! !
!AbstractMatrix categoriesFor: #asAbstractMatrix!public! !
!AbstractMatrix categoriesFor: #asArray!public! !
!AbstractMatrix categoriesFor: #asBag!public! !
!AbstractMatrix categoriesFor: #asColumnMatrix!public! !
!AbstractMatrix categoriesFor: #asDoubleComplexMatrix!public! !
!AbstractMatrix categoriesFor: #asDoubleMatrix!public! !
!AbstractMatrix categoriesFor: #asDoublePrecisionComplexMatrix!public! !
!AbstractMatrix categoriesFor: #asDoublePrecisionMatrix!public! !
!AbstractMatrix categoriesFor: #asFloatComplexMatrix!public! !
!AbstractMatrix categoriesFor: #asFloatMatrix!public! !
!AbstractMatrix categoriesFor: #asMatrix!public! !
!AbstractMatrix categoriesFor: #asOrderedCollection!public! !
!AbstractMatrix categoriesFor: #asRowMatrix!public! !
!AbstractMatrix categoriesFor: #asSet!public! !
!AbstractMatrix categoriesFor: #asSinglePrecisionComplexMatrix!public! !
!AbstractMatrix categoriesFor: #asSinglePrecisionMatrix!public! !
!AbstractMatrix categoriesFor: #asSortedCollection!public! !
!AbstractMatrix categoriesFor: #asSortedCollection:!public! !
!AbstractMatrix categoriesFor: #at:!public! !
!AbstractMatrix categoriesFor: #at:at:!public! !
!AbstractMatrix categoriesFor: #at:at:ifInvalid:!public! !
!AbstractMatrix categoriesFor: #at:at:incrementBy:!public! !
!AbstractMatrix categoriesFor: #at:at:put:!public! !
!AbstractMatrix categoriesFor: #at:put:!public! !
!AbstractMatrix categoriesFor: #atAllPut:!public! !
!AbstractMatrix categoriesFor: #atColumn:!public! !
!AbstractMatrix categoriesFor: #atColumn:put:!public! !
!AbstractMatrix categoriesFor: #atColumn:putAll:!public! !
!AbstractMatrix categoriesFor: #atColumn:putSequence:!public! !
!AbstractMatrix categoriesFor: #atColumns:!public! !
!AbstractMatrix categoriesFor: #atInteger:!public! !
!AbstractMatrix categoriesFor: #atInteger:andInteger:!public! !
!AbstractMatrix categoriesFor: #atInteger:andInteger:put:!public! !
!AbstractMatrix categoriesFor: #atInteger:put:!public! !
!AbstractMatrix categoriesFor: #atIntervalFrom:to:by:!public! !
!AbstractMatrix categoriesFor: #atPoint:!public! !
!AbstractMatrix categoriesFor: #atPoint:put:!public! !
!AbstractMatrix categoriesFor: #atRow:!public! !
!AbstractMatrix categoriesFor: #atRow:column:!public! !
!AbstractMatrix categoriesFor: #atRow:column:put:!public! !
!AbstractMatrix categoriesFor: #atRow:put:!public! !
!AbstractMatrix categoriesFor: #atRow:putAll:!public! !
!AbstractMatrix categoriesFor: #atRow:putSequence:!public! !
!AbstractMatrix categoriesFor: #atRows:!public! !
!AbstractMatrix categoriesFor: #capacity!public! !
!AbstractMatrix categoriesFor: #collect:!public! !
!AbstractMatrix categoriesFor: #columnAt:!public! !
!AbstractMatrix categoriesFor: #columnAt:putAll:!public! !
!AbstractMatrix categoriesFor: #columnAt:putSequence:!public! !
!AbstractMatrix categoriesFor: #columnCount!public! !
!AbstractMatrix categoriesFor: #columns!public! !
!AbstractMatrix categoriesFor: #columns:!public! !
!AbstractMatrix categoriesFor: #columnsDo:!public! !
!AbstractMatrix categoriesFor: #concatColumnsFromLapackMatrix:!public! !
!AbstractMatrix categoriesFor: #concatColumnsFromMatrix:!public! !
!AbstractMatrix categoriesFor: #concatColumnsWithMatrix:!public! !
!AbstractMatrix categoriesFor: #concatRowsFromLapackMatrix:!public! !
!AbstractMatrix categoriesFor: #concatRowsFromMatrix:!public! !
!AbstractMatrix categoriesFor: #concatRowsWithMatrix:!public! !
!AbstractMatrix categoriesFor: #conjugated!public! !
!AbstractMatrix categoriesFor: #copy:elementsFrom:sourceIncrement:destIncrement:!public! !
!AbstractMatrix categoriesFor: #copy:elementsFrom:sourceOffset:sourceIncrement:destOffset:destIncrement:!public! !
!AbstractMatrix categoriesFor: #copy:rowsStartingAt:and:columnsStartingAt:from:!public! !
!AbstractMatrix categoriesFor: #count:!public! !
!AbstractMatrix categoriesFor: #cumulativeProduct:dimension:!public! !
!AbstractMatrix categoriesFor: #cumulativeSum:dimension:!public! !
!AbstractMatrix categoriesFor: #diagonal!public! !
!AbstractMatrix categoriesFor: #diagonalAt:!public! !
!AbstractMatrix categoriesFor: #diagonalSizeAt:!public! !
!AbstractMatrix categoriesFor: #differenceFromComplex:!public! !
!AbstractMatrix categoriesFor: #differenceFromDouble:!public! !
!AbstractMatrix categoriesFor: #differenceFromFixedPoint:!public! !
!AbstractMatrix categoriesFor: #differenceFromFloat:!public! !
!AbstractMatrix categoriesFor: #differenceFromFraction:!public! !
!AbstractMatrix categoriesFor: #differenceFromInteger:!public! !
!AbstractMatrix categoriesFor: #differenceFromLapackMatrix:!public! !
!AbstractMatrix categoriesFor: #differenceFromMatrix:!public! !
!AbstractMatrix categoriesFor: #dimensions!public! !
!AbstractMatrix categoriesFor: #do:!public! !
!AbstractMatrix categoriesFor: #dotProduct:elementsIncrement:with:increment:!public! !
!AbstractMatrix categoriesFor: #elementwisePowerFromNumber:!public! !
!AbstractMatrix categoriesFor: #elementwisePowerWithMatrix:!public! !
!AbstractMatrix categoriesFor: #elementwisePowerWithNumber:!public! !
!AbstractMatrix categoriesFor: #elementwiseProductFromNumber:!public! !
!AbstractMatrix categoriesFor: #elementwiseProductWithMatrix:!public! !
!AbstractMatrix categoriesFor: #elementwiseProductWithNumber:!public! !
!AbstractMatrix categoriesFor: #elementwiseQuotientFromNumber:!public! !
!AbstractMatrix categoriesFor: #elementwiseQuotientWithMatrix:!public! !
!AbstractMatrix categoriesFor: #elementwiseQuotientWithNumber:!public! !
!AbstractMatrix categoriesFor: #fill:elementsWithStride:withSelfPlusScalar:timesVector:stride:!public! !
!AbstractMatrix categoriesFor: #fill:elementsWithStride:withSelfScaledBy:plusScalar:timesMatrix:transposed:timesVector:length:stride:!public! !
!AbstractMatrix categoriesFor: #fillM:byN:withScalar:timesColumnVector:stride:timesRowVector:stride:!public! !
!AbstractMatrix categoriesFor: #fillM:byN:withSelfScaledBy:plusScalar:timesLeftMatrix:transposed:timesRightMatrix:transposed:length:!public! !
!AbstractMatrix categoriesFor: #findMax!public! !
!AbstractMatrix categoriesFor: #findMaxOf:!public! !
!AbstractMatrix categoriesFor: #findMin!public! !
!AbstractMatrix categoriesFor: #findMinOf:!public! !
!AbstractMatrix categoriesFor: #first!public! !
!AbstractMatrix categoriesFor: #fromColumn:toColumn:by:!public! !
!AbstractMatrix categoriesFor: #fromRow:toRow:by:!public! !
!AbstractMatrix categoriesFor: #generalizedAt:!public! !
!AbstractMatrix categoriesFor: #generalizedAt:put:!public! !
!AbstractMatrix categoriesFor: #hash!public! !
!AbstractMatrix categoriesFor: #hasSameShapeAs:!public! !
!AbstractMatrix categoriesFor: #hasShape:by:!public! !
!AbstractMatrix categoriesFor: #i!public! !
!AbstractMatrix categoriesFor: #i:!public! !
!AbstractMatrix categoriesFor: #identity!public! !
!AbstractMatrix categoriesFor: #imaginaryPart!public! !
!AbstractMatrix categoriesFor: #indicesCollect:!public! !
!AbstractMatrix categoriesFor: #indicesDo:!public! !
!AbstractMatrix categoriesFor: #indicesInject:into:!public! !
!AbstractMatrix categoriesFor: #inject:into:!public! !
!AbstractMatrix categoriesFor: #isColumnMatrix!public! !
!AbstractMatrix categoriesFor: #isColumnVector!public! !
!AbstractMatrix categoriesFor: #isComplexMatrix!public! !
!AbstractMatrix categoriesFor: #isDiagonal!public! !
!AbstractMatrix categoriesFor: #isEmpty!public! !
!AbstractMatrix categoriesFor: #isHermitian!public! !
!AbstractMatrix categoriesFor: #isLowerTriangular!public! !
!AbstractMatrix categoriesFor: #isMatrix!public! !
!AbstractMatrix categoriesFor: #isRowMatrix!public! !
!AbstractMatrix categoriesFor: #isRowVector!public! !
!AbstractMatrix categoriesFor: #isSameSequenceAs:!public! !
!AbstractMatrix categoriesFor: #isSequenceable!public! !
!AbstractMatrix categoriesFor: #isSquare!public! !
!AbstractMatrix categoriesFor: #isSymmetric!public! !
!AbstractMatrix categoriesFor: #isTriangular!public! !
!AbstractMatrix categoriesFor: #isUpperTriangular!public! !
!AbstractMatrix categoriesFor: #isVector!public! !
!AbstractMatrix categoriesFor: #isZero!public! !
!AbstractMatrix categoriesFor: #last!public! !
!AbstractMatrix categoriesFor: #lowerTriangle!public! !
!AbstractMatrix categoriesFor: #lowerTriangle:!public! !
!AbstractMatrix categoriesFor: #max!public! !
!AbstractMatrix categoriesFor: #maxOf:!public! !
!AbstractMatrix categoriesFor: #maxOf:dimension:!public! !
!AbstractMatrix categoriesFor: #min!public! !
!AbstractMatrix categoriesFor: #minOf:!public! !
!AbstractMatrix categoriesFor: #minOf:dimension:!public! !
!AbstractMatrix categoriesFor: #multiplyByFloat:!public! !
!AbstractMatrix categoriesFor: #multiplyByFraction:!public! !
!AbstractMatrix categoriesFor: #multiplyByInteger:!public! !
!AbstractMatrix categoriesFor: #multiplyByScaledDecimal:!public! !
!AbstractMatrix categoriesFor: #naiveSetOffDiagonal:diagonal:!public! !
!AbstractMatrix categoriesFor: #ncol!public! !
!AbstractMatrix categoriesFor: #nCols!public! !
!AbstractMatrix categoriesFor: #negated!public! !
!AbstractMatrix categoriesFor: #norm1!public! !
!AbstractMatrix categoriesFor: #norm2!public! !
!AbstractMatrix categoriesFor: #normFrobenius!public! !
!AbstractMatrix categoriesFor: #normInfinity!public! !
!AbstractMatrix categoriesFor: #nrow!public! !
!AbstractMatrix categoriesFor: #nRows!public! !
!AbstractMatrix categoriesFor: #numberOfColumns!public! !
!AbstractMatrix categoriesFor: #numberOfRows!public! !
!AbstractMatrix categoriesFor: #postCopy!public! !
!AbstractMatrix categoriesFor: #prependColumns:!public! !
!AbstractMatrix categoriesFor: #prependRows:!public! !
!AbstractMatrix categoriesFor: #printOn:!public! !
!AbstractMatrix categoriesFor: #product!public! !
!AbstractMatrix categoriesFor: #product:!public! !
!AbstractMatrix categoriesFor: #product:dimension:!public! !
!AbstractMatrix categoriesFor: #productColumnVectorWithRowVector:!public! !
!AbstractMatrix categoriesFor: #productFromComplex:!public! !
!AbstractMatrix categoriesFor: #productFromDouble:!public! !
!AbstractMatrix categoriesFor: #productFromFixedPoint:!public! !
!AbstractMatrix categoriesFor: #productFromFloat:!public! !
!AbstractMatrix categoriesFor: #productFromFraction:!public! !
!AbstractMatrix categoriesFor: #productFromInteger:!public! !
!AbstractMatrix categoriesFor: #productFromLapackMatrix:!public! !
!AbstractMatrix categoriesFor: #productFromMatrix:!public! !
!AbstractMatrix categoriesFor: #productMatrixAtRightWithMatrix:!public! !
!AbstractMatrix categoriesFor: #productMatrixTransposeWithColumnVector:!public! !
!AbstractMatrix categoriesFor: #productMatrixWithColumnVector:!public! !
!AbstractMatrix categoriesFor: #productMatrixWithMatrix:!public! !
!AbstractMatrix categoriesFor: #productRowVectorWithColumnVector:!public! !
!AbstractMatrix categoriesFor: #productRowVectorWithMatrix:!public! !
!AbstractMatrix categoriesFor: #realPart!public! !
!AbstractMatrix categoriesFor: #replicateNrow:timesNcol:!public! !
!AbstractMatrix categoriesFor: #respondsToArithmetic!public! !
!AbstractMatrix categoriesFor: #rowAt:!public! !
!AbstractMatrix categoriesFor: #rowAt:columnAt:!public! !
!AbstractMatrix categoriesFor: #rowAt:columnAt:put:!public! !
!AbstractMatrix categoriesFor: #rowAt:putAll:!public! !
!AbstractMatrix categoriesFor: #rowAt:putSequence:!public! !
!AbstractMatrix categoriesFor: #rowCount!public! !
!AbstractMatrix categoriesFor: #rows!public! !
!AbstractMatrix categoriesFor: #rows:!public! !
!AbstractMatrix categoriesFor: #rowsDo:!public! !
!AbstractMatrix categoriesFor: #scale:elementsBy:increment:!public! !
!AbstractMatrix categoriesFor: #scaledByComplex:!public! !
!AbstractMatrix categoriesFor: #scaledByNumber:!public! !
!AbstractMatrix categoriesFor: #setArray:nrow:ncol:!public! !
!AbstractMatrix categoriesFor: #setDiagonal:!public! !
!AbstractMatrix categoriesFor: #setOffDiagonal:diagonal:!public! !
!AbstractMatrix categoriesFor: #setToEye!public! !
!AbstractMatrix categoriesFor: #size!public! !
!AbstractMatrix categoriesFor: #subtractFromFloat:!public! !
!AbstractMatrix categoriesFor: #subtractFromFraction:!public! !
!AbstractMatrix categoriesFor: #subtractFromInteger:!public! !
!AbstractMatrix categoriesFor: #subtractFromScaledDecimal:!public! !
!AbstractMatrix categoriesFor: #sum!public! !
!AbstractMatrix categoriesFor: #sum:!public! !
!AbstractMatrix categoriesFor: #sum:dimension:!public! !
!AbstractMatrix categoriesFor: #sumFromComplex:!public! !
!AbstractMatrix categoriesFor: #sumFromDouble:!public! !
!AbstractMatrix categoriesFor: #sumFromFixedPoint:!public! !
!AbstractMatrix categoriesFor: #sumFromFloat:!public! !
!AbstractMatrix categoriesFor: #sumFromFraction:!public! !
!AbstractMatrix categoriesFor: #sumFromInteger:!public! !
!AbstractMatrix categoriesFor: #sumFromLapackMatrix:!public! !
!AbstractMatrix categoriesFor: #sumFromMatrix:!public! !
!AbstractMatrix categoriesFor: #swap:at:with:at:!public! !
!AbstractMatrix categoriesFor: #swapColumn:withColumn:!public! !
!AbstractMatrix categoriesFor: #swapRow:withRow:!public! !
!AbstractMatrix categoriesFor: #swapRowAt:columnAt:withRowAt:columnAt:!public! !
!AbstractMatrix categoriesFor: #transpose!public! !
!AbstractMatrix categoriesFor: #transposeConjugated!public! !
!AbstractMatrix categoriesFor: #transposed!public! !
!AbstractMatrix categoriesFor: #transposedConjugated!public! !
!AbstractMatrix categoriesFor: #upperTriangle!public! !
!AbstractMatrix categoriesFor: #upperTriangle:!public! !
!AbstractMatrix categoriesFor: #vectorNorm1!public! !
!AbstractMatrix categoriesFor: #vectorNorm2!public! !
!AbstractMatrix categoriesFor: #vectorNormFrobenius!public! !
!AbstractMatrix categoriesFor: #vectorNormInfinity!public! !
!AbstractMatrix categoriesFor: #with:collect:!public! !
!AbstractMatrix categoriesFor: #with:do:!public! !
!AbstractMatrix categoriesFor: #withArrayOffsetBy:!public! !
!AbstractMatrix categoriesFor: #withIndicesCollect:!public! !
!AbstractMatrix categoriesFor: #withIndicesDo:!public! !
!AbstractMatrix categoriesFor: #withIndicesInject:into:!public! !
!AbstractMatrix categoriesFor: #zero!public! !

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

Settings guid: (GUID fromString: '{F2C649C5-1104-4A6D-935A-5FD8E9E8891F}')!
Settings comment: ''!
!Settings categoriesForClass!Smallapack-Matrix! !
!Settings class methodsFor!

blasLibraryEnabled

blasLibraryEnabled: aBoolean 

initialize

initializeRegistry

registry

resetRegistry

useAtlasCBlas

useAtlasCBlas: aBoolean 
!Settings class categoriesFor: #blasLibraryEnabled!public! !
!Settings class categoriesFor: #blasLibraryEnabled:!public! !
!Settings class categoriesFor: #initialize!public! !
!Settings class categoriesFor: #initializeRegistry!public! !
!Settings class categoriesFor: #registry!public! !
!Settings class categoriesFor: #resetRegistry!public! !
!Settings class categoriesFor: #useAtlasCBlas!public! !
!Settings class categoriesFor: #useAtlasCBlas:!public! !

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
					cArray asParameter 
						copyAt: 0
						from: theArray
						size: theArray basicSize
						startingAt: 1]
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
	(offx >= 0 and: [n + offx <= aLapackMatrix size]) 
		ifFalse: [^self error: 'matrix access out of bounds'].
	(offy >= 0 and: [n + offy <= self size]) 
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
				exc retryWith: 
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
!LapackMatrix categoriesFor: #-!public! !
!LapackMatrix categoriesFor: #*!public! !
!LapackMatrix categoriesFor: #,!public! !
!LapackMatrix categoriesFor: #,,!public! !
!LapackMatrix categoriesFor: #/!public! !
!LapackMatrix categoriesFor: #\!public! !
!LapackMatrix categoriesFor: #+!public! !
!LapackMatrix categoriesFor: #absMax!public! !
!LapackMatrix categoriesFor: #arrayPointer!public! !
!LapackMatrix categoriesFor: #arrayPointerWithOffset:!public! !
!LapackMatrix categoriesFor: #asAbstractMatrix!public! !
!LapackMatrix categoriesFor: #asColumnMatrix!public! !
!LapackMatrix categoriesFor: #asComplexMatrix!public! !
!LapackMatrix categoriesFor: #asDoublePrecisionComplexMatrix!public! !
!LapackMatrix categoriesFor: #asDoublePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #asGeneralMatrix!public! !
!LapackMatrix categoriesFor: #asPackedMatrix!public! !
!LapackMatrix categoriesFor: #asParameter!public! !
!LapackMatrix categoriesFor: #asParameterWithOffset:!public! !
!LapackMatrix categoriesFor: #asRowMatrix!public! !
!LapackMatrix categoriesFor: #asSinglePrecisionComplexMatrix!public! !
!LapackMatrix categoriesFor: #asSinglePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #asUnpackedMatrix!public! !
!LapackMatrix categoriesFor: #at:put:!public! !
!LapackMatrix categoriesFor: #at:put:handle:!public! !
!LapackMatrix categoriesFor: #atAllPut:!public! !
!LapackMatrix categoriesFor: #atIntervalFrom:to:by:!public! !
!LapackMatrix categoriesFor: #blasInterface!public! !
!LapackMatrix categoriesFor: #cArray!public! !
!LapackMatrix categoriesFor: #castTo:!public! !
!LapackMatrix categoriesFor: #castToColumn!public! !
!LapackMatrix categoriesFor: #castToRealWithArrayOffsetBy:!public! !
!LapackMatrix categoriesFor: #castToRow!public! !
!LapackMatrix categoriesFor: #coerceFlags:!public! !
!LapackMatrix categoriesFor: #coerceFlagsButProperty:!public! !
!LapackMatrix categoriesFor: #coerceToComplexMatrix!public! !
!LapackMatrix categoriesFor: #coerceToDoublePrecisionComplexMatrix!public! !
!LapackMatrix categoriesFor: #coerceToDoublePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #coerceToGeneralMatrix!public! !
!LapackMatrix categoriesFor: #coerceToSinglePrecisionComplexMatrix!public! !
!LapackMatrix categoriesFor: #coerceToSinglePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #coercing:do:!public! !
!LapackMatrix categoriesFor: #coercingButPropertyFromLapackMatrix:do:!public! !
!LapackMatrix categoriesFor: #coercingFromComplexNumber:do:!public! !
!LapackMatrix categoriesFor: #coercingFromLapackMatrix:do:!public! !
!LapackMatrix categoriesFor: #coercingFromNumber:do:!public! !
!LapackMatrix categoriesFor: #coercingFromRealNumber:do:!public! !
!LapackMatrix categoriesFor: #concatColumnsFromLapackMatrix:!public! !
!LapackMatrix categoriesFor: #concatColumnsWithLapackMatrix:!public! !
!LapackMatrix categoriesFor: #concatRowsFromLapackMatrix:!public! !
!LapackMatrix categoriesFor: #concatRowsWithLapackMatrix:!public! !
!LapackMatrix categoriesFor: #conjugated!public! !
!LapackMatrix categoriesFor: #copy!public! !
!LapackMatrix categoriesFor: #copy:elementsFrom:sourceOffset:sourceIncrement:destOffset:destIncrement:!public! !
!LapackMatrix categoriesFor: #copy:rowsStartingAt:and:columnsStartingAt:from:!public! !
!LapackMatrix categoriesFor: #defaultTolerance!public! !
!LapackMatrix categoriesFor: #determinant!public! !
!LapackMatrix categoriesFor: #differenceFromLapackMatrix:!public! !
!LapackMatrix categoriesFor: #differenceWithLapackMatrix:!public! !
!LapackMatrix categoriesFor: #dotProduct:elementsIncrement:with:increment:!public! !
!LapackMatrix categoriesFor: #eigenValueDecomposition!public! !
!LapackMatrix categoriesFor: #eigenValues!public! !
!LapackMatrix categoriesFor: #fill:elementsWithStride:withSelfPlusScalar:timesVector:stride:!public! !
!LapackMatrix categoriesFor: #fillM:byN:withScalar:timesColumnVector:stride:timesRowVector:stride:!public! !
!LapackMatrix categoriesFor: #fillRandNormal!public! !
!LapackMatrix categoriesFor: #fillRandNormalWithSeed:!public! !
!LapackMatrix categoriesFor: #fillRandUniform!public! !
!LapackMatrix categoriesFor: #fillRandUniformWithSeed:!public! !
!LapackMatrix categoriesFor: #fromColumn:toColumn:by:!public! !
!LapackMatrix categoriesFor: #hessenbergDecomposition!public! !
!LapackMatrix categoriesFor: #imaginaryPart!public! !
!LapackMatrix categoriesFor: #inPlaceScaledByComplex:!public! !
!LapackMatrix categoriesFor: #inPlaceScaledByNumber:!public! !
!LapackMatrix categoriesFor: #inPlaceSolve:!public! !
!LapackMatrix categoriesFor: #isBandMatrix!public! !
!LapackMatrix categoriesFor: #isComplexMatrix!public! !
!LapackMatrix categoriesFor: #isDiagonalMatrix!public! !
!LapackMatrix categoriesFor: #isDoublePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #isGeneralMatrix!public! !
!LapackMatrix categoriesFor: #isHermitianMatrix!public! !
!LapackMatrix categoriesFor: #isInCSpace!public! !
!LapackMatrix categoriesFor: #isPackedMatrix!public! !
!LapackMatrix categoriesFor: #isRealMatrix!public! !
!LapackMatrix categoriesFor: #isSinglePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #isSymmetricMatrix!public! !
!LapackMatrix categoriesFor: #isTriangularMatrix!public! !
!LapackMatrix categoriesFor: #isUnpackedMatrix!public! !
!LapackMatrix categoriesFor: #lapackInterface!public! !
!LapackMatrix categoriesFor: #leftDivideFromLapackMatrix:!public! !
!LapackMatrix categoriesFor: #leftDivideWithLapackMatrix:!public! !
!LapackMatrix categoriesFor: #multiplyByFloat:!public! !
!LapackMatrix categoriesFor: #multiplyByFraction:!public! !
!LapackMatrix categoriesFor: #multiplyByInteger:!public! !
!LapackMatrix categoriesFor: #multiplyByScaledDecimal:!public! !
!LapackMatrix categoriesFor: #naiveCoerceToDoublePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #naiveCoerceToSinglePrecisionMatrix!public! !
!LapackMatrix categoriesFor: #negated!public! !
!LapackMatrix categoriesFor: #norm1!public! !
!LapackMatrix categoriesFor: #normFrobenius!public! !
!LapackMatrix categoriesFor: #normInfinity!public! !
!LapackMatrix categoriesFor: #pluDecomposition!public! !
!LapackMatrix categoriesFor: #postCoercingFromLapackMatrix:do:!public! !
!LapackMatrix categoriesFor: #postCopy!public! !
!LapackMatrix categoriesFor: #productColumnVectorWithRowVector:!public! !
!LapackMatrix categoriesFor: #productFromComplex:!public! !
!LapackMatrix categoriesFor: #productFromDouble:!public! !
!LapackMatrix categoriesFor: #productFromFixedPoint:!public! !
!LapackMatrix categoriesFor: #productFromFloat:!public! !
!LapackMatrix categoriesFor: #productFromFraction:!public! !
!LapackMatrix categoriesFor: #productFromInteger:!public! !
!LapackMatrix categoriesFor: #productFromLapackMatrix:!public! !
!LapackMatrix categoriesFor: #productRowVectorWithColumnVector:!public! !
!LapackMatrix categoriesFor: #productRowVectorWithMatrix:!public! !
!LapackMatrix categoriesFor: #productWithLapackMatrix:!public! !
!LapackMatrix categoriesFor: #pseudoInverse!public! !
!LapackMatrix categoriesFor: #pseudoInverseTolerance:!public! !
!LapackMatrix categoriesFor: #qrDecomposition!public! !
!LapackMatrix categoriesFor: #qrpDecomposition!public! !
!LapackMatrix categoriesFor: #quotientFromLapackMatrix:!public! !
!LapackMatrix categoriesFor: #rank!public! !
!LapackMatrix categoriesFor: #rankTolerance:!public! !
!LapackMatrix categoriesFor: #realPart!public! !
!LapackMatrix categoriesFor: #reciprocal!public! !
!LapackMatrix categoriesFor: #reduceGeneralityIfPossible!public! !
!LapackMatrix categoriesFor: #rightDivideWithLapackMatrix:!public! !
!LapackMatrix categoriesFor: #scaledByComplex:!public! !
!LapackMatrix categoriesFor: #scaledByNumber:!public! !
!LapackMatrix categoriesFor: #schurDecomposition!public! !
!LapackMatrix categoriesFor: #setOffDiagonal:diagonal:!public! !
!LapackMatrix categoriesFor: #singularValueDecomposition!public! !
!LapackMatrix categoriesFor: #singularValues!public! !
!LapackMatrix categoriesFor: #solve:!public! !
!LapackMatrix categoriesFor: #storeInSmalltalkSpace!public! !
!LapackMatrix categoriesFor: #sumFromLapackMatrix:!public! !
!LapackMatrix categoriesFor: #sumWithLapackMatrix:!public! !
!LapackMatrix categoriesFor: #swapColumn:withColumn:!public! !
!LapackMatrix categoriesFor: #swapRow:withRow:!public! !
!LapackMatrix categoriesFor: #transposeConjugated!public! !
!LapackMatrix categoriesFor: #withArrayOffsetBy:!public! !

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

obsolete

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
!LapackMatrix class categoriesFor: #obsolete!public! !
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
!LapackMatrix class categoriesFor: #unpackedMatrix!public! !
!LapackMatrix class categoriesFor: #unregisterFlags!public! !

LapackDiagonalMatrix guid: (GUID fromString: '{420E585E-75C6-4311-A97D-92E7046C5230}')!
LapackDiagonalMatrix comment: 'LapackDiagonalMatrix is square and has zeroes off diagonal by definition.

It stores only diagonal elements in the array (economic).
'!
!LapackDiagonalMatrix categoriesForClass!Unclassified! !
!LapackDiagonalMatrix methodsFor!

at: anInteger 

at: anInteger put: aNumber 

columnAt: columnIndex 

diagonalAt: index 

differenceWithLapackMatrix: aLapackMatrix 

eigenValueDecomposition
	^LapackDiagonalEigenDecomposition decompose: self!

eigenValues

elementwiseProductWithMatrix: aMatrix 

inPlaceScaledByComplex: aComplex 

inPlaceScaledByNumber: aNumber 

isDiagonal

isHermitian

isLowerTriangular

isSymmetric

isUpperTriangular

lowerTriangle: ind 

pluDecomposition
	^LapackDiagonalPLUdecomposition decompose: self!

productMatrixTransposeWithColumnVector: aLapackMatrix 

productMatrixWithColumnVector: aLapackMatrix 

reciprocal

rowAt: rowIndex 

rowAt: i columnAt: j 

rowAt: i columnAt: j put: aNumber 

sumWithLapackMatrix: aLapackMatrix 

upperTriangle: ind 
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
!LapackDiagonalMatrix categoriesFor: #upperTriangle:!public! !

!LapackDiagonalMatrix class methodsFor!

allocateNrow: nr ncol: nc 
!LapackDiagonalMatrix class categoriesFor: #allocateNrow:ncol:!public! !

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

coerceToGeneralMatrix

diagonalAt: index 

eigenValueDecomposition
	^self isRealMatrix 
		ifTrue: [LapackRealEigenDecomposition decompose: self]
		ifFalse: [LapackComplexEigenDecomposition decompose: self]!

eigenValues

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

i

lowerTriangle: ind 
	"return lower triangle matrix"

	^
	[ind <= 0 
		ifTrue: 
			[| b |
			b := self class triangularMatrix nrow: nrow ncol: ncol.
			b beLower.
			self lapackInterface 
				lacpyWithuplo: self lapackInterface lower
				m: nrow + ind
				n: ncol + ind
				a: (self asParameterWithOffset: ind negated)
				lda: nrow
				b: (b asParameterWithOffset: ind negated)
				ldb: b nrow.
			b]
		ifFalse: 
			[| b offset |
			offset := (ind + 1) * nrow.
			b := self copy.
			self lapackInterface 
				lacpyWithuplo: self lapackInterface upper
				m: nrow - ind - 1
				n: ncol - ind - 1
				a: (self zero asParameterWithOffset: offset)
				lda: nrow
				b: (b asParameterWithOffset: offset)
				ldb: b nrow.
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

reciprocal

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
				m: nrow - ind
				n: ncol - ind
				a: (self asParameterWithOffset: offset)
				lda: nrow
				b: (b asParameterWithOffset: offset)
				ldb: b nrow.
			b]
		ifFalse: 
			[| b |
			b := self copy.
			self lapackInterface 
				lacpyWithuplo: self lapackInterface lower
				m: nrow + ind - 1
				n: ncol + ind - 1
				a: (self zero asParameterWithOffset: 1 - ind)
				lda: nrow
				b: (b asParameterWithOffset: 1 - ind)
				ldb: b nrow.
			b]] 
			on: Error
			do: [:exc | exc return: (super upperTriangle: ind)]! !
!LapackGeneralMatrix categoriesFor: #absMax!public! !
!LapackGeneralMatrix categoriesFor: #coerceToGeneralMatrix!public! !
!LapackGeneralMatrix categoriesFor: #diagonalAt:!public! !
!LapackGeneralMatrix categoriesFor: #eigenValueDecomposition!public! !
!LapackGeneralMatrix categoriesFor: #eigenValues!public! !
!LapackGeneralMatrix categoriesFor: #fill:elementsWithStride:withSelfScaledBy:plusScalar:timesMatrix:transposed:timesVector:length:stride:!public! !
!LapackGeneralMatrix categoriesFor: #fillM:byN:withSelfScaledBy:plusScalar:timesLeftMatrix:transposed:timesRightMatrix:transposed:length:!public! !
!LapackGeneralMatrix categoriesFor: #i!public! !
!LapackGeneralMatrix categoriesFor: #lowerTriangle:!public! !
!LapackGeneralMatrix categoriesFor: #norm1!public! !
!LapackGeneralMatrix categoriesFor: #normFrobenius!public! !
!LapackGeneralMatrix categoriesFor: #normInfinity!public! !
!LapackGeneralMatrix categoriesFor: #pluDecomposition!public! !
!LapackGeneralMatrix categoriesFor: #productMatrixTransposeWithColumnVector:!public! !
!LapackGeneralMatrix categoriesFor: #productMatrixWithColumnVector:!public! !
!LapackGeneralMatrix categoriesFor: #productMatrixWithMatrix:!public! !
!LapackGeneralMatrix categoriesFor: #reciprocal!public! !
!LapackGeneralMatrix categoriesFor: #swapColumn:withColumn:!public! !
!LapackGeneralMatrix categoriesFor: #swapRow:withRow:!public! !
!LapackGeneralMatrix categoriesFor: #upperTriangle:!public! !

LapackHalfMatrix guid: (GUID fromString: '{7BCA3B72-375B-4750-B6DD-E3D7FD401C4E}')!
LapackHalfMatrix comment: 'LapackHalfMatrix has common protocol for those matrices storing only half a triangle.

Subclasses must implement the following messages:
	private
		complementOfRow:column:
		complementOfRow:column:put:

Instance Variables:
	uplo	<SmallInteger>	code indicating if upper or lower triangular matrix is filled
'!
!LapackHalfMatrix categoriesForClass!Smallapack-Matrix! !
!LapackHalfMatrix methodsFor!

asPackedMatrix

asUnpackedMatrix

at: anInteger 

at: anInteger put: aNumber 

beLower

beUpper

coerceToDoublePrecisionComplexMatrix

coerceToDoublePrecisionMatrix

coerceToSinglePrecisionComplexMatrix

coerceToSinglePrecisionMatrix

complementOfRow: i column: j 

complementOfRow: i column: j put: aNumber 

differenceFromMatrix: aMatrix

isLower

isUpper

productFromMatrix: aMatrix

rowAt: i columnAt: j 

rowAt: i columnAt: j put: aNumber 

sumFromMatrix: aMatrix
!LapackHalfMatrix categoriesFor: #asPackedMatrix!public! !
!LapackHalfMatrix categoriesFor: #asUnpackedMatrix!public! !
!LapackHalfMatrix categoriesFor: #at:!public! !
!LapackHalfMatrix categoriesFor: #at:put:!public! !
!LapackHalfMatrix categoriesFor: #beLower!public! !
!LapackHalfMatrix categoriesFor: #beUpper!public! !
!LapackHalfMatrix categoriesFor: #coerceToDoublePrecisionComplexMatrix!public! !
!LapackHalfMatrix categoriesFor: #coerceToDoublePrecisionMatrix!public! !
!LapackHalfMatrix categoriesFor: #coerceToSinglePrecisionComplexMatrix!public! !
!LapackHalfMatrix categoriesFor: #coerceToSinglePrecisionMatrix!public! !
!LapackHalfMatrix categoriesFor: #complementOfRow:column:!public! !
!LapackHalfMatrix categoriesFor: #complementOfRow:column:put:!public! !
!LapackHalfMatrix categoriesFor: #differenceFromMatrix:!public! !
!LapackHalfMatrix categoriesFor: #isLower!public! !
!LapackHalfMatrix categoriesFor: #isUpper!public! !
!LapackHalfMatrix categoriesFor: #productFromMatrix:!public! !
!LapackHalfMatrix categoriesFor: #rowAt:columnAt:!public! !
!LapackHalfMatrix categoriesFor: #rowAt:columnAt:put:!public! !
!LapackHalfMatrix categoriesFor: #sumFromMatrix:!public! !

!LapackHalfMatrix class methodsFor!

columns: anArrayOfColumns 

eye: aShape 

fromGeneralMatrix: aMatrix 

fromSequence: aRawArray nrow: nr ncol: nc 

packedMatrix

rows: anArrayOfRows 

unpackedMatrix
!LapackHalfMatrix class categoriesFor: #columns:!public! !
!LapackHalfMatrix class categoriesFor: #eye:!public! !
!LapackHalfMatrix class categoriesFor: #fromGeneralMatrix:!public! !
!LapackHalfMatrix class categoriesFor: #fromSequence:nrow:ncol:!public! !
!LapackHalfMatrix class categoriesFor: #packedMatrix!public! !
!LapackHalfMatrix class categoriesFor: #rows:!public! !
!LapackHalfMatrix class categoriesFor: #unpackedMatrix!public! !

LapackCDIMatrix guid: (GUID fromString: '{A84888F8-F150-41A9-BEBA-D1CC1D6E696B}')!
LapackCDIMatrix comment: 'LapackCDIMatrix handles Diagonal Matrices composed of single precision complex numbers.
See superclass for more information.'!
!LapackCDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCDIMatrix class methodsFor!

initialize

initializeFlags
!LapackCDIMatrix class categoriesFor: #initialize!public! !
!LapackCDIMatrix class categoriesFor: #initializeFlags!public! !

LapackDDIMatrix guid: (GUID fromString: '{FA75D423-D2D3-4338-A1A8-5E62C7649084}')!
LapackDDIMatrix comment: 'LapackDDIMatrix handles Diagonal Matrices composed of double precision real numbers.
See superclass for more information.'!
!LapackDDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDDIMatrix class methodsFor!

initialize

initializeFlags
!LapackDDIMatrix class categoriesFor: #initialize!public! !
!LapackDDIMatrix class categoriesFor: #initializeFlags!public! !

LapackSDIMatrix guid: (GUID fromString: '{81497E87-7EBE-4054-8AC2-63E5685FD155}')!
LapackSDIMatrix comment: 'LapackSDIMatrix handles Diagonal Matrices composed of single precision real numbers.
See superclass for more information.'!
!LapackSDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSDIMatrix class methodsFor!

initialize

initializeFlags
!LapackSDIMatrix class categoriesFor: #initialize!public! !
!LapackSDIMatrix class categoriesFor: #initializeFlags!public! !

LapackZDIMatrix guid: (GUID fromString: '{AE16E502-6681-417B-9E99-8137C0704F6A}')!
LapackZDIMatrix comment: 'LapackZDIMatrix handles Diagonal Matrices composed of double precision complex numbers.
See superclass for more information.'!
!LapackZDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZDIMatrix class methodsFor!

initialize

initializeFlags
!LapackZDIMatrix class categoriesFor: #initialize!public! !
!LapackZDIMatrix class categoriesFor: #initializeFlags!public! !

LapackCGEMatrix guid: (GUID fromString: '{53D6F2C7-A470-4EF1-ADC3-4C64133F46D2}')!
LapackCGEMatrix comment: 'LapackCGEMatrix handles General Matrices (dense storage) composed of single precision complex numbers
See superclass for more information.'!
!LapackCGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCGEMatrix methodsFor!

productFromFloat: aFloat 

scale: n elementsBy: alpha increment: incx 
	
	[| comp |
	comp := alpha asFloatComplex.
	comp imaginaryPart isZero 
		ifTrue: 
			[self blasInterface 
				realScalWithN: n
				alpha: comp real
				X: self asParameter
				incX: incx]
		ifFalse: 
			[self blasInterface 
				scalWithN: n
				alpha: comp
				X: self asParameter
				incX: incx]] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							scale: n
							elementsBy: alpha
							increment: incx)]! !
!LapackCGEMatrix categoriesFor: #productFromFloat:!public! !
!LapackCGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackCGEMatrix class methodsFor!

initialize

initializeFlags
!LapackCGEMatrix class categoriesFor: #initialize!public! !
!LapackCGEMatrix class categoriesFor: #initializeFlags!public! !

LapackDGEMatrix guid: (GUID fromString: '{0BF9D151-CE10-4D9C-93E6-FFF9A3D86795}')!
LapackDGEMatrix comment: 'LapackDGEMatrix handles General Matrices (dense storage) composed of double precision real numbers
See superclass for more information.'!
!LapackDGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDGEMatrix methodsFor!

productFromDouble: aDouble 

scale: n elementsBy: alpha increment: incx 
	
	[self blasInterface 
		scalWithN: n
		alpha: alpha asFloat
		X: self asParameter
		incX: incx] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							scale: n
							elementsBy: alpha
							increment: incx)]! !
!LapackDGEMatrix categoriesFor: #productFromDouble:!public! !
!LapackDGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackDGEMatrix class methodsFor!

initialize

initializeFlags
!LapackDGEMatrix class categoriesFor: #initialize!public! !
!LapackDGEMatrix class categoriesFor: #initializeFlags!public! !

LapackSGEMatrix guid: (GUID fromString: '{BB5A2547-1EFA-4A0A-BEED-0B84B2C9A407}')!
LapackSGEMatrix comment: 'LapackSGEMatrix handles General Matrices (dense storage) composed of single precision real numbers
See superclass for more information.'!
!LapackSGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSGEMatrix methodsFor!

productFromFloat: aFloat 

scale: n elementsBy: alpha increment: incx 
	
	[self blasInterface 
		scalWithN: n
		alpha: alpha asFloat
		X: self asParameter
		incX: incx] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							scale: n
							elementsBy: alpha
							increment: incx)]! !
!LapackSGEMatrix categoriesFor: #productFromFloat:!public! !
!LapackSGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackSGEMatrix class methodsFor!

initialize

initializeFlags
!LapackSGEMatrix class categoriesFor: #initialize!public! !
!LapackSGEMatrix class categoriesFor: #initializeFlags!public! !

LapackZGEMatrix guid: (GUID fromString: '{A3FAD776-8863-4B39-8502-30726A321D32}')!
LapackZGEMatrix comment: 'LapackZGEMatrix handles General Matrices (dense storage) composed of double precision complex numbers
See superclass for more information.'!
!LapackZGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZGEMatrix methodsFor!

productFromDouble: aDouble 

scale: n elementsBy: alpha increment: incx 
	
	[| comp |
	comp := alpha asDoubleComplex.
	comp imaginaryPart isZero 
		ifTrue: 
			[self blasInterface 
				realScalWithN: n
				alpha: comp real
				X: self asParameter
				incX: incx]
		ifFalse: 
			[self blasInterface 
				scalWithN: n
				alpha: comp
				X: self asParameter
				incX: incx]] 
			on: Error
			do: 
				[:exc | 
				exc return: 
						(super 
							scale: n
							elementsBy: alpha
							increment: incx)]! !
!LapackZGEMatrix categoriesFor: #productFromDouble:!public! !
!LapackZGEMatrix categoriesFor: #scale:elementsBy:increment:!public! !

!LapackZGEMatrix class methodsFor!

initialize

initializeFlags
!LapackZGEMatrix class categoriesFor: #initialize!public! !
!LapackZGEMatrix class categoriesFor: #initializeFlags!public! !

LapackUnpackedMatrix guid: (GUID fromString: '{054D5EF7-DC05-4C05-BD03-1B4A76AD76E6}')!
LapackUnpackedMatrix comment: 'LapackUnpackedMatrix implement common behaviour for unpacked storage.
Full dense storage is allocated, but only half storage is used (either upper triangle or lower triangle, including diagonal).

In some circonstances, the other triangle might also contain valid data.

Subclasses must implement the following messages:
	private
		complementOfRow:column:


'!
!LapackUnpackedMatrix categoriesForClass!Smallapack-Matrix! !
!LapackUnpackedMatrix methodsFor!

asPackedMatrix

asUnpackedMatrix

coerceToPackedMatrix
	"Create a packed form with a copy of self.
	proceed column by column"

	| blas packed |
	blas := self blasInterface.
	packed := self class packedMatrix allocateShape: nrow.
	self isUpper 
		ifTrue: 
			[packed beUpper.
			
			[1 to: ncol
				do: 
					[:jc | 
					blas 
						copyWithN: jc
						X: (self asParameterWithOffset: (self arrayOffsetAtRow: 1 atColumn: jc))
						incX: 1
						Y: (packed asParameterWithOffset: (packed arrayOffsetAtRow: 1 atColumn: jc))
						incY: 1]] 
					on: Error
					do: 
						[:exc | 
						1 to: ncol do: [:jc | 
								1 to: jc do: [:ir | 
										packed 
											rowAt: ir
											columnAt: jc
											put: (self rowAt: ir columnAt: jc)]]]]
		ifFalse: 
			[packed beLower.
			
			[1 to: ncol
				do: 
					[:jc | 
					blas 
						copyWithN: ncol - (jc - 1)
						X: (self asParameterWithOffset: (self arrayOffsetAtRow: jc atColumn: jc))
						incX: 1
						Y: (packed asParameterWithOffset: (packed arrayOffsetAtRow: jc atColumn: jc))
						incY: 1]] 
					on: Error
					do: 
						[:exc | 
						1 to: ncol do: [:jc | 
								jc to: nrow do: [:ir | 
										packed 
											rowAt: ir
											columnAt: jc
											put: (self rowAt: ir columnAt: jc)]]]].
	^packed!

coerceToUnpackedMatrix

complementOfRow: ir column: jc put: aNumber 

fillOtherTriangle

rowAt: i columnAt: j 

rowAt: i columnAt: j put: aNumber 
!LapackUnpackedMatrix categoriesFor: #asPackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #asUnpackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #coerceToPackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #coerceToUnpackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #complementOfRow:column:put:!public! !
!LapackUnpackedMatrix categoriesFor: #fillOtherTriangle!public! !
!LapackUnpackedMatrix categoriesFor: #rowAt:columnAt:!public! !
!LapackUnpackedMatrix categoriesFor: #rowAt:columnAt:put:!public! !

!LapackUnpackedMatrix class methodsFor!

allocateNrow: nr ncol: nc 
!LapackUnpackedMatrix class categoriesFor: #allocateNrow:ncol:!public! !

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

sumWithLapackMatrix: aLapackMatrix 

transposeConjugated

transposed

upperTriangle: ind 
!LapackUnpackedHermitianMatrix categoriesFor: #absMax!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #beBothUpperLower!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #coerceToGeneralMatrix!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #coercingFromComplexNumber:do:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #complementOfRow:column:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #diagonalAt:!public! !
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
!LapackUnpackedHermitianMatrix categoriesFor: #lowerTriangle:!public! !
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
!LapackUnpackedHermitianMatrix categoriesFor: #sumWithLapackMatrix:!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #transposeConjugated!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #transposed!public! !
!LapackUnpackedHermitianMatrix categoriesFor: #upperTriangle:!public! !

!LapackUnpackedHermitianMatrix class methodsFor!

fromGeneralMatrix: aMatrix 

nrow: nr ncol: nc withAll: aNumber 
!LapackUnpackedHermitianMatrix class categoriesFor: #fromGeneralMatrix:!public! !
!LapackUnpackedHermitianMatrix class categoriesFor: #nrow:ncol:withAll:!public! !

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

complementOfRow: i column: j 

diagonalAt: index 

differenceWithLapackMatrix: aLapackMatrix 

eigenValues

fillOtherTriangle

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

isLowerTriangular

isTriangular

isUpperTriangular

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
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super reciprocal]!

sumWithLapackMatrix: aLapackMatrix 

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
!LapackUnpackedTriangularMatrix categoriesFor: #upperTriangle:!public! !

!LapackUnpackedTriangularMatrix class methodsFor!

fromGeneralMatrix: aMatrix 

nrow: nr ncol: nc withAll: aNumber 
!LapackUnpackedTriangularMatrix class categoriesFor: #fromGeneralMatrix:!public! !
!LapackUnpackedTriangularMatrix class categoriesFor: #nrow:ncol:withAll:!public! !

LapackCHEMatrix guid: (GUID fromString: '{9369B851-89B8-48E0-B64E-E442856FCFEF}')!
LapackCHEMatrix comment: 'LapackCHEMatrix handles Hermitian matrices composed of single precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackCHEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCHEMatrix class methodsFor!

initialize

initializeFlags
!LapackCHEMatrix class categoriesFor: #initialize!public! !
!LapackCHEMatrix class categoriesFor: #initializeFlags!public! !

LapackUnpackedSymmetricRealMatrix guid: (GUID fromString: '{C7EF5392-5700-412C-80FB-1A21D7A8C9E8}')!
LapackUnpackedSymmetricRealMatrix comment: 'LapackUnpackedSymmetricRealMatrix is an abstract class for matrices having symmetric property:
	(M rowAt: i columnAt: j) = (M rowAt: j columnAt: i)
Symmetric Complex Matrices are not considered since they have few interesting properties.

As stated in super class, only half triangle and diagonal need being stored.
Variable uplo does indicate which one, upper, lower or both.

Subclasses are specialized for containing either real numbers with a specific floating point format.

'!
!LapackUnpackedSymmetricRealMatrix categoriesForClass!Smallapack-Matrix! !
!LapackUnpackedSymmetricRealMatrix methodsFor!

complementOfRow: i column: j 

diagonalAt: index 

isSymmetric

transposed
!LapackUnpackedSymmetricRealMatrix categoriesFor: #complementOfRow:column:!public! !
!LapackUnpackedSymmetricRealMatrix categoriesFor: #diagonalAt:!public! !
!LapackUnpackedSymmetricRealMatrix categoriesFor: #isSymmetric!public! !
!LapackUnpackedSymmetricRealMatrix categoriesFor: #transposed!public! !

!LapackUnpackedSymmetricRealMatrix class methodsFor!

fromGeneralMatrix: aMatrix 
!LapackUnpackedSymmetricRealMatrix class categoriesFor: #fromGeneralMatrix:!public! !

LapackZHEMatrix guid: (GUID fromString: '{C685C67E-F4E6-49BD-85A7-25CA99E41526}')!
LapackZHEMatrix comment: 'LapackZHEMatrix handles Hermitian matrices composed of double precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackZHEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZHEMatrix class methodsFor!

initialize

initializeFlags
!LapackZHEMatrix class categoriesFor: #initialize!public! !
!LapackZHEMatrix class categoriesFor: #initializeFlags!public! !

LapackDSYMatrix guid: (GUID fromString: '{D1E7D673-A960-4FE4-80E0-4D00D6718580}')!
LapackDSYMatrix comment: 'LapackDSYMatrix handles Symmetric matrices composed of double precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackDSYMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDSYMatrix class methodsFor!

initialize

initializeFlags
!LapackDSYMatrix class categoriesFor: #initialize!public! !
!LapackDSYMatrix class categoriesFor: #initializeFlags!public! !

LapackSSYMatrix guid: (GUID fromString: '{FB2A8C38-E9ED-4CA6-A61B-7BDB05454AD5}')!
LapackSSYMatrix comment: 'LapackSSYMatrix handles Symmetric matrices composed of single precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackSSYMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSSYMatrix class methodsFor!

initialize

initializeFlags
!LapackSSYMatrix class categoriesFor: #initialize!public! !
!LapackSSYMatrix class categoriesFor: #initializeFlags!public! !

LapackCTRMatrix guid: (GUID fromString: '{48F592C7-F8FB-432D-86F5-512E1F736B24}')!
LapackCTRMatrix comment: 'LapackCTRMatrix handles Triangular matrices composed of single precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackCTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCTRMatrix class methodsFor!

initialize

initializeFlags
!LapackCTRMatrix class categoriesFor: #initialize!public! !
!LapackCTRMatrix class categoriesFor: #initializeFlags!public! !

LapackDTRMatrix guid: (GUID fromString: '{6D56DE32-4578-4BD5-A569-79D5ED7CDA38}')!
LapackDTRMatrix comment: 'LapackDTRMatrix handles Triangular matrices composed of double precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackDTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDTRMatrix class methodsFor!

initialize

initializeFlags
!LapackDTRMatrix class categoriesFor: #initialize!public! !
!LapackDTRMatrix class categoriesFor: #initializeFlags!public! !

LapackSTRMatrix guid: (GUID fromString: '{DDF8C48E-1CDA-4A0B-A114-94006C8ADDF2}')!
LapackSTRMatrix comment: 'LapackSTRMatrix handles Triangular matrices composed of single precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackSTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSTRMatrix class methodsFor!

initialize

initializeFlags
!LapackSTRMatrix class categoriesFor: #initialize!public! !
!LapackSTRMatrix class categoriesFor: #initializeFlags!public! !

LapackZTRMatrix guid: (GUID fromString: '{00C44E12-18C2-498F-A82B-E9656E2C28F6}')!
LapackZTRMatrix comment: 'LapackZTRMatrix handles Triangular matrices composed of double precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackZTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZTRMatrix class methodsFor!

initialize

initializeFlags
!LapackZTRMatrix class categoriesFor: #initialize!public! !
!LapackZTRMatrix class categoriesFor: #initializeFlags!public! !

"Binary Globals"!
