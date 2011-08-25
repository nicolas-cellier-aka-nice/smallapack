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

concatColumnsFromLapackMatrix: aMatrix 	^self coercing: aMatrix		do: 			[:num :mat | 			mat concatColumnsWithLapackMatrix: (mat class 						nrow: 1						ncol: 1						withAll: self)]!

concatRowsFromLapackMatrix: aMatrix 	^self coercing: aMatrix		do: 			[:num :mat | 			mat concatRowsWithLapackMatrix: (mat class 						nrow: 1						ncol: 1						withAll: self)]!

differenceFromLapackMatrix: aMatrix 	"No such function available in blas/lapack ?	Create and fill a Matrix then fallback to matrix operation	this is not fully optimized"	^self coercing: aMatrix		do: 			[:num :mat | 			mat - (mat class 						nrow: mat nrow						ncol: mat ncol						withAll: num)]!

productFromLapackMatrix: aMatrix 	^self coercing: aMatrix do: [:num :mat | mat scaledByComplex: num]!

quotientFromLapackMatrix: aMatrix 	^self coercing: aMatrix		do: 			[:num :mat | mat scaledByComplex: num reciprocal]!

reduce
	imag isZero ifTrue: [^real reduce]!

respondsToArithmetic	"this is a VW compatibility message used by Smallapack"		^true!

sumFromLapackMatrix: aMatrix 	"No such function available in blas/lapack ?	Create and fill a Matrix then fallback to matrix operation	this is not fully optimized"	^self coercing: aMatrix		do: 			[:num :mat | 			mat + (mat class 						nrow: mat nrow						ncol: mat ncol						withAll: num)]! !
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

asDoubleComplex	^self i: 0.0!

asFloatComplex	^self i: 0.0!

coercing: aMatrix do: aBlock 	^aMatrix coercingFromRealNumber: self do: aBlock! !
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

asDoubleComplex	^self asFloat asDoubleComplex!

asFloatComplex	^self asFloat asFloatComplex!

asLapackMatrix 
	^LapackDGEMatrix 
		fromSequence: (DOUBLEArray with: self asFloat)
		nrow: 1
		ncol: 1!

coercing: aMatrix do: aBlock 	^aMatrix coercingFromNumber: self do: aBlock!

concatColumnsFromLapackMatrix: aMatrix 	^self coercing: aMatrix		do: 			[:num :mat | 			mat concatColumnsWithLapackMatrix: (mat class 						nrow: 1						ncol: 1						withAll: self)]!

concatRowsFromLapackMatrix: aMatrix 	^self coercing: aMatrix		do: 			[:num :mat | 			mat concatRowsWithLapackMatrix: (mat class 						nrow: 1						ncol: 1						withAll: self)]!

differenceFromLapackMatrix: aMatrix 	"No such function available in blas/lapack ?	Create and fill a Matrix then fallback to matrix operation	this is not fully optimized"	^self coercing: aMatrix		do: 			[:num :mat | 			mat - (mat class 						nrow: mat nrow						ncol: mat ncol						withAll: num)]!

productFromLapackMatrix: aMatrix 	^self coercing: aMatrix		do: 			[:num :mat | mat scaledByNumber: num]!

quotientFromLapackMatrix: aMatrix 	^self coercing: aMatrix		do: 			[:num :mat | mat scaledByNumber: num reciprocal]!

respondsToArithmetic	"this is a VW compatibility message used by Smallapack"		^true!

sumFromLapackMatrix: aMatrix 	"No such function available in blas/lapack ?	Create and fill a Matrix then fallback to matrix operation	this is not fully optimized"	^self coercing: aMatrix		do: 			[:num :mat | 			mat + (mat class 						nrow: mat nrow						ncol: mat ncol						withAll: num)]! !
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

isSequenceable	"answer true if i am a sequenceable collection"		^false!

respondsToArithmetic	"this is a VW compatibility message used by Smallapack"		^false! !
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

- aMatrix 	^aMatrix differenceFromMatrix: self!

* aMatrix 	^aMatrix productFromMatrix: self!

, aMatrix 	^aMatrix concatColumnsFromMatrix: self!

,, aMatrix 	^aMatrix concatRowsFromMatrix: self!

+ aMatrix 	^aMatrix sumFromMatrix: self!

= aMatrix 	(aMatrix respondsToArithmetic 		and: [aMatrix isMatrix and: [nrow = aMatrix nrow and: [ncol = aMatrix ncol]]]) 			ifFalse: [^false].	1 to: self size		do: [:i | (self at: i) = (aMatrix at: i) ifFalse: [^false]].	^true!

abs	^self collect: [:e | e abs]!

absMax	"Note: for matrices, this does not have norm properties"	^self maxOf: [:e | e abs]!

adaptToComplex: rcvr andSend: selector 	"If I am involved in arithmetic with a Complex."	selector = #* ifTrue: [^self asComplexMatrix scaledByComplex: rcvr].	^self asComplexMatrix collect:  [:each | rcvr perform: selector with: each]!

adaptToFloat: rcvr andCompare: selector 	"If I am involved in arithmetic with a Fraction."	^self adaptToNumber: rcvr andSend: selector !

adaptToFloat: rcvr andSend: selector 	"If I am involved in arithmetic with a Float."	^self adaptToNumber: rcvr andSend: selector !

adaptToFraction: rcvr andSend: selector 	"If I am involved in arithmetic with a Fraction."	^self adaptToNumber: rcvr andSend: selector !

adaptToInteger: rcvr andCompare: selector 	"If I am involved in arithmetic with a Fraction."	^self adaptToNumber: rcvr andSend: selector !

adaptToInteger: rcvr andSend: selector 	"If I am involved in arithmetic with an Integer."	^self adaptToNumber: rcvr andSend: selector !

adaptToNumber: rcvr andSend: selector 	"If I am involved in arithmetic with a Fraction."	selector = #* ifTrue: [^self scaledByNumber: rcvr].	^self collect:  [:each | rcvr perform: selector with: each]!

adaptToScaledDecimal: rcvr andSend: selector 	"If I am involved in arithmetic with a ScaledDecimal."	^self adaptToNumber: rcvr andSend: selector !

addToFloat: aFloat 	^self collect: [:each | aFloat + each]!

addToFraction: aFraction 	^self collect: [:each | aFraction + each]!

addToInteger: anInteger 	^self collect: [:each | anInteger + each]!

addToScaledDecimal: aScaledDecimal	^self collect: [:each | aScaledDecimal + each]!

allSatisfy: aBlock 	1 to: self size		do: [:i | (aBlock value: (self at: i)) ifFalse: [^false]].	^true!

anySatisfy: aBlock 	1 to: self size		do: [:i | (aBlock value: (self at: i)) ifTrue: [^true]].	^false!

appendColumns: nColumns 	| res |	res := self class nrow: nrow ncol: ncol + nColumns.	res 		copy: nrow		rowsStartingAt: 1		and: ncol		columnsStartingAt: 1		from: self.	^res!

appendRows: nRows 	| res |	res := self class nrow: nrow + nRows ncol: ncol.	res 		copy: nrow		rowsStartingAt: 1		and: ncol		columnsStartingAt: 1		from: self.	^res!

arrayAt: anInteger	^array at: anInteger!

arrayAt: anInteger put: aNumber	^array at: anInteger put: aNumber!

arrayOffsetAtRow: rowIndex atColumn: columnIndex 	"Answer an offset (0-based)"	^(columnIndex - 1) * nrow + rowIndex - 1!

arraySize	^array size!

asAbstractMatrix	^self!

asArray	| result |	result := array asArray.	^result == array ifTrue: [result copy] ifFalse: [result]!

asBag	| aBag |	aBag := Bag new.	self do: [:each | aBag add: each].	^aBag!

asColumnMatrix	^ncol = 1 		ifTrue: [self]		ifFalse: 			[| res |			res := self class nrow: self size.			1 to: self size do: [:i | res at: i put: (self at: i)]]!

asDoubleComplexMatrix	^self asDoublePrecisionComplexMatrix!

asDoubleMatrix	^self asDoublePrecisionMatrix!

asDoublePrecisionComplexMatrix	| res |	res := LapackZGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asDoubleComplex].	^res!

asDoublePrecisionMatrix	| res |	res := LapackDGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asFloat].	^res!

asFloatComplexMatrix	^self asSinglePrecisionComplexMatrix!

asFloatMatrix	^self asSinglePrecisionMatrix!

asMatrix	^self!

asOrderedCollection	| anOrderedCollection |	anOrderedCollection := OrderedCollection new: self size.	self do: [:each | anOrderedCollection addLast: each].	^anOrderedCollection!

asRowMatrix	^nrow = 1 		ifTrue: [self]		ifFalse: 			[| res |			res := self class ncol: self size.			1 to: self size do: [:i | res at: i put: (self at: i)]]!

asSet	| aSet |	aSet := Set new.	self do: [:each | aSet add: each].	^aSet!

asSinglePrecisionComplexMatrix	| res |	res := LapackCGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asFloatComplex].	^res!

asSinglePrecisionMatrix	| res |	res := LapackSGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asFloat].	^res!

asSortedCollection	| aSortedCollection |	aSortedCollection := SortedCollection new: self size.	aSortedCollection addAll: self.	^aSortedCollection!

asSortedCollection: aBlock 	| aSortedCollection |	aSortedCollection := SortedCollection new: self size.	aSortedCollection sortBlock: aBlock.	aSortedCollection addAll: self.	^aSortedCollection!

at: anInteger	"Access the underlying array	should be overloaded if packed storage is used"	^array at: anInteger!

at: rowIndex at: columnIndex 	"Squak compatible"	^self rowAt: rowIndex columnAt: columnIndex!

at: r at: c ifInvalid: v	"If r,c is a valid index for this matrix, answer the corresponding element.	 Otherwise, answer v value.	Squeak compatible - I added the message v value: this enables using aBlock"	(r between: 1 and: nrow) ifFalse: [^v value].	(c between: 1 and: ncol) ifFalse: [^v value].	^self rowAt: r columnAt: c!

at: row at: column incrementBy: value 	"Squeak compatible message"	^self 		rowAt: row		columnAt: column		put: (self rowAt: row columnAt: column) + value!

at: rowIndex at: columnIndex put: aNumber	"Squak compatible"	^self rowAt: rowIndex columnAt: columnIndex put: aNumber!

at: anInteger put: aNumber 	"Access the underlying array	should be overloaded if packed storage is used"	^array at: anInteger put: aNumber!

atAllPut: aNumber 	"Put aNumber at every one of the receiver's indices."	1 to: self size do: [:index | self at: index put: aNumber]!

atColumn: columnIndex 	"synonym"	^self columnAt: columnIndex!

atColumn: colIndex put: aCollection 	"Synonym Squeak-Matrix-Compatible"	^self atColumn: colIndex putSequence: aCollection!

atColumn: columnIndex putAll: aNumber 	"synonym"	^self columnAt: columnIndex putAll: aNumber!

atColumn: colIndex putSequence: aCollection 	1 to: aCollection size		do: 			[:each | 			self 				rowAt: each				columnAt: colIndex				put: (aCollection at: each)]!

atColumns: columnsIndexCollection 	| res j |	res := self class allocateNrow: nrow ncol: columnsIndexCollection size.	j := 0.	columnsIndexCollection 		do: [:i | res columnAt: (j := j + 1) putSequence: (self columnAt: i)].	^res!

atInteger: anInteger	"Access the underlying array"	^self at: anInteger!

atInteger: rowIndex andInteger: columnIndex 	"Access the underlying array"	^self at: (columnIndex - 1) * nrow + rowIndex!

atInteger: rowIndex andInteger: columnIndex put: aNumber	"Access the underlying array"	^self at: (columnIndex - 1) * nrow + rowIndex put: aNumber!

atInteger: anInteger put: aNumber	"Access the underlying array"	^self at: anInteger put: aNumber!

atIntervalFrom: interStart to: interStop by: interStep 	"Extract a copy of a subinterval of self"	| sz result |	sz := (interStop - interStart) / interStep + 1 max: 0.	result := nrow = 1 				ifTrue: [self class ncol: sz]				ifFalse: [ncol = 1 ifTrue: [self class nrow: sz] ifFalse: [Array new: sz]].	1 to: sz		do: [:i | result at: i put: (self at: interStart + ((i - 1) * interStep))].	^result!

atPoint: aPoint 	"some implementations use Points to access 2D tables"	^self rowAt: aPoint x columnAt: aPoint y!

atPoint: aPoint put: aNumber	"some implementations use Points to access 2D tables"	^self rowAt: aPoint x columnAt: aPoint y put: aNumber!

atRow: rowIndex 	"synonym"	^self rowAt: rowIndex!

atRow: rowIndex column: columnIndex 	"synonym"	^self rowAt: rowIndex columnAt: columnIndex!

atRow: rowIndex column: columnIndex put: aNumber 	"synonym"	^self 		rowAt: rowIndex		columnAt: columnIndex		put: aNumber!

atRow: colIndex put: aCollection 	"Synonym Squeak-Matrix-Compatible"	^self atRow: colIndex putSequence: aCollection!

atRow: rowIndex putAll: aNumber 	"synonym"	^self rowAt: rowIndex putAll: aNumber!

atRow: rowIndex putSequence: aCollection 	"synonym"	^self rowAt: rowIndex putSequence: aCollection!

atRows: rowsIndexCollection 	| res j |	res := self class allocateNrow: rowsIndexCollection size ncol: ncol.	j := 0.	rowsIndexCollection 		do: [:i | res rowAt: (j := j + 1) putSequence: (self rowAt: i)].	^res!

capacity	^nrow * ncol!

collect: aBlock 	| result |	result := self class allocateNrow: nrow ncol: ncol.	1 to: self size		do: [:i | result at: i put: (aBlock value: (self at: i))].	^result!

columnAt: columnIndex 	^(self 		atIntervalFrom: (columnIndex - 1) * nrow + 1		to: columnIndex * nrow		by: 1) asColumnMatrix!

columnAt: columnIndex putAll: aNumber 	1 to: nrow		do: 			[:each | 			self 				rowAt: each				columnAt: columnIndex				put: aNumber]!

columnAt: colIndex putSequence: aCollection 	1 to: aCollection size		do: 			[:each | 			self 				rowAt: each				columnAt: colIndex				put: (aCollection at: each)]!

columnCount	"Squeak compatible"	^ncol!

columns	^(1 to: ncol) collect: [:columnIndex | self columnAt: columnIndex]!

columns: anArrayOfColumns 	1 to: anArrayOfColumns size		do: [:columnIndex | self columnAt: columnIndex putSequence: (anArrayOfColumns at: columnIndex)]!

columnsDo: aBlock 	"evaluate aBlock with each column"	1 to: ncol do: [:i | aBlock value: (self columnAt: i)]!

concatColumnsFromLapackMatrix: aLapackMatrix	^self concatColumnsFromMatrix: aLapackMatrix!

concatColumnsFromMatrix: aMatrix 	^aMatrix concatColumnsWithMatrix: self!

concatColumnsWithMatrix: aMatrix 	"Generic code"	| res |	self size = 0 ifTrue: [^aMatrix copy].	aMatrix size = 0 ifTrue: [^self copy].	aMatrix nrow = nrow 		ifFalse: [self error: 'cannot concatenate columns if not same number of rows'].	res := self class allocateNrow: nrow ncol: ncol + aMatrix ncol.	res 		copy: self size		elementsFrom: self		sourceIncrement: 1		destIncrement: 1.	(res withArrayOffsetBy: self size) 		copy: aMatrix size		elementsFrom: aMatrix		sourceIncrement: 1		destIncrement: 1.	^res!

concatRowsFromLapackMatrix: aLapackMatrix	^self concatRowsFromMatrix: aLapackMatrix!

concatRowsFromMatrix: aMatrix 	^aMatrix concatRowsWithMatrix: self!

concatRowsWithMatrix: aMatrix 	"Generic implementation"	| res |	self size = 0 ifTrue: [^aMatrix copy].	aMatrix size = 0 ifTrue: [^self copy].	aMatrix ncol = ncol 		ifFalse: [self error: 'cannot concatenate rows if not same number of columns'].	res := self class allocateNrow: nrow + aMatrix nrow ncol: ncol.	res 		copy: nrow		rowsStartingAt: 1		and: ncol		columnsStartingAt: 1		from: self.	res 		copy: aMatrix nrow		rowsStartingAt: nrow + 1		and: ncol		columnsStartingAt: 1		from: aMatrix.	^res!

conjugated	^self collect: [:e | e conjugated]!

copy: n elementsFrom: aMatrix sourceIncrement: incx destIncrement: incy 	"BLAS primitve xCOPY - naive implementation"	^self 		copy: n		elementsFrom: aMatrix		sourceOffset: 0		sourceIncrement: incx		destOffset: 0		destIncrement: incy!

copy: n elementsFrom: aMatrix sourceOffset: offx sourceIncrement: incx destOffset: offy destIncrement: incy 	"BLAS primitve xCOPY - naive implementation"	1 to: n		do: 			[:i | 			self at: (i - 1) * incy + offy + 1				put: (aMatrix at: (i - 1) * incx + offx + 1)]!

copy: m rowsStartingAt: i and: n columnsStartingAt: j from: a 	"Equivalent Matlab code:		self( i:i+m-1 , j:j+n-1 ) = a(1:m , 1:n)	subclass would use auxiliary lapack routine"	1 to: n		do: 			[:jc | 			1 to: m				do: 					[:ir | 					self at: (j + jc - 2) * nrow + i + ir - 1						put: (a at: (jc - 1) * a nrow + ir)]]!

count: aBlock 	"Count the number of elements that satisfy a condition.	anElement is satisfying a condition means	(aBlock value: anElement) = true."	| count |	count := 0.	1 to: self size		do: [:i | (aBlock value: (self at: i)) ifTrue: [count := count + 1]].	^count!

cumulativeProduct: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: ncol.			1 to: nrow				do: 					[:i | 					res rowAt: i						putSequence: ((1 to: ncol) 								cumulativeProduct: [:j | aBlock value: (self rowAt: i columnAt: j)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: nrow ncol: ncol.			1 to: ncol				do: 					[:i | 					res columnAt: i						putSequence: ((1 to: nrow) 								cumulativeProduct: [:j | aBlock value: (self rowAt: j columnAt: i)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

cumulativeSum: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: ncol.			1 to: nrow				do: 					[:i | 					res rowAt: i						putSequence: ((1 to: ncol) 								cumulativeSum: [:j | aBlock value: (self rowAt: i columnAt: j)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: nrow ncol: ncol.			1 to: ncol				do: 					[:i | 					res columnAt: i						putSequence: ((1 to: nrow) 								cumulativeSum: [:j | aBlock value: (self rowAt: j columnAt: i)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

diagonal	"Answer a Column Matrix with elements extracted from my diagonal.	This is not compatible with Matlab function diag.	Matlab code would be:		^self isVector			ifTrue: [self clas diagonal: self]			ifFalse: [self diagonalAt: 0]"	^self diagonalAt: 0!

diagonalAt: index 	"Answer a Column Matrix with elements extracted from :	- my diagonal if index is 0	- super diagonal (upper right) number index if index > 0	- sub diagonal (lower left) number index negated if index < 0	This is not compatible with Matlab function diag"	| diag |	index >= 0 		ifTrue: 			[diag := self class nrow: (self diagonalSizeAt: index).			1 to: diag size				do: [:i | diag at: i put: (self rowAt: i columnAt: i + index)]]		ifFalse: 			[diag := self class nrow: (self diagonalSizeAt: index).			1 to: diag size				do: [:i | diag at: i put: (self rowAt: i - index columnAt: i)]].	^diag!

diagonalSizeAt: index 	"answer the size of a diagonal"	^index >= 0 		ifTrue: [nrow min: ncol - index]		ifFalse: [nrow + index min: ncol]!

differenceFromComplex: aComplex 	^self collect: [:each | aComplex - each]!

differenceFromDouble: aDouble 	^self collect: [:each | aDouble - each]!

differenceFromFixedPoint: aFixedPoint 	^self collect: [:each | aFixedPoint - each]!

differenceFromFloat: aFloat 	^self collect: [:each | aFloat - each]!

differenceFromFraction: aFraction 	^self collect: [:each | aFraction - each]!

differenceFromInteger: anInteger 	^self collect: [:each | anInteger - each]!

differenceFromLapackMatrix: aLapackMatrix	^self differenceFromMatrix: aLapackMatrix asGeneralMatrix!

differenceFromMatrix: aMatrix 	| res |	(nrow = aMatrix nrow and: [ncol = aMatrix ncol]) 		ifFalse: [^self error: 'matrix dimensions are not compatible'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: self size		do: [:i | res at: i put: (aMatrix at: i) - (self at: i)].	^res!

dimensions	^Array with: nrow with: ncol!

do: aBlock 	"Evaluate aBlock with each of the receiver's elements as the argument."	1 to: self size do: [:i | aBlock value: (self at: i)]!

dotProduct: n elementsIncrement: incx with: aMatrix increment: incy 	"BLAS primitve xDOT - naive implementation"	^(1 to: n) sum: 			[:i | 			(self at: (i - 1) * incx + 1) 				* (aMatrix at: (i - 1) * incy + 1)]!

elementwisePowerFromNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: aNumber ** (res rowAt: ir columnAt: jc)]].	^res!

elementwisePowerWithMatrix: aMatrix 	| res |	(aMatrix nrow = nrow and: [aMatrix ncol = ncol]) 		ifFalse: [self error: 'matrix size mismatch'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) ** (aMatrix rowAt: ir columnAt: jc)]].	^res!

elementwisePowerWithNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) ** aNumber]].	^res!

elementwiseProductFromNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: aNumber * (res rowAt: ir columnAt: jc)]].	^res!

elementwiseProductWithMatrix: aMatrix 	| res |	(aMatrix nrow = nrow and: [aMatrix ncol = ncol]) 		ifFalse: [self error: 'matrix size mismatch'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) * (aMatrix rowAt: ir columnAt: jc)]].	^res!

elementwiseProductWithNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) * aNumber]].	^res!

elementwiseQuotientFromNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: aNumber / (res rowAt: ir columnAt: jc)]].	^res!

elementwiseQuotientWithMatrix: aMatrix 	| res |	(aMatrix nrow = nrow and: [aMatrix ncol = ncol]) 		ifFalse: [self error: 'matrix size mismatch'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) / (aMatrix rowAt: ir columnAt: jc)]].	^res!

elementwiseQuotientWithNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) / aNumber]].	^res!

fill: n elementsWithStride: incy withSelfPlusScalar: alpha timesVector: aMatrix stride: incx 	"BLAS primitve xAXPY - naive implementation"	1 to: n		do: 			[:i | 			self at: (i - 1) * incx + 1				put: (self at: (i - 1) * incx + 1) * alpha 						+ (aMatrix at: (i - 1) * incy + 1)]!

fill: m elementsWithStride: incy withSelfScaledBy: beta plusScalar: alpha timesMatrix: a transposed: trans timesVector: x length: n stride: incx 	"BLAS primitve xGEMV - naive implementation"	trans 		ifTrue: 			[1 to: m				do: 					[:i | 					self at: (i - 1) * incy + 1						put: alpha * ((1 to: n) 										sum: [:j | (a rowAt: j columnAt: i) * (x at: (j - 1) * incx + 1)]) 								+ (beta * (self at: (i - 1) * incy + 1))]]		ifFalse: 			[1 to: m				do: 					[:i | 					self at: (i - 1) * incy + 1						put: alpha * ((1 to: n) 										sum: [:j | (a rowAt: i columnAt: j) * (x at: (j - 1) * incx + 1)]) 								+ (beta * (self at: (i - 1) * incy + 1))]]!

fillM: m byN: n withScalar: alpha timesColumnVector: x stride: incx timesRowVector: y stride: incy 	"Blas library xGERx 	fill mxn elements of self from following m-length x and a n-length y vector product		alpha*x*transpose(y)"	1 to: n		do: 			[:j | 			1 to: m				do: 					[:i | 					self 						rowAt: i						columnAt: j						put: alpha * (x at: (i - 1) * incx + 1) 								* (y at: (j - 1) * incy + 1)]]!

fillM: m byN: n withSelfScaledBy: beta plusScalar: alpha timesLeftMatrix: a transposed: transa timesRightMatrix: b transposed: transb length: k 	"BLAS primitve xGEMM - naive implementation"	transa 		ifTrue: 			[transb 				ifTrue: 					[1 to: n						do: 							[:j | 							1 to: m								do: 									[:i | 									self 										rowAt: i										columnAt: j										put: alpha * ((1 to: k) 														sum: [:kk | (a rowAt: kk columnAt: i) * (b rowAt: j columnAt: kk)]) 												+ (beta * (self rowAt: i columnAt: j))]]]				ifFalse: 					[1 to: n						do: 							[:j | 							1 to: m								do: 									[:i | 									self 										rowAt: i										columnAt: j										put: alpha * ((1 to: k) 														sum: [:kk | (a rowAt: kk columnAt: i) * (b rowAt: kk columnAt: j)]) 												+ (beta * (self rowAt: i columnAt: j))]]]]		ifFalse: 			[transb 				ifTrue: 					[1 to: n						do: 							[:j | 							1 to: m								do: 									[:i | 									self 										rowAt: i										columnAt: j										put: alpha * ((1 to: k) 														sum: [:kk | (a rowAt: i columnAt: kk) * (b rowAt: j columnAt: kk)]) 												+ (beta * (self rowAt: i columnAt: j))]]]				ifFalse: 					[1 to: n						do: 							[:j | 							1 to: m								do: 									[:i | 									self 										rowAt: i										columnAt: j										put: alpha * ((1 to: k) 														sum: [:kk | (a rowAt: i columnAt: kk) * (b rowAt: kk columnAt: j)]) 												+ (beta * (self rowAt: i columnAt: j))]]]]!

findMax	"answer the index of the max of all elements  "	| max index |	self isEmpty ifTrue: [^0].	index := 1.	max := self at: 1.	2 to: self size		do: 			[:i | 			| tmp |			(tmp := self at: i) > max 				ifTrue: 					[max := tmp.					index := i]].	^index!

findMaxOf: aBlock 	"answer the index of the max of all elements after applying a Block"	| max index |	self isEmpty ifTrue: [^0].	index := 1.	max := aBlock value: (self at: 1).	2 to: self size		do: 			[:i | 			| tmp |			(tmp := aBlock value: (self at: i)) > max 				ifTrue: 					[max := tmp.					index := i]].	^index!

findMin	| min index |	self isEmpty ifTrue: [^0].	index := 1.	min := self at: 1.	2 to: self size		do: 			[:i | 			| tmp |			(tmp := self at: i) < min 				ifTrue: 					[min := tmp.					index := i]].	^index!

findMinOf: aBlock 	"answer the index of the min of all elements after applying a Block"	| min index |	self isEmpty ifTrue: [^0].	index := 1.	min := aBlock value: (self at: 1).	2 to: self size		do: 			[:i | 			| tmp |			(tmp := aBlock value: (self at: i)) < min 				ifTrue: 					[min := tmp.					index := i]].	^index!

first	^self at: 1!

fromColumn: jStart toColumn: jStop by: jStep	^self atColumns: (jStart to: jStop by: jStep)!

fromRow: iStart toRow: iStop by: iStep	^self atRows: (iStart to: iStop by: iStep)!

generalizedAt: index 	^index indexAccessInto: self!

generalizedAt: index put: anObject 	^index indexAccessInto: self put: anObject!

hash	"Answer a uniformly distributed SmallInteger computed from the contents 	 of the receiver. This algorithm is congruent with String>stringhash. I.e. 	 an Array of a sequence of characters has the same hash as a String of 	 those same characters."	| size |	^0 = (size := self size)		ifTrue: [12345]		ifFalse: 			[| midLeft midRight nextToLast firstc mlc mrc nlc lastc |			size = 1				ifTrue: [midLeft := midRight := nextToLast := 1]				ifFalse: 					[midLeft := size bitShift: -1.					midRight := midLeft + 1.					nextToLast := size - 1].			firstc := (self at: 1) hash.			mlc := (self at: midLeft) hash.			mrc := (self at: midRight) hash.			nlc := (self at: nextToLast) hash.			lastc := (self at: size) hash.			mrc			+ (mrc + size bitShift: 8)			+ ((firstc + nlc bitAnd: 16383) bitShift: 12)			+ (nlc bitShift: 2)			+ ((lastc bitAnd: 2047) bitShift: 16)			+ (firstc + lastc bitShift: 4)			+ ((mlc bitAnd: 255) bitShift: 20)			+ (mlc bitShift: 6)]!

hasSameShapeAs: aMatrix	^nrow = aMatrix nrow and: [ncol = aMatrix ncol]!

hasShape: nRows by: nColumns 	^nrow = nRows and: [ncol = nColumns]!

i	^self collect: [:e | e i]!

i: aMatrix	^self + aMatrix i!

identity	nrow = ncol ifFalse: [self error: 'should be square'].	^self class eye: nrow!

imaginaryPart	^self collect: [:e | e imaginaryPart]!

indicesCollect: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					res 						rowAt: row						columnAt: column						put: (aBlock value: row value: column)]].	^res!

indicesDo: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	1 to: ncol		do: [:column | 1 to: nrow do: [:row | aBlock value: row value: column]]!

indicesInject: start into: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	| current |	current := start.	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					current := aBlock 								value: current								value: row								value: column]].	^current!

inject: aValue into: aBlock 	| result |	result := aValue.	1 to: self size		do: [:i | result := aBlock value: result value: (self at: i)].	^result!

isColumnMatrix	^ncol = 1!

isColumnVector	^ncol = 1!

isComplexMatrix	^self anySatisfy: [:e | e isComplex]!

isDiagonal	nrow = ncol ifFalse: [^false].	^(1 to: ncol) 		allSatisfy: [:j | (1 to: nrow) allSatisfy: [:i | i = j or: [(self rowAt: i columnAt: j) isZero]]]!

isEmpty	^self size = 0!

isHermitian	nrow = ncol ifFalse: [^false].	^self = self transposeConjugated!

isLowerTriangular	nrow = ncol ifFalse: [^false].	^(2 to: ncol) 		allSatisfy: [:jc | (1 to: jc - 1) allSatisfy: [:ir | (self rowAt: ir columnAt: jc) isZero]]!

isMatrix	^true!

isRowMatrix	^nrow = 1!

isRowVector	^nrow = 1!

isSameSequenceAs: otherCollection	"Answer whether the receiver's size is the same as otherCollection's size, and each	 of the receiver's elements equal the corresponding element of otherCollection."	| size |	(size := self size) = otherCollection size ifFalse: [^false].	1 to: size do: [:index |		(self at: index) = (otherCollection at: index) ifFalse: [^false]].	^true!

isSequenceable	"I implement at: at:put: and size, this should be enough"	^true!

isSquare	^nrow = ncol!

isSymmetric	nrow = ncol ifFalse: [^false].	^self = self transposed!

isTriangular	^self isLowerTriangular or: [self isUpperTriangular]!

isUpperTriangular	nrow = ncol ifFalse: [^false].	^(2 to: nrow) 		allSatisfy: [:ir | (1 to: ir - 1) allSatisfy: [:jc | (self rowAt: ir columnAt: jc) isZero]]!

isVector	^self isColumnVector or: [self isRowVector]!

isZero	^self allSatisfy: [:each | each isZero]!

last	^self at: self size!

lowerTriangle	"return lower triangle matrix"	^self lowerTriangle: 0!

lowerTriangle: ind 	"return lower triangle matrix starting with super-diagonal ind"	| lower |	lower := self copy.	1 to: ncol		do: 			[:j | 			1 to: nrow				do: 					[:i | 					j - ind > i 						ifTrue: 							[lower 								rowAt: i								columnAt: j								put: 0]]].	^lower!

max	| max |	max := self first.	1 to: self size do: [:i | max := max max: (self at: i)].	^max!

maxOf: aBlock 	| max |	self isEmpty ifTrue: [^nil].	max := aBlock value: (self at: 1).	2 to: self size do: [:i | max := max max: (aBlock value: (self at: i))].	^max!

maxOf: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: 1.			1 to: nrow				do: 					[:ir | 					res at: ir						put: ((1 to: ncol) 								maxOf: [:jc | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: 1 ncol: ncol.			1 to: ncol				do: 					[:jc | 					res at: jc						put: ((1 to: nrow) 								maxOf: [:ir | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

min	| min |	min := self first.	1 to: self size do: [:i | min := min min: (self at: i)].	^min!

minOf: aBlock 	"answer the min of all elements after applying a Block"	| min |	self isEmpty ifTrue: [^nil].	min := aBlock value: (self at: 1).	2 to: self size do: [:i | min := min min: (aBlock value: (self at: i))].	^min!

minOf: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: 1.			1 to: nrow				do: 					[:ir | 					res at: ir						put: ((1 to: ncol) 								minOf: [:jc | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: 1 ncol: ncol.			1 to: ncol				do: 					[:jc | 					res at: jc						put: ((1 to: nrow) 								minOf: [:ir | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

multiplyByFloat: aFloat 	^self collect: [:each | aFloat * each]!

multiplyByFraction: aFraction 	^self collect: [:each | aFraction * each]!

multiplyByInteger: anInteger 	^self collect: [:each | anInteger * each]!

multiplyByScaledDecimal: aScaledDecimal 	^self collect: [:each | aScaledDecimal * each]!

naiveSetOffDiagonal: alpha diagonal: beta 	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					self 						rowAt: ir						columnAt: jc						put: (ir = jc ifTrue: [beta] ifFalse: [alpha])]]!

ncol	^ncol!

nCols	"Synonym Smallpack compatible"	^ncol!

negated	^self collect: [:e | e negated]!

norm1	^(1 to: ncol) 		maxOf: [:jc | (1 to: nrow) sum: [:ir | (self rowAt: ir columnAt: jc) abs]]!

norm2	^self singularValues max!

normFrobenius	^self vectorNorm2!

normInfinity	^(1 to: nrow) 		maxOf: [:ir | (1 to: ncol) sum: [:jc | (self rowAt: ir columnAt: jc) abs]]!

nrow	^nrow!

nRows	"Synonym Smallpack compatible"	^nrow!

numberOfColumns	^ncol!

numberOfRows	^nrow!

postCopy	array := array copy!

prependColumns: nColumns 	| res |	res := self class nrow: nrow ncol: ncol + nColumns.	res 		copy: nrow		rowsStartingAt: 1		and: ncol		columnsStartingAt: nColumns + 1		from: self.	^res!

prependRows: nRows 	| res |	res := self class nrow: nrow + nRows ncol: ncol.	res 		copy: nrow		rowsStartingAt: nRows + 1		and: ncol		columnsStartingAt: 1		from: self.	^res!

printOn: aStream 	aStream		nextPut: $(;		nextPutAll: self class name;		space;		nextPutAll: #rows:;		space;		nextPutAll: '#('.	1 to: nrow		do: 			[:i | 			aStream				crtab;				nextPutAll: '#('.			1 to: ncol				do: 					[:j | 					(self rowAt: i columnAt: j) printOn: aStream.					aStream space].			aStream nextPut: $)].	aStream		cr;		nextPut: $);		nextPut: $)!

product	| prod |	self isEmpty ifTrue: [^1].	prod := self at: 1.	2 to: self size do: [:i | prod := prod * (self at: i)].	^prod!

product: aBlock 	| product |	self isEmpty ifTrue: [^1].	product := aBlock value: (self at: 1).	2 to: self size		do: [:i | product := product * (aBlock value: (self at: i))].	^product!

product: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: 1.			1 to: nrow				do: 					[:ir | 					res at: ir						put: ((1 to: ncol) 								product: [:jc | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: 1 ncol: ncol.			1 to: ncol				do: 					[:jc | 					res at: jc						put: ((1 to: nrow) 								product: [:ir | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

productColumnVectorWithRowVector: aVector 	"Vector * Vector :transpose	naive algorithm"	| result |	result := self class allocateNrow: self size ncol: aVector size.	1 to: aVector size		do: 			[:jc | 			1 to: self size				do: 					[:ir | 					result 						rowAt: ir						columnAt: jc						put: (self at: ir) * (aVector at: jc)]].	^result!

productFromComplex: aComplex 	^self collect: [:each | aComplex * each]!

productFromDouble: aDouble 	^self collect: [:each | aDouble * each]!

productFromFixedPoint: aFixedPoint 	^self collect: [:each | aFixedPoint * each]!

productFromFloat: aFloat 	^self collect: [:each | aFloat * each]!

productFromFraction: aFraction 	^self collect: [:each | aFraction * each]!

productFromInteger: anInteger 	^self collect: [:each | anInteger * each]!

productFromLapackMatrix: aLapackMatrix	^self productFromMatrix: aLapackMatrix asGeneralMatrix!

productFromMatrix: aMatrix 	"Algorithm ^(aMatrix productMatrixWithMatrix: self)	is general and would work well.	But individual methods might be optimized in subclasses"	aMatrix ncol = nrow 		ifFalse: [^self error: 'matrix dimensions are not compatible'].	^self isColumnVector 		ifTrue: 			[aMatrix isRowVector 				ifTrue: [aMatrix productRowVectorWithColumnVector: self]				ifFalse: [aMatrix productMatrixWithColumnVector: self]]		ifFalse: 			[aMatrix isColumnVector 				ifTrue: [aMatrix productColumnVectorWithRowVector: self]				ifFalse: 					[aMatrix isRowVector 						ifTrue: [aMatrix productRowVectorWithMatrix: self]						ifFalse: [aMatrix productMatrixWithMatrix: self]]]!

productMatrixAtRightWithMatrix: aMatrix 	"Answer the matrix product		aMatrix * self	This can be optimized in special cases of hermitian or triangular Matrix	and should be overloaded in these subclasses"	^aMatrix productMatrixWithMatrix: self!

productMatrixTransposeWithColumnVector: aVector 	"Matrix transpose * vector	naive algorithm"	| result |	result := self class allocateNrow: nrow ncol: 1.	1 to: nrow		do: 			[:ir | 			result at: ir				put: ((1 to: ncol) 						sum: [:jc | (self rowAt: jc columnAt: ir) * (aVector at: jc)])].	^result!

productMatrixWithColumnVector: aVector 	"Matrix * vector	naive algorithm"	| result |	result := self class allocateNrow: nrow ncol: 1.	1 to: nrow		do: 			[:ir | 			result at: ir				put: ((1 to: ncol) 						sum: [:jc | (self rowAt: ir columnAt: jc) * (aVector at: jc)])].	^result!

productMatrixWithMatrix: aMatrix 	| a b c iA iB iC |	a := self.	b := aMatrix.	c := aMatrix class allocateNrow: a nrow ncol: b ncol.	iC := 0.	iB := 1.	1 to: b ncol		do: 			[:jc | 			iA := 1.			1 to: a nrow				do: 					[:ir | 					| cij kA kB |					cij := (a at: (kA := iA)) * (b at: (kB := iB)).					2 to: b nrow						do: [:k | cij := cij + ((a at: (kA := kA + a nrow)) * (b at: (kB := kB + 1)))].					c at: (iC := iC + 1) put: cij.					iA := iA + 1].			iB := iB + b nrow].	^c!

productRowVectorWithColumnVector: aVector 	"Answer the result of operation (a dot product)		leftVector transpose * aVector	where		leftVector transpose = self"	| res |	res := self class allocateNrow: 1 ncol: 1.	res at: 1		put: ((1 to: self size) sum: [:i | (self at: i) * (aVector at: i)]).	^res!

productRowVectorWithMatrix: aMatrix 	"Vector * Matrix	naive algorithm"	| result |	result := self class allocateNrow: 1 ncol: aMatrix ncol.	1 to: aMatrix ncol		do: 			[:jc | 			result at: jc				put: ((1 to: aMatrix nrow) 						sum: [:ir | (self at: ir) * (aMatrix rowAt: ir columnAt: jc)])].	^result!

realPart	^self collect: [:e | e real]!

replicateNrow: nr timesNcol: nc 	"build a Matrix by replicating self in rows and in columns	Analogous to Matlab repmat"	| res |	res := self class allocateNrow: nrow * nr ncol: ncol * nc.	1 to: nc		do: 			[:jc | 			1 to: nr				do: 					[:ir | 					res 						copy: nrow rowsStartingAt: (ir - 1) * nrow + 1						and: ncol columnsStartingAt: (jc - 1) * ncol + 1						from: self]].	^res!

respondsToArithmetic	^true!

rowAt: rowIndex 	^(self 		atIntervalFrom: rowIndex		to: (ncol - 1) * nrow + rowIndex		by: nrow) asRowMatrix!

rowAt: rowIndex columnAt: columnIndex 	"Access the underlying array"	^self at: (columnIndex - 1) * nrow + rowIndex!

rowAt: rowIndex columnAt: columnIndex  put: aNumber	"Access the underlying array"	^self at: (columnIndex - 1) * nrow + rowIndex put: aNumber!

rowAt: rowIndex putAll: aNumber 	1 to: ncol		do: 			[:each | 			self 				rowAt: rowIndex				columnAt: each				put: aNumber]!

rowAt: rowIndex putSequence: aCollection 	1 to: aCollection size		do: 			[:each | 			self 				rowAt: rowIndex				columnAt: each				put: (aCollection at: each)]!

rowCount	"Squeak compatible"	^nrow!

rows	^(1 to: nrow) collect: [:rowIndex | self rowAt: rowIndex]!

rows: anArrayOfRows 	1 to: anArrayOfRows size		do: [:rowIndex | self rowAt: rowIndex putSequence: (anArrayOfRows at: rowIndex)]!

rowsDo: aBlock 	"evaluate aBlock with each row"	1 to: nrow do: [:i | aBlock value: (self rowAt: i)]!

scale: n elementsBy: alpha increment: incx 	"BLAS primitve xSCAL - naive implementation"	1 to: n		do: 			[:i | 			self at: (i - 1) * incx + 1				put: (self at: (i - 1) * incx + 1) * alpha]!

scaledByComplex: aComplex 	"Answer a copy of self scaled."	aComplex imaginaryPart isZero ifTrue: [^self scaledByNumber: aComplex real].	^self collect: [:each | each * aComplex]!

scaledByNumber: aNumber 	"Answer a copy of self scaled."	^self collect: [:each | each * aNumber]!

setArray: anArray nrow: nr ncol: nc 	array := anArray.	nrow := nr.	ncol := nc!

setDiagonal: aMatrix	"set the diagonal elements"	1 to: aMatrix size do: [:i | self rowAt: i columnAt: i put: (aMatrix at: i)].!

setOffDiagonal: alpha diagonal: beta 	self atAllPut: alpha.	1 to: (nrow min: ncol)		do: 			[:i | 			self 				rowAt: i				columnAt: i				put: beta]!

setToEye	"Initialize diagonal to 1, off diagonal to 0"	self setOffDiagonal: 0 diagonal: 1!

size	^self capacity!

subtractFromFloat: aFloat 	^self collect: [:each | aFloat - each]!

subtractFromFraction: aFraction 	^self collect: [:each | aFraction - each]!

subtractFromInteger: anInteger	^self collect: [:each | anInteger - each]!

subtractFromScaledDecimal: aScaledDecimal	^self collect: [:each | aScaledDecimal - each]!

sum	| sum |	self isEmpty ifTrue: [^0].	sum := self at: 1.	2 to: self size do: [:i | sum := sum + (self at: i)].	^sum!

sum: aBlock 	| sum |	self isEmpty ifTrue: [^0].	sum := aBlock value: (self at: 1).	2 to: self size do: [:i | sum := sum + (aBlock value: (self at: i))].	^sum!

sum: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: 1.			1 to: nrow				do: 					[:ir | 					res at: ir						put: ((1 to: ncol) 								sum: [:jc | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: 1 ncol: ncol.			1 to: ncol				do: 					[:jc | 					res at: jc						put: ((1 to: nrow) 								sum: [:ir | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

sumFromComplex: aComplex 	^self collect: [:each | aComplex + each]!

sumFromDouble: aDouble 	^self collect: [:each | aDouble + each]!

sumFromFixedPoint: aFixedPoint 	^self collect: [:each | aFixedPoint + each]!

sumFromFloat: aFloat 	^self collect: [:each | aFloat + each]!

sumFromFraction: aFraction 	^self collect: [:each | aFraction + each]!

sumFromInteger: anInteger 	^self collect: [:each | anInteger + each]!

sumFromLapackMatrix: aLapackMatrix	^self sumFromMatrix: aLapackMatrix asGeneralMatrix!

sumFromMatrix: aMatrix 	| res |	(nrow = aMatrix nrow and: [ncol = aMatrix ncol]) 		ifFalse: [^self error: 'matrix dimensions are not compatible'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: self size		do: [:i | res at: i put: (aMatrix at: i) + (self at: i)].	^res!

swap: r1 at: c1 with: r2 at: c2 	"Squeak compatible protocol"	^self swapRowAt: r1 columnAt: c1 withRowAt: r2 columnAt: c2!

swapColumn: c1 withColumn: c2	"Squeak compatible - avoid direct access to array because subclasses might implement other behaviour	NAIVE implementation"	1 to: nrow do: [:row |		self swapRowAt: row columnAt: c1 withRowAt: row columnAt: c2]!

swapRow: r1 withRow: r2	"Squeak compatible - avoid direct access to array because subclasses might implement other behaviour	NAIVE implementation"	1 to: ncol do: [:col |		self swapRowAt: r1 columnAt: col withRowAt: r2 columnAt: col]!

swapRowAt: r1 columnAt: c1 withRowAt: r2 columnAt: c2 	"Swap two elements.	Avoid direct access to array because subclasses might implement other behaviour"	| tmp |	tmp := self at: r1 at: c1.	self 		at: r1		at: c1		put: (self at: r2 at: c2).	self 		at: r1		at: c1		put: tmp!

transpose	"synonym"	^self transposed!

transposeConjugated	"naive un-optimized algorithm"	| res |	self isRowMatrix ifTrue: [^self asColumnMatrix conjugated].	self isColumnMatrix ifTrue: [^self asRowMatrix conjugated].	res := self class allocateNrow: ncol ncol: nrow.	1 to: ncol		do: 			[:j | 			1 to: nrow				do: 					[:i | 					res 						rowAt: j						columnAt: i						put: (self rowAt: i columnAt: j) conjugated]].	^res!

transposed	"naive un-optimized algorithm"	| res |	self isRowMatrix ifTrue: [^self asColumnMatrix].	self isColumnMatrix ifTrue: [^self asRowMatrix].	res := self class allocateNrow: ncol ncol: nrow.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: jc						columnAt: ir						put: (self rowAt: ir columnAt: jc)]].	^res!

transposedConjugated	"Synonym"	^self transposeConjugated!

upperTriangle	"return upper triangle matrix"	^self upperTriangle: 0!

upperTriangle: ind 	"return upper triangle matrix starting with super-diagonal ind"	| upper |	upper := self copy.	1 to: ncol		do: 			[:j | 			1 to: nrow				do: 					[:i | 					j - ind < i 						ifTrue: 							[upper 								rowAt: i								columnAt: j								put: 0]]].	^upper!

vectorNorm1	^self sum: [:e | e abs]!

vectorNorm2	"Naive algorithm - not protected against underflow/overflow"	^(self sum: [:e | e abs squared]) sqrt!

vectorNormFrobenius	^self vectorNorm2!

vectorNormInfinity	^self maxOf: [:e | e abs]!

with: aCollection collect: aBlock 	"aCollection must support #at:at: and be at least as large as the receiver.	Squeak compatible message"	^self withIndicesCollect: 			[:each :row :column | 			aBlock value: each value: (aCollection at: row at: column)]!

with: aCollection do: aBlock 	"aCollection must support #at:at: and be at least as large as the receiver.	Squeak compatible message"	self 		withIndicesDo: [:each :row :column | aBlock value: each value: (aCollection at: row at: column)]!

withArrayOffsetBy: aPositiveInteger 	"Return a kind of shallow copy of self pointing on the same array object, but with an offset.	This is ugly like C code but maybe more efficient"	^self class basicNew 		setArray: (array reindexedThrough: (aPositiveInteger + 1 to: self size))		nrow: self size - aPositiveInteger		ncol: 1!

withIndicesCollect: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					res 						rowAt: row						columnAt: column						put: (aBlock 								value: (self rowAt: row columnAt: column)								value: row								value: column)]].	^res!

withIndicesDo: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					aBlock 						value: (self rowAt: row columnAt: column)						value: row						value: column]]!

withIndicesInject: start into: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	| current |	current := start.	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					"NOT EFFICIENT AT ALL... CREATE AN ARRAY AT EACH STEP					SHOULD CONSIDER ADDING value:value:value:value: EXTENSION IN VW"					current := aBlock valueWithArguments: (Array 										with: current										with: (self rowAt: row columnAt: column)										with: row										with: column)]].	^current!

zero	^self class nrow: nrow ncol: ncol! !
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

allocateNrow: nr ncol: nc 	"Allocate the array, but do not initialize its contents"	^self basicNew 		setArray: (Array new: nr * nc)		nrow: nr		ncol: nc!

allocateShape: aShape 	"Allocate the array, but do not initialize its contents"	^self shape: aShape do: [:nr :nc | self allocateNrow: nr ncol: nc]!

column: aSequenceableCollection	"Form a Matrix with a single column from a collection	This is Squeak-Matrix compatible"	^self columns: (Array with: aSequenceableCollection)!

columnMatrix	^self!

columns: anArrayOfColumns 	"Form a Matrix from an Array of columns.	Each column should have same number of elements"	| nr nc mat |	anArrayOfColumns isEmpty ifTrue: [^self nrow: 0 ncol: 0].	nc := anArrayOfColumns size.	nr := anArrayOfColumns first size.	(anArrayOfColumns allSatisfy: [:each | each size = nr]) 		ifFalse: [self error: 'Not a conformant array of columns'].	mat := self nrow: nr ncol: nc.	mat columns: anArrayOfColumns.	^mat!

diagonal: aSequenceOfNumber 	"Form a diagonal Matrix from a collection"	| n mat |	n := aSequenceOfNumber size.	mat := self nrow: n ncol: n.	mat setDiagonal: aSequenceOfNumber.	^mat!

dimensions: anIntegerOrPointOrArrayOfInteger 	"Synonym"	^self shape: anIntegerOrPointOrArrayOfInteger!

eye: anIntegerOrPointOrArrayOfInteger 	"Create eye (identity) matrix"	| res |	res := self shape: anIntegerOrPointOrArrayOfInteger.	res setToEye.	^res!

fromSequence: aSequenceableCollection nrow: nr ncol: nc 	"Create a matrix from a collection	BEWARE:	- elements ordering in the collection should be column-major	- no copy of the collection is made: the collection might be modified whenever the Matrix will be modified	  However, user cannot rely on this feature, it will not always be true."	^(aSequenceableCollection isSequenceable and: [ aSequenceableCollection size = (nr * nc)]) 			ifTrue: 				[self basicNew 					setArray: aSequenceableCollection					nrow: nr					ncol: nc]			ifFalse: 				[self error: #badArraySpecification << #Smallapack 							>> 'Bad sequenceableCollection specification for creating a Matrix']!

identity: anIntegerOrPointOrArrayOfInteger 	"Form a square or not square identity Matrix of dimensions specified by anIntegerOrPointOrArrayOfInteger	Example		AbstractMatrix identity: 3.		AbstractMatrix identity: 3 @ 2.		AbstractMatrix identity: #(3 2).	This is Squeak-Matrix compatible"	^self eye: anIntegerOrPointOrArrayOfInteger!

ncol: nc	"create a row matrix filled with 0"	^self rowMatrix		nrow: 1		ncol: nc		withAll: 0!

new: anInteger	"behave like a Collection. I will answer anInteger as my size.	Since I am column-major, favor a column	WARNING: THIS IS NOT COMPATIBLE WITH SQUEAK nor NUMERICAL METHODS	BUT MORE CONSISTENT WITh SEQUENCEABLE COLLECTION	use shape: anInteger to get equivalent behavior"	^self ncol: anInteger!

nrow: nr	"create a column matrix filled with 0"	^self columnMatrix		nrow: nr		ncol: 1		withAll: 0!

nrow: nr ncol: nc 	"create a matrix filled with 0"	^self 		nrow: nr		ncol: nc		withAll: 0!

nrow: nrow ncol: ncol tabulate: aBlock 	"Answer a new Matrix of the given dimensions where	 result at: i at: j     is   aBlock value: i value: j	Borrowed from Squeak.Matrix	Beware: unlike Squeak, this class is column-major-ordered"	| a i |	a := self nrow: nrow ncol: ncol.	i := 0.	1 to: ncol		do: 			[:column | 			1 to: nrow				do: [:row | a at: (i := i + 1) put: (aBlock value: row value: column)]].	^a!

nrow: nr ncol: nc withAll: aNumber 	"Create a matrix filled with aNumber	This is analog the the ArrayedCollection>>new:withAll: , not the Collection>>withAll:"	^self basicNew 		setArray: (Array new: nr * nc withAll: aNumber)		nrow: nr		ncol: nc!

nRows: numRows nCols: numCols 	"Synonym Smallpack-compatible protocol"	^self nrow: numRows ncol: numCols!

ones: anIntegerOrPointOrArrayOfInteger 	"Form a Matrix filled with 1 of dimensionsof dimensions specified by anIntegerOrPointOrArrayOfInteger	Example		AbstractMatrix ones: 3.		AbstractMatrix ones: 3 @ 2.		AbstractMatrix ones: #(3 2).	This is Squeak-Matrix compatible"	^self shape: anIntegerOrPointOrArrayOfInteger		do: 			[:nrow :ncol | 			self 				nrow: nrow				ncol: ncol				withAll: 1]!

row: aSequenceableCollection	"Form a Matrix with a single row from a collection	This is Squeak-Matrix compatible"	^self rows: (Array with: aSequenceableCollection)!

rowMatrix	^self!

rows: anArrayOfRows 	"Form a Matrix from an Array of rows.	Each row should have same number of elements"	| nr nc mat |	anArrayOfRows isEmpty ifTrue: [^self nrow: 0 ncol: 0].	nr := anArrayOfRows size.	nc := anArrayOfRows first size.	(anArrayOfRows allSatisfy: [:each | each size = nc]) 		ifFalse: [self error: 'Not a conformant array of rows'].	mat := self nrow: nr ncol: nc.	mat rows: anArrayOfRows.	^mat!

rows: rows columns: columns 	"Synonym Squeak-Matrix-compatible protocol"	^self nrow: rows ncol: columns!

rows: rows columns: columns contents: contents 	"Synonym Squeak-Matrix-compatible protocol	BEWARE:		in Squeak Matrix, order is row-major		in Smallapack, order is column-major	Assume the contents is Smallapack-column-major"	^self 		fromSequence: contents		nrow: rows		ncol: columns!

rows: rows columns: columns element: element 	"Synonym Squeak-Matrix-compatible protocol"	^self 		nrow: rows		ncol: columns		withAll: element!

rows: rows columns: columns tabulate: aBlock 	"Synonym Squeak-Matrix-compatible protocol"	^self 		nrow: rows		ncol: columns		tabulate: aBlock!

shape: anIntegerOrPointOrArrayOfInteger 	"Form a Matrix of dimensions (or shape) :		either square anInteger * anInteger		or aPoint x*aPoint y		or anArrayOfInteger first*anArrayOfInteger last	Depending on argument class"	^self shape: anIntegerOrPointOrArrayOfInteger		do: [:nrow :ncol | self nrow: nrow ncol: ncol]!

shape: anIntegerOrPointOrArrayOfInteger do: aBlock 	"Extract the dimensions (or shape) :		either square anInteger * anInteger		or aPoint x*aPoint y		or anArrayOfInteger first*anArrayOfInteger last	And evaluate aBlock with: nrow with: ncol"	^anIntegerOrPointOrArrayOfInteger isInteger 		ifTrue: 			[aBlock value: anIntegerOrPointOrArrayOfInteger				value: anIntegerOrPointOrArrayOfInteger]		ifFalse: 			[anIntegerOrPointOrArrayOfInteger isSequenceable 				ifTrue: 					[(anIntegerOrPointOrArrayOfInteger size between: 1 and: 2) 						ifTrue: 							[aBlock value: anIntegerOrPointOrArrayOfInteger first								value: anIntegerOrPointOrArrayOfInteger last]						ifFalse: 							[self 								error: 'dimensions: should be called with a collection of 1 or 2 integers']]				ifFalse: 					["Assume the argument is aPoint"					aBlock value: anIntegerOrPointOrArrayOfInteger x						value: anIntegerOrPointOrArrayOfInteger y]]!

zeros: anIntegerOrPointOrArrayOfInteger 	"Form a Matrix filled with 0 of dimensions specified by anIntegerOrPointOrArrayOfInteger	Example		AbstractMatrix zeros: 3.		AbstractMatrix zeros: 3 @ 2.		AbstractMatrix zeros: #(3 2).	This is Squeak-Matrix compatible"	^(self shape: anIntegerOrPointOrArrayOfInteger)		"atAllPut: 0; This is the default behaviour; do not do it again"		yourself! !
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

blasLibraryEnabled	^self registry at: #blasLibraryEnabled ifAbsent: [true]!

blasLibraryEnabled: aBoolean 	^self registry at: #blasLibraryEnabled put: aBoolean!

initialize	"Settings initialize"	self initializeRegistry!

initializeRegistry	Registry isNil ifTrue: [ self resetRegistry ].!

registry	^Registry!

resetRegistry	" self resetRegistry "		Registry := Dictionary new.!

useAtlasCBlas	^self registry at: #useAtlasCBlas ifAbsent: [true]!

useAtlasCBlas: aBoolean 	self registry at: #useAtlasCBlas put: aBoolean.	LapackMatrix		resetBlasInterfaces;		resetLapackInterfaces! !
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

- aMatrix	^aMatrix differenceFromLapackMatrix: self!

* aMatrix	^aMatrix productFromLapackMatrix: self!

, aMatrix 	^aMatrix concatColumnsFromLapackMatrix: self!

,, aMatrix 	^aMatrix concatRowsFromLapackMatrix: self!

/ aMatrix	^aMatrix quotientFromLapackMatrix: self!

\ aMatrix	^aMatrix leftDivideFromLapackMatrix: self!

+ aMatrix	^aMatrix sumFromLapackMatrix: self!

absMax	^self class isGeneralMatrix 		ifTrue: [super absMax]		ifFalse: [self asGeneralMatrix absMax]!

arrayPointer	"Answer a pointer on an array directly usable in C primitives..."	^self cArray asParameter!

arrayPointerWithOffset: anOffset	"Answer a pointer on an array directly usable in C primitives...	The offset is given in number of elements, not in bytes."	^(self cArray withArrayOffsetBy: anOffset) asParameter!

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

atIntervalFrom: interStart to: interStop by: interStep 	"fast method : bounds checking must be done externally"	| sz |	sz := (interStop - interStart) / interStep + 1.	^(nrow = 1 		ifTrue: [self class allocateNrow: 1 ncol: sz]		ifFalse: [self class allocateNrow: sz ncol: 1]) 			copy: sz			elementsFrom: (self withArrayOffsetBy: interStart - 1)			sourceIncrement: interStep			destIncrement: 1!

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

castTo: aMatrixClass 	"Answer a Matrix sharing the same array and that can be used in place of me.	This should not be used to cast type (real/complex) or precision (single/double),	because the underlying arrays will not be compatible.	This can be used only to cast the properties.	This message avoids a copy and is efficient, but should be used with care.	It is as ugly as a C-code cast"	^aMatrixClass basicNew 		setArray: self cArray		nrow: nrow		ncol: ncol!

castToColumn	^self class generalMatrix basicNew 		setArray: array		nrow: self arraySize		ncol: 1!

castToRealWithArrayOffsetBy: aPositiveInteger 	"Return a kind of shallow copy of self pointing on the same C underlying object, but as if it was a sequence of reals with an offset.	This is ugly like C code but maybe more efficient"	^self isComplexMatrix 		ifTrue: 			[| castToReal |			castToReal := self cArray castToRealWithArrayOffsetBy: aPositiveInteger.			self class realMatrix generalMatrix basicNew 				setArray: castToReal				nrow: castToReal size				ncol: 1]		ifFalse: [self withArrayOffsetBy: aPositiveInteger]!

castToRow	^self class generalMatrix basicNew 		setArray: array		nrow: 1		ncol: self arraySize!

coerceFlags: destFlags 	| flags m |	(flags := self class flags) = destFlags ifTrue: [^self].	m := self.	(flags bitAnd: PropertyMask) = (destFlags bitAnd: PropertyMask) 		ifFalse: 			[m := m asGeneralMatrix.			flags := m class flags].	(flags bitAnd: ComplexityMask) = (destFlags bitAnd: ComplexityMask) 		ifFalse: 			[m := m asComplexMatrix.			flags := m class flags].	(flags bitAnd: PrecisionMask) = (destFlags bitAnd: PrecisionMask) 		ifFalse: 			[m := m asDoublePrecisionMatrix.			flags := m class flags].	^m!

coerceFlagsButProperty: destFlags 	| flags m |	(flags := self class flags) = destFlags ifTrue: [^self].	m := self.	(flags bitAnd: ComplexityMask) = (destFlags bitAnd: ComplexityMask) 		ifFalse: 			[m := m asComplexMatrix.			flags := m class flags].	(flags bitAnd: PrecisionMask) = (destFlags bitAnd: PrecisionMask) 		ifFalse: 			[m := m asDoublePrecisionMatrix.			flags := m class flags].	^m!

coerceToComplexMatrix	"Fast implementation: copy every two reals"	| dst src |	dst := self class complexMatrix nrow: nrow ncol: ncol.	src := self withArrayOffsetBy: 0.	(dst castToRealWithArrayOffsetBy: 0) 		copy: src arraySize		elementsFrom: src		sourceIncrement: 1		destIncrement: 2.	^dst!

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

hessenbergDecomposition
	"hessenberg decomposition"

	nrow = ncol 
		ifFalse: 
			[^self error: 'hessenberg decomposition apply only on square Matrices'].
	^LapackHessenbergDecomposition decompose: self asGeneralMatrix!

imaginaryPart	"Fast implementation: copy every two reals"	^self class isComplexMatrix 		ifTrue: 			[| res |			((res := self class realMatrix nrow: nrow ncol: ncol) withArrayOffsetBy: 0) 				copy: self arraySize				elementsFrom: (self castToRealWithArrayOffsetBy: 1)				sourceIncrement: 2				destIncrement: 1.			res]		ifFalse: [self zero]!

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

productRowVectorWithColumnVector: aLapackMatrix 	"Implementation notes:	We must not use DOT in complex case because BLAS return a complex by value,	and VW Smalltalk does not seem to handle the case correctly (at least with g77)"	| res |	res := self class nrow: 1 ncol: 1.	(self isComplexMatrix not or: [Settings useAtlasCBlas]) 		ifTrue: 			["Case when we can use DOT"			| dotProduct |			dotProduct := self 						dotProduct: self size						elementsIncrement: 1						with: aLapackMatrix						increment: 1.			res at: 1 put: dotProduct]		ifFalse: 			["Fall back to matrix*vector product"			self productMatrixWithColumnVector: aLapackMatrix].	^res!

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

realPart	"Fast implementation: copy every two reals"	^self class isComplexMatrix 		ifTrue: 			[| res |			((res := self class realMatrix nrow: nrow ncol: ncol) withArrayOffsetBy: 0) 				copy: self arraySize				elementsFrom: (self castToRealWithArrayOffsetBy: 0)				sourceIncrement: 2				destIncrement: 1.			res]		ifFalse: [self]!

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

scaledByComplex: aComplex 	"Answer a copy of self scaled.	aComplex and self should agree on precision before sending this message."	aComplex imaginaryPart isZero ifTrue: [^self scaledByNumber: aComplex real].	^self asComplexMatrix copy inPlaceScaledByComplex: aComplex!

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

allocateNrow: nr ncol: nc 	"Allocate the array, but do not initialize its contents"	^self basicNew 		setArray: (self smalltalkArrayClass new: nr * nc)		nrow: nr		ncol: nc!

arrayInterface	^ArrayInterfaces at: self sdczIndex!

blasInterface	^BlasInterfaces at: self sdczIndex!

cAccessError	^cAccessError!

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

libraryError	^libraryErrorCollection!

nrow: nr ncol: nc withAll: aNumber 	"Create a matrix filled with aNumber"	^(self allocateNrow: nr ncol: nc) atAllPut: aNumber!

obsolete	super obsolete.	self unregisterFlags!

onStartup
	"reset the ExternalLibrary nterfaces on image startup"

	[self resetBlasInterfaces] on: Win32Error do: [:exc | exc return: nil].
	[self resetLapackInterfaces] on: Win32Error do: [:exc | exc return: nil].
	[self resetArrayInterfaces] on: Win32Error do: [:exc | exc return: nil].!

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

resetBlasInterfaces	BlasInterfaces := Array new: 4.	"Settings useAtlasCBlas 		ifTrue: 			[BlasInterfaces at: 1 + SinglePrecisionMask + RealMask put: CBlasSLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + RealMask put: CBlasDLibrary default.			BlasInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: CBlasCLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: CBlasZLibrary default]		ifFalse: 			["BlasInterfaces at: 1 + SinglePrecisionMask + RealMask put: BlasSLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + RealMask put: BlasDLibrary default.			BlasInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: BlasCLibrary default.			BlasInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: BlasZLibrary default"]"!

resetLapackInterfaces	LapackInterfaces := Array new: 4.	LapackInterfaces at: 1 + SinglePrecisionMask + RealMask put: LapackSLibrary default.	LapackInterfaces at: 1 + DoublePrecisionMask + RealMask put: LapackDLibrary default.	LapackInterfaces at: 1 + SinglePrecisionMask + ComplexMask put: LapackCLibrary default.	LapackInterfaces at: 1 + DoublePrecisionMask + ComplexMask put: LapackZLibrary default!

rowMatrix	^self generalMatrix!

sdczIndex	"Answer an index used for indirection according to precision and complexity"	^1 + (self flags bitAnd: SDCZMask)!

singlePrecisionMatrix	^self findClassWithFlags: (self flags maskClear: PrecisionMask)!

smalltalkAccessError	^smalltalkAccessError!

smalltalkArrayClass	"answer the array suitable for storing data in Smalltalk space"	^SmalltalkArrayClasses at: self sdczIndex!

storeInstancesInSmalltalkSpace	self allInstancesDo: [:e | e storeInSmalltalkSpace]!

triangularMatrix	^self 		findClassWithFlags: ((self flags maskClear: PropertyMask) bitOr: TriangularMask)!

uninitialize
	SessionManager current removeEventsTriggeredFor: self!

unpackedMatrix	"This must be overloaded where it make sense"	^self!

unregisterFlags	"remove reference to a self before being unloaded"	FlagsToClassDictionary removeKey: self flags ifAbsent: []! !
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

at: anInteger 	"prefer double index accessing"	| ir ic |	ir := (anInteger - 1) \\ nrow + 1.	ic := (anInteger - 1) // nrow + 1.	^self rowAt: ir columnAt: ic!

at: anInteger put: aNumber 	"prefer double index accessing"	| ir ic |	ir := (anInteger - 1) \\ nrow + 1.	ic := (anInteger - 1) // nrow + 1.	^self 		rowAt: ir		columnAt: ic		put: aNumber!

columnAt: columnIndex 	| column |	column := self class generalMatrix nrow: nrow ncol: 1.	column at: columnIndex put: (self arrayAt: columnIndex).	^column!

diagonalAt: index 	^index = 0 		ifTrue: [self castToColumn copy]		ifFalse: [self class generalMatrix nrow: (self diagonalSizeAt: index)]!

differenceWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class"	| res |	res := self copy.	res castToColumn 		fill: self arraySize		elementsWithStride: 1		withSelfPlusScalar: -1		timesVector: aLapackMatrix castToColumn		stride: 1.	^res!

eigenValueDecomposition
	^LapackDiagonalEigenDecomposition decompose: self!

eigenValues	^self diagonal!

elementwiseProductWithMatrix: aMatrix 	^(self copy)		setDiagonal: (self castToColumn 					elementwiseProductWithMatrix: aMatrix diagonal);		yourself!

inPlaceScaledByComplex: aComplex 	"only scale diagonal..."	self castToColumn inPlaceScaledByComplex: aComplex!

inPlaceScaledByNumber: aNumber 	"only scale diagonal..."	self castToColumn inPlaceScaledByNumber: aNumber!

isDiagonal	^true!

isHermitian	^self diagonal imaginaryPart isZero!

isLowerTriangular	^true!

isSymmetric	^true!

isUpperTriangular	^true!

lowerTriangle: ind 	"return lower triangle matrix"	^ind >= 0 ifTrue: [self] ifFalse: [self class nrow: nrow ncol: ncol]!

pluDecomposition
	^LapackDiagonalPLUdecomposition decompose: self!

productMatrixTransposeWithColumnVector: aLapackMatrix 	"Product Matrix transposed*vector"	^self productMatrixWithColumnVector: aLapackMatrix!

productMatrixWithColumnVector: aLapackMatrix 	"Product Matrix * vector"	^self castToColumn elementwiseProductWithMatrix: aLapackMatrix!

reciprocal	^self class diagonal: (array collect: [:each | each reciprocal])!

rowAt: rowIndex 	| row |	row := self class generalMatrix nrow: 1 ncol: ncol.	row at: rowIndex put: (self arrayAt: rowIndex).	^row!

rowAt: i columnAt: j 	^i = j ifTrue: [self arrayAt: i] ifFalse: [0]!

rowAt: i columnAt: j put: aNumber 	^i = j 		ifTrue: [self arrayAt: i put: aNumber]		ifFalse: 			[aNumber isZero 				ifTrue: 					["do nothing, only diagonal is stored"					]				ifFalse: 					[| coerced |					coerced := self class generalMatrix diagonal: self diagonal.					coerced 						rowAt: i						columnAt: j						put: aNumber.					self become: coerced.					aNumber]]!

sumWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class"	| res |	res := self copy.	res castToColumn 		fill: self arraySize		elementsWithStride: 1		withSelfPlusScalar: 1		timesVector: aLapackMatrix castToColumn		stride: 1.	^res!

upperTriangle: ind 	"return upper triangle matrix"	^ind <= 0 ifTrue: [self] ifFalse: [self class nrow: nrow ncol: ncol]! !
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

allocateNrow: nr ncol: nc 	"Only allocate storage for the diagonal.	A diagonal matrix should be squared"	nr = nc ifFalse: [^self generalMatrix allocateNrow: nr ncol: nc].	^self basicNew 		setArray: (Array new: nr)		nrow: nr		ncol: nc! !
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

coerceToGeneralMatrix	^self!

diagonalAt: index 	| diag |	index >= 0 		ifTrue: 			[diag := self class nrow: (self diagonalSizeAt: index).			diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index * nrow)				sourceIncrement: nrow + 1				destIncrement: 1]		ifFalse: 			[diag := self class nrow: (self diagonalSizeAt: index).			diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index negated)				sourceIncrement: nrow + 1				destIncrement: 1].	^diag!

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

i	| res |	res := self class complexMatrix nrow: nrow ncol: ncol.	^self class isComplexMatrix 		ifTrue: 			[| selfReal selfImag resReal resImag |			selfReal := self castToRealWithArrayOffsetBy: 0.			selfImag := self castToRealWithArrayOffsetBy: 1.			resReal := res castToRealWithArrayOffsetBy: 0.			resImag := res castToRealWithArrayOffsetBy: 1.			resImag 				copy: self arraySize				elementsFrom: selfReal				sourceIncrement: 2				destIncrement: 2.			resReal 				copy: self arraySize				elementsFrom: selfImag				sourceIncrement: 2				destIncrement: 2.			resReal 				scale: self arraySize				elementsBy: -1				increment: 2.			res]		ifFalse: 			[(res castToRealWithArrayOffsetBy: 1) 				copy: self arraySize				elementsFrom: self				sourceIncrement: 1				destIncrement: 2.			res]!

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

reciprocal	nrow = ncol ifFalse: [self error: 'should be square'].	^[self pluDecomposition inverse] on: Error		do: [:exc | exc return: super reciprocal]!

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

asPackedMatrix	self subclassResponsibility!

asUnpackedMatrix	self subclassResponsibility!

at: anInteger 	"prefer double index accessing"	| ir ic |	ir := (anInteger - 1) \\ nrow + 1.	ic := (anInteger - 1) // nrow + 1.	^self rowAt: ir columnAt: ic!

at: anInteger put: aNumber 	"prefer double index accessing"	| ir ic |	ir := (anInteger - 1) \\ nrow + 1.	ic := (anInteger - 1) // nrow + 1.	^self 		rowAt: ir		columnAt: ic		put: aNumber!

beLower	uplo := LowerMask!

beUpper	uplo := UpperMask!

coerceToDoublePrecisionComplexMatrix	| res |	res := super coerceToDoublePrecisionComplexMatrix 				castTo: self class doublePrecisionMatrix complexMatrix.	self isUpper ifTrue: [res beUpper] ifFalse: [res beLower].	^res!

coerceToDoublePrecisionMatrix	| res |	res := super coerceToDoublePrecisionMatrix 				castTo: self class doublePrecisionMatrix.	self isUpper ifTrue: [res beUpper] ifFalse: [res beLower].	^res!

coerceToSinglePrecisionComplexMatrix	| res |	res := super coerceToSinglePrecisionComplexMatrix 				castTo: self class singlePrecisionMatrix complexMatrix.	self isUpper ifTrue: [res beUpper] ifFalse: [res beLower].	^res!

coerceToSinglePrecisionMatrix	| res |	res := super coerceToSinglePrecisionMatrix 				castTo: self class singlePrecisionMatrix.	self isUpper ifTrue: [res beUpper] ifFalse: [res beLower].	^res!

complementOfRow: i column: j 	self subclassResponsibility!

complementOfRow: i column: j put: aNumber 	self subclassResponsibility!

differenceFromMatrix: aMatrix	^self asGeneralMatrix differenceFromMatrix: aMatrix!

isLower	^(uplo bitAnd: BothUpperLowerMask) = LowerMask!

isUpper	^(uplo bitAnd: BothUpperLowerMask) = UpperMask!

productFromMatrix: aMatrix	^self asGeneralMatrix productFromMatrix: aMatrix!

rowAt: i columnAt: j 	self subclassResponsibility!

rowAt: i columnAt: j put: aNumber 	self subclassResponsibility!

sumFromMatrix: aMatrix	^self asGeneralMatrix sumFromMatrix: aMatrix! !
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

columns: anArrayOfColumns 	| aMatrix sizes |	(anArrayOfColumns isSequenceable 		and: [anArrayOfColumns allSatisfy: [:aColumn | aColumn isSequenceable]]) 			ifFalse: 				[self 					error: 'a collection of columns should be provided for columns: creation message'].	sizes := anArrayOfColumns collect: [:e | e size].	sizes = (1 to: anArrayOfColumns size) 		ifTrue: 			[| res nr |			nr := anArrayOfColumns size.			res := self allocateShape: nr.			res beUpper.			1 to: nr				do: 					[:jc | 					1 to: jc						do: 							[:ir | 							res 								rowAt: ir								columnAt: jc								put: ((anArrayOfColumns at: jc) at: ir)]].			^res].	sizes = (anArrayOfColumns size to: 1 by: -1) 		ifTrue: 			[| res nr |			nr := anArrayOfColumns size.			res := self allocateShape: nr.			res beLower.			1 to: nr				do: 					[:jc | 					jc to: nr						do: 							[:ir | 							res 								rowAt: ir								columnAt: jc								put: ((anArrayOfColumns at: jc) at: ir -jc + 1)]].			^res].	sizes asSet size = 1 		ifFalse: 			[self 				error: 'a collection of columns of equal size should be provided for columns: creation message'].	aMatrix := self generalMatrix columns: anArrayOfColumns.	^self fromGeneralMatrix: aMatrix!

eye: aShape 	^(super eye: aShape)		beUpper;		yourself!

fromGeneralMatrix: aMatrix 	self subclassResponsibility!

fromSequence: aRawArray nrow: nr ncol: nc 	self fromGeneralMatrix: (self generalMatrix 				fromSequence: aRawArray				nrow: nr				ncol: nc)!

packedMatrix	^self findClassWithFlags: ((self flags maskClear: StorageMask) bitOr: PackedStorageMask)!

rows: anArrayOfRows 	| aMatrix sizes |	(anArrayOfRows isSequenceable 		and: [anArrayOfRows allSatisfy: [:aRow | aRow isSequenceable]]) 			ifFalse: 				[self 					error: 'a collection of rows should be provided for rows: creation message'].	sizes := anArrayOfRows collect: [:e | e size].	sizes = (1 to: anArrayOfRows size) 		ifTrue: 			[| res nr |			nr := anArrayOfRows size.			res := self allocateShape: nr.			res beLower.			1 to: nr				do: 					[:jc | 					jc to: nr						do: 							[:ir | 							res 								rowAt: ir								columnAt: jc								put: ((anArrayOfRows at: ir) at: jc)]].			^res].	sizes = (anArrayOfRows size to: 1 by: -1) 		ifTrue: 			[| res nr |			nr := anArrayOfRows size.			res := self allocateShape: nr.			res beUpper.			1 to: nr				do: 					[:jc | 					1 to: jc						do: 							[:ir | 							res 								rowAt: ir								columnAt: jc								put: ((anArrayOfRows at: ir) at: jc - ir + 1)]].			^res].	sizes asSet size = 1 		ifFalse: 			[self 				error: 'a collection of columns of equal size should be provided for columns: creation message'].	aMatrix := self generalMatrix rows: anArrayOfRows.	^self fromGeneralMatrix: aMatrix!

unpackedMatrix	^self findClassWithFlags: ((self flags maskClear: StorageMask) bitOr: FullStorageMask)! !
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

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
!LapackCDIMatrix class categoriesFor: #initialize!public! !
!LapackCDIMatrix class categoriesFor: #initializeFlags!public! !

LapackDDIMatrix guid: (GUID fromString: '{FA75D423-D2D3-4338-A1A8-5E62C7649084}')!
LapackDDIMatrix comment: 'LapackDDIMatrix handles Diagonal Matrices composed of double precision real numbers.
See superclass for more information.'!
!LapackDDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDDIMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDDIMatrix class categoriesFor: #initialize!public! !
!LapackDDIMatrix class categoriesFor: #initializeFlags!public! !

LapackSDIMatrix guid: (GUID fromString: '{81497E87-7EBE-4054-8AC2-63E5685FD155}')!
LapackSDIMatrix comment: 'LapackSDIMatrix handles Diagonal Matrices composed of single precision real numbers.
See superclass for more information.'!
!LapackSDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSDIMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSDIMatrix class categoriesFor: #initialize!public! !
!LapackSDIMatrix class categoriesFor: #initializeFlags!public! !

LapackZDIMatrix guid: (GUID fromString: '{AE16E502-6681-417B-9E99-8137C0704F6A}')!
LapackZDIMatrix comment: 'LapackZDIMatrix handles Diagonal Matrices composed of double precision complex numbers.
See superclass for more information.'!
!LapackZDIMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZDIMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: DiagonalMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
!LapackZDIMatrix class categoriesFor: #initialize!public! !
!LapackZDIMatrix class categoriesFor: #initializeFlags!public! !

LapackCGEMatrix guid: (GUID fromString: '{53D6F2C7-A470-4EF1-ADC3-4C64133F46D2}')!
LapackCGEMatrix comment: 'LapackCGEMatrix handles General Matrices (dense storage) composed of single precision complex numbers
See superclass for more information.'!
!LapackCGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCGEMatrix methodsFor!

productFromFloat: aFloat 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aFloat!

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

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: GeneralMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
!LapackCGEMatrix class categoriesFor: #initialize!public! !
!LapackCGEMatrix class categoriesFor: #initializeFlags!public! !

LapackDGEMatrix guid: (GUID fromString: '{0BF9D151-CE10-4D9C-93E6-FFF9A3D86795}')!
LapackDGEMatrix comment: 'LapackDGEMatrix handles General Matrices (dense storage) composed of double precision real numbers
See superclass for more information.'!
!LapackDGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDGEMatrix methodsFor!

productFromDouble: aDouble 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aDouble!

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

initialize	self initializeClassInstVars!

initializeFlags	flags :=  (FullStorageMask bitOr: GeneralMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDGEMatrix class categoriesFor: #initialize!public! !
!LapackDGEMatrix class categoriesFor: #initializeFlags!public! !

LapackSGEMatrix guid: (GUID fromString: '{BB5A2547-1EFA-4A0A-BEED-0B84B2C9A407}')!
LapackSGEMatrix comment: 'LapackSGEMatrix handles General Matrices (dense storage) composed of single precision real numbers
See superclass for more information.'!
!LapackSGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSGEMatrix methodsFor!

productFromFloat: aFloat 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aFloat!

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

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: GeneralMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSGEMatrix class categoriesFor: #initialize!public! !
!LapackSGEMatrix class categoriesFor: #initializeFlags!public! !

LapackZGEMatrix guid: (GUID fromString: '{A3FAD776-8863-4B39-8502-30726A321D32}')!
LapackZGEMatrix comment: 'LapackZGEMatrix handles General Matrices (dense storage) composed of double precision complex numbers
See superclass for more information.'!
!LapackZGEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZGEMatrix methodsFor!

productFromDouble: aDouble 	"FAST: avoid super dispatching technique"	^self scaledByNumber: aDouble!

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

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: GeneralMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
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

asPackedMatrix	^self coerceToPackedMatrix!

asUnpackedMatrix	^self!

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

coerceToUnpackedMatrix	^self!

complementOfRow: ir column: jc put: aNumber 	"handle triangular/hermitian property	Implementation Notes : cannot use changeClassTo: because uplo instVar"	^aNumber = (self complementOfRow: ir column: jc) 		ifTrue: [array at: (ir - 1) * nrow + jc put: aNumber]		ifFalse: 			[| coerced |			self fillOtherTriangle.			coerced := self castTo: self class generalMatrix.			coerced 				rowAt: jc				columnAt: ir				put: aNumber.			self become: coerced.			aNumber]!

fillOtherTriangle	self subclassResponsibility!

rowAt: i columnAt: j 	^self isUpper 		ifTrue: 			[i <= j 				ifTrue: [self arrayAt: (j - 1) * nrow + i]				ifFalse: [self complementOfRow: j column: i]]		ifFalse: 			[j <= i 				ifTrue: [self arrayAt: (j - 1) * nrow + i]				ifFalse: [self complementOfRow: j column: i]]!

rowAt: i columnAt: j put: aNumber 	^self isUpper 		ifTrue: 			[i <= j 				ifTrue: [array at: (j - 1) * nrow + i put: aNumber]				ifFalse: 					[self 						complementOfRow: j						column: i						put: aNumber]]		ifFalse: 			[j <= i 				ifTrue: [array at: (j - 1) * nrow + i put: aNumber]				ifFalse: 					[self 						complementOfRow: j						column: i						put: aNumber]]! !
!LapackUnpackedMatrix categoriesFor: #asPackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #asUnpackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #coerceToPackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #coerceToUnpackedMatrix!public! !
!LapackUnpackedMatrix categoriesFor: #complementOfRow:column:put:!public! !
!LapackUnpackedMatrix categoriesFor: #fillOtherTriangle!public! !
!LapackUnpackedMatrix categoriesFor: #rowAt:columnAt:!public! !
!LapackUnpackedMatrix categoriesFor: #rowAt:columnAt:put:!public! !

!LapackUnpackedMatrix class methodsFor!

allocateNrow: nr ncol: nc 	^nr = nc 		ifTrue: [(super allocateNrow: nr ncol: nc) beUpper; yourself]		ifFalse: [self generalMatrix allocateNrow: nr ncol: nc]! !
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

beBothUpperLower	"This means that both upper and lower triangles are set"	uplo := BothUpperLowerMask!

coerceToGeneralMatrix	"Implementation Notes: we need to set both upper and lower"	| a |	self fillOtherTriangle.	a := self class generalMatrix allocateNrow: nrow ncol: ncol.	a 		copy: self size		elementsFrom: self		sourceIncrement: 1		destIncrement: 1.	^a!

coercingFromComplexNumber: aComplex do: aBlock 	"a hermitian matrix will loose its property when	multiplied by/added to/divided by aComplex"	^self asGeneralMatrix coercingFromComplexNumber: aComplex do: aBlock!

complementOfRow: i column: j 	^(self rowAt: i columnAt: j) conjugated!

diagonalAt: index 	| diag |	((index > 0 and: [self isLower]) or: [index < 0 and: [self isUpper]]) 		ifTrue: [^(self diagonalAt: index negated) conjugated].	diag := self class nrow: (self diagonalSizeAt: index).	index >= 0 		ifTrue: 			[diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index * nrow)				sourceIncrement: nrow + 1				destIncrement: 1]		ifFalse: 			[diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index negated)				sourceIncrement: nrow + 1				destIncrement: 1].	^diag!

differenceWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class.	but are they both upper / lower ?"	| res |	((aLapackMatrix isUpper and: [self isLower]) 		or: [aLapackMatrix isLower and: [self isUpper]]) 			ifTrue: [aLapackMatrix fillOtherTriangle].	res := self copy.	res 		fill: self size		elementsWithStride: 1		withSelfPlusScalar: -1		timesVector: aLapackMatrix		stride: 1.	aLapackMatrix isBothUpperLower 		ifTrue: 			[self isUpper 				ifTrue: [res beUpper]				ifFalse: [self isLower ifTrue: [res beLower]]].	^res!

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

fillLowerTriangleNaive	"fill lower triangle.	naive element by element algorithm"	self class isComplexMatrix 		ifTrue: 			[2 to: nrow do: [:i | 				1 to: nrow - 1 do: [:j | 					self 						rowAt: i						columnAt: j						put: (self rowAt: j columnAt: i) conjugated]]]		ifFalse: 			[2 to: nrow do: [:i | 				1 to: nrow - 1 do: [:j | 					self 						rowAt: i						columnAt: j						put: (self rowAt: j columnAt: i)]]].	self beBothUpperLower!

fillOtherTriangle	self isBothUpperLower ifTrue: [^self].	self isUpper 		ifTrue: 			["fill lower triangle"			nrow < 10 				ifTrue: [self fillLowerTriangleNaive]				ifFalse: [self fillLowerTriangleLapack]]		ifFalse: 			["fill upper triangle"			nrow < 10 				ifTrue: [self fillUpperTriangleNaive]				ifFalse: [self fillUpperTriangleLapack]]!

fillRandNormalWithSeed: seedArray 	"A Hermitian Matrix should have a real diagonal"	super fillRandNormalWithSeed: seedArray.	self class isComplexMatrix 		ifTrue: 			[| diag |			diag := self diagonal.			diag imaginaryPart isZero ifFalse: [self setDiagonal: diag realPart]]!

fillRandUniformWithSeed: seedArray 	"A Hermitian Matrix should have a real diagonal"	super fillRandUniformWithSeed: seedArray.	self class isComplexMatrix 		ifTrue: 			[| diag |			diag := self diagonal.			diag imaginaryPart isZero ifFalse: [self setDiagonal: diag realPart]]!

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

fillUpperTriangleNaive	"fill upper triangle.	naive element by element algorithm"	self class isComplexMatrix 		ifTrue: 			[2 to: nrow do: [:i | 				1 to: nrow - 1 do: [:j | 					self 						rowAt: j						columnAt: i						put: (self rowAt: i columnAt: j) conjugated]]]		ifFalse: 			[2 to: nrow do: [:i | 				1 to: nrow - 1 do: [:j | 					self 						rowAt: j						columnAt: i						put: (self rowAt: i columnAt: j)]]].	self beBothUpperLower!

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

isBothUpperLower	^uplo = BothUpperLowerMask!

isHermitian	^true!

isSymmetric	^self imaginaryPart isZero!

lowerTriangle: ind 	"return lower triangle matrix"	(ind <= 0 and: [self isLower]) ifFalse: [self fillOtherTriangle].	^(self castTo: self class generalMatrix) lowerTriangle: ind!

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

productMatrixTransposeWithColumnVector: aLapackMatrix 	"the vector must have conforming precision and complexity	BLAS do not handle transpose of hermitian..."	^self transposed productMatrixWithColumnVector: aLapackMatrix!

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

reciprocal	^[self pluDecomposition inverse] on: Error		do: [:exc | exc return: super reciprocal]!

scaledByComplex: aComplex 	"a Hermitian Matrix when scaled by complex number is no more hermitian... It is general"	aComplex imaginaryPart isZero ifTrue: [^self scaledByNumber: aComplex real].	^self asGeneralMatrix scaledByComplex: aComplex!

setArray: anArray nrow: nr ncol: nc 	self beBothUpperLower.	super 		setArray: anArray		nrow: nr		ncol: nc!

singularValueDecomposition	self fillOtherTriangle.	^super singularValueDecomposition!

sumWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class.	but are they both upper / lower ?"	| res |	((aLapackMatrix isUpper and: [self isLower]) 		or: [aLapackMatrix isLower and: [self isUpper]]) 			ifTrue: [self fillOtherTriangle].	res := self copy.	res 		fill: self size		elementsWithStride: 1		withSelfPlusScalar: 1		timesVector: aLapackMatrix		stride: 1.	self isBothUpperLower 		ifTrue: 			[aLapackMatrix isUpper 				ifTrue: [res beUpper]				ifFalse: [aLapackMatrix isLower ifTrue: [res beLower]]].	^res!

transposeConjugated	^self!

transposed	^self conjugated!

upperTriangle: ind 	"return upper triangle matrix"	(ind >= 0 and: [self isUpper]) ifFalse: [self fillOtherTriangle].	^(self castTo: self class generalMatrix) upperTriangle: ind! !
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

fromGeneralMatrix: aMatrix 	| new |	^aMatrix isHermitian 		ifTrue: 			[new := aMatrix castTo: self.			new beBothUpperLower.			new]		ifFalse: [aMatrix]!

nrow: nr ncol: nc withAll: aNumber 	"cannot be hermitian if not square or complex"	| aMatrix |	aMatrix := self generalMatrix 				nrow: nr				ncol: nc				withAll: aNumber.	(((aNumber isKindOf: Complex) not or: [aNumber imaginaryPart isZero]) "aNumber toMinimumGenerality isComplex not" and: [aMatrix isSquare]) 		ifTrue: 			[aMatrix := aMatrix castTo: self.			aMatrix beUpper].	^aMatrix! !
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
		lda: nrow] 
			on: Error
			do: [:exc | exc return: super reciprocal]!

sumWithLapackMatrix: aLapackMatrix 	"at this point, matrices should have same class.	but are they both upper / lower ?"	| res |	res := self copy.	res 		fill: self size		elementsWithStride: 1		withSelfPlusScalar: 1		timesVector: aLapackMatrix		stride: 1.	(self isUpper xor: aLapackMatrix isUpper) 		ifTrue: [res := res castTo: self class generalMatrix].	^res!

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

fromGeneralMatrix: aMatrix 	| new |	^aMatrix isLowerTriangular 		ifTrue: 			[new := aMatrix castTo: self.			new beLower.			new]		ifFalse: 			[aMatrix isUpperTriangular 				ifTrue: 					[new := aMatrix castTo: self.					new beUpper.					new]				ifFalse: [aMatrix]]!

nrow: nr ncol: nc withAll: aNumber 	"cannot be triangular if not all zero"	| aMatrix |	aMatrix := self generalMatrix 				nrow: nr				ncol: nc				withAll: aNumber.	(aNumber isZero and: [aMatrix isSquare])		ifTrue: 			[aMatrix := aMatrix castTo: self.			aMatrix beUpper].	^aMatrix! !
!LapackUnpackedTriangularMatrix class categoriesFor: #fromGeneralMatrix:!public! !
!LapackUnpackedTriangularMatrix class categoriesFor: #nrow:ncol:withAll:!public! !

LapackCHEMatrix guid: (GUID fromString: '{9369B851-89B8-48E0-B64E-E442856FCFEF}')!
LapackCHEMatrix comment: 'LapackCHEMatrix handles Hermitian matrices composed of single precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackCHEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCHEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags :=  (FullStorageMask bitOr: HermitianMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
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

complementOfRow: i column: j 	^self rowAt: i columnAt: j!

diagonalAt: index 	| diag |	((index > 0 and: [self isLower]) or: [index < 0 and: [self isUpper]]) 		ifTrue: [^self diagonalAt: index negated].	diag := self class nrow: (self diagonalSizeAt: index).	index >= 0 		ifTrue: 			[diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index * nrow)				sourceIncrement: nrow + 1				destIncrement: 1]		ifFalse: 			[diag 				copy: diag size				elementsFrom: (self withArrayOffsetBy: index negated)				sourceIncrement: nrow + 1				destIncrement: 1].	^diag!

isSymmetric	^true!

transposed	^self! !
!LapackUnpackedSymmetricRealMatrix categoriesFor: #complementOfRow:column:!public! !
!LapackUnpackedSymmetricRealMatrix categoriesFor: #diagonalAt:!public! !
!LapackUnpackedSymmetricRealMatrix categoriesFor: #isSymmetric!public! !
!LapackUnpackedSymmetricRealMatrix categoriesFor: #transposed!public! !

!LapackUnpackedSymmetricRealMatrix class methodsFor!

fromGeneralMatrix: aMatrix 	| new |	^aMatrix isSymmetric 		ifTrue: 			[new := aMatrix castTo: self.			new beBothUpperLower.			new]		ifFalse: [aMatrix]! !
!LapackUnpackedSymmetricRealMatrix class categoriesFor: #fromGeneralMatrix:!public! !

LapackZHEMatrix guid: (GUID fromString: '{C685C67E-F4E6-49BD-85A7-25CA99E41526}')!
LapackZHEMatrix comment: 'LapackZHEMatrix handles Hermitian matrices composed of double precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackZHEMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZHEMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: HermitianMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
!LapackZHEMatrix class categoriesFor: #initialize!public! !
!LapackZHEMatrix class categoriesFor: #initializeFlags!public! !

LapackDSYMatrix guid: (GUID fromString: '{D1E7D673-A960-4FE4-80E0-4D00D6718580}')!
LapackDSYMatrix comment: 'LapackDSYMatrix handles Symmetric matrices composed of double precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackDSYMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDSYMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: HermitianMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDSYMatrix class categoriesFor: #initialize!public! !
!LapackDSYMatrix class categoriesFor: #initializeFlags!public! !

LapackSSYMatrix guid: (GUID fromString: '{FB2A8C38-E9ED-4CA6-A61B-7BDB05454AD5}')!
LapackSSYMatrix comment: 'LapackSSYMatrix handles Symmetric matrices composed of single precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackSSYMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSSYMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: HermitianMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSSYMatrix class categoriesFor: #initialize!public! !
!LapackSSYMatrix class categoriesFor: #initializeFlags!public! !

LapackCTRMatrix guid: (GUID fromString: '{48F592C7-F8FB-432D-86F5-512E1F736B24}')!
LapackCTRMatrix comment: 'LapackCTRMatrix handles Triangular matrices composed of single precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackCTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackCTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (ComplexMask bitOr: SinglePrecisionMask)! !
!LapackCTRMatrix class categoriesFor: #initialize!public! !
!LapackCTRMatrix class categoriesFor: #initializeFlags!public! !

LapackDTRMatrix guid: (GUID fromString: '{6D56DE32-4578-4BD5-A569-79D5ED7CDA38}')!
LapackDTRMatrix comment: 'LapackDTRMatrix handles Triangular matrices composed of double precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackDTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackDTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (RealMask bitOr: DoublePrecisionMask)! !
!LapackDTRMatrix class categoriesFor: #initialize!public! !
!LapackDTRMatrix class categoriesFor: #initializeFlags!public! !

LapackSTRMatrix guid: (GUID fromString: '{DDF8C48E-1CDA-4A0B-A114-94006C8ADDF2}')!
LapackSTRMatrix comment: 'LapackSTRMatrix handles Triangular matrices composed of single precision real numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackSTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackSTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (RealMask bitOr: SinglePrecisionMask)! !
!LapackSTRMatrix class categoriesFor: #initialize!public! !
!LapackSTRMatrix class categoriesFor: #initializeFlags!public! !

LapackZTRMatrix guid: (GUID fromString: '{00C44E12-18C2-498F-A82B-E9656E2C28F6}')!
LapackZTRMatrix comment: 'LapackZTRMatrix handles Triangular matrices composed of double precision complex numbers
Dense storage is allocated but only half is used.
See superclass for more information.
'!
!LapackZTRMatrix categoriesForClass!Smallapack-Matrix! !
!LapackZTRMatrix class methodsFor!

initialize	self initializeClassInstVars!

initializeFlags	flags := (FullStorageMask bitOr: TriangularMask) 				bitOr: (ComplexMask bitOr: DoublePrecisionMask)! !
!LapackZTRMatrix class categoriesFor: #initialize!public! !
!LapackZTRMatrix class categoriesFor: #initializeFlags!public! !

"Binary Globals"!

