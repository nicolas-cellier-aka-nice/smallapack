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

- aMatrix 	^aMatrix differenceFromMatrix: self!

* aMatrix 	^aMatrix productFromMatrix: self!

, aMatrix 	^aMatrix concatColumnsFromMatrix: self!

,, aMatrix 	^aMatrix concatRowsFromMatrix: self!

+ aMatrix 	^aMatrix sumFromMatrix: self!

= aMatrix 	(aMatrix respondsToArithmetic 		and: [aMatrix isMatrix and: [nrow = aMatrix nrow and: [ncol = aMatrix ncol]]]) 			ifFalse: [^false].	1 to: self size		do: [:i | (self at: i) = (aMatrix at: i) ifFalse: [^false]].	^true!

abs	^self collect: [:e | e abs]!

absMax	"Note: for matrices, this does not have norm properties"	^self maxOf: [:e | e abs]!

adaptToComplex: rcvr andSend: selector 	"If I am involved in arithmetic with a Complex."	selector = #* ifTrue: [^self asComplexMatrix scaledByComplex: rcvr].	^self asComplexMatrix collect:  [:each | rcvr perform: selector with: each]!

adaptToFloat: rcvr andCompare: selector 	"If I am involved in comparison with a Float."
	^self adaptToNumber: rcvr andSend: selector !

adaptToFloat: rcvr andSend: selector 	"If I am involved in arithmetic with a Float."	^self adaptToNumber: rcvr andSend: selector !

adaptToFraction: rcvr andSend: selector 	"If I am involved in arithmetic with a Fraction."	^self adaptToNumber: rcvr andSend: selector !

adaptToInteger: rcvr andCompare: selector 
	"If I am involved in comparison with an Integer."

	^self adaptToNumber: rcvr andSend: selector!

adaptToInteger: rcvr andSend: selector 	"If I am involved in arithmetic with an Integer."	^self adaptToNumber: rcvr andSend: selector !

adaptToNumber: rcvr andSend: selector 	"If I am involved in arithmetic with a Number."	selector = #* ifTrue: [^self scaledByNumber: rcvr].	^self collect:  [:each | rcvr perform: selector with: each]!

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

asColumnMatrix	^ncol = 1 		ifTrue: [self]		ifFalse: 			[| res |			res := self class nrow: self size.			1 to: self size do: [:i | res at: i put: (self at: i)].
			res]!

asDoubleComplexMatrix	^self asDoublePrecisionComplexMatrix!

asDoubleMatrix	^self asDoublePrecisionMatrix!

asDoublePrecisionComplexMatrix	| res |	res := LapackZGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asDoubleComplex].	^res!

asDoublePrecisionMatrix	| res |	res := LapackDGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asFloat].	^res!

asFloatComplexMatrix	^self asSinglePrecisionComplexMatrix!

asFloatMatrix	^self asSinglePrecisionMatrix!

asMatrix	^self!

asOrderedCollection	| anOrderedCollection |	anOrderedCollection := OrderedCollection new: self size.	self do: [:each | anOrderedCollection addLast: each].	^anOrderedCollection!

asRowMatrix	^nrow = 1 		ifTrue: [self]		ifFalse: 			[| res |			res := self class ncol: self size.			1 to: self size do: [:i | res at: i put: (self at: i)].
			res]!

asSet	| aSet |	aSet := Set new.	self do: [:each | aSet add: each].	^aSet!

asSinglePrecisionComplexMatrix	| res |	res := LapackCGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asFloatComplex].	^res!

asSinglePrecisionMatrix	| res |	res := LapackSGEMatrix allocateNrow: nrow ncol: ncol.	1 to: self size do: [:i | res at: i put: (self at: i) asFloat].	^res!

asSortedCollection	| aSortedCollection |	aSortedCollection := SortedCollection new: self size.	aSortedCollection addAll: self.	^aSortedCollection!

asSortedCollection: aBlock 	| aSortedCollection |	aSortedCollection := SortedCollection new: self size.	aSortedCollection sortBlock: aBlock.	aSortedCollection addAll: self.	^aSortedCollection!

at: anInteger	"Access the underlying array	should be overloaded if packed storage is used"	^array at: anInteger!

at: rowIndex at: columnIndex 	"Squeak compatible"	^self rowAt: rowIndex columnAt: columnIndex!

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

atIntervalFrom: interStart to: interStop by: interStep 	"Extract a copy of a subinterval of self"	| sz result |	sz := (interStop - interStart) // interStep + 1 max: 0.	result := nrow = 1 				ifTrue: [self class ncol: sz]				ifFalse: [ncol = 1 ifTrue: [self class nrow: sz] ifFalse: [Array new: sz]].	1 to: sz		do: [:i | result at: i put: (self at: interStart + ((i - 1) * interStep))].	^result!

atPoint: aPoint 	"some implementations use Points to access 2D tables"	^self rowAt: aPoint x columnAt: aPoint y!

atPoint: aPoint put: aNumber	"some implementations use Points to access 2D tables"	^self rowAt: aPoint x columnAt: aPoint y put: aNumber!

atRow: rowIndex 	"synonym"	^self rowAt: rowIndex!

atRow: rowIndex column: columnIndex 	"synonym"	^self rowAt: rowIndex columnAt: columnIndex!

atRow: rowIndex column: columnIndex put: aNumber 	"synonym"	^self 		rowAt: rowIndex		columnAt: columnIndex		put: aNumber!

atRow: colIndex put: aCollection 	"Synonym Squeak-Matrix-Compatible"	^self atRow: colIndex putSequence: aCollection!

atRow: rowIndex putAll: aNumber 	"synonym"	^self rowAt: rowIndex putAll: aNumber!

atRow: rowIndex putSequence: aCollection 	"synonym"	^self rowAt: rowIndex putSequence: aCollection!

atRows: rowsIndexCollection 	| res j |	res := self class allocateNrow: rowsIndexCollection size ncol: ncol.	j := 0.	rowsIndexCollection 		do: [:i | res rowAt: (j := j + 1) putSequence: (self rowAt: i)].	^res!

atRows: rowsIndexCollection atColumns: columnIndexCollection
	| res |
	res := self class allocateNrow: rowsIndexCollection size ncol: columnIndexCollection size.
	columnIndexCollection keysAndValuesDo: [:j :jCol |
		rowsIndexCollection  keysAndValuesDo: [:i :iRow |
			res rowAt: i columnAt: j put: (self rowAt: iRow columnAt: jCol)]].
	^res!

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

dotProduct: n elementsIncrement: incx with: aMatrix increment: incy 
	"BLAS primitve xDOT - naive implementation"

	| sx sy |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - n) * incx + 1].
	sy := incy positive ifTrue: [ 1] ifFalse: [(1 - n) * incy +1].
	^(1 to: n) sum: 
			[:i | 
			(self at: (i - 1) * incx + sx) 
				* (aMatrix at: (i - 1) * incy + sy)]!

elementwisePowerFromNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: aNumber ** (res rowAt: ir columnAt: jc)]].	^res!

elementwisePowerWithMatrix: aMatrix 	| res |	(aMatrix nrow = nrow and: [aMatrix ncol = ncol]) 		ifFalse: [self error: 'matrix size mismatch'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) ** (aMatrix rowAt: ir columnAt: jc)]].	^res!

elementwisePowerWithNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) ** aNumber]].	^res!

elementwiseProductFromNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: aNumber * (res rowAt: ir columnAt: jc)]].	^res!

elementwiseProductWithMatrix: aMatrix 	| res |	(aMatrix nrow = nrow and: [aMatrix ncol = ncol]) 		ifFalse: [self error: 'matrix size mismatch'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) * (aMatrix rowAt: ir columnAt: jc)]].	^res!

elementwiseProductWithNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) * aNumber]].	^res!

elementwiseQuotientFromNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: aNumber / (res rowAt: ir columnAt: jc)]].	^res!

elementwiseQuotientWithMatrix: aMatrix 	| res |	(aMatrix nrow = nrow and: [aMatrix ncol = ncol]) 		ifFalse: [self error: 'matrix size mismatch'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) / (aMatrix rowAt: ir columnAt: jc)]].	^res!

elementwiseQuotientWithNumber: aNumber 	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: ir						columnAt: jc						put: (res rowAt: ir columnAt: jc) / aNumber]].	^res!

fill: n elementsWithStride: incy withSelfPlusScalar: alpha timesVector: aMatrix stride: incx 
	"BLAS primitve xAXPY - naive implementation"

	| sx sy |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - n) * incx + 1].
	sy := incy positive ifTrue: [1] ifFalse: [(1 - n) * incy + 1].
	1 to: n
		do: 
			[:i | 
			self at: (i - 1) * incy + sy
				put: (self at: (i - 1) * incy + sy) 
						+ (alpha * (aMatrix at: (i - 1) * incx + sx))]!

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

findMax	"answer the index of the max of all elements  "	| max index |	self isEmpty ifTrue: [^0].	index := 1.	max := self at: 1.	2 to: self size		do: 			[:i | 			| tmp |			(tmp := self at: i) > max 				ifTrue: 					[max := tmp.					index := i]].	^index!

findMaxOf: aBlock 	"answer the index of the max of all elements after applying a Block"	| max index |	self isEmpty ifTrue: [^0].	index := 1.	max := aBlock value: (self at: 1).	2 to: self size		do: 			[:i | 			| tmp |			(tmp := aBlock value: (self at: i)) > max 				ifTrue: 					[max := tmp.					index := i]].	^index!

findMin	| min index |	self isEmpty ifTrue: [^0].	index := 1.	min := self at: 1.	2 to: self size		do: 			[:i | 			| tmp |			(tmp := self at: i) < min 				ifTrue: 					[min := tmp.					index := i]].	^index!

findMinOf: aBlock 	"answer the index of the min of all elements after applying a Block"	| min index |	self isEmpty ifTrue: [^0].	index := 1.	min := aBlock value: (self at: 1).	2 to: self size		do: 			[:i | 			| tmp |			(tmp := aBlock value: (self at: i)) < min 				ifTrue: 					[min := tmp.					index := i]].	^index!

first	^self at: 1!

fromColumn: jStart toColumn: jStop by: jStep	^self atColumns: (jStart to: jStop by: jStep)!

fromRow: iStart toRow: iStop by: iStep	^self atRows: (iStart to: iStop by: iStep)!

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

norm1	^(1 to: ncol) 		maxOf: [:jc | (1 to: nrow) sumOf: [:ir | (self rowAt: ir columnAt: jc) abs]]!

norm2	^self isVector
		ifTrue: [self vectorNorm2]
		ifFalse: [self singularValues max]!

normFrobenius	^self vectorNorm2!

normInfinity	^(1 to: nrow) 		maxOf: [:ir | (1 to: ncol) sumOf: [:jc | (self rowAt: ir columnAt: jc) abs]]!

nrow	^nrow!

nRows	"Synonym Smallpack compatible"	^nrow!

numberOfColumns	^ncol!

numberOfRows	^nrow!

postCopy	array := array copy!

prependColumns: nColumns 	| res |	res := self class nrow: nrow ncol: ncol + nColumns.	res 		copy: nrow		rowsStartingAt: 1		and: ncol		columnsStartingAt: nColumns + 1		from: self.	^res!

prependRows: nRows 	| res |	res := self class nrow: nrow + nRows ncol: ncol.	res 		copy: nrow		rowsStartingAt: nRows + 1		and: ncol		columnsStartingAt: 1		from: self.	^res!

printOn: aStream 	aStream		nextPut: $(;		nextPutAll: self class name;		space;		nextPutAll: #rows:;		space;		nextPutAll: '#('.	1 to: nrow		do: 			[:i | 			aStream				crtab;				nextPutAll: '#('.			1 to: ncol				do: 					[:j | 					(self rowAt: i columnAt: j) printOn: aStream.					aStream space].			aStream nextPut: $)].	aStream		cr;		nextPut: $);		nextPut: $)!

product	| prod |	self isEmpty ifTrue: [^1].	prod := self at: 1.	2 to: self size do: [:i | prod := prod * (self at: i)].	^prod!

product: aBlock 
	"backward compatibility - synonym of productOf:"
	| product |	#deprecated.
	^self productOf: aBlock!

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

productMatrixTransposeWithColumnVector: aVector 	"Matrix transpose * vector	naive algorithm"	| result |	result := self class allocateNrow: ncol ncol: 1.	1 to: ncol		do: 			[:jc | 			result at: jc				put: ((1 to: nrow) 						sumOf: [:ir | (self rowAt: ir columnAt: jc) * (aVector at: ir)])].	^result!

productMatrixWithColumnVector: aVector 	"Matrix * vector	naive algorithm"	| result |	result := self class allocateNrow: nrow ncol: 1.	1 to: nrow		do: 			[:ir | 			result at: ir				put: ((1 to: ncol) 						sumOf: [:jc | (self rowAt: ir columnAt: jc) * (aVector at: jc)])].	^result!

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
	| product |
	self isEmpty ifTrue: [^1].	product := aBlock value: (self at: 1).	2 to: self size		do: [:i | product := product * (aBlock value: (self at: i))].	^product!

productOf: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: 1.			1 to: nrow				do: 					[:ir | 					res at: ir						put: ((1 to: ncol) 								product: [:jc | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: 1 ncol: ncol.			1 to: ncol				do: 					[:jc | 					res at: jc						put: ((1 to: nrow) 								product: [:ir | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

productRowVectorWithColumnVector: aVector 	"Answer the result of operation (a dot product)		leftVector transpose * aVector	where		leftVector transpose = self"	| res |	res := self class allocateNrow: 1 ncol: 1.	res at: 1		put: ((1 to: self size) sumOf: [:i | (self at: i) * (aVector at: i)]).	^res!

productRowVectorWithMatrix: aMatrix 	"Vector * Matrix	naive algorithm"	| result |	result := self class allocateNrow: 1 ncol: aMatrix ncol.	1 to: aMatrix ncol		do: 			[:jc | 			result at: jc				put: ((1 to: aMatrix nrow) 						sumOf: [:ir | (self at: ir) * (aMatrix rowAt: ir columnAt: jc)])].	^result!

realPart	^self collect: [:e | e real]!

replicate: newDimensions
	^self class shape: newDimensions do: [:nr :nc | self replicateNrow: nr timesNcol: nc]!

replicateNrow: nr timesNcol: nc 	"build a Matrix by replicating self in rows and in columns	Analogous to Matlab repmat"	| res |	res := self class allocateNrow: nrow * nr ncol: ncol * nc.	1 to: nc		do: 			[:jc | 			1 to: nr				do: 					[:ir | 					res 						copy: nrow rowsStartingAt: (ir - 1) * nrow + 1						and: ncol columnsStartingAt: (jc - 1) * ncol + 1						from: self]].	^res!

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

scale: n elementsBy: alpha increment: incx 
	"BLAS primitve xSCAL - naive implementation"

	| sx |
	sx := incx positive ifTrue: [1] ifFalse: [(1 - n) * incx + 1].
	1 to: n
		do: 
			[:i | 
			self at: (i - 1) * incx + sx
				put: (self at: (i - 1) * incx + sx) * alpha]!

scaledByComplex: aComplex 	"Answer a copy of self scaled."	aComplex imaginaryPart isZero ifTrue: [^self scaledByNumber: aComplex real].	^self collect: [:each | each * aComplex]!

scaledByNumber: aNumber 	"Answer a copy of self scaled."	^self collect: [:each | each * aNumber]!

setArray: anArray nrow: nr ncol: nc 	array := anArray.	nrow := nr.	ncol := nc!

setDiagonal: aMatrix	"set the diagonal elements"	1 to: aMatrix size do: [:i | self rowAt: i columnAt: i put: (aMatrix at: i)].!

setOffDiagonal: alpha diagonal: beta 	self atAllPut: alpha.	1 to: (nrow min: ncol)		do: 			[:i | 			self 				rowAt: i				columnAt: i				put: beta]!

setToEye	"Initialize diagonal to 1, off diagonal to 0"	self setOffDiagonal: 0 diagonal: 1!

singularValues
	^self subclassResponsibility!

size	^self capacity!

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

subtractFromFloat: aFloat 	^self collect: [:each | aFloat - each]!

subtractFromFraction: aFraction 	^self collect: [:each | aFraction - each]!

subtractFromInteger: anInteger	^self collect: [:each | anInteger - each]!

subtractFromScaledDecimal: aScaledDecimal	^self collect: [:each | aScaledDecimal - each]!

sum	| sum |	self isEmpty ifTrue: [^0].	sum := self at: 1.	2 to: self size do: [:i | sum := sum + (self at: i)].	^sum!

sum: aBlock 
	"backward compatibility - synonym of sumOf:"

	#deprecated.	^self sumOf: aBlock!

sum: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: 1.			1 to: nrow				do: 					[:ir | 					res at: ir						put: ((1 to: ncol) 								sumOf: [:jc | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: 1 ncol: ncol.			1 to: ncol				do: 					[:jc | 					res at: jc						put: ((1 to: nrow) 								sumOf: [:ir | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

sumFromComplex: aComplex 	^self collect: [:each | aComplex + each]!

sumFromDouble: aDouble 	^self collect: [:each | aDouble + each]!

sumFromFixedPoint: aFixedPoint 	^self collect: [:each | aFixedPoint + each]!

sumFromFloat: aFloat 	^self collect: [:each | aFloat + each]!

sumFromFraction: aFraction 	^self collect: [:each | aFraction + each]!

sumFromInteger: anInteger 	^self collect: [:each | anInteger + each]!

sumFromLapackMatrix: aLapackMatrix	^self sumFromMatrix: aLapackMatrix asGeneralMatrix!

sumFromMatrix: aMatrix 	| res |	(nrow = aMatrix nrow and: [ncol = aMatrix ncol]) 		ifFalse: [^self error: 'matrix dimensions are not compatible'].	res := self class allocateNrow: nrow ncol: ncol.	1 to: self size		do: [:i | res at: i put: (aMatrix at: i) + (self at: i)].	^res!

sumOf: aBlock 	| sum |	self isEmpty ifTrue: [^0].	sum := aBlock value: (self at: 1).	2 to: self size do: [:i | sum := sum + (aBlock value: (self at: i))].	^sum!

sumOf: aBlock dimension: aDimension 	| res |	aDimension = 2 		ifTrue: 			[res := self class allocateNrow: nrow ncol: 1.			1 to: nrow				do: 					[:ir | 					res at: ir						put: ((1 to: ncol) 								sum: [:jc | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	aDimension = 1 		ifTrue: 			[res := self class allocateNrow: 1 ncol: ncol.			1 to: ncol				do: 					[:jc | 					res at: jc						put: ((1 to: nrow) 								sum: [:ir | aBlock value: (self rowAt: ir columnAt: jc)])].			^res].	self error: 'UNKNOWN DIMENSION : should be 1 or 2'.	^nil!

swap: r1 at: c1 with: r2 at: c2 	"Squeak compatible protocol"	^self swapRowAt: r1 columnAt: c1 withRowAt: r2 columnAt: c2!

swapColumn: c1 withColumn: c2	"Squeak compatible - avoid direct access to array because subclasses might implement other behaviour	NAIVE implementation"	1 to: nrow do: [:row |		self swapRowAt: row columnAt: c1 withRowAt: row columnAt: c2]!

swapRow: r1 withRow: r2	"Squeak compatible - avoid direct access to array because subclasses might implement other behaviour	NAIVE implementation"	1 to: ncol do: [:col |		self swapRowAt: r1 columnAt: col withRowAt: r2 columnAt: col]!

swapRowAt: r1 columnAt: c1 withRowAt: r2 columnAt: c2 	"Swap two elements.	Avoid direct access to array because subclasses might implement other behaviour"	| tmp |	tmp := self at: r1 at: c1.	self 		at: r1		at: c1		put: (self at: r2 at: c2).	self 		at: r2		at: c2		put: tmp!

transpose	"synonym"	^self transposed!

transposeConjugated	"naive un-optimized algorithm"	| res |	self isRowMatrix ifTrue: [^self asColumnMatrix conjugated].	self isColumnMatrix ifTrue: [^self asRowMatrix conjugated].	res := self class allocateNrow: ncol ncol: nrow.	1 to: ncol		do: 			[:j | 			1 to: nrow				do: 					[:i | 					res 						rowAt: j						columnAt: i						put: (self rowAt: i columnAt: j) conjugated]].	^res!

transposed	"naive un-optimized algorithm"	| res |	self isRowMatrix ifTrue: [^self asColumnMatrix].	self isColumnMatrix ifTrue: [^self asRowMatrix].	res := self class allocateNrow: ncol ncol: nrow.	1 to: ncol		do: 			[:jc | 			1 to: nrow				do: 					[:ir | 					res 						rowAt: jc						columnAt: ir						put: (self rowAt: ir columnAt: jc)]].	^res!

transposedConjugated	"Synonym"	^self transposeConjugated!

upperTriangle	"return upper triangle matrix"	^self upperTriangle: 0!

upperTriangle: ind 	"return upper triangle matrix starting with super-diagonal ind"	| upper |	upper := self copy.	1 to: ncol		do: 			[:j | 			1 to: nrow				do: 					[:i | 					j - ind < i 						ifTrue: 							[upper 								rowAt: i								columnAt: j								put: 0]]].	^upper!

vectorNorm1	^self sumOf: [:e | e abs]!

vectorNorm2	"Naive algorithm - not protected against underflow/overflow"	^(self sumOf: [:e | e abs squared]) sqrt!

vectorNormFrobenius	^self vectorNorm2!

vectorNormInfinity	^self maxOf: [:e | e abs]!

with: aCollection collect: aBlock 	"aCollection must support #at:at: and be at least as large as the receiver.	Squeak compatible message"	^self withIndicesCollect: 			[:each :row :column | 			aBlock value: each value: (aCollection at: row at: column)]!

with: aCollection do: aBlock 	"aCollection must support #at:at: and be at least as large as the receiver.	Squeak compatible message"	self 		withIndicesDo: [:each :row :column | aBlock value: each value: (aCollection at: row at: column)]!

withArrayOffsetBy: aPositiveInteger 	"Return a kind of shallow copy of self pointing on the same array object, but with an offset.	This is ugly like C code but maybe more efficient"	^self class basicNew 		setArray: (array reindexedThrough: (aPositiveInteger + 1 to: self size))		nrow: self size - aPositiveInteger		ncol: 1!

withIndicesCollect: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	| res |	res := self class allocateNrow: nrow ncol: ncol.	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					res 						rowAt: row						columnAt: column						put: (aBlock 								value: (self rowAt: row columnAt: column)								value: row								value: column)]].	^res!

withIndicesDo: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					aBlock 						value: (self rowAt: row columnAt: column)						value: row						value: column]]!

withIndicesInject: start into: aBlock 	"Squeak compatible message	BEWARE this is column-major ordered"	| current |	current := start.	1 to: ncol		do: 			[:column | 			1 to: nrow				do: 					[:row | 					"NOT EFFICIENT AT ALL... CREATE AN ARRAY AT EACH STEP					SHOULD CONSIDER ADDING value:value:value:value: EXTENSION IN VW"					current := aBlock valueWithArguments: (Array 										with: current										with: (self rowAt: row columnAt: column)										with: row										with: column)]].	^current!

zero	^self class nrow: nrow ncol: ncol! !
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

