"Filed out from Dolphin Smalltalk 7"!

LapackMatrix subclass: #LapackHalfMatrix
	instanceVariableNames: 'uplo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

