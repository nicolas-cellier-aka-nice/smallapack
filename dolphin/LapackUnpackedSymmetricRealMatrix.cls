"Filed out from Dolphin Smalltalk 7"!

LapackUnpackedHermitianMatrix subclass: #LapackUnpackedSymmetricRealMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

