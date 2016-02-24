"Filed out from Dolphin Smalltalk 7"!

LapackHalfMatrix subclass: #LapackUnpackedMatrix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

