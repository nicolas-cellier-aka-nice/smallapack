"Filed out from Dolphin Smalltalk 7"!

CMathLibrary subclass: #CBlasLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CBlasLibrary guid: (GUID fromString: '{96613F99-4779-49A0-9458-9E82FF14EFB0}')!
CBlasLibrary comment: ''!
!CBlasLibrary categoriesForClass!Unclassified! !
!CBlasLibrary methodsFor!

asumWithN: N X: X incX: incX 

axpyWithN: N alpha: alpha X: X incX: incX Y: Y incY: incY 

cComplexPointerOn: aComplex 

cElementArgumentOn: aComplex 

cElementPointerOn: aComplex 

colMajor

conjugated

copyWithN: N X: X incX: incX Y: Y incY: incY 

cRealPointerOn: aComplex 

dotcWithN: N X: X incX: incX Y: Y incY: incY 

dotuWithN: N X: X incX: incX Y: Y incY: incY 

dotWithN: N X: X incX: incX Y: Y incY: incY 

gemmWithTransA: TransA TransB: TransB M: M N: N K: K alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 

gemvWithTransA: TransA M: M N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 

gercWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 

geruWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 

gerWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 

hemmWithSide: Side Uplo: Uplo M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 

hemvWithUplo: Uplo N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 

hpmvWithUplo: Uplo N: N alpha: alpha Ap: Ap X: X incX: incX beta: beta Y: Y incY: incY 

left

lower

nonUnit

notTransposed

realScalWithN: N alpha: alpha X: X incX: incX 

right

rowMajor

scalWithN: N alpha: alpha X: X incX: incX 

swapWithN: N X: X incX: incX Y: Y incY: incY 

tpmvWithUplo: Uplo TransA: TransA Diag: Diag N: N Ap: A X: X incX: incX 

transposeConjugated

transposed

trmmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 

trmvWithUplo: Uplo TransA: TransA Diag: Diag N: N A: A lda: lda X: X incX: incX 

trsmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 

unit

upper
!CBlasLibrary categoriesFor: #asumWithN:X:incX:!public! !
!CBlasLibrary categoriesFor: #axpyWithN:alpha:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #cComplexPointerOn:!public! !
!CBlasLibrary categoriesFor: #cElementArgumentOn:!public! !
!CBlasLibrary categoriesFor: #cElementPointerOn:!public! !
!CBlasLibrary categoriesFor: #colMajor!public! !
!CBlasLibrary categoriesFor: #conjugated!public! !
!CBlasLibrary categoriesFor: #copyWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #cRealPointerOn:!public! !
!CBlasLibrary categoriesFor: #dotcWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #dotuWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #dotWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #gemmWithTransA:TransB:M:N:K:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!CBlasLibrary categoriesFor: #gemvWithTransA:M:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!CBlasLibrary categoriesFor: #gercWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!CBlasLibrary categoriesFor: #geruWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!CBlasLibrary categoriesFor: #gerWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!CBlasLibrary categoriesFor: #hemmWithSide:Uplo:M:N:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!CBlasLibrary categoriesFor: #hemvWithUplo:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!CBlasLibrary categoriesFor: #hpmvWithUplo:N:alpha:Ap:X:incX:beta:Y:incY:!public! !
!CBlasLibrary categoriesFor: #left!public! !
!CBlasLibrary categoriesFor: #lower!public! !
!CBlasLibrary categoriesFor: #nonUnit!public! !
!CBlasLibrary categoriesFor: #notTransposed!public! !
!CBlasLibrary categoriesFor: #realScalWithN:alpha:X:incX:!public! !
!CBlasLibrary categoriesFor: #right!public! !
!CBlasLibrary categoriesFor: #rowMajor!public! !
!CBlasLibrary categoriesFor: #scalWithN:alpha:X:incX:!public! !
!CBlasLibrary categoriesFor: #swapWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #tpmvWithUplo:TransA:Diag:N:Ap:X:incX:!public! !
!CBlasLibrary categoriesFor: #transposeConjugated!public! !
!CBlasLibrary categoriesFor: #transposed!public! !
!CBlasLibrary categoriesFor: #trmmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!CBlasLibrary categoriesFor: #trmvWithUplo:TransA:Diag:N:A:lda:X:incX:!public! !
!CBlasLibrary categoriesFor: #trsmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!CBlasLibrary categoriesFor: #unit!public! !
!CBlasLibrary categoriesFor: #upper!public! !

!CBlasLibrary class methodsFor!

fileName
	"Answer the host system file name for the library"

	^SmallapackSettings cblasLibraryName! !
!CBlasLibrary class categoriesFor: #fileName!public! !
