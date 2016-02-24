"Filed out from Dolphin Smalltalk 7"!

CMathLibrary subclass: #BlasLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasLibrary guid: (GUID fromString: '{DB5BFA89-AF2A-4A02-8FA0-365333BA4790}')!
BlasLibrary comment: ''!
!BlasLibrary categoriesForClass!Unclassified! !
!BlasLibrary methodsFor!

asumWithn: n x: x incx: incx 

asumWithN: N X: X incX: incX 

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy 

axpyWithN: N alpha: alpha X: X incX: incX Y: Y incY: incY 

cComplexPointerOn: aComplex 

cElementPointerOn: aComplex 

copyWithn: n x: x incx: incx y: y incy: incy 

copyWithN: N X: X incX: incX Y: Y incY: incY 

cRealPointerOn: aComplex 

dotcWithn: n x: x incx: incx y: y incy: incy 

dotcWithN: N X: X incX: incX Y: Y incY: incY 

dotuWithn: n x: x incx: incx y: y incy: incy 

dotuWithN: N X: X incX: incX Y: Y incY: incY 

dotWithN: N X: X incX: incX Y: Y incY: incY 

gemmWithTransA: TransA TransB: TransB M: M N: N K: K alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 

gemmWithtransa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: ltransa length: ltransb 

gemvWithtrans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: l 

gemvWithTransA: TransA M: M N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 

gercWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

gercWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 

geruWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

geruWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 

gerWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 

hemmWithSide: Side Uplo: Uplo M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 

hemmWithside: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: sideLength length: uploLength 

hemvWithUplo: Uplo N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 

hemvWithuplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: uploLength

hpmvWithUplo: Uplo N: N alpha: alpha Ap: Ap X: X incX: incX beta: beta Y: Y incY: incY 

hpmvWithuplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy length: uploLength 

left

lower

nonUnit

notTransposed

realScalWithn: n alpha: alpha x: x incx: incx

realScalWithN: N alpha: alpha X: X incX: incX 

right

scalWithn: n alpha: alpha x: x incx: incx

scalWithN: N alpha: alpha X: X incX: incX 

swapWithn: n x: x incx: incx y: y incy: incy 

swapWithN: N X: X incX: incX Y: Y incY: incY 

tpmvWithuplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx length: uploLength length: transLength length: diagLength 

tpmvWithUplo: Uplo TransA: TransA Diag: Diag N: N Ap: A X: X incX: incX 

transposeConjugated

transposed

trmmWithside: side uplo: uplo trans: transa diag: diag m: m n: n alpha: alpha a: A lda: lda b: B ldb: ldb length: lside length: luplo length: ltrans length: ldiag 

trmmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 

trmvWithuplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx length: uploLength length: transLength length: diagLength 

trmvWithUplo: Uplo TransA: TransA Diag: Diag N: N A: A lda: lda X: X incX: incX 

trsmWithside: side uplo: uplo trans: transa diag: diag m: m n: n alpha: alpha a: A lda: lda b: B ldb: ldb length: lside length: luplo length: ltrans length: ldiag 

trsmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 

unit

upper
!BlasLibrary categoriesFor: #asumWithn:x:incx:!public! !
!BlasLibrary categoriesFor: #asumWithN:X:incX:!public! !
!BlasLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #axpyWithN:alpha:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #cComplexPointerOn:!public! !
!BlasLibrary categoriesFor: #cElementPointerOn:!public! !
!BlasLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #copyWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #cRealPointerOn:!public! !
!BlasLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #dotcWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #dotuWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #dotWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #gemmWithTransA:TransB:M:N:K:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!BlasLibrary categoriesFor: #gemmWithtransa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasLibrary categoriesFor: #gemvWithtrans:m:n:alpha:a:lda:x:incx:beta:y:incy:length:!public! !
!BlasLibrary categoriesFor: #gemvWithTransA:M:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!BlasLibrary categoriesFor: #gercWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasLibrary categoriesFor: #gercWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!BlasLibrary categoriesFor: #geruWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasLibrary categoriesFor: #geruWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!BlasLibrary categoriesFor: #gerWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!BlasLibrary categoriesFor: #hemmWithSide:Uplo:M:N:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!BlasLibrary categoriesFor: #hemmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasLibrary categoriesFor: #hemvWithUplo:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!BlasLibrary categoriesFor: #hemvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasLibrary categoriesFor: #hpmvWithUplo:N:alpha:Ap:X:incX:beta:Y:incY:!public! !
!BlasLibrary categoriesFor: #hpmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasLibrary categoriesFor: #left!public! !
!BlasLibrary categoriesFor: #lower!public! !
!BlasLibrary categoriesFor: #nonUnit!public! !
!BlasLibrary categoriesFor: #notTransposed!public! !
!BlasLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!BlasLibrary categoriesFor: #realScalWithN:alpha:X:incX:!public! !
!BlasLibrary categoriesFor: #right!public! !
!BlasLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!BlasLibrary categoriesFor: #scalWithN:alpha:X:incX:!public! !
!BlasLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #swapWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #tpmvWithuplo:trans:diag:n:ap:x:incx:length:length:length:!public! !
!BlasLibrary categoriesFor: #tpmvWithUplo:TransA:Diag:N:Ap:X:incX:!public! !
!BlasLibrary categoriesFor: #transposeConjugated!public! !
!BlasLibrary categoriesFor: #transposed!public! !
!BlasLibrary categoriesFor: #trmmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasLibrary categoriesFor: #trmmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!BlasLibrary categoriesFor: #trmvWithuplo:trans:diag:n:a:lda:x:incx:length:length:length:!public! !
!BlasLibrary categoriesFor: #trmvWithUplo:TransA:Diag:N:A:lda:X:incX:!public! !
!BlasLibrary categoriesFor: #trsmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasLibrary categoriesFor: #trsmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!BlasLibrary categoriesFor: #unit!public! !
!BlasLibrary categoriesFor: #upper!public! !

!BlasLibrary class methodsFor!

fileName
	"Answer the host system file name for the library"

	^'BLAS'! !
!BlasLibrary class categoriesFor: #fileName!public! !
