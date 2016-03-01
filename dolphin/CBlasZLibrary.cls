"Filed out from Dolphin Smalltalk 7"!

CBlasLibrary subclass: #CBlasZLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CBlasZLibrary guid: (GUID fromString: '{25DFBE0A-3F43-4988-9C4F-32D2733096AF}')!
CBlasZLibrary comment: ''!
!CBlasZLibrary categoriesForClass!Unclassified! !
!CBlasZLibrary methodsFor!

asumWithn: n x: x incx: incx

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy

cComplexPointerOn: aComplex 

cElementArgumentOn: aComplex 

cElementPointerOn: aComplex 

copyWithn: n x: x incx: incx y: y incy: incy

cRealPointerOn: aDouble 

dotcWithN: N X: X incX: incX Y: Y incY: incY 

dotcWithn: n x: x incx: incx y: y incy: incy dotc: dotc

dotuWithN: N X: X incX: incX Y: Y incY: incY 

dotuWithn: n x: x incx: incx y: y incy: incy dotu: dotu

gemmWithorder: order transa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc

gemvWithorder: order trans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy

gercWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

geruWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

hemmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc

hemvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy

hpmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy

iamaxWithn: n x: x incx: incx

nrm2Withn: n x: x incx: incx

realScalWithn: n alpha: alpha x: x incx: incx

scalWithn: n alpha: alpha x: x incx: incx

swapWithn: n x: x incx: incx y: y incy: incy

tpmvWithorder: order uplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx

trmmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb

trmvWithorder: order uplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx

trsmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb
!CBlasZLibrary categoriesFor: #asumWithn:x:incx:!public! !
!CBlasZLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!CBlasZLibrary categoriesFor: #cComplexPointerOn:!public! !
!CBlasZLibrary categoriesFor: #cElementArgumentOn:!public! !
!CBlasZLibrary categoriesFor: #cElementPointerOn:!public! !
!CBlasZLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!CBlasZLibrary categoriesFor: #cRealPointerOn:!public! !
!CBlasZLibrary categoriesFor: #dotcWithN:X:incX:Y:incY:!public! !
!CBlasZLibrary categoriesFor: #dotcWithn:x:incx:y:incy:dotc:!public! !
!CBlasZLibrary categoriesFor: #dotuWithN:X:incX:Y:incY:!public! !
!CBlasZLibrary categoriesFor: #dotuWithn:x:incx:y:incy:dotu:!public! !
!CBlasZLibrary categoriesFor: #gemmWithorder:transa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasZLibrary categoriesFor: #gemvWithorder:trans:m:n:alpha:a:lda:x:incx:beta:y:incy:!public! !
!CBlasZLibrary categoriesFor: #gercWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasZLibrary categoriesFor: #geruWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasZLibrary categoriesFor: #hemmWithorder:side:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasZLibrary categoriesFor: #hemvWithorder:uplo:n:alpha:n:lda:x:incx:beta:y:incy:!public! !
!CBlasZLibrary categoriesFor: #hpmvWithorder:uplo:n:alpha:ap:x:incx:beta:y:incy:!public! !
!CBlasZLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!CBlasZLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!CBlasZLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!CBlasZLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!CBlasZLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!CBlasZLibrary categoriesFor: #tpmvWithorder:uplo:trans:diag:n:ap:x:incx:!public! !
!CBlasZLibrary categoriesFor: #trmmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
!CBlasZLibrary categoriesFor: #trmvWithorder:uplo:trans:diag:n:a:lda:x:incx:!public! !
!CBlasZLibrary categoriesFor: #trsmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
