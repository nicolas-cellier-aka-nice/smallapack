"Filed out from Dolphin Smalltalk 7"!

CBlasLibrary subclass: #CBlasCLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CBlasCLibrary guid: (GUID fromString: '{9FFFC04D-3C54-489A-B786-45B5ECEFF0AE}')!
CBlasCLibrary comment: ''!
!CBlasCLibrary categoriesForClass!Unclassified! !
!CBlasCLibrary methodsFor!

asumWithn: n x: x incx: incx

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy

cComplexPointerOn: aComplex 

cElementArgumentOn: aComplex 

cElementPointerOn: aComplex 

copyWithn: n x: x incx: incx y: y incy: incy

cRealPointerOn: aDouble 

dotcWithn: n x: x incx: incx y: y incy: incy dotc: dotc

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
!CBlasCLibrary categoriesFor: #asumWithn:x:incx:!public! !
!CBlasCLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!CBlasCLibrary categoriesFor: #cComplexPointerOn:!public! !
!CBlasCLibrary categoriesFor: #cElementArgumentOn:!public! !
!CBlasCLibrary categoriesFor: #cElementPointerOn:!public! !
!CBlasCLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!CBlasCLibrary categoriesFor: #cRealPointerOn:!public! !
!CBlasCLibrary categoriesFor: #dotcWithn:x:incx:y:incy:dotc:!public! !
!CBlasCLibrary categoriesFor: #dotuWithn:x:incx:y:incy:dotu:!public! !
!CBlasCLibrary categoriesFor: #gemmWithorder:transa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasCLibrary categoriesFor: #gemvWithorder:trans:m:n:alpha:a:lda:x:incx:beta:y:incy:!public! !
!CBlasCLibrary categoriesFor: #gercWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasCLibrary categoriesFor: #geruWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasCLibrary categoriesFor: #hemmWithorder:side:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasCLibrary categoriesFor: #hemvWithorder:uplo:n:alpha:n:lda:x:incx:beta:y:incy:!public! !
!CBlasCLibrary categoriesFor: #hpmvWithorder:uplo:n:alpha:ap:x:incx:beta:y:incy:!public! !
!CBlasCLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!CBlasCLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!CBlasCLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!CBlasCLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!CBlasCLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!CBlasCLibrary categoriesFor: #tpmvWithorder:uplo:trans:diag:n:ap:x:incx:!public! !
!CBlasCLibrary categoriesFor: #trmmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
!CBlasCLibrary categoriesFor: #trmvWithorder:uplo:trans:diag:n:a:lda:x:incx:!public! !
!CBlasCLibrary categoriesFor: #trsmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
