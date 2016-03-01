"Filed out from Dolphin Smalltalk 7"!

CBlasLibrary subclass: #CBlasDLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CBlasDLibrary guid: (GUID fromString: '{9CF03A7C-687F-4C52-920E-6A1958D55C6C}')!
CBlasDLibrary comment: ''!
!CBlasDLibrary categoriesForClass!Unclassified! !
!CBlasDLibrary methodsFor!

asumWithn: n x: x incx: incx

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy

cComplexPointerOn: aComplex 

cElementArgumentOn: aDouble 

cElementPointerOn: aDouble 

copyWithn: n x: x incx: incx y: y incy: incy

cRealPointerOn: aDouble 

dotcWithn: n x: x incx: incx y: y incy: incy

dotuWithn: n x: x incx: incx y: y incy: incy

dotWithn: n x: x incx: incx y: y incy: incy

gemmWithorder: order transa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc

gemvWithorder: order trans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy

gercWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

geruWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

gerWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

hemmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc

hemvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy

hpmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy

iamaxWithn: n x: x incx: incx

nrm2Withn: n x: x incx: incx

realScalWithn: n alpha: alpha x: x incx: incx

rotgWitha: a b: b c: c s: s

rotWithn: n x: x incx: incx y: y incy: incy c: c s: s

scalWithn: n alpha: alpha x: x incx: incx

spmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy

swapWithn: n x: x incx: incx y: y incy: incy

symmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc

symvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy 

tpmvWithorder: order uplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx

trmmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb

trmvWithorder: order uplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx

trsmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb
!CBlasDLibrary categoriesFor: #asumWithn:x:incx:!public! !
!CBlasDLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!CBlasDLibrary categoriesFor: #cComplexPointerOn:!public! !
!CBlasDLibrary categoriesFor: #cElementArgumentOn:!public! !
!CBlasDLibrary categoriesFor: #cElementPointerOn:!public! !
!CBlasDLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!CBlasDLibrary categoriesFor: #cRealPointerOn:!public! !
!CBlasDLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!CBlasDLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!CBlasDLibrary categoriesFor: #dotWithn:x:incx:y:incy:!public! !
!CBlasDLibrary categoriesFor: #gemmWithorder:transa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasDLibrary categoriesFor: #gemvWithorder:trans:m:n:alpha:a:lda:x:incx:beta:y:incy:!public! !
!CBlasDLibrary categoriesFor: #gercWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasDLibrary categoriesFor: #geruWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasDLibrary categoriesFor: #gerWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasDLibrary categoriesFor: #hemmWithorder:side:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasDLibrary categoriesFor: #hemvWithorder:uplo:n:alpha:n:lda:x:incx:beta:y:incy:!public! !
!CBlasDLibrary categoriesFor: #hpmvWithorder:uplo:n:alpha:ap:x:incx:beta:y:incy:!public! !
!CBlasDLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!CBlasDLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!CBlasDLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!CBlasDLibrary categoriesFor: #rotgWitha:b:c:s:!public! !
!CBlasDLibrary categoriesFor: #rotWithn:x:incx:y:incy:c:s:!public! !
!CBlasDLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!CBlasDLibrary categoriesFor: #spmvWithorder:uplo:n:alpha:ap:x:incx:beta:y:incy:!public! !
!CBlasDLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!CBlasDLibrary categoriesFor: #symmWithorder:side:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasDLibrary categoriesFor: #symvWithorder:uplo:n:alpha:n:lda:x:incx:beta:y:incy:!public! !
!CBlasDLibrary categoriesFor: #tpmvWithorder:uplo:trans:diag:n:ap:x:incx:!public! !
!CBlasDLibrary categoriesFor: #trmmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
!CBlasDLibrary categoriesFor: #trmvWithorder:uplo:trans:diag:n:a:lda:x:incx:!public! !
!CBlasDLibrary categoriesFor: #trsmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
