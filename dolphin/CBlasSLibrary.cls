"Filed out from Dolphin Smalltalk 7"!

CBlasLibrary subclass: #CBlasSLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CBlasSLibrary guid: (GUID fromString: '{C72162BA-37AF-4B1E-A9E5-4E0A7060C2D6}')!
CBlasSLibrary comment: ''!
!CBlasSLibrary categoriesForClass!Unclassified! !
!CBlasSLibrary methodsFor!

asumWithn: n x: x incx: incx

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy

cComplexPointerOn: aComplex 

cElementArgumentOn: aFloat 

cElementPointerOn: aFloat 

copyWithn: n x: x incx: incx y: y incy: incy

cRealPointerOn: aFloat 

dotcWithn: n x: x incx: incx y: y incy: incy

dotuWithn: n x: x incx: incx y: y incy: incy

dotWithn: n x: x incx: incx y: y incy: incy

gemmWithorder: order transa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc

gemvWithorder: order trans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy

gercWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

geruWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 

gerWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	<cdecl: void 'cblas_sger' SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD float * SDWORD>
	^self invalidCall!

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
!CBlasSLibrary categoriesFor: #asumWithn:x:incx:!public! !
!CBlasSLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!CBlasSLibrary categoriesFor: #cComplexPointerOn:!public! !
!CBlasSLibrary categoriesFor: #cElementArgumentOn:!public! !
!CBlasSLibrary categoriesFor: #cElementPointerOn:!public! !
!CBlasSLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!CBlasSLibrary categoriesFor: #cRealPointerOn:!public! !
!CBlasSLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!CBlasSLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!CBlasSLibrary categoriesFor: #dotWithn:x:incx:y:incy:!public! !
!CBlasSLibrary categoriesFor: #gemmWithorder:transa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasSLibrary categoriesFor: #gemvWithorder:trans:m:n:alpha:a:lda:x:incx:beta:y:incy:!public! !
!CBlasSLibrary categoriesFor: #gercWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasSLibrary categoriesFor: #geruWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasSLibrary categoriesFor: #gerWithorder:m:n:alpha:x:incx:y:incy:a:lda:!public! !
!CBlasSLibrary categoriesFor: #hemmWithorder:side:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasSLibrary categoriesFor: #hemvWithorder:uplo:n:alpha:n:lda:x:incx:beta:y:incy:!public! !
!CBlasSLibrary categoriesFor: #hpmvWithorder:uplo:n:alpha:ap:x:incx:beta:y:incy:!public! !
!CBlasSLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!CBlasSLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!CBlasSLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!CBlasSLibrary categoriesFor: #rotgWitha:b:c:s:!public! !
!CBlasSLibrary categoriesFor: #rotWithn:x:incx:y:incy:c:s:!public! !
!CBlasSLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!CBlasSLibrary categoriesFor: #spmvWithorder:uplo:n:alpha:ap:x:incx:beta:y:incy:!public! !
!CBlasSLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!CBlasSLibrary categoriesFor: #symmWithorder:side:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:!public! !
!CBlasSLibrary categoriesFor: #symvWithorder:uplo:n:alpha:n:lda:x:incx:beta:y:incy:!public! !
!CBlasSLibrary categoriesFor: #tpmvWithorder:uplo:trans:diag:n:ap:x:incx:!public! !
!CBlasSLibrary categoriesFor: #trmmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
!CBlasSLibrary categoriesFor: #trmvWithorder:uplo:trans:diag:n:a:lda:x:incx:!public! !
!CBlasSLibrary categoriesFor: #trsmWithorder:side:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:!public! !
