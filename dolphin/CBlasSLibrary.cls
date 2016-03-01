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

asumWithn: n x: x incx: incx	"||real(x)||_1 + ||imag(x)||_1"	<cdecl: float 'cblas_sasum' SDWORD float * SDWORD >	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy	"y := alpha*x+y"	<cdecl: void 'cblas_saxpy' SDWORD float float * SDWORD float * SDWORD >	^self invalidCall!

cComplexPointerOn: aComplex 	^self cFloatComplexPointerOn: aComplex!

cElementArgumentOn: aFloat 	^aFloat!

cElementPointerOn: aFloat 	^self cRealPointerOn: aFloat!

copyWithn: n x: x incx: incx y: y incy: incy	"y := x"	<cdecl: void 'cblas_scopy' SDWORD float * SDWORD float * SDWORD >	^self invalidCall!

cRealPointerOn: aFloat 	^self cFloatPointerOn: aFloat!

dotcWithn: n x: x incx: incx y: y incy: incy	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotuWithn: n x: x incx: incx y: y incy: incy	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotWithn: n x: x incx: incx y: y incy: incy	<cdecl: float 'cblas_sdot' SDWORD float * SDWORD float * SDWORD >	^self invalidCall!

gemmWithorder: order transa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"	<cdecl: void 'cblas_sgemm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD float float * SDWORD >	^self invalidCall!

gemvWithorder: order trans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugate"	<cdecl: void 'cblas_sgemv' SDWORD SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD float float * SDWORD >	^self invalidCall!

gercWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transposeConjugated(y)+A"	^self 		gerWithorder: order		m: m		n: n		alpha: alpha		x: x		incx: incx		y: y		incy: incy		a: a		lda: lda!

geruWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transpose(y)+A"	^self 		gerWithorder: order		m: m		n: n		alpha: alpha		x: x		incx: incx		y: y		incy: incy		a: a		lda: lda!

gerWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	<cdecl: void 'cblas_sger' SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD float * SDWORD>
	^self invalidCall!

hemmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C=alpha*A*B+beta*C"	^self 		symmWithorder: order		side: side		uplo: uplo		m: m		n: n		alpha: alpha		a: a		lda: lda		b: b		ldb: ldb		beta: beta		c: c		ldc: ldc!

hemvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	^self 		symvWithorder: order		uplo: uplo		n: n		alpha: alpha		n: a		lda: lda		x: x		incx: incx		beta: beta		y: y		incy: incy!

hpmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	^self 		spmvWithorder: order		uplo: uplo		n: n		alpha: alpha		ap: ap		x: x		incx: incx		beta: beta		y: y		incy: incy!

iamaxWithn: n x: x incx: incx	"index of max : max_i (abs(real(x_i))+abs(imag(x_i)))"	<cdecl: SDWORD 'cblas_isamax' SDWORD float * SDWORD >	^self invalidCall!

nrm2Withn: n x: x incx: incx	"||x||_2"	<cdecl: float 'cblas_snrm2' SDWORD float * SDWORD >	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx	^self scalWithn: n alpha: alpha x: x incx: incx!

rotgWitha: a b: b c: c s: s	<cdecl: void 'cblas_srotg' float * float * float * float * >	^self invalidCall!

rotWithn: n x: x incx: incx y: y incy: incy c: c s: s	<cdecl: void 'cblas_srot' SDWORD float * SDWORD float * SDWORD float float >	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx	<cdecl: void 'cblas_sscal' SDWORD float float * SDWORD >	^self invalidCall!

spmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	<cdecl: void 'cblas_sspmv' SDWORD SDWORD SDWORD float float * float * SDWORD float float * SDWORD >	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy	"x <-> y"	<cdecl: void 'cblas_sswap' SDWORD float * SDWORD float * SDWORD >	^self invalidCall!

symmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C=alpha*A*B+beta*C"	<cdecl: void 'cblas_ssymm' SDWORD SDWORD SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD float float * SDWORD >	^self invalidCall!

symvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy 	"y=alpha*X+beta*y"	<cdecl: void 'cblas_ssymv' SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD float float * SDWORD >	^self invalidCall!

tpmvWithorder: order uplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_stpmv' SDWORD SDWORD SDWORD SDWORD SDWORD float * float * SDWORD >	^self invalidCall!

trmmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_strmm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD >	^self invalidCall!

trmvWithorder: order uplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_strmv' SDWORD SDWORD SDWORD SDWORD SDWORD float * SDWORD float * SDWORD >	^self invalidCall!

trsmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_strsm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD float float * SDWORD float * SDWORD >	^self invalidCall! !
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

