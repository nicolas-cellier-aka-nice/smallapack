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

asumWithn: n x: x incx: incx	"||real(x)||_1 + ||imag(x)||_1"	<cdecl: double 'cblas_dasum' SDWORD double * SDWORD >	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy	"y := alpha*x+y"	<cdecl: void 'cblas_daxpy' SDWORD double double * SDWORD double * SDWORD >	^self invalidCall!

cComplexPointerOn: aComplex 	^self cDoubleComplexPointerOn: aComplex!

cElementArgumentOn: aDouble 	^aDouble!

cElementPointerOn: aDouble 	^self cRealPointerOn: aDouble!

copyWithn: n x: x incx: incx y: y incy: incy	"y := x"	<cdecl: void 'cblas_dcopy' SDWORD double * SDWORD double * SDWORD >	^self invalidCall!

cRealPointerOn: aDouble 	^self cDoublePointerOn: aDouble!

dotcWithn: n x: x incx: incx y: y incy: incy	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotuWithn: n x: x incx: incx y: y incy: incy	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotWithn: n x: x incx: incx y: y incy: incy	<cdecl: double 'cblas_ddot' SDWORD double * SDWORD double * SDWORD >	^self invalidCall!

gemmWithorder: order transa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"	<cdecl: void 'cblas_dgemm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD double double * SDWORD double * SDWORD double double * SDWORD >	^self invalidCall!

gemvWithorder: order trans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugate"	<cdecl: void 'cblas_dgemv' SDWORD SDWORD SDWORD SDWORD double double * SDWORD double * SDWORD double double * SDWORD >	^self invalidCall!

gercWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transposeConjugated(y)+A"	^self 		gerWithorder: order		m: m		n: n		alpha: alpha		x: x		incx: incx		y: y		incy: incy		a: a		lda: lda!

geruWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transpose(y)+A"	^self 		gerWithorder: order		m: m		n: n		alpha: alpha		x: x		incx: incx		y: y		incy: incy		a: a		lda: lda!

gerWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transpose(y)+A"	<cdecl: void 'cblas_dger' SDWORD SDWORD SDWORD double double * SDWORD double * SDWORD double * SDWORD>	^self invalidCall!

hemmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C=alpha*A*B+beta*C"	^self 		symmWithorder: order		side: side		uplo: uplo		m: m		n: n		alpha: alpha		a: a		lda: lda		b: b		ldb: ldb		beta: beta		c: c		ldc: ldc!

hemvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	^self 		symvWithorder: order		uplo: uplo		n: n		alpha: alpha		n: a		lda: lda		x: x		incx: incx		beta: beta		y: y		incy: incy!

hpmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	^self 		spmvWithorder: order		uplo: uplo		n: n		alpha: alpha		ap: ap		x: x		incx: incx		beta: beta		y: y		incy: incy!

iamaxWithn: n x: x incx: incx	"index of max : max_i (abs(real(x_i))+abs(imag(x_i)))"	<cdecl: SDWORD 'cblas_idamax' SDWORD double * SDWORD >	^self invalidCall!

nrm2Withn: n x: x incx: incx	"||x||_2"	<cdecl: double 'cblas_dnrm2' SDWORD double * SDWORD >	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx	^self scalWithn: n alpha: alpha x: x incx: incx!

rotgWitha: a b: b c: c s: s	<cdecl: void 'cblas_drotg' double * double * double * double * >	^self invalidCall!

rotWithn: n x: x incx: incx y: y incy: incy c: c s: s	<cdecl: void 'cblas_drot' SDWORD double * SDWORD double * SDWORD double double >	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx	<cdecl: void 'cblas_dscal' SDWORD double double * SDWORD >	^self invalidCall!

spmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	<cdecl: void 'cblas_dspmv' SDWORD SDWORD SDWORD double double * double * SDWORD double double * SDWORD >	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy	"x <-> y"	<cdecl: void 'cblas_dswap' SDWORD double * SDWORD double * SDWORD >	^self invalidCall!

symmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C=alpha*A*B+beta*C"	<cdecl: void 'cblas_dsymm' SDWORD SDWORD SDWORD SDWORD SDWORD double double * SDWORD double * SDWORD double double * SDWORD >	^self invalidCall!

symvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy 	"y=alpha*X+beta*y"	<cdecl: void 'cblas_dsymv' SDWORD SDWORD SDWORD double double * SDWORD double * SDWORD double double * SDWORD >	^self invalidCall!

tpmvWithorder: order uplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_dtpmv' SDWORD SDWORD SDWORD SDWORD SDWORD double * double * SDWORD >	^self invalidCall!

trmmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_dtrmm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD double double * SDWORD double * SDWORD >	^self invalidCall!

trmvWithorder: order uplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_dtrmv' SDWORD SDWORD SDWORD SDWORD SDWORD double * SDWORD double * SDWORD >	^self invalidCall!

trsmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_dtrsm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD double double * SDWORD double * SDWORD >	^self invalidCall! !
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

