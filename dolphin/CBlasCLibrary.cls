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

asumWithn: n x: x incx: incx	"||real(x)||_1 + ||imag(x)||_1"	<cdecl: float 'cblas_scasum' SDWORD  "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy	"y := alpha*x+y"	<cdecl: void 'cblas_caxpy' SDWORD  "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD  "ExternalFloatComplex"void * SDWORD  >	^self invalidCall!

cComplexPointerOn: aComplex 	^self cFloatComplexPointerOn: aComplex!

cElementArgumentOn: aComplex 	^self cComplexPointerOn: aComplex!

cElementPointerOn: aComplex 	^self cComplexPointerOn: aComplex!

copyWithn: n x: x incx: incx y: y incy: incy	"y := x"	<cdecl: void 'cblas_ccopy' SDWORD  "ExternalFloatComplex"void * SDWORD  "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

cRealPointerOn: aDouble 	^self cFloatPointerOn: aDouble!

dotcWithn: n x: x incx: incx y: y incy: incy dotc: dotc	"dotc := transposeConjugate(x)*y"	<cdecl: void 'cblas_cdotc_sub' SDWORD  "ExternalFloatComplex"void * SDWORD  "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * >	^self invalidCall!

dotuWithn: n x: x incx: incx y: y incy: incy dotu: dotu	"dotu := transpose(x)*y"	<cdecl: void 'cblas_cdotu_sub' SDWORD  "ExternalFloatComplex"void * SDWORD  "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * >	^self invalidCall!

gemmWithorder: order transa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"	<cdecl: void 'cblas_cgemm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD  "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

gemvWithorder: order trans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_cgemv' SDWORD SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD  "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

gercWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transposeConjugate(y)+A"	<cdecl: void 'cblas_cgerc' SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

geruWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transpose(y)+A"	<cdecl: void 'cblas_cgeru' SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

hemmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C=alpha*A*B+beta*C"	<cdecl: void 'cblas_chemm' SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

hemvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	<cdecl: void 'cblas_chemv' SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

hpmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	<cdecl: void 'cblas_chpmv' SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

iamaxWithn: n x: x incx: incx	"index of max : max_i (abs(real(x_i))+abs(imag(x_i)))"	<cdecl: SDWORD 'cblas_icamax' SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

nrm2Withn: n x: x incx: incx	"||x||_2"	<cdecl: float 'cblas_scnrm2' SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx	<cdecl: void 'cblas_csscal' SDWORD float "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx	<cdecl: void 'cblas_cscal' SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy	"x <-> y"	<cdecl: void 'cblas_cswap' SDWORD "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

tpmvWithorder: order uplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_ctpmv' SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

trmmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_ctrmm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

trmvWithorder: order uplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_ctrmv' SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall!

trsmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_ctrsm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD "ExternalFloatComplex"void * SDWORD >	^self invalidCall! !
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

