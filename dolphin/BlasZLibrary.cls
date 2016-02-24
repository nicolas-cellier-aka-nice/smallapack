"Filed out from Dolphin Smalltalk 7"!

BlasLibrary subclass: #BlasZLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasZLibrary guid: (GUID fromString: '{5E4F44EB-3712-4263-8A54-4B010B2F4925}')!
BlasZLibrary comment: ''!
!BlasZLibrary categoriesForClass!Unclassified! !
!BlasZLibrary methodsFor!

asumWithn: n x: x incx: incx
	"||real(x)||_1 + ||imag(x)||_1"

	<cdecl: double 'dzasum_'  SDWORD * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy
	"y := alpha*x+y"

	<cdecl: void 'zaxpy_'  SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

cComplexPointerOn: aComplex 	^self cDoubleComplexPointerOn: aComplex!

cElementPointerOn: aComplex 	^self cComplexPointerOn: aComplex!

copyWithn: n x: x incx: incx y: y incy: incy
	"y := x"

	<cdecl: void 'zcopy_'  SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

cRealPointerOn: aDouble 	^self cDoublePointerOn: aDouble!

dotcWithn: n x: x incx: incx y: y incy: incy
	"^transposeConjugated(x)*y"

	<cdecl: DOUBLECOMPLEX 'zdotc_'  SDWORD * "DOUBLECOMPLEX"void * SDWORD * "DOUBLECOMPLEX"void * SDWORD * >
	^self invalidCall!

dotuWithn: n x: x incx: incx y: y incy: incy
	"^transpose(x)*y"

	<cdecl: DOUBLECOMPLEX 'zdotu_'  SDWORD * "DOUBLECOMPLEX"void * SDWORD * "DOUBLECOMPLEX"void * SDWORD * >
	^self invalidCall!

gemmWithtransa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: ltransa length: ltransb 
	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"

	<cdecl: void 'zgemm_'  char * char * SDWORD* SDWORD* SDWORD* "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD* SDWORD SDWORD >
	^self invalidCall!

gemvWithtrans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: ltrans
	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugate"

	<cdecl: void 'zgemv_'  char * SDWORD* SDWORD* "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD* SDWORD >
	^self invalidCall!

gercWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transposeConjugated(y)+A"

	<cdecl: void 'zgerc_' SDWORD* SDWORD* "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * SDWORD*>
	^self invalidCall!

geruWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	<cdecl: void 'zgeru_' SDWORD* SDWORD* "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * SDWORD* "ExternalDoubleComplex"void * SDWORD*>
	^self invalidCall!

hemmWithside: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: lside length: luplo 
	"C=alpha*A*B+beta*C"

	<cdecl: void 'zhemm_'  char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD SDWORD >
	^self invalidCall!

hemvWithuplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: luplo
	"y=alpha*X+beta*y"

	<cdecl: void 'zhemv_'  char * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

hpmvWithuplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy length: luplo 
	"y=alpha*X+beta*y"

	<cdecl: void 'zhpmv_'  char * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

iamaxWithn: n x: x incx: incx
	"index of max : max_i (abs(real(x_i))+abs(imag(x_i)))"

	<cdecl: SDWORD 'izamax_'  SDWORD * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

nrm2Withn: n x: x incx: incx
	"||x||_2"

	<cdecl: double 'dznrm2_'  SDWORD * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx
	<cdecl: void 'zdscal_'  SDWORD * double * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx
	<cdecl: void 'zscal_'  SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy
	"x <-> y"

	<cdecl: void 'zswap_'  SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

tpmvWithuplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx length: uploLength length: transLength length: diagLength 
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'ztpmv_'  char * char * char * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trmmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'ztrmm_'  char * char * char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

trmvWithuplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx length: uploLength length: transLength length: diagLength
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'ztrmv_'  char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trsmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'ztrsm_'  char * char * char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall! !
!BlasZLibrary categoriesFor: #asumWithn:x:incx:!public! !
!BlasZLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!BlasZLibrary categoriesFor: #cComplexPointerOn:!public! !
!BlasZLibrary categoriesFor: #cElementPointerOn:!public! !
!BlasZLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!BlasZLibrary categoriesFor: #cRealPointerOn:!public! !
!BlasZLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!BlasZLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!BlasZLibrary categoriesFor: #gemmWithtransa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasZLibrary categoriesFor: #gemvWithtrans:m:n:alpha:a:lda:x:incx:beta:y:incy:length:!public! !
!BlasZLibrary categoriesFor: #gercWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasZLibrary categoriesFor: #geruWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasZLibrary categoriesFor: #hemmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasZLibrary categoriesFor: #hemvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasZLibrary categoriesFor: #hpmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasZLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!BlasZLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!BlasZLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!BlasZLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!BlasZLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!BlasZLibrary categoriesFor: #tpmvWithuplo:trans:diag:n:ap:x:incx:length:length:length:!public! !
!BlasZLibrary categoriesFor: #trmmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasZLibrary categoriesFor: #trmvWithuplo:trans:diag:n:a:lda:x:incx:length:length:length:!public! !
!BlasZLibrary categoriesFor: #trsmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !

