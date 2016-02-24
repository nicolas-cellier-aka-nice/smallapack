"Filed out from Dolphin Smalltalk 7"!

BlasLibrary subclass: #BlasCLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasCLibrary guid: (GUID fromString: '{860DFF86-BCF3-4544-8D7B-B07B7E0883AD}')!
BlasCLibrary comment: ''!
!BlasCLibrary categoriesForClass!Unclassified! !
!BlasCLibrary methodsFor!

asumWithn: n x: x incx: incx
	"||real(x)||_1 + ||imag(x)||_1"

	<cdecl: float 'scasum_'  SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy
	"y := alpha*x+y"

	<cdecl: void 'caxpy_'  SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

cComplexPointerOn: aComplex 	^self cFloatComplexPointerOn: aComplex!

cElementPointerOn: aComplex 	^self cComplexPointerOn: aComplex!

copyWithn: n x: x incx: incx y: y incy: incy
	"y := x"

	<cdecl: void 'ccopy_'  SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

cRealPointerOn: aDouble 	^self cFloatPointerOn: aDouble!

dotcWithn: n x: x incx: incx y: y incy: incy
	"^transposeConjugate(x)*y"

	<cdecl: FLOATCOMPLEX 'cdotc_'  SDWORD * "FLOATCOMPLEX"void * SDWORD * "FLOATCOMPLEX"void * SDWORD * >
	^self invalidCall!

dotuWithn: n x: x incx: incx y: y incy: incy
	"^transpose(x)*y"

	<cdecl: FLOATCOMPLEX 'cdotu_'  SDWORD * "FLOATCOMPLEX"void * SDWORD * "FLOATCOMPLEX"void * SDWORD * >
	^self invalidCall!

gemmWithtransa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: ltransa length: ltransb 
	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"

	<cdecl: void 'cgemm_'  char * char * SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD SDWORD >
	^self invalidCall!

gemvWithtrans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: ltrans
	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugated"

	<cdecl: void 'cgemv_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

gercWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transposeConjugate(y)+A"

	<cdecl: void 'cgerc_'  SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

geruWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	<cdecl: void 'cgeru_'  SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

hemmWithside: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: lside length: luplo 
	"C=alpha*A*B+beta*C"

	<cdecl: void 'chemm_'  char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD SDWORD >
	^self invalidCall!

hemvWithuplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: luplo
	"y=alpha*X+beta*y"

	<cdecl: void 'chemv_'  char * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

hpmvWithuplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy length: luplo 
	"y=alpha*X+beta*y"

	<cdecl: void 'chpmv_'  char * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

iamaxWithn: n x: x incx: incx
	"index of max : max_i (abs(real(x_i))+abs(imag(x_i)))"

	<cdecl: SDWORD 'icamax_'  SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

nrm2Withn: n x: x incx: incx
	"||x||_2"

	<cdecl: float 'scnrm2_'  SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx
	<cdecl: void 'csscal_'  SDWORD * float * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx
	<cdecl: void 'cscal_'  SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy
	"x <-> y"

	<cdecl: void 'cswap_'  SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

tpmvWithuplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx length: uploLength length: transLength length: diagLength 
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'ctpmv_'  char * char * char * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trmmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'ctrmm_'  char * char * char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

trmvWithuplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx length: uploLength length: transLength length: diagLength
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'ctrmv_'  char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trsmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'ctrsm_'  char * char * char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall! !
!BlasCLibrary categoriesFor: #asumWithn:x:incx:!public! !
!BlasCLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!BlasCLibrary categoriesFor: #cComplexPointerOn:!public! !
!BlasCLibrary categoriesFor: #cElementPointerOn:!public! !
!BlasCLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!BlasCLibrary categoriesFor: #cRealPointerOn:!public! !
!BlasCLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!BlasCLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!BlasCLibrary categoriesFor: #gemmWithtransa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasCLibrary categoriesFor: #gemvWithtrans:m:n:alpha:a:lda:x:incx:beta:y:incy:length:!public! !
!BlasCLibrary categoriesFor: #gercWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasCLibrary categoriesFor: #geruWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasCLibrary categoriesFor: #hemmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasCLibrary categoriesFor: #hemvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasCLibrary categoriesFor: #hpmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasCLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!BlasCLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!BlasCLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!BlasCLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!BlasCLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!BlasCLibrary categoriesFor: #tpmvWithuplo:trans:diag:n:ap:x:incx:length:length:length:!public! !
!BlasCLibrary categoriesFor: #trmmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasCLibrary categoriesFor: #trmvWithuplo:trans:diag:n:a:lda:x:incx:length:length:length:!public! !
!BlasCLibrary categoriesFor: #trsmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !

