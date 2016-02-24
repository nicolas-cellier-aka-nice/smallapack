"Filed out from Dolphin Smalltalk 7"!

BlasLibrary subclass: #BlasDLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasDLibrary guid: (GUID fromString: '{98A31CA8-9A20-4B7A-B28C-E0799F10D0A8}')!
BlasDLibrary comment: ''!
!BlasDLibrary categoriesForClass!Unclassified! !
!BlasDLibrary methodsFor!

asumWithn: n x: x incx: incx
	"||real(x)||_1 + ||imag(x)||_1"

	<cdecl: double 'dasum_' SDWORD * double * SDWORD * >
	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy
	"y := alpha*x+y"

	<cdecl: void 'daxpy_' SDWORD * double * double * SDWORD * double * SDWORD * >
	^self invalidCall!

cComplexPointerOn: aComplex 	^self cDoubleComplexPointerOn: aComplex!

cElementPointerOn: aDouble 	^self cRealPointerOn: aDouble!

copyWithn: n x: x incx: incx y: y incy: incy
	"y := x"

	<cdecl: void 'dcopy_' SDWORD * double * SDWORD * double * SDWORD * >
	^self invalidCall!

cRealPointerOn: aDouble 	^self cDoublePointerOn: aDouble!

dotcWithn: n x: x incx: incx y: y incy: incy	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotuWithn: n x: x incx: incx y: y incy: incy
	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotWithn: n x: x incx: incx y: y incy: incy
	<cdecl: double 'ddot_'  SDWORD * double * SDWORD * double * SDWORD * >
	^self invalidCall!

gemmWithtransa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: ltransa length: ltransb
	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"

	<cdecl: void 'dgemm_'  char * char * SDWORD* SDWORD* SDWORD* double * double * SDWORD* double * SDWORD* double * double * SDWORD* SDWORD SDWORD >
	^self invalidCall!

gemvWithtrans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: l
	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugate"

	<cdecl: void 'dgemv_'  char* SDWORD* SDWORD* double* double* SDWORD* double* SDWORD* double* double* SDWORD* SDWORD >
	^self invalidCall!

gercWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transposeConjugated(y)+A"

	^self 
		gerWithm: m
		n: n
		alpha: alpha
		x: x
		incx: incx
		y: y
		incy: incy
		a: a
		lda: lda!

geruWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	^self 
		gerWithm: m
		n: n
		alpha: alpha
		x: x
		incx: incx
		y: y
		incy: incy
		a: a
		lda: lda!

gerWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	<cdecl: void 'dger_' SDWORD* SDWORD* double * double * SDWORD* double * SDWORD* double * SDWORD*>
	^self invalidCall!

hemmWithside: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: lside length: luplo 
	"C=alpha*A*B+beta*C"

	^self 
		symmWithside: side
		uplo: uplo
		m: m
		n: n
		alpha: alpha
		a: a
		lda: lda
		b: b
		ldb: ldb
		beta: beta
		c: c
		ldc: ldc
		length: lside
		length: luplo!

hemvWithuplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: luplo 
	"y=alpha*X+beta*y"

	^self 
		symvWithuplo: uplo
		n: n
		alpha: alpha
		n: a
		lda: lda
		x: x
		incx: incx
		beta: beta
		y: y
		incy: incy
		length: luplo!

hpmvWithuplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy length: luplo 
	"y=alpha*X+beta*y"

	^self 
		spmvWithuplo: uplo
		n: n
		alpha: alpha
		ap: ap
		x: x
		incx: incx
		beta: beta
		y: y
		incy: incy
		length: luplo!

iamaxWithn: n x: x incx: incx
	"index of max : max_i (abs(real(x_i))+abs(imag(x_i)))"

	<cdecl: SDWORD 'idamax_'  SDWORD * double * SDWORD * >
	^self invalidCall!

nrm2Withn: n x: x incx: incx
	"||x||_2"

	<cdecl: double 'dnrm2_'  SDWORD * double * SDWORD * >
	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx
	^self scalWithn: n alpha: alpha x: x incx: incx!

rotgWitha: a b: b c: c s: s
	<cdecl: void 'drotg_'  double * double * double * double * >
	^self invalidCall!

rotWithn: n x: x incx: incx y: y incy: incy c: c s: s
	<cdecl: void 'drot_'  SDWORD * double * SDWORD * double * SDWORD * double * double * >
	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx
	<cdecl: void 'dscal_'  SDWORD * double * double * SDWORD * >
	^self invalidCall!

spmvWithuplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy length: luplo 
	"y=alpha*X+beta*y"

	<cdecl: void 'dspmv_'  char * SDWORD * double * double * double * SDWORD * double * double * SDWORD * SDWORD >
	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy
	"x <-> y"

	<cdecl: void 'dswap_'  SDWORD * double * SDWORD * double * SDWORD * >
	^self invalidCall!

symmWithside: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: lside length: luplo 
	"C=alpha*A*B+beta*C"

	<cdecl: void 'dsymm_'  char * char * SDWORD * SDWORD * double * double * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

symvWithuplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: luplo
	"y=alpha*X+beta*y"

	<cdecl: void 'dsymv_'  char * SDWORD * double * double * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD >
	^self invalidCall!

tpmvWithuplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx length: uploLength length: transLength length: diagLength 
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'dtpmv_'  char * char * char * SDWORD * double * double * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trmmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'dtrmm_'  char * char * char * char * SDWORD * SDWORD * double * double * SDWORD * double * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

trmvWithuplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx length: uploLength length: transLength length: diagLength
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'dtrmv_'  char * char * char * SDWORD * double * SDWORD * double * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trsmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'dtrsm_' char * char * char * char * SDWORD * SDWORD * double * double * SDWORD * double * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall! !
!BlasDLibrary categoriesFor: #asumWithn:x:incx:!public! !
!BlasDLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!BlasDLibrary categoriesFor: #cComplexPointerOn:!public! !
!BlasDLibrary categoriesFor: #cElementPointerOn:!public! !
!BlasDLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!BlasDLibrary categoriesFor: #cRealPointerOn:!public! !
!BlasDLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!BlasDLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!BlasDLibrary categoriesFor: #dotWithn:x:incx:y:incy:!public! !
!BlasDLibrary categoriesFor: #gemmWithtransa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasDLibrary categoriesFor: #gemvWithtrans:m:n:alpha:a:lda:x:incx:beta:y:incy:length:!public! !
!BlasDLibrary categoriesFor: #gercWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasDLibrary categoriesFor: #geruWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasDLibrary categoriesFor: #gerWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasDLibrary categoriesFor: #hemmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasDLibrary categoriesFor: #hemvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasDLibrary categoriesFor: #hpmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasDLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!BlasDLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!BlasDLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!BlasDLibrary categoriesFor: #rotgWitha:b:c:s:!public! !
!BlasDLibrary categoriesFor: #rotWithn:x:incx:y:incy:c:s:!public! !
!BlasDLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!BlasDLibrary categoriesFor: #spmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasDLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!BlasDLibrary categoriesFor: #symmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasDLibrary categoriesFor: #symvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasDLibrary categoriesFor: #tpmvWithuplo:trans:diag:n:ap:x:incx:length:length:length:!public! !
!BlasDLibrary categoriesFor: #trmmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasDLibrary categoriesFor: #trmvWithuplo:trans:diag:n:a:lda:x:incx:length:length:length:!public! !
!BlasDLibrary categoriesFor: #trsmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !

