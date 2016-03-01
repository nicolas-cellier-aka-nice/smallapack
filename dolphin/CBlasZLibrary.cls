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

asumWithn: n x: x incx: incx	"||real(x)||_1 + ||imag(x)||_1"	<cdecl: double 'cblas_dzasum' SDWORD  "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy	"y := alpha*x+y"	<cdecl: void 'cblas_zaxpy' SDWORD  "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD  "ExternalDoubleComplex"void * SDWORD  >	^self invalidCall!

cComplexPointerOn: aComplex 	^self cDoubleComplexPointerOn: aComplex!

cElementArgumentOn: aComplex 	^self cComplexPointerOn: aComplex!

cElementPointerOn: aComplex 	^self cComplexPointerOn: aComplex!

copyWithn: n x: x incx: incx y: y incy: incy	"y := x"	<cdecl: void 'cblas_zcopy' SDWORD  "ExternalDoubleComplex"void * SDWORD  "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

cRealPointerOn: aDouble 	^self cDoublePointerOn: aDouble!

dotcWithN: N X: X incX: incX Y: Y incY: incY 	| dotc |	dotc := DOUBLECOMPLEXArray with: Complex zero.	self 		dotcWithn: N		x: X		incx: incX		y: Y		incy: incY		dotc: dotc arrayPointer.	^dotc at: 1!

dotcWithn: n x: x incx: incx y: y incy: incy dotc: dotc	"dotc := transposeConjugate(x)*y"	<cdecl: void 'cblas_zdotc_sub' SDWORD  "ExternalDoubleComplex"void * SDWORD  "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * >	^self invalidCall!

dotuWithN: N X: X incX: incX Y: Y incY: incY 	| dotu |	dotu := DOUBLECOMPLEXArray with: Complex zero.	self 		dotuWithn: N		x: X		incx: incX		y: Y		incy: incY		dotu: dotu arrayPointer.	^dotu at: 1!

dotuWithn: n x: x incx: incx y: y incy: incy dotu: dotu	"dotu := transpose(x)*y"	<cdecl: void 'cblas_zdotu_sub' SDWORD  "ExternalDoubleComplex"void * SDWORD  "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * >	^self invalidCall!

gemmWithorder: order transa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"	<cdecl: void 'cblas_zgemm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD  "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

gemvWithorder: order trans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_zgemv' SDWORD SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD  "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

gercWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transposeConjugate(y)+A"	<cdecl: void 'cblas_zgerc' SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

geruWithorder: order m: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transpose(y)+A"	<cdecl: void 'cblas_zgeru' SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

hemmWithorder: order side: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc	"C=alpha*A*B+beta*C"	<cdecl: void 'cblas_zhemm' SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

hemvWithorder: order uplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	<cdecl: void 'cblas_zhemv' SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

hpmvWithorder: order uplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy	"y=alpha*X+beta*y"	<cdecl: void 'cblas_zhpmv' SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

iamaxWithn: n x: x incx: incx	"index of max : max_i (abs(real(x_i))+abs(imag(x_i)))"	<cdecl: SDWORD 'cblas_izamax' SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

nrm2Withn: n x: x incx: incx	"||x||_2"	<cdecl: double 'cblas_sznrm2' SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx	<cdecl: void 'cblas_zdscal' SDWORD double "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx	<cdecl: void 'cblas_zscal' SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy	"x <-> y"	<cdecl: void 'cblas_zswap' SDWORD "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

tpmvWithorder: order uplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_ztpmv' SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

trmmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_ztrmm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

trmvWithorder: order uplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx	"x=op(a)*x op=yourself, transpose, transposeConjugated"	<cdecl: void 'cblas_ztrmv' SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall!

trsmWithorder: order side: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb	<cdecl: void 'cblas_ztrsm' SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD SDWORD "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD "ExternalDoubleComplex"void * SDWORD >	^self invalidCall! !
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

