"Filed out from Dolphin Smalltalk 7"!

CMathLibrary subclass: #BlasLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasLibrary guid: (GUID fromString: '{DB5BFA89-AF2A-4A02-8FA0-365333BA4790}')!
BlasLibrary comment: ''!
!BlasLibrary categoriesForClass!Unclassified! !
!BlasLibrary methodsFor!

asumWithn: n x: x incx: incx 	"||real(x)||_1 + ||imag(x)||_1"	self subclassResponsibility!

asumWithN: N X: X incX: incX 	| cARGn cARGincx |	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	^self 		asumWithn: cARGn		x: X		incx: cARGincx!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy 	"y := alpha*x+y"	self subclassResponsibility!

axpyWithN: N alpha: alpha X: X incX: incX Y: Y incY: incY 	| cARGincx cARGincy cARGn cARGalpha |	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGn := self cIntegerPointerOn: N.	cARGalpha := self cElementPointerOn: alpha.	^self 		axpyWithn: cARGn		alpha: cARGalpha		x: X		incx: cARGincx		y: Y		incy: cARGincy!

cComplexPointerOn: aComplex 	^self subclassResponsibility!

cElementPointerOn: aComplex 	^self subclassResponsibility!

copyWithn: n x: x incx: incx y: y incy: incy 	"y := x"	self subclassResponsibility!

copyWithN: N X: X incX: incX Y: Y incY: incY 	| cARGn cARGincx cARGincy |	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	^self 		copyWithn: cARGn		x: X		incx: cARGincx		y: Y		incy: cARGincy!

cRealPointerOn: aComplex 	^self subclassResponsibility!

dotcWithn: n x: x incx: incx y: y incy: incy 	self subclassResponsibility!

dotcWithN: N X: X incX: incX Y: Y incY: incY 	| cARGincx cARGincy cARGn |	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGn := self cIntegerPointerOn: N.	^self 		dotcWithn: cARGn		x: X		incx: cARGincx		y: Y		incy: cARGincy!

dotuWithn: n x: x incx: incx y: y incy: incy 	self subclassResponsibility!

dotuWithN: N X: X incX: incX Y: Y incY: incY 	| cARGincx cARGincy cARGn |	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGn := self cIntegerPointerOn: N.	^self 		dotuWithn: cARGn		x: X		incx: cARGincx		y: Y		incy: cARGincy!

dotWithN: N X: X incX: incX Y: Y incY: incY 	| cARGincx cARGincy cARGn |	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGn := self cIntegerPointerOn: N.	^self 		dotuWithn: cARGn		x: X		incx: cARGincx		y: Y		incy: cARGincy!

gemmWithTransA: TransA TransB: TransB M: M N: N K: K alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 	| cARGtransa cARGtransb cARGm cARGn cARGk cARGalpha cARGbeta cARGldc cARGlda cARGldb |	cARGm := self cIntegerPointerOn: M.	cARGk := self cIntegerPointerOn: K.	cARGn := self cIntegerPointerOn: N.	cARGlda := self cIntegerPointerOn: lda.	cARGldb := self cIntegerPointerOn: ldb.	cARGldc := self cIntegerPointerOn: ldc.	cARGtransa := self cCharPointerOn: TransA.	cARGtransb := self cCharPointerOn: TransB.	cARGalpha := self cElementPointerOn: alpha.	cARGbeta := self cElementPointerOn: beta.	^self 		gemmWithtransa: cARGtransa		transb: cARGtransb		m: cARGm		n: cARGn		k: cARGk		alpha: cARGalpha		a: A		lda: cARGlda		b: B		ldb: cARGldb		beta: cARGbeta		c: C		ldc: cARGldc		length: 1		length: 1!

gemmWithtransa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: ltransa length: ltransb 	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"	self subclassResponsibility!

gemvWithtrans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: l 	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugated"	self subclassResponsibility!

gemvWithTransA: TransA M: M N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 	| cARGtransa cARGm cARGn cARGincx cARGincy cARGalpha cARGbeta cARGlda |	cARGm := self cIntegerPointerOn: M.	cARGn := self cIntegerPointerOn: N.	cARGlda := self cIntegerPointerOn: lda.	cARGtransa := self cCharPointerOn: TransA.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGalpha := self cElementPointerOn: alpha.	cARGbeta := self cElementPointerOn: beta.	^self 		gemvWithtrans: cARGtransa		m: cARGm		n: cARGn		alpha: cARGalpha		a: A		lda: cARGlda		x: X		incx: cARGincx		beta: cARGbeta		y: Y		incy: cARGincy		length: 1!

gercWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transposeConjugated(y)+A"	self subclassResponsibility!

gercWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 	| cARGm cARGn cARGalpha cARGlda cARGincx cARGincy |	cARGlda := self cIntegerPointerOn: lda.	cARGm := self cIntegerPointerOn: M.	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGalpha := self cElementPointerOn: alpha.	^self 		gercWithm: cARGm		n: cARGn		alpha: cARGalpha		x: X		incx: cARGincx		y: Y		incy: cARGincy		a: A		lda: cARGlda!

geruWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 	"A=alpha*x*transpose(y)+A"	self subclassResponsibility!

geruWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 	| cARGm cARGn cARGalpha cARGlda cARGincx cARGincy |	cARGlda := self cIntegerPointerOn: lda.	cARGm := self cIntegerPointerOn: M.	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGalpha := self cElementPointerOn: alpha.	^self 		geruWithm: cARGm		n: cARGn		alpha: cARGalpha		x: X		incx: cARGincx		y: Y		incy: cARGincy		a: A		lda: cARGlda!

gerWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 	| cARGm cARGn cARGalpha cARGlda cARGincx cARGincy |	cARGlda := self cIntegerPointerOn: lda.	cARGm := self cIntegerPointerOn: M.	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGalpha := self cElementPointerOn: alpha.	^self 		geruWithm: cARGm		n: cARGn		alpha: cARGalpha		x: X		incx: cARGincx		y: Y		incy: cARGincy		a: A		lda: cARGlda!

hemmWithSide: Side Uplo: Uplo M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 	| cARGm cARGn cARGalpha cARGbeta cARGldc cARGlda cARGldb cARGside cARGuplo |	cARGside := self cCharPointerOn: Side.	cARGuplo := self cCharPointerOn: Uplo.	cARGm := self cIntegerPointerOn: M.	cARGn := self cIntegerPointerOn: N.	cARGlda := self cIntegerPointerOn: lda.	cARGldb := self cIntegerPointerOn: ldb.	cARGldc := self cIntegerPointerOn: ldc.	cARGalpha := self cElementPointerOn: alpha.	cARGbeta := self cElementPointerOn: beta.	^self 		hemmWithside: cARGside		uplo: cARGuplo		m: cARGm		n: cARGn		alpha: cARGalpha		a: A		lda: cARGlda		b: B		ldb: cARGldb		beta: cARGbeta		c: C		ldc: cARGldc		length: 1		length: 1!

hemmWithside: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: sideLength length: uploLength 	"C=alpha*A*B+beta*C"	self subclassResponsibility!

hemvWithUplo: Uplo N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 	| cARGuplo cARGn cARGincx cARGincy cARGalpha cARGbeta cARGlda |	cARGn := self cIntegerPointerOn: N.	cARGlda := self cIntegerPointerOn: lda.	cARGuplo := self cCharPointerOn: Uplo.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGalpha := self cElementPointerOn: alpha.	cARGbeta := self cElementPointerOn: beta.	^self 		hemvWithuplo: cARGuplo		n: cARGn		alpha: cARGalpha		n: A		lda: cARGlda		x: X		incx: cARGincx		beta: cARGbeta		y: Y		incy: cARGincy		length: 1!

hemvWithuplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: uploLength	"y=alpha*X+beta*y"	self subclassResponsibility!

hpmvWithUplo: Uplo N: N alpha: alpha Ap: Ap X: X incX: incX beta: beta Y: Y incY: incY 	| cARGuplo cARGn cARGincx cARGincy cARGalpha cARGbeta |	cARGn := self cIntegerPointerOn: N.	cARGuplo := self cCharPointerOn: Uplo.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	cARGalpha := self cElementPointerOn: alpha.	cARGbeta := self cElementPointerOn: beta.	^self 		hpmvWithuplo: cARGuplo		n: cARGn		alpha: cARGalpha		ap: Ap		x: X		incx: cARGincx		beta: cARGbeta		y: Y		incy: cARGincy		length: 1!

hpmvWithuplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy length: uploLength 	"y=alpha*X+beta*y"	self subclassResponsibility!

left	^$L!

lower	^$L!

nonUnit	^$N!

notTransposed	^$N!

realScalWithn: n alpha: alpha x: x incx: incx	self subclassResponsibility!

realScalWithN: N alpha: alpha X: X incX: incX 	| cARGn cARGincx cARGalpha |	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	cARGalpha := self cRealPointerOn: alpha.	^self 		realScalWithn: cARGn		alpha: cARGalpha		x: X		incx: cARGincx!

right	^$R!

scalWithn: n alpha: alpha x: x incx: incx	self subclassResponsibility!

scalWithN: N alpha: alpha X: X incX: incX 	| cARGn cARGincx cARGalpha |	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	cARGalpha := self cElementPointerOn: alpha.	^self 		scalWithn: cARGn		alpha: cARGalpha		x: X		incx: cARGincx!

swapWithn: n x: x incx: incx y: y incy: incy 	"x <-> y"	self subclassResponsibility!

swapWithN: N X: X incX: incX Y: Y incY: incY 	| cARGn cARGincx cARGincy |	cARGn := self cIntegerPointerOn: N.	cARGincx := self cIntegerPointerOn: incX.	cARGincy := self cIntegerPointerOn: incY.	^self 		swapWithn: cARGn		x: X		incx: cARGincx		y: Y		incy: cARGincy!

tpmvWithuplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx length: uploLength length: transLength length: diagLength 	"x=op(a)*x op=yourself, transpose, transposeConjugated"	self subclassResponsibility!

tpmvWithUplo: Uplo TransA: TransA Diag: Diag N: N Ap: A X: X incX: incX 	| cARGtransa cARGn cARGincx cARGuplo cARGdiag |	cARGn := self cIntegerPointerOn: N.	cARGtransa := self cCharPointerOn: TransA.	cARGuplo := self cCharPointerOn: Uplo.	cARGdiag := self cCharPointerOn: Diag.	cARGincx := self cIntegerPointerOn: incX.	^self 		tpmvWithuplo: cARGuplo		trans: cARGtransa		diag: cARGdiag		n: cARGn		ap: A		x: X		incx: cARGincx		length: 1		length: 1		length: 1!

transposeConjugated	^$C!

transposed	^$T!

trmmWithside: side uplo: uplo trans: transa diag: diag m: m n: n alpha: alpha a: A lda: lda b: B ldb: ldb length: lside length: luplo length: ltrans length: ldiag 	self subclassResponsibility!

trmmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 	| cARGm cARGn cARGalpha cARGlda cARGldb cARGside cARGuplo cARGtransa cARGdiag |	cARGside := self cCharPointerOn: Side.	cARGuplo := self cCharPointerOn: Uplo.	cARGtransa := self cCharPointerOn: TransA.	cARGdiag := self cCharPointerOn: Diag.	cARGm := self cIntegerPointerOn: M.	cARGn := self cIntegerPointerOn: N.	cARGlda := self cIntegerPointerOn: lda.	cARGldb := self cIntegerPointerOn: ldb.	cARGalpha := self cElementPointerOn: alpha.	^self 		trmmWithside: cARGside		uplo: cARGuplo		trans: cARGtransa		diag: cARGdiag		m: cARGm		n: cARGn		alpha: cARGalpha		a: A		lda: cARGlda		b: B		ldb: cARGldb		length: 1		length: 1		length: 1		length: 1!

trmvWithuplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx length: uploLength length: transLength length: diagLength 	"x=op(a)*x op=yourself, transpose, transposeConjugated"	self subclassResponsibility!

trmvWithUplo: Uplo TransA: TransA Diag: Diag N: N A: A lda: lda X: X incX: incX 	| cARGtransa cARGn cARGincx cARGlda cARGuplo cARGdiag |	cARGn := self cIntegerPointerOn: N.	cARGlda := self cIntegerPointerOn: lda.	cARGtransa := self cCharPointerOn: TransA.	cARGuplo := self cCharPointerOn: Uplo.	cARGdiag := self cCharPointerOn: Diag.	cARGincx := self cIntegerPointerOn: incX.	^self 		trmvWithuplo: cARGuplo		trans: cARGtransa		diag: cARGdiag		n: cARGn		a: A		lda: cARGlda		x: X		incx: cARGincx		length: 1		length: 1		length: 1!

trsmWithside: side uplo: uplo trans: transa diag: diag m: m n: n alpha: alpha a: A lda: lda b: B ldb: ldb length: lside length: luplo length: ltrans length: ldiag 	self subclassResponsibility!

trsmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 	| cARGm cARGn cARGalpha cARGlda cARGldb cARGside cARGuplo cARGtransa cARGdiag |	cARGside := self cCharPointerOn: Side.	cARGuplo := self cCharPointerOn: Uplo.	cARGtransa := self cCharPointerOn: TransA.	cARGdiag := self cCharPointerOn: Diag.	cARGm := self cIntegerPointerOn: M.	cARGn := self cIntegerPointerOn: N.	cARGlda := self cIntegerPointerOn: lda.	cARGldb := self cIntegerPointerOn: ldb.	cARGalpha := self cElementPointerOn: alpha.	^self 		trsmWithside: cARGside		uplo: cARGuplo		trans: cARGtransa		diag: cARGdiag		m: cARGm		n: cARGn		alpha: cARGalpha		a: A		lda: cARGlda		b: B		ldb: cARGldb		length: 1		length: 1		length: 1		length: 1!

unit	^$U!

upper	^$U! !
!BlasLibrary categoriesFor: #asumWithn:x:incx:!public! !
!BlasLibrary categoriesFor: #asumWithN:X:incX:!public! !
!BlasLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #axpyWithN:alpha:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #cComplexPointerOn:!public! !
!BlasLibrary categoriesFor: #cElementPointerOn:!public! !
!BlasLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #copyWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #cRealPointerOn:!public! !
!BlasLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #dotcWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #dotuWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #dotWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #gemmWithTransA:TransB:M:N:K:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!BlasLibrary categoriesFor: #gemmWithtransa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasLibrary categoriesFor: #gemvWithtrans:m:n:alpha:a:lda:x:incx:beta:y:incy:length:!public! !
!BlasLibrary categoriesFor: #gemvWithTransA:M:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!BlasLibrary categoriesFor: #gercWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasLibrary categoriesFor: #gercWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!BlasLibrary categoriesFor: #geruWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasLibrary categoriesFor: #geruWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!BlasLibrary categoriesFor: #gerWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!BlasLibrary categoriesFor: #hemmWithSide:Uplo:M:N:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!BlasLibrary categoriesFor: #hemmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasLibrary categoriesFor: #hemvWithUplo:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!BlasLibrary categoriesFor: #hemvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasLibrary categoriesFor: #hpmvWithUplo:N:alpha:Ap:X:incX:beta:Y:incY:!public! !
!BlasLibrary categoriesFor: #hpmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasLibrary categoriesFor: #left!public! !
!BlasLibrary categoriesFor: #lower!public! !
!BlasLibrary categoriesFor: #nonUnit!public! !
!BlasLibrary categoriesFor: #notTransposed!public! !
!BlasLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!BlasLibrary categoriesFor: #realScalWithN:alpha:X:incX:!public! !
!BlasLibrary categoriesFor: #right!public! !
!BlasLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!BlasLibrary categoriesFor: #scalWithN:alpha:X:incX:!public! !
!BlasLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!BlasLibrary categoriesFor: #swapWithN:X:incX:Y:incY:!public! !
!BlasLibrary categoriesFor: #tpmvWithuplo:trans:diag:n:ap:x:incx:length:length:length:!public! !
!BlasLibrary categoriesFor: #tpmvWithUplo:TransA:Diag:N:Ap:X:incX:!public! !
!BlasLibrary categoriesFor: #transposeConjugated!public! !
!BlasLibrary categoriesFor: #transposed!public! !
!BlasLibrary categoriesFor: #trmmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasLibrary categoriesFor: #trmmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!BlasLibrary categoriesFor: #trmvWithuplo:trans:diag:n:a:lda:x:incx:length:length:length:!public! !
!BlasLibrary categoriesFor: #trmvWithUplo:TransA:Diag:N:A:lda:X:incX:!public! !
!BlasLibrary categoriesFor: #trsmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasLibrary categoriesFor: #trsmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!BlasLibrary categoriesFor: #unit!public! !
!BlasLibrary categoriesFor: #upper!public! !

!BlasLibrary class methodsFor!

fileName
	"Answer the host system file name for the library"

	^'BLAS'! !
!BlasLibrary class categoriesFor: #fileName!public! !

