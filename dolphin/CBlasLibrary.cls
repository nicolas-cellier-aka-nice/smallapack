"Filed out from Dolphin Smalltalk 7"!

CMathLibrary subclass: #CBlasLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CBlasLibrary guid: (GUID fromString: '{96613F99-4779-49A0-9458-9E82FF14EFB0}')!
CBlasLibrary comment: ''!
!CBlasLibrary categoriesForClass!Unclassified! !
!CBlasLibrary methodsFor!

asumWithN: N X: X incX: incX 	^self 		asumWithn: N		x: X		incx: incX!

axpyWithN: N alpha: alpha X: X incX: incX Y: Y incY: incY 	| cARGalpha |	cARGalpha := self cElementArgumentOn: alpha.	^self 		axpyWithn: N		alpha: cARGalpha		x: X		incx: incX		y: Y		incy: incY!

cComplexPointerOn: aComplex 	^self subclassResponsibility!

cElementArgumentOn: aComplex 	^self subclassResponsibility!

cElementPointerOn: aComplex 	^self subclassResponsibility!

colMajor	^102!

conjugated	^114!

copyWithN: N X: X incX: incX Y: Y incY: incY 	^self 		copyWithn: N		x: X		incx: incX		y: Y		incy: incY!

cRealPointerOn: aComplex 	^self subclassResponsibility!

dotcWithN: N X: X incX: incX Y: Y incY: incY 	^self 		dotcWithn: N		x: X		incx: incX		y: Y		incy: incY!

dotuWithN: N X: X incX: incX Y: Y incY: incY 	^self 		dotuWithn: N		x: X		incx: incX		y: Y		incy: incY!

dotWithN: N X: X incX: incX Y: Y incY: incY 	^self 		dotuWithn: N		x: X		incx: incX		y: Y		incy: incY!

gemmWithTransA: TransA TransB: TransB M: M N: N K: K alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 	| cARGalpha cARGbeta |	cARGalpha := self cElementArgumentOn: alpha.	cARGbeta := self cElementArgumentOn: beta.	^self 		gemmWithorder: self colMajor		transa: TransA		transb: TransB		m: M		n: N		k: K		alpha: cARGalpha		a: A		lda: lda		b: B		ldb: ldb		beta: cARGbeta		c: C		ldc: ldc!

gemvWithTransA: TransA M: M N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 	| cARGalpha cARGbeta |	cARGalpha := self cElementArgumentOn: alpha.	cARGbeta := self cElementArgumentOn: beta.	^self 		gemvWithorder: self colMajor		trans: TransA		m: M		n: N		alpha: cARGalpha		a: A		lda: lda		x: X		incx: incX		beta: cARGbeta		y: Y		incy: incY!

gercWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 	| cARGalpha |	cARGalpha := self cElementArgumentOn: alpha.	^self 		gercWithorder: self colMajor		m: M		n: N		alpha: cARGalpha		x: X		incx: incX		y: Y		incy: incY		a: A		lda: lda!

geruWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 	| cARGalpha |	cARGalpha := self cElementArgumentOn: alpha.	^self 		geruWithorder: self colMajor		m: M		n: N		alpha: cARGalpha		x: X		incx: incX		y: Y		incy: incY		a: A		lda: lda!

gerWithM: M N: N alpha: alpha X: X incX: incX Y: Y incY: incY A: A lda: lda 	| cARGalpha |	cARGalpha := self cElementArgumentOn: alpha.	^self 		geruWithorder: self colMajor		m: M		n: N		alpha: cARGalpha		x: X		incx: incX		y: Y		incy: incY		a: A		lda: lda!

hemmWithSide: Side Uplo: Uplo M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb beta: beta C: C ldc: ldc 	| cARGalpha cARGbeta |	cARGalpha := self cElementArgumentOn: alpha.	cARGbeta := self cElementArgumentOn: beta.	^self 		hemmWithorder: self colMajor		side: Side		uplo: Uplo		m: M		n: N		alpha: cARGalpha		a: A		lda: lda		b: B		ldb: ldb		beta: cARGbeta		c: C		ldc: ldc!

hemvWithUplo: Uplo N: N alpha: alpha A: A lda: lda X: X incX: incX beta: beta Y: Y incY: incY 	| cARGalpha cARGbeta |	cARGalpha := self cElementArgumentOn: alpha.	cARGbeta := self cElementArgumentOn: beta.	self 		hemvWithorder: self colMajor		uplo: Uplo		n: N		alpha: cARGalpha		n: A		lda: lda		x: X		incx: incX		beta: cARGbeta		y: Y		incy: incY!

hpmvWithUplo: Uplo N: N alpha: alpha Ap: Ap X: X incX: incX beta: beta Y: Y incY: incY 	| cARGalpha cARGbeta |	cARGalpha := self cElementArgumentOn: alpha.	cARGbeta := self cElementArgumentOn: beta.	^self 		hpmvWithorder: self colMajor		uplo: Uplo		n: N		alpha: cARGalpha		ap: Ap		x: X		incx: incX		beta: cARGbeta		y: Y		incy: incY!

left	^141!

lower	^122!

nonUnit	^131!

notTransposed	^111!

realScalWithN: N alpha: alpha X: X incX: incX 	^self 		realScalWithn: N		alpha: alpha		x: X		incx: incX!

right	^142!

rowMajor	^101!

scalWithN: N alpha: alpha X: X incX: incX 	| cARGalpha |	cARGalpha := self cElementArgumentOn: alpha.	^self 		scalWithn: N		alpha: cARGalpha		x: X		incx: incX!

swapWithN: N X: X incX: incX Y: Y incY: incY 	^self 		swapWithn: N		x: X		incx: incX		y: Y		incy: incY!

tpmvWithUplo: Uplo TransA: TransA Diag: Diag N: N Ap: A X: X incX: incX 	^self 		tpmvWithorder: self colMajor		uplo: Uplo		trans: TransA		diag: Diag		n: N		ap: A		x: X		incx: incX!

transposeConjugated	^113!

transposed	^112!

trmmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 	| cARGalpha |	cARGalpha := self cElementArgumentOn: alpha.	^self 		trmmWithorder: self colMajor		side: Side		uplo: Uplo		trans: TransA		diag: Diag		m: M		n: N		alpha: cARGalpha		a: A		lda: lda		b: B		ldb: ldb!

trmvWithUplo: Uplo TransA: TransA Diag: Diag N: N A: A lda: lda X: X incX: incX 	^self 		trmvWithorder: self colMajor		uplo: Uplo		trans: TransA		diag: Diag		n: N		a: A		lda: lda		x: X		incx: incX!

trsmWithSide: Side Uplo: Uplo TransA: TransA Diag: Diag M: M N: N alpha: alpha A: A lda: lda B: B ldb: ldb 	| cARGalpha |	cARGalpha := self cElementArgumentOn: alpha.	^self 		trsmWithorder: self colMajor		side: Side		uplo: Uplo		trans: TransA		diag: Diag		m: M		n: N		alpha: cARGalpha		a: A		lda: lda		b: B		ldb: ldb!

unit	^132!

upper	^121! !
!CBlasLibrary categoriesFor: #asumWithN:X:incX:!public! !
!CBlasLibrary categoriesFor: #axpyWithN:alpha:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #cComplexPointerOn:!public! !
!CBlasLibrary categoriesFor: #cElementArgumentOn:!public! !
!CBlasLibrary categoriesFor: #cElementPointerOn:!public! !
!CBlasLibrary categoriesFor: #colMajor!public! !
!CBlasLibrary categoriesFor: #conjugated!public! !
!CBlasLibrary categoriesFor: #copyWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #cRealPointerOn:!public! !
!CBlasLibrary categoriesFor: #dotcWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #dotuWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #dotWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #gemmWithTransA:TransB:M:N:K:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!CBlasLibrary categoriesFor: #gemvWithTransA:M:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!CBlasLibrary categoriesFor: #gercWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!CBlasLibrary categoriesFor: #geruWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!CBlasLibrary categoriesFor: #gerWithM:N:alpha:X:incX:Y:incY:A:lda:!public! !
!CBlasLibrary categoriesFor: #hemmWithSide:Uplo:M:N:alpha:A:lda:B:ldb:beta:C:ldc:!public! !
!CBlasLibrary categoriesFor: #hemvWithUplo:N:alpha:A:lda:X:incX:beta:Y:incY:!public! !
!CBlasLibrary categoriesFor: #hpmvWithUplo:N:alpha:Ap:X:incX:beta:Y:incY:!public! !
!CBlasLibrary categoriesFor: #left!public! !
!CBlasLibrary categoriesFor: #lower!public! !
!CBlasLibrary categoriesFor: #nonUnit!public! !
!CBlasLibrary categoriesFor: #notTransposed!public! !
!CBlasLibrary categoriesFor: #realScalWithN:alpha:X:incX:!public! !
!CBlasLibrary categoriesFor: #right!public! !
!CBlasLibrary categoriesFor: #rowMajor!public! !
!CBlasLibrary categoriesFor: #scalWithN:alpha:X:incX:!public! !
!CBlasLibrary categoriesFor: #swapWithN:X:incX:Y:incY:!public! !
!CBlasLibrary categoriesFor: #tpmvWithUplo:TransA:Diag:N:Ap:X:incX:!public! !
!CBlasLibrary categoriesFor: #transposeConjugated!public! !
!CBlasLibrary categoriesFor: #transposed!public! !
!CBlasLibrary categoriesFor: #trmmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!CBlasLibrary categoriesFor: #trmvWithUplo:TransA:Diag:N:A:lda:X:incX:!public! !
!CBlasLibrary categoriesFor: #trsmWithSide:Uplo:TransA:Diag:M:N:alpha:A:lda:B:ldb:!public! !
!CBlasLibrary categoriesFor: #unit!public! !
!CBlasLibrary categoriesFor: #upper!public! !

!CBlasLibrary class methodsFor!

fileName
	"Answer the host system file name for the library"

	^SmallapackSettings cblasLibraryName! !
!CBlasLibrary class categoriesFor: #fileName!public! !

