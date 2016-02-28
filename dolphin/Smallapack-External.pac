| package |
package := Package name: 'Smallapack-External'.
package paxVersion: 1;
	basicComment: 'Smallapack-External defines the BLAS-LAPACK external libraries.

These libraries are for doing LINEAR ALGEBRA on numerical matrices
- single or double precision floating point
- in real or complex field'.


package classNames
	add: #ArrayCLibrary;
	add: #ArrayDLibrary;
	add: #ArrayLibrary;
	add: #ArraySLibrary;
	add: #ArrayZLibrary;
	add: #BlasCLibrary;
	add: #BlasDLibrary;
	add: #BlasLibrary;
	add: #BlasSLibrary;
	add: #BlasZLibrary;
	add: #CMathLibrary;
	add: #DOUBLECOMPLEX;
	add: #DOUBLECOMPLEXArray;
	add: #FLOATCOMPLEX;
	add: #FLOATCOMPLEXArray;
	add: #LapackCLibrary;
	add: #LapackDLibrary;
	add: #LapackLibrary;
	add: #LapackSLibrary;
	add: #LapackZLibrary;
	yourself.

package methodNames
	add: #ExternalArray -> #replaceFrom:to:with:;
	add: #ExternalArray -> #withArrayOffsetBy:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	add: 'Smallapack-Settings';
	yourself).

package!

"Class Definitions"!

ExternalLibrary subclass: #CMathLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CMathLibrary subclass: #ArrayLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CMathLibrary subclass: #BlasLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CMathLibrary subclass: #LapackLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArrayLibrary subclass: #ArrayCLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArrayLibrary subclass: #ArrayDLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArrayLibrary subclass: #ArraySLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArrayLibrary subclass: #ArrayZLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasLibrary subclass: #BlasCLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasLibrary subclass: #BlasDLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasLibrary subclass: #BlasSLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BlasLibrary subclass: #BlasZLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackLibrary subclass: #LapackCLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackLibrary subclass: #LapackDLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackLibrary subclass: #LapackSLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackLibrary subclass: #LapackZLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #DOUBLECOMPLEX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #FLOATCOMPLEX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalArray subclass: #DOUBLECOMPLEXArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalArray subclass: #FLOATCOMPLEXArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ExternalArray methodsFor!

replaceFrom: start to: stop with: replacementElements
	"Destructively replace the elements of the receiver between the <integer> arguments
	start and stop with the <Object> elements of the <sequencedReadableCollection> argument, 
	replacementElements. Answer the receiver. Raise an Exception if  replacementElements does 
	not contain the number of elements required to exactly fill the replacement interval in the receiver."

	replacementElements size = (stop - start + 1)
		ifFalse: [^self error: 'size of replacement incorrect'].
	replacementElements class isBytes
		ifTrue: [^self replaceFrom: start to: stop with: replacementElements startingAt: 1].

	"revert to slow implementation"
	start to: stop do: [:i | self at: i put: (replacementElements at: 1 + i - start)]!

withArrayOffsetBy: anInteger
	"Answer a copy pointing on a subArray"

	| subArray subAddress |
	anInteger = 0 ifTrue: [^self].
	(anInteger >= self size or: [anInteger < 0]) ifTrue: [self error: 'array offset point out of memory'].
	subAddress := bytes yourAddress + (anInteger * self elementSize).
	subArray := self class fromAddress: subAddress length: (self size - anInteger).
	subArray beUnfinalizable.
	^subArray! !
!ExternalArray categoriesFor: #replaceFrom:to:with:!public!replacing! !
!ExternalArray categoriesFor: #withArrayOffsetBy:!converting!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

CMathLibrary guid: (GUID fromString: '{2AD5423B-7890-4A4B-BA5F-1148296E867B}')!
CMathLibrary comment: ''!
!CMathLibrary categoriesForClass!Unclassified! !
!CMathLibrary methodsFor!

allocateDoubleArraySize: n
	^(DOUBLEArray new: n) asParameter!

allocateDoubleComplexArraySize: n
	^(DOUBLECOMPLEXArray new: n) asParameter!

allocateFloatArraySize: n
	^(FLOATArray new: n) asParameter!

allocateFloatComplexArraySize: n
	^(FLOATCOMPLEXArray new: n) asParameter!

allocateIntegerArraySize: n
	^(SDWORDArray new: n) asParameter!

allocateLogicalArraySize: n
	^DWORDArray new: n!

cCharPointerOn: aCharacter 	"Usefull for passing a character*1 argument to FORTRAN"	^(String with: aCharacter)	"Shouldn't we encode it ?"!

cDoubleComplexPointerOn: aComplex 
	"Usefull for passing a double complex (complex*16) argument to FORTRAN"
	
	| comp |
	comp := aComplex asComplex.
	^(DOUBLECOMPLEXArray with: comp)
		asParameter!

cDoublePointerOn: aDouble 
	"Usefull for passing a double precision (real*8) argument to FORTRAN"

	^(DOUBLEArray with: aDouble asFloat)
		asParameter!

cFloatComplexPointerOn: aComplex 
	"Usefull for passing a complex (complex*8) argument to FORTRAN"
	
	| comp |
	comp := aComplex asComplex.
	^(FLOATCOMPLEXArray with: comp)
		asParameter!

cFloatPointerOn: aFloat 
	"Usefull for passing a real (real*4) argument to FORTRAN"

	^(FLOATArray with: aFloat)
		asParameter!

cIntegerPointerOn: anInteger 
	"Usefull for passing an integer (integer*4) argument to FORTRAN.
	We do not use WordArray, because they are unsigned"

	^(SDWORDArray with: anInteger) asParameter!

cLogicalPointerOn: aBoolean 
	"Usefull for passing a logical (logical*4) argument to FORTRAN"

	^DWORDArray with: (aBoolean ifTrue: [1] ifFalse: [0])!

free: aVariable
	"free aVariable memory if allocated on external heap"
	
	"(aVariable class isKindOf: ExternalAddress) ifTrue: [aVariable free]"! !
!CMathLibrary categoriesFor: #allocateDoubleArraySize:!memory!public! !
!CMathLibrary categoriesFor: #allocateDoubleComplexArraySize:!public! !
!CMathLibrary categoriesFor: #allocateFloatArraySize:!memory!public! !
!CMathLibrary categoriesFor: #allocateFloatComplexArraySize:!public! !
!CMathLibrary categoriesFor: #allocateIntegerArraySize:!memory!public! !
!CMathLibrary categoriesFor: #allocateLogicalArraySize:!public! !
!CMathLibrary categoriesFor: #cCharPointerOn:!public! !
!CMathLibrary categoriesFor: #cDoubleComplexPointerOn:!public! !
!CMathLibrary categoriesFor: #cDoublePointerOn:!public! !
!CMathLibrary categoriesFor: #cFloatComplexPointerOn:!public! !
!CMathLibrary categoriesFor: #cFloatPointerOn:!public! !
!CMathLibrary categoriesFor: #cIntegerPointerOn:!public! !
!CMathLibrary categoriesFor: #cLogicalPointerOn:!public! !
!CMathLibrary categoriesFor: #free:!public! !

ArrayLibrary guid: (GUID fromString: '{02F6E2FD-9EA4-46FE-A025-5D64C4CD6056}')!
ArrayLibrary comment: ''!
!ArrayLibrary categoriesForClass!Unclassified! !
!ArrayLibrary class methodsFor!

fileName
	"Answer the host system file name for the library"

	^'SMALLAPACKARRAY'! !
!ArrayLibrary class categoriesFor: #fileName!public! !

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

	^SmallapackSettings blasLibraryName! !
!BlasLibrary class categoriesFor: #fileName!public! !

LapackLibrary guid: (GUID fromString: '{021ADB71-B752-493D-8724-4E576D275EBA}')!
LapackLibrary comment: ''!
!LapackLibrary categoriesForClass!Unclassified! !
!LapackLibrary methodsFor!

allEigenValues	^$A!

allocateComplexArraySize: anInteger 	^self isDoublePrecision		ifTrue: [self allocateDoubleComplexArraySize: anInteger]		ifFalse: [self allocateFloatComplexArraySize: anInteger]!

allocateElementArraySize: anInteger 	^self isReal		ifTrue: [self allocateRealArraySize: anInteger]		ifFalse: [self allocateComplexArraySize: anInteger]!

allocateRealArraySize: anInteger 	^self isDoublePrecision		ifTrue: [self allocateDoubleArraySize: anInteger]		ifFalse: [self allocateFloatArraySize: anInteger]!

allSingularVector	^$A!

balanceDoNothing	"balance do nothing, means finding ilo and ihi bounds of non triangular submatrix"	^$N!

balancePermute	^$P!

balancePermuteAndScale	^$B!

balanceScale	^$S!

cComplexPointerOn: aComplex 	self subclassResponsibility!

cElementPointerOn: aComplex 	self subclassResponsibility!

cRealPointerOn: aDouble 	self subclassResponsibility!

dlamch: cmach length: lcmach
	"extract doublereal precision machine parameters
	.	'E' or 'e',   DLAMCH := eps
	.	'S' or 's ,   DLAMCH := sfmin
	.	'B' or 'b',   DLAMCH := base
	.	'P' or 'p',   DLAMCH := eps*base
	.	'N' or 'n',   DLAMCH := t
	.	'R' or 'r',   DLAMCH := rnd
	.	'M' or 'm',   DLAMCH := emin
	.	'U' or 'u',   DLAMCH := rmin
	.	'L' or 'l',   DLAMCH := emax
	.	'O' or 'o',   DLAMCH := rmax
	where
	.	eps   = relative machine precision
	.	sfmin = safe minimum, such that 1/sfmin does not overflow
	.	base  = base of the machine
	.	prec  = eps*base
	.	t     = number of (base) digits in the mantissa
	.	rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
	.	emin  = minimum exponent before (gradual) underflow
	.	rmin  = underflow threshold - base**(emin-1)
	.	emax  = largest exponent before overflow
	.	rmax  = overflow threshold  - (base**emax)*(1-eps)"

	<cdecl: double 'dlamch_'  char *  SDWORD >!

doComputeVector	"used by eigen value or svd decomposition"	^$V!

dontComputeVector	"used by eigen value or svd decomposition"	^$N!

eps	"return relative machine precision"	^self lamch: $E!

gebakWithjob: job side: side n: n ilo: ilo ihi: ihi scale: scale m: m v: v ldv: ldv 	"balance back a general matrix	ilo and ihi must be allocated outside this routine"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgebakWithjob: (self cCharPointerOn: job)		side: (self cCharPointerOn: side)		n: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		scale: scale		m: (self cIntegerPointerOn: m)		v: v		ldv: (self cIntegerPointerOn: ldv)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

gebalWithjob: job n: n a: a lda: lda ilo: ilo ihi: ihi scale: scale 	"balance a general matrix	ilo and ihi must be allocated outside this routine"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgebalWithjob: (self cCharPointerOn: job)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ilo: ilo		ihi: ihi		scale: scale		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

geconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeconWithnorm: (self cCharPointerOn: norm)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		anorm: (self cElementPointerOn: anorm)		rcond: rcond		work: work		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

geconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeconWithnorm: (self cCharPointerOn: norm)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		anorm: (self cRealPointerOn: anorm)		rcond: rcond		work: work		rwork: rwork		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim w: w vs: vs ldvs: ldvs 	"schur decomposition (for complex general matrices)	Note: sdim must be allocated outside	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork bwork lwork |	work := self allocateElementArraySize: 1.	bwork := self allocateLogicalArraySize: n.	rwork := self allocateRealArraySize: n.	(self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		w: w		vs: vs		ldvs: ldvs		work: work		lwork: -1		rwork: rwork		bwork: bwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 2 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		w: w		vs: vs		ldvs: ldvs		work: work		lwork: lwork		rwork: rwork		bwork: bwork!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim w: w vs: vs ldvs: ldvs work: work lwork: lwork rwork: rwork bwork: bwork 	"schur decomposition (for complex general matrices)	Note: sdim must be allocated outsied"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self 		xgeesWithjobvs: (self cCharPointerOn: jobvs)		sort: (self cCharPointerOn: sort)		select: select		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		sdim: sdim		w: w		vs: vs		ldvs: (self cIntegerPointerOn: ldvs)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		bwork: bwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs 	"schur decomposition (for real general matrices)	Note: sdim must be allocated outside	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work bwork lwork |	work := self allocateElementArraySize: 1.	bwork := self allocateLogicalArraySize: n.	(self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		wr: wr		wi: wi		vs: vs		ldvs: ldvs		work: work		lwork: -1		bwork: bwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 3 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		wr: wr		wi: wi		vs: vs		ldvs: ldvs		work: work		lwork: lwork		bwork: bwork!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs work: work lwork: lwork bwork: bwork 	"schur decomposition (for real general matrices)	Note: sdim must be allocated outsied"	| cARGjobvs cARGsort cARGn cARGlda cARGldvs cARGlwork cARGinfo |	cARGjobvs := self cCharPointerOn: jobvs.	cARGsort := self cCharPointerOn: sort.	cARGn := self cIntegerPointerOn: n.	cARGlda := self cIntegerPointerOn: lda.	cARGldvs := self cIntegerPointerOn: ldvs.	cARGlwork := self cIntegerPointerOn: lwork.	cARGinfo := self cIntegerPointerOn: 0.	self 		xgeesWithjobvs: cARGjobvs		sort: cARGsort		select: select		n: cARGn		a: a		lda: cARGlda		sdim: sdim		wr: wr		wi: wi		vs: vs		ldvs: cARGldvs		work: work		lwork: cARGlwork 		bwork: bwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"eigenvalue/vector decomposition (for complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: 2 * n.	(self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		w: w		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1		rwork: rwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 2 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		w: w		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork		rwork: rwork!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork 	"eigenvalue/vector decomposition (for complex general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"eigenvalue/vector decomposition (for real general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		wr: wr		wi: wi		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		wr: wr		wi: wi		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork 	"eigenvalue/vector decomposition (for real general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		wr: wr		wi: wi		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

geevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr scale: scale abnrm: abnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeevxWithbalanc: (self cCharPointerOn: balanc)		jobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		sense: (self cCharPointerOn: sense)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		scale: scale		abnrm: abnrm		rconde: rconde		rcondv: rcondv		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

gehrdWithn: n a: a lda: lda tau: tau 	"hessenberg decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork ilo ihi |	work := self allocateElementArraySize: 1.	ilo := self cIntegerPointerOn: 1.	ihi := self cIntegerPointerOn: n.	(self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

gehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau 	"hessenberg decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK	ilo and ihi must be allocated outside"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

gehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork 	"hessenberg decomposition (for real/complex general matrices)	ilo and ihi must be allocated outside this routine"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgehrdWithn: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelqfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank 	"least squares by SVD divide and conquer algorithm	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork iwork smlsiz nlvl minmn |	minmn := 1 max: (m min: n).	^self isComplex 		ifTrue: 			[			smlsiz := self 						ilaenvWithispec: 9						name: 'CGELSD'						opts: ' '						n1: 0						n2: 0						n3: 0						n4: 0.			nlvl := 1 + (minmn / (smlsiz + 1)) floorLog2.			work := self allocateElementArraySize: 1.			iwork := self allocateIntegerArraySize: 3 * minmn * nlvl + (11 * minmn).			rwork := self allocateRealArraySize: (m >= n 								ifTrue: [8 * n + (2 * n * smlsiz) + (8 * n * nlvl) + (n * nrhs)]								ifFalse: [8 * m + (2 * m * smlsiz) + (8 * m * nlvl) + (m * nrhs)]).			(self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: -1				rwork: rwork				iwork: iwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: 					[lwork := m >= n 								ifTrue: [1 max: n * (2 + nrhs)]								ifFalse: [1 max: m * (2 + nrhs)]].			work := self allocateElementArraySize: lwork.			self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				rwork: rwork				iwork: iwork] 		ifFalse: 			[			smlsiz := self 						ilaenvWithispec: 9						name: 'SGELSD'						opts: ' '						n1: 0						n2: 0						n3: 0						n4: 0.			nlvl := 1 + (minmn / (smlsiz + 1)) floorLog2.			work := self allocateElementArraySize: 1.			iwork := self allocateIntegerArraySize: 3 * minmn * nlvl + (11 * minmn).			(self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				iwork: iwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: 					[lwork := m >= n 								ifTrue: [11 * n + (2 * n * smlsiz) + (8 * n * nlvl) + (n * nrhs)]								ifFalse: [11 * m + (2 * m * smlsiz) + (8 * m * nlvl) + (m * nrhs)]].			work := self allocateElementArraySize: lwork.			self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				iwork: iwork]!

gelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsdWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsdWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank 	"least squares by SVD	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: (5 * (m min: n) max: 1).			(self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 1 max: (m min: n) * 2 + (nrhs max: (m max: n))].			work := self allocateElementArraySize: lwork.			self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: 					[lwork := 1 								max: (m min: n) * 3 + ((nrhs max: (m max: n)) max: 2 * (m min: n))].			work := self allocateElementArraySize: lwork.			self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork]!

gelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelssWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelssWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelsWithtrans: trans m: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsWithtrans: (self cCharPointerOn: trans)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

gelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsxWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsxWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		rwork: rwork		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank 	"least squares by complete orthogonalization	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| mn work rwork lwork |	mn := m min: n.	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: (2 * n max: 1).			(self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 1 max: mn + (2 * mn max: (n + 1 max: mn + nrhs))].			work := self allocateElementArraySize: lwork.			self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 1 max: (n * 3 + 1 + mn max: mn * 2 + nrhs)].			work := self allocateElementArraySize: lwork.			self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: lwork]!

gelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsyWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsyWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

geqlfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqlfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

geqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau 	"qr decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: 2 * n.			(self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := n + 1].			work := self allocateElementArraySize: lwork.			self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 3 * n + 1].			work := self allocateElementArraySize: lwork.			self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: lwork]!

geqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqp3Withm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		jpvt: jpvt		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

geqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqp3Withm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		jpvt: jpvt		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

geqrfWithm: m n: n a: a lda: lda tau: tau 	"qr decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		geqrfWithm: m		n: n		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geqrfWithm: m		n: n		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

geqrfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	"qr decomposition (for real/complex general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqrfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gerqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgerqfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesddWithjobz: (self cCharPointerOn: jobz)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

gesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesddWithjobz: (self cCharPointerOn: jobz)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

gesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt 	"singular value/vector decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: 5 * (m min: n).			(self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 2 * (m min: n) + (m max: n)].			work := self allocateElementArraySize: lwork.			self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 3 * (m min: n) + (m max: n) max: 5 * (m min: n)].			work := self allocateElementArraySize: lwork.			self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: lwork]!

gesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork 	"singular value/vector decomposition (for real general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesvdWithjobu: (self cCharPointerOn: jobu)		jobvt: (self cCharPointerOn: jobvt)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

gesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork 	"singular value/vector decomposition (for complex general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesvdWithjobu: (self cCharPointerOn: jobu)		jobvt: (self cCharPointerOn: jobvt)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

gesvWithn: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	"find x such that a*x=b, that is solve a linear system of equations	on return:		x is stored in b on return		a contains L and U of P*L*U decomposition (diagonal 1 of L not included)		ipiv contains permutations of rows P of P*L*U decomposition	return:		info contains error code is not 0"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesvWithn: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

getrfWithm: m n: n a: a lda: lda ipiv: ipiv 	"P*L*U decomposition of a general matrix	return 0 if OK"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgetrfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

getriWithn: n a: a lda: lda ipiv: ipiv 	"inverse of a general matrix"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		getriWithn: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 10 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		getriWithn: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: lwork!

getriWithn: n a: a lda: lda ipiv: ipiv work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgetriWithn: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

getrsWithtrans: trans n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	"Solve a general system of equations A*x=B in x"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgetrsWithtrans: (self cCharPointerOn: trans)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

ggbakWithjob: job side: side n: n ilo: ilo ihi: ihi lscale: lscale rscale: rscale m: m v: v ldv: ldv 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggbakWithjob: (self cCharPointerOn: job)		side: (self cCharPointerOn: side)		n: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		lscale: lscale		rscale: rscale		m: (self cIntegerPointerOn: m)		v: v		ldv: (self cIntegerPointerOn: ldv)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

ggbalWithjob: job n: n a: a lda: lda b: b ldb: ldb ilo: ilo ihi: ihi lscale: lscale rscale: rscale work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggbalWithjob: (self cCharPointerOn: job)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		ilo: ilo		ihi: ihi		lscale: lscale		rscale: rscale		work: work		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"generalized eigenvalue/vector decomposition (for complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: 8 * n.	(self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alpha: alpha		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1		rwork: rwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 1 max: 2 * n].	work := self allocateElementArraySize: lwork.	^self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alpha: alpha		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork		rwork: rwork!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"generalized eigenvalue/vector decomposition (for real general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alphar: alphar		alphai: alphai		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 1 max: 8 * n * n].	work := self allocateElementArraySize: lwork.	^self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alphar: alphar		alphai: alphai		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alphar: alphar		alphai: alphai		beta: beta		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

ggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork iwork: iwork bwork: bwork 
	| cARGbalanc cARGjobvl cARGjobvr cARGsense cARGn cARGlda cARGldb cARGldvl cARGldvr cARGlwork cARGinfo |
	^
	[cARGbalanc := self cCharPointerOn: balanc.
	cARGjobvl := self cCharPointerOn: jobvl.
	cARGjobvr := self cCharPointerOn: jobvr.
	cARGsense := self cCharPointerOn: sense.
	cARGn := self cIntegerPointerOn: n.
	cARGlda := self cIntegerPointerOn: lda.
	cARGldb := self cIntegerPointerOn: ldb.
	cARGldvl := self cIntegerPointerOn: ldvl.
	cARGldvr := self cIntegerPointerOn: ldvr.
	cARGlwork := self cIntegerPointerOn: lwork.
	cARGinfo := self cIntegerPointerOn: 0.
	self 
		xggevxWithbalanc: cARGbalanc
		jobvl: cARGjobvl
		jobvr: cARGjobvr
		sense: cARGsense
		n: cARGn
		a: a
		lda: cARGlda
		b: b
		ldb: cARGldb
		alpha: alpha
		beta: beta
		vl: vl
		ldvl: cARGldvl
		vr: vr
		ldvr: cARGldvr
		lscale: lscale
		rscale: rscale
		abnrm: abnrm
		bbnrm: bbnrm
		rconde: rconde
		rcondv: rcondv
		work: work
		lwork: cARGlwork
		rwork: rwork
		iwork: iwork
		bwork: bwork
		info: cARGinfo
		length: 1
		length: 1
		length: 1
		length: 1.
	cARGinfo sdwordAtOffset: 0] 
			ensure: 
				[self free: cARGbalanc.
				self free: cARGjobvl.
				self free: cARGjobvr.
				self free: cARGsense.
				self free: cARGn.
				self free: cARGlda.
				self free: cARGldb.
				self free: cARGldvl.
				self free: cARGldvr.
				self free: cARGlwork.
				self free: cARGinfo]!

ggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork iwork: iwork bwork: bwork 
	| cARGbalanc cARGjobvl cARGjobvr cARGsense cARGn cARGlda cARGldb cARGldvl cARGldvr cARGlwork cARGinfo |
	^
	[cARGbalanc := self cCharPointerOn: balanc.
	cARGjobvl := self cCharPointerOn: jobvl.
	cARGjobvr := self cCharPointerOn: jobvr.
	cARGsense := self cCharPointerOn: sense.
	cARGn := self cIntegerPointerOn: n.
	cARGlda := self cIntegerPointerOn: lda.
	cARGldb := self cIntegerPointerOn: ldb.
	cARGldvl := self cIntegerPointerOn: ldvl.
	cARGldvr := self cIntegerPointerOn: ldvr.
	cARGlwork := self cIntegerPointerOn: lwork.
	cARGinfo := self cIntegerPointerOn: 0.
	self 
		xggevxWithbalanc: cARGbalanc
		jobvl: cARGjobvl
		jobvr: cARGjobvr
		sense: cARGsense
		n: cARGn
		a: a
		lda: cARGlda
		b: b
		ldb: cARGldb
		alphar: alphar
		alphai: alphai
		beta: beta
		vl: vl
		ldvl: cARGldvl
		vr: vr
		ldvr: cARGldvr
		lscale: lscale
		rscale: rscale
		abnrm: abnrm
		bbnrm: bbnrm
		rconde: rconde
		rcondv: rcondv
		work: work
		lwork: cARGlwork
		iwork: iwork
		bwork: bwork
		info: cARGinfo
		length: 1
		length: 1
		length: 1
		length: 1.
	cARGinfo sdwordAtOffset: 0] 
			ensure: 
				[self free: cARGbalanc.
				self free: cARGjobvl.
				self free: cARGjobvr.
				self free: cARGsense.
				self free: cARGn.
				self free: cARGlda.
				self free: cARGldb.
				self free: cARGldvl.
				self free: cARGldvr.
				self free: cARGlwork.
				self free: cARGinfo]!

ggglmWithn: n m: m p: p a: a lda: lda b: b ldb: ldb d: d x: x y: y work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggglmWithn: (self cIntegerPointerOn: n)		m: (self cIntegerPointerOn: m)		p: (self cIntegerPointerOn: p)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		d: d		x: x		y: y		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

gghrdWithcompq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgghrdWithcompq: (self cCharPointerOn: compq)		compz: (self cCharPointerOn: compz)		n: (self cIntegerPointerOn: n)		ilo: (self cIntegerPointerOn: ilo)		ihi: (self cIntegerPointerOn: ihi)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

gglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x 	"least squares subject to equality constraints by orthogonalization	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"		| work lwork |	work := self allocateElementArraySize: 1.	(self			gglseWithm: m			n: n			p: p			a: a			lda: lda			b: b			ldb: ldb			c: c			d: d			x: x			work: work			lwork: -1)			= 0		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 1 max: m + n + p].	work := self allocateElementArraySize: lwork.	^self		gglseWithm: m		n: n		p: p		a: a		lda: lda		b: b		ldb: ldb		c: c		d: d		x: x		work: work		lwork: lwork!

gglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgglseWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		p: (self cIntegerPointerOn: p)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		c: c		d: d		x: x		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

ggqrfWithn: n m: m p: p a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggqrfWithn: (self cIntegerPointerOn: n)		m: (self cIntegerPointerOn: m)		p: (self cIntegerPointerOn: p)		a: a		lda: (self cIntegerPointerOn: lda)		taua: taua		b: b		ldb: (self cIntegerPointerOn: ldb)		taub: taub		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

ggrqfWithm: m p: p n: n a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggrqfWithm: (self cIntegerPointerOn: m)		p: (self cIntegerPointerOn: p)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		taua: taua		b: b		ldb: (self cIntegerPointerOn: ldb)		taub: taub		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

ggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq	"Generalized singular value/vector decomposition (for real/complex general matrices)	return 0 if OK"	| work rwork iwork |	^self isComplex 		ifTrue: 			[work := self allocateElementArraySize: ((3 * n max: m) max: p)+n.			rwork := self allocateRealArraySize: 2 * n.			iwork := self allocateIntegerArraySize: n.			self 				ggsvdWithjobu: jobu				jobv: jobv				jobq: jobq				m: m				n: n				p: p				k: k				l: l				a: a				lda: lda				b: b				ldb: ldb				alpha: alpha				beta: beta				u: u				ldu: ldu				v: v				ldv: ldv				q: q				ldq: ldq				work: work				rwork: rwork				iwork: iwork]		ifFalse: 			[work := self allocateElementArraySize: ((3*n max: m) max: p)+n.			iwork := self allocateIntegerArraySize: n.			self 				ggsvdWithjobu: jobu				jobv: jobv				jobq: jobq				m: m				n: n				p: p				k: k				l: l				a: a				lda: lda				b: b				ldb: ldb				alpha: alpha				beta: beta				u: u				ldu: ldu				v: v				ldv: ldv				q: q				ldq: ldq				work: work				iwork: iwork]!

ggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggsvdWithjobu: (self cCharPointerOn: jobu)		jobv: (self cCharPointerOn: jobv)		jobq: (self cCharPointerOn: jobq)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		p: (self cIntegerPointerOn: p)		k: k		l: l		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		u: u		ldu: (self cIntegerPointerOn: ldu)		v: v		ldv: (self cIntegerPointerOn: ldv)		q: q		ldq: (self cIntegerPointerOn: ldq)		work: work		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

ggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggsvdWithjobu: (self cCharPointerOn: jobu)		jobv: (self cCharPointerOn: jobv)		jobq: (self cCharPointerOn: jobq)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		p: (self cIntegerPointerOn: p)		k: k		l: l		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		u: u		ldu: (self cIntegerPointerOn: ldu)		v: v		ldv: (self cIntegerPointerOn: ldv)		q: q		ldq: (self cIntegerPointerOn: ldq)		work: work		rwork: rwork		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

heconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheconWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		anorm: (self cRealPointerOn: anorm)		rcond: rcond		work: work		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

heevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for real hermitian matrices)	use divide and conquer algorithm	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork iwork lwork lrwork liwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: 1.	iwork := self allocateIntegerArraySize: 1.	(self 		heevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1		rwork: rwork		lrwork: -1		iwork: iwork		liwork: -1) = 0 		ifTrue: 			[lwork := self retrieveLengthQueryAnswerFrom: work.			lrwork := self retrieveLengthQueryAnswerFrom: rwork.			liwork := iwork sdwordAtOffset: 0]		ifFalse: 			[lwork := 1 						max: (jobz = self doComputeVector ifTrue: [n * n + (2 * n)] ifFalse: [n + 1]).			lrwork := 1 max: (jobz = self doComputeVector 								ifTrue: [2 * n * n + (5 * n) + 1]								ifFalse: [n]).			liwork := 1 						max: (jobz = self doComputeVector ifTrue: [5 * n + 3] ifFalse: [1])].	work := self allocateElementArraySize: lwork.	rwork := self allocateRealArraySize: lrwork.	iwork := self allocateIntegerArraySize: liwork.	^self 		heevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork		rwork: rwork		lrwork: lrwork		iwork: iwork		liwork: liwork!

heevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheevdWithjobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		lrwork: (self cIntegerPointerOn: lrwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

heevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheevrWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cRealPointerOn: vl)		vu: (self cRealPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cRealPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		isuppz: isuppz		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

heevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for complex hermitian matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: (3 * n - 2 max: 1).	(self 		heevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1		rwork: rwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 2 * n - 1 max: 1].	work := self allocateElementArraySize: lwork.	^self 		heevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork		rwork: rwork!

heevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork 	"eigenvalue/vector decomposition (for complex hermitian matrices)"	| cARGjobz cARGuplo cARGn cARGlda cARGlwork cARGinfo |	cARGjobz := self cCharPointerOn: jobz.	cARGuplo := self cCharPointerOn: uplo.	cARGn := self cIntegerPointerOn: n.	cARGlda := self cIntegerPointerOn: lda.	cARGlwork := self cIntegerPointerOn: lwork.	cARGinfo := self cIntegerPointerOn: 0.	self 		xheevWithjobz: cARGjobz		uplo: cARGuplo		n: cARGn		a: a		lda: cARGlda		w: w		work: work		lwork: cARGlwork		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

heevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheevxWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cRealPointerOn: vl)		vu: (self cRealPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cRealPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

hegvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhegvdWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		lrwork: (self cIntegerPointerOn: lrwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

hegvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhegvWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

hegvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhegvxWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		vl: (self cRealPointerOn: vl)		vu: (self cRealPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cRealPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

hesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	"Solve x such that A*x = B with A hermitian	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		hesvWithuplo: uplo		n: n		nrhs: nrhs		a: a		lda: lda		ipiv: ipiv		b: b		ldb: ldb		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		hesvWithuplo: uplo		n: n		nrhs: nrhs		a: a		lda: lda		ipiv: ipiv		b: b		ldb: ldb		work: work		lwork: lwork!

hesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork 	"find x such that a*x=b, that is solve a linear system of equations	on return:		x is stored in b on return		a contains L and U of P*L*U decomposition (diagonal 1 of L not included)		ipiv contains permutations of rows P of P*L*U decomposition	return:		info contains error code is not 0"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhesvWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

hetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"P*L*U decomposition of a hermitian matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		hetrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		hetrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: lwork!

hetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork 	"P*L*U decomposition of a hermitian matrix	return 0 if OK"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhetrfWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

hetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"inverse of a hermitian matrix"	| work |	work := self allocateElementArraySize: n.	^self 		hetriWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work!

hetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhetriWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

hetrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhetrsWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

ilaenvWithispec: ispec name: name opts: opts n1: n1 n2: n2 n3: n3 n4: n4 	"retrieve needed workspace dimensions for some problems"	| cARGispec cARGname cARGopts cARGn1 cARGn2 cARGn3 cARGn4 |	cARGispec := self cIntegerPointerOn: ispec.	cARGn1 := self cIntegerPointerOn: n1.	cARGn2 := self cIntegerPointerOn: n2.	cARGn3 := self cIntegerPointerOn: n3.	cARGn4 := self cIntegerPointerOn: n4.	cARGname := name copyToHeap.	cARGopts := opts copyToHeap.	^self 		ilaenvWithispec: cARGispec		name: cARGname		opts: cARGopts		n1: cARGn1		n2: cARGn2		n3: cARGn3		n4: cARGn4		length: name size		length: opts size!

ilaenvWithispec: ispec name: name opts: opts n1: n1 n2: n2 n3: n3 n4: n4 length: lengthOfname length: lengthOfopts 
	"
*  Purpose
*  =======
*  ILAENV is called from the LAPACK routines to choose problem-dependent
*  parameters for the local environment.  See ISPEC for a description of
*  the parameters.
*  This version provides a set of parameters which should give good,
*  but not optimal, performance on many of the currently available
*  computers.  Users are encouraged to modify this subroutine to set
*  the tuning parameters for their particular machine using the option
*  and problem size information in the arguments.
*  This routine will not function correctly if it is converted to all
*  lower case.  Converting it to all upper case is allowed.
"

	<cdecl: SDWORD 'ilaenv_'  SDWORD * char * char * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

isComplex	self subclassResponsibility!

isDoublePrecision	self subclassResponsibility!

isReal	^self isComplex not!

isSinglePrecision	^self isDoublePrecision not!

ithEigenValuesInterval	^$I!

lacgvWithn: n x: x incx: incx 	"conjugate a vector"	^self 		xlacgvWithn: (self cIntegerPointerOn: n)		x: x		incx: (self cIntegerPointerOn: incx)!

lacpyWithuplo: uplo m: m n: n a: a lda: lda b: b ldb: ldb 	"copy a into b"	^self		xlacpyWithuplo: (self cCharPointerOn: uplo)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		length: 1!

lamch: code 	"extract single precision machine parameters	.	'E' or 'e',   DLAMCH := eps	.	'S' or 's ,   DLAMCH := sfmin	.	'B' or 'b',   DLAMCH := base	.	'P' or 'p',   DLAMCH := eps*base	.	'N' or 'n',   DLAMCH := t	.	'R' or 'r',   DLAMCH := rnd	.	'M' or 'm',   DLAMCH := emin	.	'U' or 'u',   DLAMCH := rmin	.	'L' or 'l',   DLAMCH := emax	.	'O' or 'o',   DLAMCH := rmax	where	.	eps   = relative machine precision	.	sfmin = safe minimum, such that 1/sfmin does not overflow	.	base  = base of the machine	.	prec  = eps*base	.	t     = number of (base) digits in the mantissa	.	rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise	.	emin  = minimum exponent before (gradual) underflow	.	rmin  = underflow threshold - base**(emin-1)	.	emax  = largest exponent before overflow	.	rmax  = overflow threshold  - (base**emax)*(1-eps)"	| cmach |	cmach := self cCharPointerOn: code.	^self isDoublePrecision 		ifTrue: [self dlamch: cmach length: 1]		ifFalse: [self slamch: cmach length: 1]!

langeWithnorm: norm m: m n: n a: a lda: lda 	"compute norm of a general matrix or vector"	| work |	work := self allocateRealArraySize: m.	^self 		langeWithnorm: norm		m: m		n: n		a: a		lda: lda		work: work!

langeWithnorm: norm m: m n: n a: a lda: lda work: work 	"compute norm of a general matrix or vector"	^self		xlangeWithnorm: (self cCharPointerOn: norm)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		work: work		length: 1!

lanheWithnorm: norm uplo: uplo n: n a: a lda: lda 	"compute norm of a hermitian matrix"	| work |	work := self allocateElementArraySize: n.	^self 		lanheWithnorm: norm		uplo: uplo		n: n		a: a		lda: lda		work: work!

lanheWithnorm: norm uplo: uplo n: n a: a lda: lda work: work 	^self		xlanheWithnorm: (self cCharPointerOn: norm)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		work: work		length: 1		length: 1!

lansyWithnorm: norm uplo: uplo n: n a: a lda: lda 	"compute norm of a symmetric matrix"	| work |	work := self allocateElementArraySize: n.	^self 		lansyWithnorm: norm		uplo: uplo		n: n		a: a		lda: lda		work: work!

lansyWithnorm: norm uplo: uplo n: n a: a lda: lda work: work 	^self		xlansyWithnorm: (self cCharPointerOn: norm)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		work: work		length: 1		length: 1!

lantrWithnorm: norm uplo: uplo diag: diag m: m n: n a: a lda: lda 	"compute norm of a triangular matrix"	| work |	work := self allocateElementArraySize: n.	^self 		lantrWithnorm: norm		uplo: uplo		diag: diag		m: m		n: n		a: a		lda: lda		work: work!

lantrWithnorm: norm uplo: uplo diag: diag m: m n: n a: a lda: lda work: work 	^self		xlantrWithnorm: (self cCharPointerOn: norm)		uplo: (self cCharPointerOn: uplo)		diag: (self cCharPointerOn: diag)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		work: work		length: 1		length: 1		length: 1!

larnvWithidist: idist iseed: iseed n: n x: x 	"fill x with random numbers"	^self		xlarnvWithidist: (self cIntegerPointerOn: idist)		iseed: iseed		n: (self cIntegerPointerOn: n)		x: x!

lasetWithuplo: uplo m: m n: n alpha: alpha beta: beta a: a lda: lda 	"set alpha off diagonal, beta on diagonal"	^self		xlasetWithuplo: (self cCharPointerOn: uplo)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		alpha: (self cElementPointerOn: alpha)		beta: (self cElementPointerOn: beta)		a: a		lda: (self cIntegerPointerOn: lda)		length: 1!

lower	^$L!

maxAbs	^$M!

nonUnit	^$N!

norm1	^$1!

normal01	"used by random number generator"	^3!

normF	^$F!

normI	^$I!

noSingularVector	^$N!

notTransposed	^$N!

orghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		orghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := (ihi sdwordAtOffset: 0) - (ilo sdwordAtOffset: 0) max: 1].	work := self allocateElementArraySize: lwork.	^self 		orghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

orghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix	ilo and ihi must be allocated outside"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorghrWithn: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

orglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorglqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

orgqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorgqlWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

orgqrWithm: m n: n k: k a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		orgqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		orgqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

orgqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorgqrWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

orgrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorgrqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

retrieveLengthQueryAnswerFrom: workPointer 
	"After a length query (setting LWORK=-1 as LAPACK 3.0 argument),
	answer is returned into the first element of the work array.
	This work array can be either real or complex...
	This method will retrieve information whatever"

	^(self isDoublePrecision
		ifTrue: [workPointer doubleAtOffset: 0]
		ifFalse: [workPointer floatAtOffset: 0]) asInteger!

schurDoNotSort	^$N!

schurDoSort	^$S!

schurSelectFunction
	"Answer a descriptor for the shur select function callback"
	self subclassResponsibility!

selectAbsLessThanUnity
	^self isComplex 
		ifTrue: 
			[ExternalCallback
				block: [:w | (w asComplex) abs <= 1 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]
		ifFalse: 
			[ExternalCallback
				block: [:wr :wi | (wr value i: wi value) abs <= 1 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]!

selectAbsStriclyLessThanUnity
	^self isComplex 
		ifTrue: 
			[ExternalCallback
				block: [:w | (w asComplex) abs < 1 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]
		ifFalse: 
			[ExternalCallback
				block: [:wr :wi | (wr value i: wi value) abs < 1 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]!

selectNegativeReal
	^self isComplex 
		ifTrue: 
			[ExternalCallback
				block: [:w | (w asComplex) realPart <= 0 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]
		ifFalse: 
			[ExternalCallback
				block: [:wr :wi | wr <= 0 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]!

selectNone
	^self isComplex 
		ifTrue: 
			[ExternalCallback
				block: [:w | 0]
				descriptor: self schurSelectFunction]
		ifFalse: 
			[ExternalCallback
				block: [:wr :wi | 0]
				descriptor: self schurSelectFunction]!

selectStrictlyNegativeReal
	^self isComplex 
		ifTrue: 
			[ExternalCallback
				block: [:w | (w asComplex) realPart < 0 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]
		ifFalse: 
			[ExternalCallback
				block: [:wr :wi | wr < 0 ifTrue: [1] ifFalse: [0]]
				descriptor: self schurSelectFunction]!

slamch: cmach length: lcmach
	"extract single precision machine parameters
	.	'E' or 'e',   DLAMCH := eps
	.	'S' or 's ,   DLAMCH := sfmin
	.	'B' or 'b',   DLAMCH := base
	.	'P' or 'p',   DLAMCH := eps*base
	.	'N' or 'n',   DLAMCH := t
	.	'R' or 'r',   DLAMCH := rnd
	.	'M' or 'm',   DLAMCH := emin
	.	'U' or 'u',   DLAMCH := rmin
	.	'L' or 'l',   DLAMCH := emax
	.	'O' or 'o',   DLAMCH := rmax
	where
	.	eps   = relative machine precision
	.	sfmin = safe minimum, such that 1/sfmin does not overflow
	.	base  = base of the machine
	.	prec  = eps*base
	.	t     = number of (base) digits in the mantissa
	.	rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
	.	emin  = minimum exponent before (gradual) underflow
	.	rmin  = underflow threshold - base**(emin-1)
	.	emax  = largest exponent before overflow
	.	rmax  = overflow threshold  - (base**emax)*(1-eps)"

	<cdecl: float 'slamch_'  char *  SDWORD >!

someSingularVector
	^$S!

syconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyconWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		anorm: (self cRealPointerOn: anorm)		rcond: rcond		work: work		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

syconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyconWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		anorm: (self cElementPointerOn: anorm)		rcond: rcond		work: work		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

syevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for real symmetric matrices)	use divide and conquer algorithm	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work iwork lwork liwork |	work := self allocateElementArraySize: 1.	iwork := self allocateIntegerArraySize: 1.	(self 		syevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1		iwork: iwork		liwork: -1) = 0 		ifTrue: 			[lwork := self retrieveLengthQueryAnswerFrom: work.			liwork := iwork sdwordAtOffset: 0]		ifFalse: 			[lwork := 1 max: (jobz = self doComputeVector 								ifTrue: [2 * n * n + (6 * n) + 1]								ifFalse: [2 * n + 1]).			liwork := 1 max: (jobz = self doComputeVector ifTrue: [5 * n + 3] ifFalse: [1])].	work := self allocateElementArraySize: lwork.	iwork := self allocateIntegerArraySize: liwork.	^self 		syevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork		iwork: iwork		liwork: liwork!

syevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevdWithjobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

syevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevrWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cElementPointerOn: vl)		vu: (self cElementPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cElementPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		isuppz: isuppz		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

syevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for real symmetric matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		syevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 3 * n - 1 max: 1].	work := self allocateElementArraySize: lwork.	^self 		syevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork!

syevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork 	"eigenvalue/vector decomposition (for real symmetric matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevWithjobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

syevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevxWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cElementPointerOn: vl)		vu: (self cElementPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cElementPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

sygvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsygvdWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

sygvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsygvWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

sygvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsygvxWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		vl: (self cElementPointerOn: vl)		vu: (self cElementPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cElementPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

sysvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsysvWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

sytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"P*L*U decomposition of a syrmitian matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		sytrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		sytrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: lwork!

sytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork 	"P*L*U decomposition of a symmetric matrix	return 0 if OK"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsytrfWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

sytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"inverse of a symmetric matrix"	| work |	work := self allocateElementArraySize: n.	^self 		sytriWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work!

sytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsytriWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

sytrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsytrsWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

tgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgexcWithwantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		ifst: ifst		ilst: ilst		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

tgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgexcWithwantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		ifst: ifst		ilst: ilst		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

tgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgsenWithijob: (self cIntegerPointerOn: ijob)		wantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		select: select		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		m: m		dif: dif		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

tgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgsenWithijob: (self cIntegerPointerOn: ijob)		wantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		select: select		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alphar: alphar		alphai: alphai		beta: beta		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		m: m		dif: dif		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

tgsylWithtrans: trans ijob: ijob m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc d: d ldd: ldd e: e lde: lde f: f ldf: ldf dif: dif scale: scale work: work lwork: lwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgsylWithtrans: (self cCharPointerOn: trans)		ijob: (self cIntegerPointerOn: ijob)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		c: c		ldc: (self cIntegerPointerOn: ldc)		d: d		ldd: (self cIntegerPointerOn: ldd)		e: e		lde: (self cIntegerPointerOn: lde)		f: f		ldf: (self cIntegerPointerOn: ldf)		dif: dif		scale: scale		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

transposeConjugated	^$C!

transposed	^$T!

trconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrconWithnorm: (self cCharPointerOn: norm)		uplo: (self cCharPointerOn: uplo)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		rcond: rcond		work: work		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

trconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrconWithnorm: (self cCharPointerOn: norm)		uplo: (self cCharPointerOn: uplo)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		rcond: rcond		work: work		rwork: rwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

trexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrexcWithcompq: (self cCharPointerOn: compq)		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		ifst: (self cIntegerPointerOn: ifst)		ilst: (self cIntegerPointerOn: ilst)		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

trexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrexcWithcompq: (self cCharPointerOn: compq)		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		ifst: ifst		ilst: ilst		work: work		info: cARGinfo		length: 1.	^cARGinfo sdwordAtOffset: 0!

trsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq w: w m: m s: s sep: sep work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrsenWithjob: (self cCharPointerOn: job)		compq: (self cCharPointerOn: compq)		select: select		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		w: w		m: m		s: s		sep: sep		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

trsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq wr: wr wi: wi m: m s: s sep: sep work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrsenWithjob: (self cCharPointerOn: job)		compq: (self cCharPointerOn: compq)		select: select		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		wr: wr		wi: wi		m: m		s: s		sep: sep		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

trsylWithtrana: trana tranb: tranb isgn: isgn m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc scale: scale 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrsylWithtrana: (self cCharPointerOn: trana)		tranb: (self cCharPointerOn: tranb)		isgn: (self cIntegerPointerOn: isgn)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		c: c		ldc: (self cIntegerPointerOn: ldc)		scale: scale		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

trtriWithuplo: uplo diag: diag n: n a: a lda: lda 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrtriWithuplo: (self cCharPointerOn: uplo)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		info: cARGinfo		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

trtrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb 	"Solve a triangular system of equations A*x=B in x"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrtrsWithuplo: (self cCharPointerOn: uplo)		trans: (self cCharPointerOn: trans)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo sdwordAtOffset: 0!

unghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		unghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := (ihi sdwordAtOffset: 0) - (ilo sdwordAtOffset: 0) max: 1].	work := self allocateElementArraySize: lwork.	^self 		unghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

unghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix	ilo and ihi must be allocated outside"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xunghrWithn: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

unglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xunglqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

ungqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xungqlWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

ungqrWithm: m n: n k: k a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		ungqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		ungqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

ungqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xungqrWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

ungrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xungrqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo sdwordAtOffset: 0!

uniform01	"used by random number generator"	^1!

uniform11	"used by random number generator"	^2!

uniformCircle	"used by random number generator"	^5!

uniformDisc	"used by random number generator"	^4!

upper	^$U!

valueEigenValuesInterval	"for finding eigenvalues lying in a certain interval"	^$V!

wantQ: aBoolean
	^aBoolean
		ifTrue: [$Q]
		ifFalse: [$N]!

wantU: aBoolean
	^aBoolean
		ifTrue: [$U]
		ifFalse: [$N]!

wantV: aBoolean
	^aBoolean
		ifTrue: [$V]
		ifFalse: [$N]!

xgebakWithjob: job side: side n: n ilo: ilo ihi: ihi scale: scale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 

	self subclassResponsibility!

xgebalWithjob: job n: n a: a lda: lda ilo: ilo ihi: ihi scale: scale info: info length: lengthOfjob 

	self subclassResponsibility!

xgeconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm 

	self subclassResponsibility!

xgeconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm 

	self subclassResponsibility!

xgeesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim w: w vs: vs ldvs: ldvs work: work lwork: lwork rwork: rwork bwork: bwork info: info length: lengthOfjobvs length: lengthOfsort 

	self subclassResponsibility!

xgeesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs work: work lwork: lwork bwork: bwork info: info length: lengthOfjobvs length: lengthOfsort 

	self subclassResponsibility!

xgeevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork info: info length: lengthOfjobvl length: lengthOfjobvr 

	self subclassResponsibility!

xgeevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork info: info length: lengthOfjobvl length: lengthOfjobvr 

	self subclassResponsibility!

xgeevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr scale: scale abnrm: abnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 

	self subclassResponsibility!

xgehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	self subclassResponsibility!

xgelqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xgelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork iwork: iwork info: info 

	self subclassResponsibility!

xgelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork iwork: iwork info: info 

	self subclassResponsibility!

xgelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork info: info 

	self subclassResponsibility!

xgelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork info: info 

	self subclassResponsibility!

xgelsWithtrans: trans m: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb work: work lwork: lwork info: info length: lengthOftrans 

	self subclassResponsibility!

xgelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work info: info 

	self subclassResponsibility!

xgelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work rwork: rwork info: info 

	self subclassResponsibility!

xgelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork info: info 

	self subclassResponsibility!

xgelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork rwork: rwork info: info 

	self subclassResponsibility!

xgeqlfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xgeqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xgeqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork rwork: rwork info: info 

	self subclassResponsibility!

xgeqrfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xgerqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xgesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork iwork: iwork info: info length: lengthOfjobz 

	self subclassResponsibility!

xgesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork iwork: iwork info: info length: lengthOfjobz 

	self subclassResponsibility!

xgesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork info: info length: lengthOfjobu length: lengthOfjobvt 

	self subclassResponsibility!

xgesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork info: info length: lengthOfjobu length: lengthOfjobvt 

	self subclassResponsibility!

xgesvWithn: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info 

	self subclassResponsibility!

xgetrfWithm: m n: n a: a lda: lda ipiv: ipiv info: info 

	self subclassResponsibility!

xgetriWithn: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info 

	self subclassResponsibility!

xgetrsWithtrans: trans n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOftrans 

	self subclassResponsibility!

xggbakWithjob: job side: side n: n ilo: ilo ihi: ihi lscale: lscale rscale: rscale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 

	self subclassResponsibility!

xggbalWithjob: job n: n a: a lda: lda b: b ldb: ldb ilo: ilo ihi: ihi lscale: lscale rscale: rscale work: work info: info length: lengthOfjob 

	self subclassResponsibility!

xggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork info: info length: lengthOfjobvl length: lengthOfjobvr 

	self subclassResponsibility!

xggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork info: info length: lengthOfjobvl length: lengthOfjobvr 

	self subclassResponsibility!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 

	self subclassResponsibility!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork iwork: iwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 

	self subclassResponsibility!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork iwork: iwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 

	self subclassResponsibility!

xggglmWithn: n m: m p: p a: a lda: lda b: b ldb: ldb d: d x: x y: y work: work lwork: lwork info: info 

	self subclassResponsibility!

xgghrdWithcompq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz info: info length: lengthOfcompq length: lengthOfcompz 

	self subclassResponsibility!

xgglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x work: work lwork: lwork info: info 

	self subclassResponsibility!

xggqrfWithn: n m: m p: p a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 

	self subclassResponsibility!

xggrqfWithm: m p: p n: n a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 

	self subclassResponsibility!

xggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work iwork: iwork info: info length: lengthOfjobu length: lengthOfjobv length: lengthOfjobq 

	self subclassResponsibility!

xggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work rwork: rwork iwork: iwork info: info length: lengthOfjobu length: lengthOfjobv length: lengthOfjobq 

	self subclassResponsibility!

xheconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work info: info length: lengthOfuplo 

	self subclassResponsibility!

xheevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 

	self subclassResponsibility!

xheevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork rwork: rwork iwork: iwork info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 

	self subclassResponsibility!

xheevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork info: info length: ljobz length: luplo 
	self subclassResponsibility!

xheevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 

	self subclassResponsibility!

xhegvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 

	self subclassResponsibility!

xhegvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork info: info length: lengthOfjobz length: lengthOfuplo 

	self subclassResponsibility!

xhegvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 

	self subclassResponsibility!

xhesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 

	self subclassResponsibility!

xhetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 

	self subclassResponsibility!

xhetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 

	self subclassResponsibility!

xhetrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 

	self subclassResponsibility!

xlacgvWithn: n x: x incx: incx 

	self subclassResponsibility!

xlacpyWithuplo: uplo m: m n: n a: a lda: lda b: b ldb: ldb length: lengthOfuplo 

	self subclassResponsibility!

xlamchWithcmach: cmach length: lengthOfcmach 

	self subclassResponsibility!

xlangeWithnorm: norm m: m n: n a: a lda: lda work: work length: lengthOfnorm 

	self subclassResponsibility!

xlanheWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 

	self subclassResponsibility!

xlanhpWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 

	self subclassResponsibility!

xlanspWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 

	self subclassResponsibility!

xlansyWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 

	self subclassResponsibility!

xlantpWithnorm: norm uplo: uplo diag: diag n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xlantrWithnorm: norm uplo: uplo diag: diag m: m n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xlarnvWithidist: idist iseed: iseed n: n x: x 

	self subclassResponsibility!

xlasetWithuplo: uplo m: m n: n alpha: alpha beta: beta a: a lda: lda length: lengthOfuplo 

	self subclassResponsibility!

xorghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	self subclassResponsibility!

xorglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xorgqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xorgqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xorgrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xpotrfWithuplo: uplo n: n a: a lda: lda info: info length: lengthOfuplo 

	self subclassResponsibility!

xsyconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work info: info length: lengthOfuplo 

	self subclassResponsibility!

xsyconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work iwork: iwork info: info length: lengthOfuplo 

	self subclassResponsibility!

xsyevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 

	self subclassResponsibility!

xsyevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 

	self subclassResponsibility!

xsyevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork info: info length: lengthOfjobz length: lengthOfuplo 

	self subclassResponsibility!

xsyevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 

	self subclassResponsibility!

xsygvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 

	self subclassResponsibility!

xsygvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork info: info length: lengthOfjobz length: lengthOfuplo 

	self subclassResponsibility!

xsygvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 

	self subclassResponsibility!

xsysvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 

	self subclassResponsibility!

xsytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 

	self subclassResponsibility!

xsytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 

	self subclassResponsibility!

xsytrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 

	self subclassResponsibility!

xtgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst info: info 

	self subclassResponsibility!

xtgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst work: work lwork: lwork info: info 

	self subclassResponsibility!

xtgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork info: info 

	self subclassResponsibility!

xtgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork info: info 

	self subclassResponsibility!

xtgsylWithtrans: trans ijob: ijob m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc d: d ldd: ldd e: e lde: lde f: f ldf: ldf dif: dif scale: scale work: work lwork: lwork iwork: iwork info: info length: lengthOftrans 

	self subclassResponsibility!

xtpconWithnorm: norm uplo: uplo diag: diag n: n ap: ap rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xtpconWithnorm: norm uplo: uplo diag: diag n: n ap: ap rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xtptriWithuplo: uplo diag: diag n: n ap: ap info: info length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xtptrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs ap: ap b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 

	self subclassResponsibility!

xtrconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xtrconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xtrexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst info: info length: lengthOfcompq 

	self subclassResponsibility!

xtrexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst work: work info: info length: lengthOfcompq 

	self subclassResponsibility!

xtrsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq w: w m: m s: s sep: sep work: work lwork: lwork info: info length: lengthOfjob length: lengthOfcompq 

	self subclassResponsibility!

xtrsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq wr: wr wi: wi m: m s: s sep: sep work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjob length: lengthOfcompq 

	self subclassResponsibility!

xtrsylWithtrana: trana tranb: tranb isgn: isgn m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc scale: scale info: info length: lengthOftrana length: lengthOftranb 

	self subclassResponsibility!

xtrtriWithuplo: uplo diag: diag n: n a: a lda: lda info: info length: lengthOfuplo length: lengthOfdiag 

	self subclassResponsibility!

xtrtrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 

	self subclassResponsibility!

xunghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	self subclassResponsibility!

xunglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xungqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xungqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility!

xungrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 

	self subclassResponsibility! !
!LapackLibrary categoriesFor: #allEigenValues!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #allocateComplexArraySize:!accessing!public! !
!LapackLibrary categoriesFor: #allocateElementArraySize:!accessing!public! !
!LapackLibrary categoriesFor: #allocateRealArraySize:!accessing!public! !
!LapackLibrary categoriesFor: #allSingularVector!processing-svd!public! !
!LapackLibrary categoriesFor: #balanceDoNothing!processing-balance!public! !
!LapackLibrary categoriesFor: #balancePermute!processing-balance!public! !
!LapackLibrary categoriesFor: #balancePermuteAndScale!processing-balance!public! !
!LapackLibrary categoriesFor: #balanceScale!processing-balance!public! !
!LapackLibrary categoriesFor: #cComplexPointerOn:!accessing!public! !
!LapackLibrary categoriesFor: #cElementPointerOn:!accessing!public! !
!LapackLibrary categoriesFor: #cRealPointerOn:!accessing!public! !
!LapackLibrary categoriesFor: #dlamch:length:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #doComputeVector!accessing!public! !
!LapackLibrary categoriesFor: #dontComputeVector!accessing!public! !
!LapackLibrary categoriesFor: #eps!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #gebakWithjob:side:n:ilo:ihi:scale:m:v:ldv:!processing-balance!public! !
!LapackLibrary categoriesFor: #gebalWithjob:n:a:lda:ilo:ihi:scale:!processing-balance!public! !
!LapackLibrary categoriesFor: #geconWithnorm:n:a:lda:anorm:rcond:work:iwork:!processing-condition number!public! !
!LapackLibrary categoriesFor: #geconWithnorm:n:a:lda:anorm:rcond:work:rwork:!processing-condition number!public! !
!LapackLibrary categoriesFor: #geesWithjobvs:sort:select:n:a:lda:sdim:w:vs:ldvs:!processing-schur!public! !
!LapackLibrary categoriesFor: #geesWithjobvs:sort:select:n:a:lda:sdim:w:vs:ldvs:work:lwork:rwork:bwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #geesWithjobvs:sort:select:n:a:lda:sdim:wr:wi:vs:ldvs:!processing-schur!public! !
!LapackLibrary categoriesFor: #geesWithjobvs:sort:select:n:a:lda:sdim:wr:wi:vs:ldvs:work:lwork:bwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #geevWithjobvl:jobvr:n:a:lda:w:vl:ldvl:vr:ldvr:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #geevWithjobvl:jobvr:n:a:lda:w:vl:ldvl:vr:ldvr:work:lwork:rwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #geevWithjobvl:jobvr:n:a:lda:wr:wi:vl:ldvl:vr:ldvr:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #geevWithjobvl:jobvr:n:a:lda:wr:wi:vl:ldvl:vr:ldvr:work:lwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #geevxWithbalanc:jobvl:jobvr:sense:n:a:lda:w:vl:ldvl:vr:ldvr:scale:abnrm:rconde:rcondv:work:lwork:rwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #gehrdWithn:a:lda:tau:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #gehrdWithn:ilo:ihi:a:lda:tau:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #gehrdWithn:ilo:ihi:a:lda:tau:work:lwork:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #gelqfWithm:n:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #gelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:iwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:iwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsWithtrans:m:n:nrhs:a:lda:b:ldb:work:lwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:rwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:rwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #geqlfWithm:n:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #geqp3Withm:n:a:lda:jpvt:tau:!processing-qr!public! !
!LapackLibrary categoriesFor: #geqp3Withm:n:a:lda:jpvt:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #geqp3Withm:n:a:lda:jpvt:tau:work:lwork:rwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #geqrfWithm:n:a:lda:tau:!processing-qr!public! !
!LapackLibrary categoriesFor: #geqrfWithm:n:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #gerqfWithm:n:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #gesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:iwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #gesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:iwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #gesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:!processing-svd!public! !
!LapackLibrary categoriesFor: #gesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:!processing-svd!public! !
!LapackLibrary categoriesFor: #gesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:!processing-svd!public! !
!LapackLibrary categoriesFor: #gesvWithn:nrhs:a:lda:ipiv:b:ldb:!processing-solve!public! !
!LapackLibrary categoriesFor: #getrfWithm:n:a:lda:ipiv:!processing-solve!public! !
!LapackLibrary categoriesFor: #getriWithn:a:lda:ipiv:!processing-solve!public! !
!LapackLibrary categoriesFor: #getriWithn:a:lda:ipiv:work:lwork:!processing-solve!public! !
!LapackLibrary categoriesFor: #getrsWithtrans:n:nrhs:a:lda:ipiv:b:ldb:!processing-solve!public! !
!LapackLibrary categoriesFor: #ggbakWithjob:side:n:ilo:ihi:lscale:rscale:m:v:ldv:!processing-balance!public! !
!LapackLibrary categoriesFor: #ggbalWithjob:n:a:lda:b:ldb:ilo:ihi:lscale:rscale:work:!processing-balance!public! !
!LapackLibrary categoriesFor: #ggevWithjobvl:jobvr:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #ggevWithjobvl:jobvr:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:work:lwork:rwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #ggevWithjobvl:jobvr:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #ggevWithjobvl:jobvr:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:work:lwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #ggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:rwork:iwork:bwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #ggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:iwork:bwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #ggglmWithn:m:p:a:lda:b:ldb:d:x:y:work:lwork:!public! !
!LapackLibrary categoriesFor: #gghrdWithcompq:compz:n:ilo:ihi:a:lda:b:ldb:q:ldq:z:ldz:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #gglseWithm:n:p:a:lda:b:ldb:c:d:x:!processing-least squares!public! !
!LapackLibrary categoriesFor: #gglseWithm:n:p:a:lda:b:ldb:c:d:x:work:lwork:!processing-least squares!public! !
!LapackLibrary categoriesFor: #ggqrfWithn:m:p:a:lda:taua:b:ldb:taub:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #ggrqfWithm:p:n:a:lda:taua:b:ldb:taub:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #ggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:!processing-svd!public! !
!LapackLibrary categoriesFor: #ggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:iwork:!processing-svd!public! !
!LapackLibrary categoriesFor: #ggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:rwork:iwork:!processing-svd!public! !
!LapackLibrary categoriesFor: #heconWithuplo:n:a:lda:ipiv:anorm:rcond:work:!processing-condition number!public! !
!LapackLibrary categoriesFor: #heevdWithjobz:uplo:n:a:lda:w:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #heevdWithjobz:uplo:n:a:lda:w:work:lwork:rwork:lrwork:iwork:liwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #heevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:rwork:iwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #heevWithjobz:uplo:n:a:lda:w:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #heevWithjobz:uplo:n:a:lda:w:work:lwork:rwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #heevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #hegvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:lrwork:iwork:liwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #hegvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #hegvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #hesvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:!public! !
!LapackLibrary categoriesFor: #hesvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:!public! !
!LapackLibrary categoriesFor: #hetrfWithuplo:n:a:lda:ipiv:!processing-solve!public! !
!LapackLibrary categoriesFor: #hetrfWithuplo:n:a:lda:ipiv:work:lwork:!processing-solve!public! !
!LapackLibrary categoriesFor: #hetriWithuplo:n:a:lda:ipiv:!processing-solve!public! !
!LapackLibrary categoriesFor: #hetriWithuplo:n:a:lda:ipiv:work:!processing-solve!public! !
!LapackLibrary categoriesFor: #hetrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:!processing-solve!public! !
!LapackLibrary categoriesFor: #ilaenvWithispec:name:opts:n1:n2:n3:n4:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #ilaenvWithispec:name:opts:n1:n2:n3:n4:length:length:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #isComplex!public!testing! !
!LapackLibrary categoriesFor: #isDoublePrecision!public!testing! !
!LapackLibrary categoriesFor: #isReal!public!testing! !
!LapackLibrary categoriesFor: #isSinglePrecision!public!testing! !
!LapackLibrary categoriesFor: #ithEigenValuesInterval!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #lacgvWithn:x:incx:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lacpyWithuplo:m:n:a:lda:b:ldb:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lamch:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #langeWithnorm:m:n:a:lda:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #langeWithnorm:m:n:a:lda:work:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lanheWithnorm:uplo:n:a:lda:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lanheWithnorm:uplo:n:a:lda:work:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lansyWithnorm:uplo:n:a:lda:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lansyWithnorm:uplo:n:a:lda:work:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lantrWithnorm:uplo:diag:m:n:a:lda:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lantrWithnorm:uplo:diag:m:n:a:lda:work:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #larnvWithidist:iseed:n:x:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lasetWithuplo:m:n:alpha:beta:a:lda:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #lower!public! !
!LapackLibrary categoriesFor: #maxAbs!public! !
!LapackLibrary categoriesFor: #nonUnit!accessing!public! !
!LapackLibrary categoriesFor: #norm1!public! !
!LapackLibrary categoriesFor: #normal01!public! !
!LapackLibrary categoriesFor: #normF!public! !
!LapackLibrary categoriesFor: #normI!public! !
!LapackLibrary categoriesFor: #noSingularVector!processing-svd!public! !
!LapackLibrary categoriesFor: #notTransposed!public! !
!LapackLibrary categoriesFor: #orghrWithn:ilo:ihi:a:lda:tau:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #orghrWithn:ilo:ihi:a:lda:tau:work:lwork:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #orglqWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #orgqlWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #orgqrWithm:n:k:a:lda:tau:!processing-qr!public! !
!LapackLibrary categoriesFor: #orgqrWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #orgrqWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #retrieveLengthQueryAnswerFrom:!private! !
!LapackLibrary categoriesFor: #schurDoNotSort!processing-schur!public! !
!LapackLibrary categoriesFor: #schurDoSort!processing-schur!public! !
!LapackLibrary categoriesFor: #schurSelectFunction!processing-schur!public! !
!LapackLibrary categoriesFor: #selectAbsLessThanUnity!processing-schur!public! !
!LapackLibrary categoriesFor: #selectAbsStriclyLessThanUnity!processing-schur!public! !
!LapackLibrary categoriesFor: #selectNegativeReal!processing-schur!public! !
!LapackLibrary categoriesFor: #selectNone!processing-schur!public! !
!LapackLibrary categoriesFor: #selectStrictlyNegativeReal!processing-schur!public! !
!LapackLibrary categoriesFor: #slamch:length:!processing-auxiliary!public! !
!LapackLibrary categoriesFor: #someSingularVector!processing-svd!public! !
!LapackLibrary categoriesFor: #syconWithuplo:n:a:lda:ipiv:anorm:rcond:work:!processing-condition number!public! !
!LapackLibrary categoriesFor: #syconWithuplo:n:a:lda:ipiv:anorm:rcond:work:iwork:!processing-condition number!public! !
!LapackLibrary categoriesFor: #syevdWithjobz:uplo:n:a:lda:w:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #syevdWithjobz:uplo:n:a:lda:w:work:lwork:iwork:liwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #syevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:iwork:liwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #syevWithjobz:uplo:n:a:lda:w:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #syevWithjobz:uplo:n:a:lda:w:work:lwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #syevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #sygvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:iwork:liwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #sygvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #sygvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #sysvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:!public! !
!LapackLibrary categoriesFor: #sytrfWithuplo:n:a:lda:ipiv:!processing-solve!public! !
!LapackLibrary categoriesFor: #sytrfWithuplo:n:a:lda:ipiv:work:lwork:!processing-solve!public! !
!LapackLibrary categoriesFor: #sytriWithuplo:n:a:lda:ipiv:!processing-solve!public! !
!LapackLibrary categoriesFor: #sytriWithuplo:n:a:lda:ipiv:work:!processing-solve!public! !
!LapackLibrary categoriesFor: #sytrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:!processing-solve!public! !
!LapackLibrary categoriesFor: #tgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:!processing-schur!public! !
!LapackLibrary categoriesFor: #tgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:work:lwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #tgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alpha:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #tgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alphar:alphai:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #tgsylWithtrans:ijob:m:n:a:lda:b:ldb:c:ldc:d:ldd:e:lde:f:ldf:dif:scale:work:lwork:iwork:!processing-sylvester!public! !
!LapackLibrary categoriesFor: #transposeConjugated!accessing!public! !
!LapackLibrary categoriesFor: #transposed!accessing!public! !
!LapackLibrary categoriesFor: #trconWithnorm:uplo:diag:n:a:lda:rcond:work:iwork:!processing-condition number!public! !
!LapackLibrary categoriesFor: #trconWithnorm:uplo:diag:n:a:lda:rcond:work:rwork:!processing-condition number!public! !
!LapackLibrary categoriesFor: #trexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:!processing-schur!public! !
!LapackLibrary categoriesFor: #trexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:work:!processing-schur!public! !
!LapackLibrary categoriesFor: #trsenWithjob:compq:select:n:t:ldt:q:ldq:w:m:s:sep:work:lwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #trsenWithjob:compq:select:n:t:ldt:q:ldq:wr:wi:m:s:sep:work:lwork:iwork:liwork:!processing-schur!public! !
!LapackLibrary categoriesFor: #trsylWithtrana:tranb:isgn:m:n:a:lda:b:ldb:c:ldc:scale:!processing-sylvester!public! !
!LapackLibrary categoriesFor: #trtriWithuplo:diag:n:a:lda:!public! !
!LapackLibrary categoriesFor: #trtrsWithuplo:trans:diag:n:nrhs:a:lda:b:ldb:!public! !
!LapackLibrary categoriesFor: #unghrWithn:ilo:ihi:a:lda:tau:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #unghrWithn:ilo:ihi:a:lda:tau:work:lwork:!processing-hessenberg!public! !
!LapackLibrary categoriesFor: #unglqWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #ungqlWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #ungqrWithm:n:k:a:lda:tau:!processing-qr!public! !
!LapackLibrary categoriesFor: #ungqrWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #ungrqWithm:n:k:a:lda:tau:work:lwork:!processing-qr!public! !
!LapackLibrary categoriesFor: #uniform01!public! !
!LapackLibrary categoriesFor: #uniform11!public! !
!LapackLibrary categoriesFor: #uniformCircle!public! !
!LapackLibrary categoriesFor: #uniformDisc!public! !
!LapackLibrary categoriesFor: #upper!accessing!public! !
!LapackLibrary categoriesFor: #valueEigenValuesInterval!processing-eigenvalue!public! !
!LapackLibrary categoriesFor: #wantQ:!processing-svd!public! !
!LapackLibrary categoriesFor: #wantU:!processing-svd!public! !
!LapackLibrary categoriesFor: #wantV:!processing-svd!public! !
!LapackLibrary categoriesFor: #xgebakWithjob:side:n:ilo:ihi:scale:m:v:ldv:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgebalWithjob:n:a:lda:ilo:ihi:scale:info:length:!private! !
!LapackLibrary categoriesFor: #xgeconWithnorm:n:a:lda:anorm:rcond:work:iwork:info:length:!private! !
!LapackLibrary categoriesFor: #xgeconWithnorm:n:a:lda:anorm:rcond:work:rwork:info:length:!private! !
!LapackLibrary categoriesFor: #xgeesWithjobvs:sort:select:n:a:lda:sdim:w:vs:ldvs:work:lwork:rwork:bwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgeesWithjobvs:sort:select:n:a:lda:sdim:wr:wi:vs:ldvs:work:lwork:bwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgeevWithjobvl:jobvr:n:a:lda:w:vl:ldvl:vr:ldvr:work:lwork:rwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgeevWithjobvl:jobvr:n:a:lda:wr:wi:vl:ldvl:vr:ldvr:work:lwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgeevxWithbalanc:jobvl:jobvr:sense:n:a:lda:w:vl:ldvl:vr:ldvr:scale:abnrm:rconde:rcondv:work:lwork:rwork:info:length:length:length:length:!private! !
!LapackLibrary categoriesFor: #xgehrdWithn:ilo:ihi:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgelqfWithm:n:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:iwork:info:!private! !
!LapackLibrary categoriesFor: #xgelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:iwork:info:!private! !
!LapackLibrary categoriesFor: #xgelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:info:!private! !
!LapackLibrary categoriesFor: #xgelsWithtrans:m:n:nrhs:a:lda:b:ldb:work:lwork:info:length:!private! !
!LapackLibrary categoriesFor: #xgelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:info:!private! !
!LapackLibrary categoriesFor: #xgelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:rwork:info:!private! !
!LapackLibrary categoriesFor: #xgelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:rwork:info:!private! !
!LapackLibrary categoriesFor: #xgeqlfWithm:n:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgeqp3Withm:n:a:lda:jpvt:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgeqp3Withm:n:a:lda:jpvt:tau:work:lwork:rwork:info:!private! !
!LapackLibrary categoriesFor: #xgeqrfWithm:n:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgerqfWithm:n:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:iwork:info:length:!private! !
!LapackLibrary categoriesFor: #xgesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:iwork:info:length:!private! !
!LapackLibrary categoriesFor: #xgesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgesvWithn:nrhs:a:lda:ipiv:b:ldb:info:!private! !
!LapackLibrary categoriesFor: #xgetrfWithm:n:a:lda:ipiv:info:!private! !
!LapackLibrary categoriesFor: #xgetriWithn:a:lda:ipiv:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgetrsWithtrans:n:nrhs:a:lda:ipiv:b:ldb:info:length:!private! !
!LapackLibrary categoriesFor: #xggbakWithjob:side:n:ilo:ihi:lscale:rscale:m:v:ldv:info:length:length:!private! !
!LapackLibrary categoriesFor: #xggbalWithjob:n:a:lda:b:ldb:ilo:ihi:lscale:rscale:work:info:length:!private! !
!LapackLibrary categoriesFor: #xggevWithjobvl:jobvr:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:work:lwork:rwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xggevWithjobvl:jobvr:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:work:lwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:rwork:bwork:info:length:length:length:length:!private! !
!LapackLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:rwork:iwork:bwork:info:length:length:length:length:!private! !
!LapackLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:iwork:bwork:info:length:length:length:length:!private! !
!LapackLibrary categoriesFor: #xggglmWithn:m:p:a:lda:b:ldb:d:x:y:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xgghrdWithcompq:compz:n:ilo:ihi:a:lda:b:ldb:q:ldq:z:ldz:info:length:length:!private! !
!LapackLibrary categoriesFor: #xgglseWithm:n:p:a:lda:b:ldb:c:d:x:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xggqrfWithn:m:p:a:lda:taua:b:ldb:taub:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xggrqfWithm:p:n:a:lda:taua:b:ldb:taub:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:iwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:rwork:iwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xheconWithuplo:n:a:lda:ipiv:anorm:rcond:work:info:length:!private! !
!LapackLibrary categoriesFor: #xheevdWithjobz:uplo:n:a:lda:w:work:lwork:rwork:lrwork:iwork:liwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xheevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:rwork:iwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xheevWithjobz:uplo:n:a:lda:w:work:lwork:rwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xheevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xhegvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:lrwork:iwork:liwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xhegvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xhegvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xhesvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!private! !
!LapackLibrary categoriesFor: #xhetrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!private! !
!LapackLibrary categoriesFor: #xhetriWithuplo:n:a:lda:ipiv:work:info:length:!private! !
!LapackLibrary categoriesFor: #xhetrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!private! !
!LapackLibrary categoriesFor: #xlacgvWithn:x:incx:!private! !
!LapackLibrary categoriesFor: #xlacpyWithuplo:m:n:a:lda:b:ldb:length:!private! !
!LapackLibrary categoriesFor: #xlamchWithcmach:length:!private! !
!LapackLibrary categoriesFor: #xlangeWithnorm:m:n:a:lda:work:length:!private! !
!LapackLibrary categoriesFor: #xlanheWithnorm:uplo:n:a:lda:work:length:length:!private! !
!LapackLibrary categoriesFor: #xlanhpWithnorm:uplo:n:ap:work:length:length:!private! !
!LapackLibrary categoriesFor: #xlanspWithnorm:uplo:n:ap:work:length:length:!private! !
!LapackLibrary categoriesFor: #xlansyWithnorm:uplo:n:a:lda:work:length:length:!private! !
!LapackLibrary categoriesFor: #xlantpWithnorm:uplo:diag:n:ap:work:length:length:length:!private! !
!LapackLibrary categoriesFor: #xlantrWithnorm:uplo:diag:m:n:a:lda:work:length:length:length:!private! !
!LapackLibrary categoriesFor: #xlarnvWithidist:iseed:n:x:!private! !
!LapackLibrary categoriesFor: #xlasetWithuplo:m:n:alpha:beta:a:lda:length:!private! !
!LapackLibrary categoriesFor: #xorghrWithn:ilo:ihi:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xorglqWithm:n:k:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xorgqlWithm:n:k:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xorgqrWithm:n:k:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xorgrqWithm:n:k:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xpotrfWithuplo:n:a:lda:info:length:!private! !
!LapackLibrary categoriesFor: #xsyconWithuplo:n:a:lda:ipiv:anorm:rcond:work:info:length:!private! !
!LapackLibrary categoriesFor: #xsyconWithuplo:n:a:lda:ipiv:anorm:rcond:work:iwork:info:length:!private! !
!LapackLibrary categoriesFor: #xsyevdWithjobz:uplo:n:a:lda:w:work:lwork:iwork:liwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xsyevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:iwork:liwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xsyevWithjobz:uplo:n:a:lda:w:work:lwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xsyevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xsygvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:iwork:liwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xsygvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xsygvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xsysvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!private! !
!LapackLibrary categoriesFor: #xsytrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!private! !
!LapackLibrary categoriesFor: #xsytriWithuplo:n:a:lda:ipiv:work:info:length:!private! !
!LapackLibrary categoriesFor: #xsytrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!private! !
!LapackLibrary categoriesFor: #xtgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:info:!private! !
!LapackLibrary categoriesFor: #xtgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xtgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alpha:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:info:!private! !
!LapackLibrary categoriesFor: #xtgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alphar:alphai:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:info:!private! !
!LapackLibrary categoriesFor: #xtgsylWithtrans:ijob:m:n:a:lda:b:ldb:c:ldc:d:ldd:e:lde:f:ldf:dif:scale:work:lwork:iwork:info:length:!private! !
!LapackLibrary categoriesFor: #xtpconWithnorm:uplo:diag:n:ap:rcond:work:iwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xtpconWithnorm:uplo:diag:n:ap:rcond:work:rwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xtptriWithuplo:diag:n:ap:info:length:length:!private! !
!LapackLibrary categoriesFor: #xtptrsWithuplo:trans:diag:n:nrhs:ap:b:ldb:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xtrconWithnorm:uplo:diag:n:a:lda:rcond:work:iwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xtrconWithnorm:uplo:diag:n:a:lda:rcond:work:rwork:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xtrexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:info:length:!private! !
!LapackLibrary categoriesFor: #xtrexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:work:info:length:!private! !
!LapackLibrary categoriesFor: #xtrsenWithjob:compq:select:n:t:ldt:q:ldq:w:m:s:sep:work:lwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xtrsenWithjob:compq:select:n:t:ldt:q:ldq:wr:wi:m:s:sep:work:lwork:iwork:liwork:info:length:length:!private! !
!LapackLibrary categoriesFor: #xtrsylWithtrana:tranb:isgn:m:n:a:lda:b:ldb:c:ldc:scale:info:length:length:!private! !
!LapackLibrary categoriesFor: #xtrtriWithuplo:diag:n:a:lda:info:length:length:!private! !
!LapackLibrary categoriesFor: #xtrtrsWithuplo:trans:diag:n:nrhs:a:lda:b:ldb:info:length:length:length:!private! !
!LapackLibrary categoriesFor: #xunghrWithn:ilo:ihi:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xunglqWithm:n:k:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xungqlWithm:n:k:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xungqrWithm:n:k:a:lda:tau:work:lwork:info:!private! !
!LapackLibrary categoriesFor: #xungrqWithm:n:k:a:lda:tau:work:lwork:info:!private! !

!LapackLibrary class methodsFor!

fileName
	"Answer the host system file name for the library"

	^SmallapackSettings lapackLibraryName! !
!LapackLibrary class categoriesFor: #fileName!public! !

ArrayCLibrary guid: (GUID fromString: '{6C2FEBF8-5C7C-4A85-9B75-9B969DCB8A9F}')!
ArrayCLibrary comment: ''!
!ArrayCLibrary categoriesForClass!Unclassified! !
!ArrayCLibrary methodsFor!

arraystodWithn: n source: source result: result 
	^ArraySLibrary new 
		arraystodWithn: 2 * n
		source: source
		result: result! !
!ArrayCLibrary categoriesFor: #arraystodWithn:source:result:!public! !

ArrayDLibrary guid: (GUID fromString: '{D7187478-D034-4C30-8E1A-A3B4B6BF1736}')!
ArrayDLibrary comment: ''!
!ArrayDLibrary categoriesForClass!Unclassified! !
!ArrayDLibrary methodsFor!

arraydtosWithn: n source: source result: result 
	<cdecl: void 'array_dtos'  SDWORD double * float * >
	^self invalidCall! !
!ArrayDLibrary categoriesFor: #arraydtosWithn:source:result:!public! !

ArraySLibrary guid: (GUID fromString: '{CD194712-AE9A-402D-A3FB-E482C7A411CC}')!
ArraySLibrary comment: ''!
!ArraySLibrary categoriesForClass!Unclassified! !
!ArraySLibrary methodsFor!

arraystodWithn: n source: source result: result 
	<cdecl: void 'array_stod'  SDWORD float * double * >
	^self invalidCall! !
!ArraySLibrary categoriesFor: #arraystodWithn:source:result:!public! !

ArrayZLibrary guid: (GUID fromString: '{17F1D9E8-C669-4445-AB4F-4E2F191F9FEC}')!
ArrayZLibrary comment: ''!
!ArrayZLibrary categoriesForClass!Unclassified! !
!ArrayZLibrary methodsFor!

arraydtosWithn: n source: source result: result 
	^ArrayDLibrary new 
		arraydtosWithn: 2 * n
		source: source
		result: result! !
!ArrayZLibrary categoriesFor: #arraydtosWithn:source:result:!public! !

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

BlasSLibrary guid: (GUID fromString: '{2DBB5FEC-1AAD-42BF-82D5-0E010C71D445}')!
BlasSLibrary comment: ''!
!BlasSLibrary categoriesForClass!Unclassified! !
!BlasSLibrary methodsFor!

asumWithn: n x: x incx: incx
	"||real(x)||_1 + ||imag(x)||_1"

	<cdecl: float 'sasum_'  SDWORD * float * SDWORD * >
	^self invalidCall!

axpyWithn: n alpha: alpha x: x incx: incx y: y incy: incy
	"y := alpha*x+y"

	<cdecl: void 'saxpy_'  SDWORD * float * float * SDWORD * float * SDWORD * >
	^self invalidCall!

cComplexPointerOn: aComplex 	^self cFloatComplexPointerOn: aComplex!

cElementPointerOn: aFloat 	^self cRealPointerOn: aFloat!

copyWithn: n x: x incx: incx y: y incy: incy
	"y := x"

	<cdecl: void 'scopy_'  SDWORD * float * SDWORD * float * SDWORD * >
	^self invalidCall!

cRealPointerOn: aDouble 	^self cFloatPointerOn: aDouble!

dotcWithn: n x: x incx: incx y: y incy: incy
	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotuWithn: n x: x incx: incx y: y incy: incy
	^self dotWithn: n x: x incx: incx y: y incy: incy!

dotWithn: n x: x incx: incx y: y incy: incy
	<cdecl: float 'sdot_'  SDWORD * float * SDWORD * float * SDWORD * >
	^self invalidCall!

gemmWithtransa: transa transb: transb m: m n: n k: k alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: ltransa length: ltransb
	"C:=alpha*op(A)*op(B)+beta*C  , C has dimension (m,n)"

	<cdecl: void 'sgemm_'  char * char * SDWORD* SDWORD* SDWORD* float * float * SDWORD* float * SDWORD* float * float * SDWORD* SDWORD SDWORD >
	^self invalidCall!

gemvWithtrans: trans m: m n: n alpha: alpha a: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: l
	"y=alpha*op(X)+beta*y op=yourself, transpose, transposeConjugated"

	<cdecl: void 'sgemv_'  char * SDWORD* SDWORD* float * float * SDWORD* float * SDWORD*  float * float * SDWORD* SDWORD >
	^self invalidCall!

gercWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transposeConjugated(y)+A"

	^self gerWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda!

geruWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	^self gerWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda!

gerWithm: m n: n alpha: alpha x: x incx: incx y: y incy: incy a: a lda: lda 
	"A=alpha*x*transpose(y)+A"

	<cdecl: void 'sger_' SDWORD* SDWORD* float * float * SDWORD* float * SDWORD* float * SDWORD*>
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

	<cdecl: SDWORD 'isamax_'  SDWORD * float * SDWORD * >
	^self invalidCall!

nrm2Withn: n x: x incx: incx
	"||x||_2"

	<cdecl: float 'snrm2_'  SDWORD * float * SDWORD * >
	^self invalidCall!

realScalWithn: n alpha: alpha x: x incx: incx
	^self scalWithn: n alpha: alpha x: x incx: incx!

rotgWitha: a b: b c: c s: s
	<cdecl: void 'srotg_'  float * float * float * float * >
	^self invalidCall!

rotWithn: n x: x incx: incx y: y incy: incy c: c s: s
	<cdecl: void 'srot_'  SDWORD * float * SDWORD * float * SDWORD * float * float * >
	^self invalidCall!

scalWithn: n alpha: alpha x: x incx: incx
	<cdecl: void 'sscal_'  SDWORD * float * float * SDWORD * >
	^self invalidCall!

spmvWithuplo: uplo n: n alpha: alpha ap: ap x: x incx: incx beta: beta y: y incy: incy length: luplo 
	"y=alpha*X+beta*y"

	<cdecl: void 'sspmv_'  char * SDWORD * double * double * double * SDWORD * double * double * SDWORD * SDWORD >
	^self invalidCall!

swapWithn: n x: x incx: incx y: y incy: incy
	"x <-> y"

	<cdecl: void 'sswap_'  SDWORD * float * SDWORD * float * SDWORD * >
	^self invalidCall!

symmWithside: side uplo: uplo m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb beta: beta c: c ldc: ldc length: lside length: luplo 
	"C=alpha*A*B+beta*C"

	<cdecl: void 'ssymm_'  char * char * SDWORD * SDWORD * float * float * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

symvWithuplo: uplo n: n alpha: alpha n: a lda: lda x: x incx: incx beta: beta y: y incy: incy length: luplo
	"y=alpha*X+beta*y"

	<cdecl: void 'ssymv_'  char * SDWORD * double * double * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD >
	^self invalidCall!

tpmvWithuplo: uplo trans: trans diag: diag n: n ap: ap x: x incx: incx length: uploLength length: transLength length: diagLength 
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'stpmv_'  char * char * char * SDWORD * float * float * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trmmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'strmm_'  char * char * char * char * SDWORD * SDWORD * float * float * SDWORD * float * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

trmvWithuplo: uplo trans: trans diag: diag n: n a: a lda: lda x: x incx: incx length: uploLength length: transLength length: diagLength
	"x=op(a)*x op=yourself, transpose, transposeConjugated"

	<cdecl: void 'strmv_'  char * char * char * SDWORD * float * SDWORD * float * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

trsmWithside: side uplo: uplo trans: trans diag: diag m: m n: n alpha: alpha a: a lda: lda b: b ldb: ldb length: lside length: luplo length: ltrans length: ldiag 
	<cdecl: void 'strsm_'  char * char * char * char * SDWORD * SDWORD * float * float * SDWORD * float * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall! !
!BlasSLibrary categoriesFor: #asumWithn:x:incx:!public! !
!BlasSLibrary categoriesFor: #axpyWithn:alpha:x:incx:y:incy:!public! !
!BlasSLibrary categoriesFor: #cComplexPointerOn:!public! !
!BlasSLibrary categoriesFor: #cElementPointerOn:!public! !
!BlasSLibrary categoriesFor: #copyWithn:x:incx:y:incy:!public! !
!BlasSLibrary categoriesFor: #cRealPointerOn:!public! !
!BlasSLibrary categoriesFor: #dotcWithn:x:incx:y:incy:!public! !
!BlasSLibrary categoriesFor: #dotuWithn:x:incx:y:incy:!public! !
!BlasSLibrary categoriesFor: #dotWithn:x:incx:y:incy:!public! !
!BlasSLibrary categoriesFor: #gemmWithtransa:transb:m:n:k:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasSLibrary categoriesFor: #gemvWithtrans:m:n:alpha:a:lda:x:incx:beta:y:incy:length:!public! !
!BlasSLibrary categoriesFor: #gercWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasSLibrary categoriesFor: #geruWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasSLibrary categoriesFor: #gerWithm:n:alpha:x:incx:y:incy:a:lda:!public! !
!BlasSLibrary categoriesFor: #hemmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasSLibrary categoriesFor: #hemvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasSLibrary categoriesFor: #hpmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasSLibrary categoriesFor: #iamaxWithn:x:incx:!public! !
!BlasSLibrary categoriesFor: #nrm2Withn:x:incx:!public! !
!BlasSLibrary categoriesFor: #realScalWithn:alpha:x:incx:!public! !
!BlasSLibrary categoriesFor: #rotgWitha:b:c:s:!public! !
!BlasSLibrary categoriesFor: #rotWithn:x:incx:y:incy:c:s:!public! !
!BlasSLibrary categoriesFor: #scalWithn:alpha:x:incx:!public! !
!BlasSLibrary categoriesFor: #spmvWithuplo:n:alpha:ap:x:incx:beta:y:incy:length:!public! !
!BlasSLibrary categoriesFor: #swapWithn:x:incx:y:incy:!public! !
!BlasSLibrary categoriesFor: #symmWithside:uplo:m:n:alpha:a:lda:b:ldb:beta:c:ldc:length:length:!public! !
!BlasSLibrary categoriesFor: #symvWithuplo:n:alpha:n:lda:x:incx:beta:y:incy:length:!public! !
!BlasSLibrary categoriesFor: #tpmvWithuplo:trans:diag:n:ap:x:incx:length:length:length:!public! !
!BlasSLibrary categoriesFor: #trmmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !
!BlasSLibrary categoriesFor: #trmvWithuplo:trans:diag:n:a:lda:x:incx:length:length:length:!public! !
!BlasSLibrary categoriesFor: #trsmWithside:uplo:trans:diag:m:n:alpha:a:lda:b:ldb:length:length:length:length:!public! !

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

LapackCLibrary guid: (GUID fromString: '{70E0D32E-9B4F-4BC0-93CE-CA5616F198CB}')!
LapackCLibrary comment: ''!
!LapackCLibrary categoriesForClass!Unclassified! !
!LapackCLibrary methodsFor!

cComplexPointerOn: aComplex 	^self cFloatComplexPointerOn: aComplex!

cElementPointerOn: aComplex 	^self cComplexPointerOn: aComplex!

cRealPointerOn: aDouble 	^self cFloatPointerOn: aDouble!

isComplex	^true!

isDoublePrecision	^false!

schurSelectFunction
	"Answer the descriptor for callback function"

	^ExternalDescriptor fromString: 'cdecl: SDWORD "FLOATCOMPLEX" void *' !

xgebakWithjob: job side: side n: n ilo: ilo ihi: ihi scale: scale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  CGEBAK forms the right or left eigenvectors of a complex general
*  matrix by backward transformation on the computed eigenvectors of the
*  balanced matrix output by CGEBAL.
"

	<cdecl: SDWORD 'cgebak_'  char * char * SDWORD * SDWORD * SDWORD * float * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgebalWithjob: job n: n a: a lda: lda ilo: ilo ihi: ihi scale: scale info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  CGEBAL balances a general complex matrix A.  This involves, first,
*  permuting A by a similarity transformation to isolate eigenvalues
*  in the first 1 to ILO-1 and last IHI+1 to N elements on the
*  diagonal; and second, applying a diagonal similarity transformation
*  to rows and columns ILO to IHI to make the rows and columns as
*  close in norm as possible.  Both steps are optional.
*  Balancing may reduce the 1-norm of the matrix, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors.
"

	<cdecl: SDWORD 'cgebal_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD * float * SDWORD * SDWORD >
	^self invalidCall!

xgeconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm 
	"
*  Purpose
*  =======
*  CGECON estimates the reciprocal of the condition number of a general
*  complex matrix A, in either the 1-norm or the infinity-norm, using
*  the LU factorization computed by CGETRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'cgecon_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * "ExternalFloatComplex"void * float * SDWORD * SDWORD >
	^self invalidCall!

xgeesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim w: w vs: vs ldvs: ldvs work: work lwork: lwork rwork: rwork bwork: bwork info: info length: lengthOfjobvs length: lengthOfsort 
	"
*  Purpose
*  =======
*  CGEES computes for an N-by-N complex nonsymmetric matrix A, the
*  eigenvalues, the Schur form T, and, optionally, the matrix of Schur
*  vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).
*  Optionally, it also orders the eigenvalues on the diagonal of the
*  Schur form so that selected eigenvalues are at the top left.
*  The leading columns of Z then form an orthonormal basis for the
*  invariant subspace corresponding to the selected eigenvalues.
*  A complex matrix is in Schur form if it is upper triangular.
"

	<cdecl: SDWORD 'cgees_'  char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgeevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  CGEEV computes for an N-by-N complex nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
"

	<cdecl: SDWORD 'cgeev_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgeevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr scale: scale abnrm: abnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  CGEEVX computes for an N-by-N complex nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*  Optionally also, it computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
*  (RCONDE), and reciprocal condition numbers for the right
*  eigenvectors (RCONDV).
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
*  Balancing a matrix means permuting the rows and columns to make it
*  more nearly upper triangular, and applying a diagonal similarity
*  transformation D * A * D**(-1), where D is a diagonal matrix, to
*  make its rows and columns closer in norm and the condition numbers
*  of its eigenvalues and eigenvectors smaller.  The computed
*  reciprocal condition numbers correspond to the balanced matrix.
*  Permuting rows and columns will not change the condition numbers
*  (in exact arithmetic) but diagonal scaling will.  For further
*  explanation of balancing, see section 4.10.2 of the LAPACK
*  Users' Guide.
"

	<cdecl: SDWORD 'cgeevx_'  char * char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * float * float * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xgehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'cgehrd_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgelqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGELQF computes an LQ factorization of a complex M-by-N matrix A:
*  A = L * Q.
"

	<cdecl: SDWORD 'cgelqf_'  SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork iwork: iwork info: info 
	"
*  Purpose
*  =======
*  CGELSD computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize 2-norm(| b - A*x |)
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The problem is solved in three steps:
*  (1) Reduce the coefficient matrix A to bidiagonal form with
*      Householder tranformations, reducing the original problem
*      into a 'bidiagonal least squares problem' (BLS)
*  (2) Solve the BLS using a divide and conquer approach.
*  (3) Apply back all the Householder tranformations to solve
*      the original least squares problem.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'cgelsd_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * >
	^self invalidCall!

xgelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork info: info 
	"
*  Purpose
*  =======
*  CGELSS computes the minimum norm solution to a complex linear
*  least squares problem:
*  Minimize 2-norm(| b - A*x |).
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
*  X.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
"

	<cdecl: SDWORD 'cgelss_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * >
	^self invalidCall!

xgelsWithtrans: trans m: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb work: work lwork: lwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  CGELS solves overdetermined or underdetermined complex linear systems
*  involving an M-by-N matrix A, or its conjugate-transpose, using a QR
*  or LQ factorization of A.  It is assumed that A has full rank.
*  The following options are provided:
*  1. If TRANS = 'N' and m >= n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A*X ||.
*  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
*     an underdetermined system A * X = B.
*  3. If TRANS = 'C' and m >= n:  find the minimum norm solution of
*     an undetermined system A**H * X = B.
*  4. If TRANS = 'C' and m < n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A**H * X ||.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
"

	<cdecl: SDWORD 'cgels_'  char * SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work rwork: rwork info: info 
	"
*  Purpose
*  =======
*  This routine is deprecated and has been replaced by routine CGELSY.
*  CGELSX computes the minimum-norm solution to a complex linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by unitary transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
"

	<cdecl: SDWORD 'cgelsx_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * float * SDWORD * "ExternalFloatComplex"void * float * SDWORD * >
	^self invalidCall!

xgelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork rwork: rwork info: info 
	"
*  Purpose
*  =======
*  CGELSY computes the minimum-norm solution to a complex linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by unitary transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
*  This routine is basically identical to the original xGELSX except
*  three differences:
*    o The permutation of matrix B (the right hand side) is faster and
*      more simple.
*    o The call to the subroutine xGEQPF has been substituted by the
*      the call to the subroutine xGEQP3. This subroutine is a Blas-3
*      version of the QR factorization with column pivoting.
*    o Matrix B (the right hand side) is updated with Blas-3.
"

	<cdecl: SDWORD 'cgelsy_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * float * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * >
	^self invalidCall!

xgeqlfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGEQLF computes a QL factorization of a complex M-by-N matrix A:
*  A = Q * L.
"

	<cdecl: SDWORD 'cgeqlf_'  SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgeqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork rwork: rwork info: info 
	"
*  Purpose
*  =======
*  CGEQP3 computes a QR factorization with column pivoting of a
*  matrix A:  A*P = Q*R  using Level 3 BLAS.
"

	<cdecl: SDWORD 'cgeqp3_'  SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * float * SDWORD * >
	^self invalidCall!

xgeqrfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGEQRF computes a QR factorization of a complex M-by-N matrix A:
*  A = Q * R.
"

	<cdecl: SDWORD 'cgeqrf_'  SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgerqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGERQF computes an RQ factorization of a complex M-by-N matrix A:
*  A = R * Q.
"

	<cdecl: SDWORD 'cgerqf_'  SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork iwork: iwork info: info length: lengthOfjobz 
	"
*  Purpose
*  =======
*  CGESDD computes the singular value decomposition (SVD) of a complex
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors, by using divide-and-conquer method. The SVD is written
*       A = U * SIGMA * conjugate-transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
*  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns VT = V**H, not V.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'cgesdd_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * float * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork info: info length: lengthOfjobu length: lengthOfjobvt 
	"
*  Purpose
*  =======
*  CGESVD computes the singular value decomposition (SVD) of a complex
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*       A = U * SIGMA * conjugate-transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
*  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns V**H, not V.
"

	<cdecl: SDWORD 'cgesvd_'  char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * float * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgesvWithn: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info 
	"
*  Purpose
*  =======
*  CGESV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*  The LU decomposition with partial pivoting and row interchanges is
*  used to factor A as
*     A = P * L * U,
*  where P is a permutation matrix, L is unit lower triangular, and U is
*  upper triangular.  The factored form of A is then used to solve the
*  system of equations A * X = B.
"

	<cdecl: SDWORD 'cgesv_'  SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgetrfWithm: m n: n a: a lda: lda ipiv: ipiv info: info 
	"
*  Purpose
*  =======
*  CGETRF computes an LU factorization of a general M-by-N matrix A
*  using partial pivoting with row interchanges.
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*  This is the right-looking Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'cgetrf_'  SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xgetriWithn: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGETRI computes the inverse of a matrix using the LU factorization
*  computed by CGETRF.
*  This method inverts U and then computes inv(A) by solving the system
*  inv(A)*L = inv(U) for inv(A).
"

	<cdecl: SDWORD 'cgetri_'  SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgetrsWithtrans: trans n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  CGETRS solves a system of linear equations
*     A * X = B,  A**T * X = B,  or  A**H * X = B
*  with a general N-by-N matrix A using the LU factorization computed
*  by CGETRF.
"

	<cdecl: SDWORD 'cgetrs_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xggbakWithjob: job side: side n: n ilo: ilo ihi: ihi lscale: lscale rscale: rscale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  CGGBAK forms the right or left eigenvectors of a complex generalized
*  eigenvalue problem A*x = lambda*B*x, by backward transformation on
*  the computed eigenvectors of the balanced pair of matrices output by
*  CGGBAL.
"

	<cdecl: SDWORD 'cggbak_'  char * char * SDWORD * SDWORD * SDWORD * float * float * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggbalWithjob: job n: n a: a lda: lda b: b ldb: ldb ilo: ilo ihi: ihi lscale: lscale rscale: rscale work: work info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  CGGBAL balances a pair of general complex matrices (A,B).  This
*  involves, first, permuting A and B by similarity transformations to
*  isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
*  elements on the diagonal; and second, applying a diagonal similarity
*  transformation to rows and columns ILO to IHI to make the rows
*  and columns as close in norm as possible. Both steps are optional.
*  Balancing may reduce the 1-norm of the matrices, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors in the
*  generalized eigenvalue problem A*x = lambda*B*x.
"

	<cdecl: SDWORD 'cggbal_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD * float * float * float * SDWORD * SDWORD >
	^self invalidCall!

xggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  CGGEV computes for a pair of N-by-N complex nonsymmetric matrices
*  (A,B), the generalized eigenvalues, and optionally, the left and/or
*  right generalized eigenvectors.
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right generalized eigenvector v(j) corresponding to the
*  generalized eigenvalue lambda(j) of (A,B) satisfies
*               A * v(j) = lambda(j) * B * v(j).
*  The left generalized eigenvector u(j) corresponding to the
*  generalized eigenvalues lambda(j) of (A,B) satisfies
*               u(j)**H * A = lambda(j) * u(j)**H * B
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'cggev_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  CGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
*  (A,B) the generalized eigenvalues, and optionally, the left and/or
*  right generalized eigenvectors.
*  Optionally, it also computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
*  the eigenvalues (RCONDE), and reciprocal condition numbers for the
*  right eigenvectors (RCONDV).
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j) .
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B.
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'cggevx_'  char * char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * float * float * float * float * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork iwork: iwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  CGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
*  (A,B) the generalized eigenvalues, and optionally, the left and/or
*  right generalized eigenvectors.
*  Optionally, it also computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
*  the eigenvalues (RCONDE), and reciprocal condition numbers for the
*  right eigenvectors (RCONDV).
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j) .
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B.
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'cggevx_'  char * char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * float * float * float * float * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xggglmWithn: n m: m p: p a: a lda: lda b: b ldb: ldb d: d x: x y: y work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGGGLM solves a general Gauss-Markov linear model (GLM) problem:
*          minimize || y ||_2   subject to   d = A*x + B*y
*              x
*  where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
*  given N-vector. It is assumed that M <= N <= M+P, and
*             rank(A) = M    and    rank( A B ) = N.
*  Under these assumptions, the constrained equation is always
*  consistent, and there is a unique solution x and a minimal 2-norm
*  solution y, which is obtained using a generalized QR factorization
*  of A and B.
*  In particular, if matrix B is square nonsingular, then the problem
*  GLM is equivalent to the following weighted linear least squares
*  problem
*               minimize || inv(B)*(d-A*x) ||_2
*                   x
*  where inv(B) denotes the inverse of B.
"

	<cdecl: SDWORD 'cggglm_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgghrdWithcompq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz info: info length: lengthOfcompq length: lengthOfcompz 
	"
*  Purpose
*  =======
*  CGGHRD reduces a pair of complex matrices (A,B) to generalized upper
*  Hessenberg form using unitary transformations, where A is a
*  general matrix and B is upper triangular.  The form of the generalized
*  eigenvalue problem is
*     A*x = lambda*B*x,
*  and B is typically made upper triangular by computing its QR
*  factorization and moving the unitary matrix Q to the left side
*  of the equation.
*  This subroutine simultaneously reduces A to a Hessenberg matrix H:
*     Q**H*A*Z = H
*  and transforms B to another upper triangular matrix T:
*     Q**H*B*Z = T
*  in order to reduce the problem to its standard form
*     H*y = lambda*T*y
*  where y = Z**H*x.
*  The unitary matrices Q and Z are determined as products of Givens
*  rotations.  They may either be formed explicitly, or they may be
*  postmultiplied into input matrices Q1 and Z1, so that
*       Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
*       Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
*  If Q1 is the unitary matrix from the QR factorization of B in the
*  original equation A*x = lambda*B*x, then CGGHRD reduces the original
*  problem to generalized Hessenberg form.
"

	<cdecl: SDWORD 'cgghrd_'  char * char * SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGGLSE solves the linear equality-constrained least squares (LSE)
*  problem:
*          minimize || c - A*x ||_2   subject to   B*x = d
*  where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
*  M-vector, and d is a given P-vector. It is assumed that
*  P <= N <= M+P, and
*           rank(B) = P and  rank( ( A ) ) = N.
*                                ( ( B ) )
*  These conditions ensure that the LSE problem has a unique solution,
*  which is obtained using a GRQ factorization of the matrices B and A.
"

	<cdecl: SDWORD 'cgglse_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xggqrfWithn: n m: m p: p a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGGQRF computes a generalized QR factorization of an N-by-M matrix A
*  and an N-by-P matrix B:
*              A = Q*R,        B = Q*T*Z,
*  where Q is an N-by-N unitary matrix, Z is a P-by-P unitary matrix,
*  and R and T assume one of the forms:
*  if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
*                  (  0  ) N-M                         N   M-N
*                     M
*  where R11 is upper triangular, and
*  if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
*                   P-N  N                           ( T21 ) P
*                                                       P
*  where T12 or T21 is upper triangular.
*  In particular, if B is square and nonsingular, the GQR factorization
*  of A and B implicitly gives the QR factorization of inv(B)*A:
*               inv(B)*A = Z'*(inv(T)*R)
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  conjugate transpose of matrix Z.
"

	<cdecl: SDWORD 'cggqrf_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xggrqfWithm: m p: p n: n a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CGGRQF computes a generalized RQ factorization of an M-by-N matrix A
*  and a P-by-N matrix B:
*              A = R*Q,        B = Z*T*Q,
*  where Q is an N-by-N unitary matrix, Z is a P-by-P unitary
*  matrix, and R and T assume one of the forms:
*  if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
*                   N-M  M                           ( R21 ) N
*                                                       N
*  where R12 or R21 is upper triangular, and
*  if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
*                  (  0  ) P-N                         P   N-P
*                     N
*  where T11 is upper triangular.
*  In particular, if B is square and nonsingular, the GRQ factorization
*  of A and B implicitly gives the RQ factorization of A*inv(B):
*               A*inv(B) = (R*inv(T))*Z'
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  conjugate transpose of the matrix Z.
"

	<cdecl: SDWORD 'cggrqf_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work rwork: rwork iwork: iwork info: info length: lengthOfjobu length: lengthOfjobv length: lengthOfjobq 
	"
*  Purpose
*  =======
*  CGGSVD computes the generalized singular value decomposition (GSVD)
*  of an M-by-N complex matrix A and P-by-N complex matrix B:
*        U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
*  where U, V and Q are unitary matrices, and Z' means the conjugate
*  transpose of Z.  Let K+L = the effective numerical rank of the
*  matrix (A',B')', then R is a (K+L)-by-(K+L) nonsingular upper
*  triangular matrix, D1 and D2 are M-by-(K+L) and P-by-(K+L) 'diagonal'
*  matrices and of the following structures, respectively:
*  If M-K-L >= 0,
*                      K  L
*         D1 =     K ( I  0 )
*                  L ( 0  C )
*              M-K-L ( 0  0 )
*                    K  L
*         D2 =   L ( 0  S )
*              P-L ( 0  0 )
*                  N-K-L  K    L
*    ( 0 R ) = K (  0   R11  R12 )
*              L (  0    0   R22 )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
*    S = diag( BETA(K+1),  ... , BETA(K+L) ),
*    C**2 + S**2 = I.
*    R is stored in A(1:K+L,N-K-L+1:N) on exit.
*  If M-K-L < 0,
*                    K M-K K+L-M
*         D1 =   K ( I  0    0   )
*              M-K ( 0  C    0   )
*                      K M-K K+L-M
*         D2 =   M-K ( 0  S    0  )
*              K+L-M ( 0  0    I  )
*                P-L ( 0  0    0  )
*                     N-K-L  K   M-K  K+L-M
*    ( 0 R ) =     K ( 0    R11  R12  R13  )
*                M-K ( 0     0   R22  R23  )
*              K+L-M ( 0     0    0   R33  )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(M) ),
*    S = diag( BETA(K+1),  ... , BETA(M) ),
*    C**2 + S**2 = I.
*    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
*    ( 0  R22 R23 )
*    in B(M-K+1:L,N+M-K-L+1:N) on exit.
*  The routine computes C, S, R, and optionally the unitary
*  transformation matrices U, V and Q.
*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
*  A and B implicitly gives the SVD of A*inv(B):
*                       A*inv(B) = U*(D1*inv(D2))*V'.
*  If ( A',B')' has orthnormal columns, then the GSVD of A and B is also
*  equal to the CS decomposition of A and B. Furthermore, the GSVD can
*  be used to derive the solution of the eigenvalue problem:
*                       A'*A x = lambda* B'*B x.
*  In some literature, the GSVD of A and B is presented in the form
*                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
*  where U and V are orthogonal and X is nonsingular, and D1 and D2 are
*  ``diagonal''.  The former GSVD form can be converted to the latter
*  form by taking the nonsingular matrix X as
*                        X = Q*(  I   0    )
*                              (  0 inv(R) )
"

	<cdecl: SDWORD 'cggsvd_'  char * char * char * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xheconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHECON estimates the reciprocal of the condition number of a complex
*  Hermitian matrix A using the factorization A = U*D*U**H or
*  A = L*D*L**H computed by CHETRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'checon_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * float * float * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xheevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHEEVD computes all eigenvalues and, optionally, eigenvectors of a
*  complex Hermitian matrix A.  If eigenvectors are desired, it uses a
*  divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'cheevd_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xheevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork rwork: rwork iwork: iwork info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHEEVR computes selected eigenvalues and, optionally, eigenvectors
*  of a complex Hermitian matrix T.  Eigenvalues and eigenvectors can
*  be selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*  Whenever possible, CHEEVR calls CSTEGR to compute the
*  eigenspectrum using Relatively Robust Representations.  CSTEGR
*  computes eigenvalues by the dqds algorithm, while orthogonal
*  eigenvectors are computed from various 'good' L D L^T representations
*  (also known as Relatively Robust Representations). Gram-Schmidt
*  orthogonalization is avoided as far as possible. More specifically,
*  the various steps of the algorithm are as follows. For the i-th
*  unreduced block of T,
*     (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
*          is a relatively robust representation,
*     (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
*         relative accuracy by the dqds algorithm,
*     (c) If there is a cluster of close eigenvalues, 'choose' sigma_i
*         close to the cluster, and go to step (a),
*     (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
*         compute the corresponding eigenvector by forming a
*         rank-revealing twisted factorization.
*  The desired accuracy of the output can be specified by the input
*  parameter ABSTOL.
*  For more details, see 'A new O(n^2) algorithm for the symmetric
*  tridiagonal eigenvalue/eigenvector problem', by Inderjit Dhillon,
*  Computer Science Division Technical Report No. UCB//CSD-97-971,
*  UC Berkeley, May 1997.
*  Note 1 : CHEEVR calls CSTEGR when the full spectrum is requested
*  on machines which conform to the ieee-754 floating point standard.
*  CHEEVR calls SSTEBZ and CSTEIN on non-ieee machines and
*  when partial spectrum requests are made.
*  Normal execution of CSTEGR may create NaNs and infinities and
*  hence may abort due to a floating point exception in environments
*  which do not handle NaNs and infinities in the ieee standard default
*  manner.
"

	<cdecl: SDWORD 'cheevr_'  char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * SDWORD * SDWORD * float * SDWORD * float * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xheevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork info: info length: ljobz length: luplo 
	<cdecl: void 'cheev_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xheevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHEEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a complex Hermitian matrix A.  Eigenvalues and eigenvectors can
*  be selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
"

	<cdecl: SDWORD 'cheevx_'  char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * SDWORD * SDWORD * float * SDWORD * float * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xhegvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHEGVD computes all the eigenvalues, and optionally, the eigenvectors
*  of a complex generalized Hermitian-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be Hermitian and B is also positive definite.
*  If eigenvectors are desired, it uses a divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'chegvd_'  SDWORD * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xhegvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHEGV computes all the eigenvalues, and optionally, the eigenvectors
*  of a complex generalized Hermitian-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
*  Here A and B are assumed to be Hermitian and B is also
*  positive definite.
"

	<cdecl: SDWORD 'chegv_'  SDWORD * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xhegvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHEGVX computes selected eigenvalues, and optionally, eigenvectors
*  of a complex generalized Hermitian-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be Hermitian and B is also positive definite.
*  Eigenvalues and eigenvectors can be selected by specifying either a
*  range of values or a range of indices for the desired eigenvalues.
"

	<cdecl: SDWORD 'chegvx_'  SDWORD * char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * SDWORD * SDWORD * float * SDWORD * float * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xhesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHESV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
*  matrices.
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**H,  if UPLO = 'U', or
*     A = L * D * L**H,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is Hermitian and block diagonal with 
*  1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
*  used to solve the system of equations A * X = B.
"

	<cdecl: SDWORD 'chesv_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xhetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHETRF computes the factorization of a complex Hermitian matrix A
*  using the Bunch-Kaufman diagonal pivoting method.  The form of the
*  factorization is
*     A = U*D*U**H  or  A = L*D*L**H
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is Hermitian and block diagonal with 
*  1-by-1 and 2-by-2 diagonal blocks.
*  This is the blocked version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'chetrf_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xhetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHETRI computes the inverse of a complex Hermitian indefinite matrix
*  A using the factorization A = U*D*U**H or A = L*D*L**H computed by
*  CHETRF.
"

	<cdecl: SDWORD 'chetri_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xhetrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CHETRS solves a system of linear equations A*X = B with a complex
*  Hermitian matrix A using the factorization A = U*D*U**H or
*  A = L*D*L**H computed by CHETRF.
"

	<cdecl: SDWORD 'chetrs_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xlacgvWithn: n x: x incx: incx 
	"
*  Purpose
*  =======
*  CLACGV conjugates a complex vector of length N.
"

	<cdecl: SDWORD 'clacgv_'  SDWORD * "ExternalFloatComplex"void * SDWORD * >
	^self invalidCall!

xlacpyWithuplo: uplo m: m n: n a: a lda: lda b: b ldb: ldb length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CLACPY copies all or part of a two-dimensional matrix A to another
*  matrix B.
"

	<cdecl: SDWORD 'clacpy_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xlangeWithnorm: norm m: m n: n a: a lda: lda work: work length: lengthOfnorm 
	"
*  Purpose
*  =======
*  CLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex matrix A.
"

	<cdecl: float 'clange_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD >
	^self invalidCall!

xlanheWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CLANHE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex hermitian matrix A.
"

	<cdecl: float 'clanhe_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD SDWORD >
	^self invalidCall!

xlanhpWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CLANHP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex hermitian matrix A,  supplied in packed form.
"

	<cdecl: float 'clanhp_'  char * char * SDWORD * "ExternalFloatComplex"void * float * SDWORD SDWORD >
	^self invalidCall!

xlanspWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CLANSP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex symmetric matrix A,  supplied in packed form.
"

	<cdecl: float 'clansp_'  char * char * SDWORD * "ExternalFloatComplex"void * float * SDWORD SDWORD >
	^self invalidCall!

xlansyWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CLANSY  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex symmetric matrix A.
"

	<cdecl: float 'clansy_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD SDWORD >
	^self invalidCall!

xlantpWithnorm: norm uplo: uplo diag: diag n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CLANTP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  triangular matrix A, supplied in packed form.
"

	<cdecl: float 'clantp_'  char * char * char * SDWORD * "ExternalFloatComplex"void * float * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlantrWithnorm: norm uplo: uplo diag: diag m: m n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CLANTR  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  trapezoidal or triangular matrix A.
"

	<cdecl: float 'clantr_'  char * char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlarnvWithidist: idist iseed: iseed n: n x: x 
	"
*  Purpose
*  =======
*  CLARNV returns a vector of n random complex numbers from a uniform or
*  normal distribution.
"

	<cdecl: SDWORD 'clarnv_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * >
	^self invalidCall!

xlasetWithuplo: uplo m: m n: n alpha: alpha beta: beta a: a lda: lda length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CLASET initializes a 2-D array A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
"

	<cdecl: SDWORD 'claset_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xpotrfWithuplo: uplo n: n a: a lda: lda info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CPOTRF computes the Cholesky factorization of a complex Hermitian
*  positive definite matrix A.
*  The factorization has the form
*     A = U**H * U,  if UPLO = 'U', or
*     A = L  * L**H,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*  This is the block version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'cpotrf_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsyconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CSYCON estimates the reciprocal of the condition number (in the
*  1-norm) of a complex symmetric matrix A using the factorization
*  A = U*D*U**T or A = L*D*L**T computed by CSYTRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'csycon_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * float * float * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xsysvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CSYSV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
*  matrices.
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**T,  if UPLO = 'U', or
*     A = L * D * L**T,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with 
*  1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
*  used to solve the system of equations A * X = B.
"

	<cdecl: SDWORD 'csysv_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CSYTRF computes the factorization of a complex symmetric matrix A
*  using the Bunch-Kaufman diagonal pivoting method.  The form of the
*  factorization is
*     A = U*D*U**T  or  A = L*D*L**T
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with
*  with 1-by-1 and 2-by-2 diagonal blocks.
*  This is the blocked version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'csytrf_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CSYTRI computes the inverse of a complex symmetric indefinite matrix
*  A using the factorization A = U*D*U**T or A = L*D*L**T computed by
*  CSYTRF.
"

	<cdecl: SDWORD 'csytri_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xsytrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  CSYTRS solves a system of linear equations A*X = B with a complex
*  symmetric matrix A using the factorization A = U*D*U**T or
*  A = L*D*L**T computed by CSYTRF.
"

	<cdecl: SDWORD 'csytrs_'  char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst info: info 
	"
*  Purpose
*  =======
*  CTGEXC reorders the generalized Schur decomposition of a complex
*  matrix pair (A,B), using an unitary equivalence transformation
*  (A, B) := Q * (A, B) * Z', so that the diagonal block of (A, B) with
*  row index IFST is moved to row ILST.
*  (A, B) must be in generalized Schur canonical form, that is, A and
*  B are both upper triangular.
*  Optionally, the matrices Q and Z of generalized Schur vectors are
*  updated.
*         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'
*         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'
"

	<cdecl: SDWORD 'ctgexc_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xtgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork info: info 
	"
*  Purpose
*  =======
*  CTGSEN reorders the generalized Schur decomposition of a complex
*  matrix pair (A, B) (in terms of an unitary equivalence trans-
*  formation Q' * (A, B) * Z), so that a selected cluster of eigenvalues
*  appears in the leading diagonal blocks of the pair (A,B). The leading
*  columns of Q and Z form unitary bases of the corresponding left and
*  right eigenspaces (deflating subspaces). (A, B) must be in
*  generalized Schur canonical form, that is, A and B are both upper
*  triangular.
*  CTGSEN also computes the generalized eigenvalues
*           w(j)= ALPHA(j) / BETA(j)
*  of the reordered matrix pair (A, B).
*  Optionally, the routine computes estimates of reciprocal condition
*  numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
*  (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
*  between the matrix pairs (A11, B11) and (A22,B22) that correspond to
*  the selected cluster and the eigenvalues outside the cluster, resp.,
*  and norms of 'projections' onto left and right eigenspaces w.r.t.
*  the selected cluster in the (1,1)-block.
"

	<cdecl: SDWORD 'ctgsen_'  SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * float * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xtgsylWithtrans: trans ijob: ijob m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc d: d ldd: ldd e: e lde: lde f: f ldf: ldf dif: dif scale: scale work: work lwork: lwork iwork: iwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  CTGSYL solves the generalized Sylvester equation:
*              A * R - L * B = scale * C            (1)
*              D * R - L * E = scale * F
*  where R and L are unknown m-by-n matrices, (A, D), (B, E) and
*  (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
*  respectively, with complex entries. A, B, D and E are upper
*  triangular (i.e., (A,D) and (B,E) in generalized Schur form).
*  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
*  is an output scaling factor chosen to avoid overflow.
*  In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
*  is defined as
*         Z = [ kron(In, A)  -kron(B', Im) ]        (2)
*             [ kron(In, D)  -kron(E', Im) ],
*  Here Ix is the identity matrix of size x and X' is the conjugate
*  transpose of X. Kron(X, Y) is the Kronecker product between the
*  matrices X and Y.
*  If TRANS = 'C', y in the conjugate transposed system Z'*y = scale*b
*  is solved for, which is equivalent to solve for R and L in
*              A' * R + D' * L = scale * C           (3)
*              R * B' + L * E' = scale * -F
*  This case (TRANS = 'C') is used to compute an one-norm-based estimate
*  of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
*  and (B,E), using CLACON.
*  If IJOB >= 1, CTGSYL computes a Frobenius norm-based estimate of
*  Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
*  reciprocal of the smallest singular value of Z.
*  This is a level-3 BLAS algorithm.
"

	<cdecl: SDWORD 'ctgsyl_'  char * SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtpconWithnorm: norm uplo: uplo diag: diag n: n ap: ap rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CTPCON estimates the reciprocal of the condition number of a packed
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'ctpcon_'  char * char * char * SDWORD * "ExternalFloatComplex"void * float * "ExternalFloatComplex"void * float * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtptriWithuplo: uplo diag: diag n: n ap: ap info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CTPTRI computes the inverse of a complex upper or lower triangular
*  matrix A stored in packed format.
"

	<cdecl: SDWORD 'ctptri_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtptrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs ap: ap b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CTPTRS solves a triangular system of the form
*     A * X = B,  A**T * X = B,  or  A**H * X = B,
*  where A is a triangular matrix of order N stored in packed format,
*  and B is an N-by-NRHS matrix.  A check is made to verify that A is
*  nonsingular.
"

	<cdecl: SDWORD 'ctptrs_'  char * char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CTRCON estimates the reciprocal of the condition number of a
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'ctrcon_'  char * char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * float * "ExternalFloatComplex"void * float * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst info: info length: lengthOfcompq 
	"
*  Purpose
*  =======
*  CTREXC reorders the Schur factorization of a complex matrix
*  A = Q*T*Q**H, so that the diagonal element of T with row index IFST
*  is moved to row ILST.
*  The Schur form T is reordered by a unitary similarity transformation
*  Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by
*  postmultplying it with Z.
"

	<cdecl: SDWORD 'ctrexc_'  char * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtrsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq w: w m: m s: s sep: sep work: work lwork: lwork info: info length: lengthOfjob length: lengthOfcompq 
	"
*  Purpose
*  =======
*  CTRSEN reorders the Schur factorization of a complex matrix
*  A = Q*T*Q**H, so that a selected cluster of eigenvalues appears in
*  the leading positions on the diagonal of the upper triangular matrix
*  T, and the leading columns of Q form an orthonormal basis of the
*  corresponding right invariant subspace.
*  Optionally the routine computes the reciprocal condition numbers of
*  the cluster of eigenvalues and/or the invariant subspace.
"

	<cdecl: SDWORD 'ctrsen_'  char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * float * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrsylWithtrana: trana tranb: tranb isgn: isgn m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc scale: scale info: info length: lengthOftrana length: lengthOftranb 
	"
*  Purpose
*  =======
*  CTRSYL solves the complex Sylvester matrix equation:
*     op(A)*X + X*op(B) = scale*C or
*     op(A)*X - X*op(B) = scale*C,
*  where op(A) = A or A**H, and A and B are both upper triangular. A is
*  M-by-M and B is N-by-N; the right hand side C and the solution X are
*  M-by-N; and scale is an output scale factor, set <= 1 to avoid
*  overflow in X.
"

	<cdecl: SDWORD 'ctrsyl_'  char * char * SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtriWithuplo: uplo diag: diag n: n a: a lda: lda info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CTRTRI computes the inverse of a complex upper or lower triangular
*  matrix A.
*  This is the Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'ctrtri_'  char * char * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  CTRTRS solves a triangular system of the form
*     A * X = B,  A**T * X = B,  or  A**H * X = B,
*  where A is a triangular matrix of order N, and B is an N-by-NRHS
*  matrix.  A check is made to verify that A is nonsingular.
"

	<cdecl: SDWORD 'ctrtrs_'  char * char * char * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xunghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'cunghr_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xunglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CUNGLQ generates an M-by-N complex matrix Q with orthonormal rows,
*  which is defined as the first M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(k)' . . . H(2)' H(1)'
*  as returned by CGELQF.
"

	<cdecl: SDWORD 'cunglq_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xungqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CUNGQL generates an M-by-N complex matrix Q with orthonormal columns,
*  which is defined as the last N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by CGEQLF.
"

	<cdecl: SDWORD 'cungql_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xungqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by CGEQRF.
"

	<cdecl: SDWORD 'cungqr_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xungrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  CUNGRQ generates an M-by-N complex matrix Q with orthonormal rows,
*  which is defined as the last M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(1)' H(2)' . . . H(k)'
*  as returned by CGERQF.
"

	<cdecl: SDWORD 'cungrq_'  SDWORD * SDWORD * SDWORD * "ExternalFloatComplex"void * SDWORD * "ExternalFloatComplex"void * "ExternalFloatComplex"void * SDWORD * SDWORD * >
	^self invalidCall! !
!LapackCLibrary categoriesFor: #cComplexPointerOn:!public! !
!LapackCLibrary categoriesFor: #cElementPointerOn:!public! !
!LapackCLibrary categoriesFor: #cRealPointerOn:!public! !
!LapackCLibrary categoriesFor: #isComplex!public! !
!LapackCLibrary categoriesFor: #isDoublePrecision!public! !
!LapackCLibrary categoriesFor: #schurSelectFunction!public! !
!LapackCLibrary categoriesFor: #xgebakWithjob:side:n:ilo:ihi:scale:m:v:ldv:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xgebalWithjob:n:a:lda:ilo:ihi:scale:info:length:!public! !
!LapackCLibrary categoriesFor: #xgeconWithnorm:n:a:lda:anorm:rcond:work:rwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xgeesWithjobvs:sort:select:n:a:lda:sdim:w:vs:ldvs:work:lwork:rwork:bwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xgeevWithjobvl:jobvr:n:a:lda:w:vl:ldvl:vr:ldvr:work:lwork:rwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xgeevxWithbalanc:jobvl:jobvr:sense:n:a:lda:w:vl:ldvl:vr:ldvr:scale:abnrm:rconde:rcondv:work:lwork:rwork:info:length:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xgehrdWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xgelqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xgelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:iwork:info:!public! !
!LapackCLibrary categoriesFor: #xgelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:info:!public! !
!LapackCLibrary categoriesFor: #xgelsWithtrans:m:n:nrhs:a:lda:b:ldb:work:lwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xgelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:rwork:info:!public! !
!LapackCLibrary categoriesFor: #xgelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:rwork:info:!public! !
!LapackCLibrary categoriesFor: #xgeqlfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xgeqp3Withm:n:a:lda:jpvt:tau:work:lwork:rwork:info:!public! !
!LapackCLibrary categoriesFor: #xgeqrfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xgerqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xgesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:iwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xgesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xgesvWithn:nrhs:a:lda:ipiv:b:ldb:info:!public! !
!LapackCLibrary categoriesFor: #xgetrfWithm:n:a:lda:ipiv:info:!public! !
!LapackCLibrary categoriesFor: #xgetriWithn:a:lda:ipiv:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xgetrsWithtrans:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackCLibrary categoriesFor: #xggbakWithjob:side:n:ilo:ihi:lscale:rscale:m:v:ldv:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xggbalWithjob:n:a:lda:b:ldb:ilo:ihi:lscale:rscale:work:info:length:!public! !
!LapackCLibrary categoriesFor: #xggevWithjobvl:jobvr:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:work:lwork:rwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:rwork:bwork:info:length:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:rwork:iwork:bwork:info:length:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xggglmWithn:m:p:a:lda:b:ldb:d:x:y:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xgghrdWithcompq:compz:n:ilo:ihi:a:lda:b:ldb:q:ldq:z:ldz:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xgglseWithm:n:p:a:lda:b:ldb:c:d:x:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xggqrfWithn:m:p:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xggrqfWithm:p:n:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:rwork:iwork:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xheconWithuplo:n:a:lda:ipiv:anorm:rcond:work:info:length:!public! !
!LapackCLibrary categoriesFor: #xheevdWithjobz:uplo:n:a:lda:w:work:lwork:rwork:lrwork:iwork:liwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xheevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:rwork:iwork:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xheevWithjobz:uplo:n:a:lda:w:work:lwork:rwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xheevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xhegvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:lrwork:iwork:liwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xhegvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xhegvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xhesvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xhetrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xhetriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackCLibrary categoriesFor: #xhetrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackCLibrary categoriesFor: #xlacgvWithn:x:incx:!public! !
!LapackCLibrary categoriesFor: #xlacpyWithuplo:m:n:a:lda:b:ldb:length:!public! !
!LapackCLibrary categoriesFor: #xlangeWithnorm:m:n:a:lda:work:length:!public! !
!LapackCLibrary categoriesFor: #xlanheWithnorm:uplo:n:a:lda:work:length:length:!public! !
!LapackCLibrary categoriesFor: #xlanhpWithnorm:uplo:n:ap:work:length:length:!public! !
!LapackCLibrary categoriesFor: #xlanspWithnorm:uplo:n:ap:work:length:length:!public! !
!LapackCLibrary categoriesFor: #xlansyWithnorm:uplo:n:a:lda:work:length:length:!public! !
!LapackCLibrary categoriesFor: #xlantpWithnorm:uplo:diag:n:ap:work:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xlantrWithnorm:uplo:diag:m:n:a:lda:work:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xlarnvWithidist:iseed:n:x:!public! !
!LapackCLibrary categoriesFor: #xlasetWithuplo:m:n:alpha:beta:a:lda:length:!public! !
!LapackCLibrary categoriesFor: #xpotrfWithuplo:n:a:lda:info:length:!public! !
!LapackCLibrary categoriesFor: #xsyconWithuplo:n:a:lda:ipiv:anorm:rcond:work:info:length:!public! !
!LapackCLibrary categoriesFor: #xsysvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xsytrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xsytriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackCLibrary categoriesFor: #xsytrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackCLibrary categoriesFor: #xtgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:info:!public! !
!LapackCLibrary categoriesFor: #xtgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alpha:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:info:!public! !
!LapackCLibrary categoriesFor: #xtgsylWithtrans:ijob:m:n:a:lda:b:ldb:c:ldc:d:ldd:e:lde:f:ldf:dif:scale:work:lwork:iwork:info:length:!public! !
!LapackCLibrary categoriesFor: #xtpconWithnorm:uplo:diag:n:ap:rcond:work:rwork:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xtptriWithuplo:diag:n:ap:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xtptrsWithuplo:trans:diag:n:nrhs:ap:b:ldb:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xtrconWithnorm:uplo:diag:n:a:lda:rcond:work:rwork:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xtrexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:info:length:!public! !
!LapackCLibrary categoriesFor: #xtrsenWithjob:compq:select:n:t:ldt:q:ldq:w:m:s:sep:work:lwork:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xtrsylWithtrana:tranb:isgn:m:n:a:lda:b:ldb:c:ldc:scale:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xtrtriWithuplo:diag:n:a:lda:info:length:length:!public! !
!LapackCLibrary categoriesFor: #xtrtrsWithuplo:trans:diag:n:nrhs:a:lda:b:ldb:info:length:length:length:!public! !
!LapackCLibrary categoriesFor: #xunghrWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xunglqWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xungqlWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xungqrWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackCLibrary categoriesFor: #xungrqWithm:n:k:a:lda:tau:work:lwork:info:!public! !

LapackDLibrary guid: (GUID fromString: '{62BB7B1F-0CBA-4ECF-A76B-A038E4A6A28E}')!
LapackDLibrary comment: ''!
!LapackDLibrary categoriesForClass!Unclassified! !
!LapackDLibrary methodsFor!

cComplexPointerOn: aComplex 	^self cDoubleComplexPointerOn: aComplex!

cElementPointerOn: aDouble 	^self cRealPointerOn: aDouble!

cRealPointerOn: aDouble 	^self cDoublePointerOn: aDouble!

isComplex	^false!

isDoublePrecision	^true!

schurSelectFunction
	"Answer the descriptor for callback function"

	^ExternalDescriptor fromString: 'cdecl: SDWORD DOUBLE* DOUBLE*' !

xgebakWithjob: job side: side n: n ilo: ilo ihi: ihi scale: scale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  DGEBAK forms the right or left eigenvectors of a real general matrix
*  by backward transformation on the computed eigenvectors of the
*  balanced matrix output by DGEBAL.
"

	<cdecl: SDWORD 'dgebak_'  char * char * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgebalWithjob: job n: n a: a lda: lda ilo: ilo ihi: ihi scale: scale info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  DGEBAL balances a general real matrix A.  This involves, first,
*  permuting A by a similarity transformation to isolate eigenvalues
*  in the first 1 to ILO-1 and last IHI+1 to N elements on the
*  diagonal; and second, applying a diagonal similarity transformation
*  to rows and columns ILO to IHI to make the rows and columns as
*  close in norm as possible.  Both steps are optional.
*  Balancing may reduce the 1-norm of the matrix, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors.
"

	<cdecl: SDWORD 'dgebal_'  char * SDWORD * double * SDWORD * SDWORD * SDWORD * double * SDWORD * SDWORD >
	^self invalidCall!

xgeconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm 
	"
*  Purpose
*  =======
*  DGECON estimates the reciprocal of the condition number of a general
*  real matrix A, in either the 1-norm or the infinity-norm, using
*  the LU factorization computed by DGETRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'dgecon_'  char * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgeequWithm: m n: n a: a lda: lda r: r c: c rowcnd: rowcnd colcnd: colcnd amax: amax info: info 
	"
*  Purpose
*  =======
*  DGEEQU computes row and column scalings intended to equilibrate an
*  M-by-N matrix A and reduce its condition number.  R returns the row
*  scale factors and C the column scale factors, chosen to try to make
*  the largest element in each row and column of the matrix B with
*  elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
*  R(i) and C(j) are restricted to be between SMLNUM = smallest safe
*  number and BIGNUM = largest safe number.  Use of these scaling
*  factors is not guaranteed to reduce the condition number of A but
*  works well in practice.
"

	<cdecl: SDWORD 'dgeequ_' SDWORD * SDWORD * double * SDWORD * double * double * double * double * double * SDWORD *>
	^self invalidCall!

xgeesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs work: work lwork: lwork bwork: bwork info: info length: lengthOfjobvs length: lengthOfsort 
	"
*  Purpose
*  =======
*  DGEES computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues, the real Schur form T, and, optionally, the matrix of
*  Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
*  Optionally, it also orders the eigenvalues on the diagonal of the
*  real Schur form so that selected eigenvalues are at the top left.
*  The leading columns of Z then form an orthonormal basis for the
*  invariant subspace corresponding to the selected eigenvalues.
*  A matrix is in real Schur form if it is upper quasi-triangular with
*  1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in the
*  form
*          [  a  b  ]
*          [  c  a  ]
*  where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
"

	<cdecl: SDWORD 'dgees_'  char * char * SDWORD * SDWORD * double * SDWORD * SDWORD * double * double * double * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgeesxWithjobvs: jobvs sort: sort select: select sense: sense n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs rconde: rconde rcondv: rcondv work: work lwork: lwork iwork: iwork liwork: liwork bwork: bwork info: info length: lengthArgjobvs length: lengthArgsort length: lengthArgsense 
	"
*  Purpose
*  =======
*  DGEESX computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues, the real Schur form T, and, optionally, the matrix of
*  Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
*  Optionally, it also orders the eigenvalues on the diagonal of the
*  real Schur form so that selected eigenvalues are at the top left;
*  computes a reciprocal condition number for the average of the
*  selected eigenvalues (RCONDE); and computes a reciprocal condition
*  number for the right invariant subspace corresponding to the
*  selected eigenvalues (RCONDV).  The leading columns of Z form an
*  orthonormal basis for this invariant subspace.
*  For further explanation of the reciprocal condition numbers RCONDE
*  and RCONDV, see Section 4.10 of the LAPACK Users' Guide (where
*  these quantities are called s and sep respectively).
*  A real matrix is in real Schur form if it is upper quasi-triangular
*  with 1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in
*  the form
*            [  a  b  ]
*            [  c  a  ]
*  where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
"

	<cdecl: SDWORD 'dgeesx_' char * char * SDWORD * char * SDWORD * double * SDWORD * SDWORD * double * double * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD>
	^self invalidCall!

xgeevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  DGEEV computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
"

	<cdecl: SDWORD 'dgeev_'  char * char * SDWORD * double * SDWORD * double * double * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgeevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr scale: scale abnrm: abnrm rconde: rconde rcondv: rcondv work: work lwork: lwork iwork: iwork info: info length: lengthArgbalanc length: lengthArgjobvl length: lengthArgjobvr length: lengthArgsense 
	"
*  Purpose
*  =======
*  DGEEVX computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*  Optionally also, it computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
*  (RCONDE), and reciprocal condition numbers for the right
*  eigenvectors (RCONDV).
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
*  Balancing a matrix means permuting the rows and columns to make it
*  more nearly upper triangular, and applying a diagonal similarity
*  transformation D * A * D**(-1), where D is a diagonal matrix, to
*  make its rows and columns closer in norm and the condition numbers
*  of its eigenvalues and eigenvectors smaller.  The computed
*  reciprocal condition numbers correspond to the balanced matrix.
*  Permuting rows and columns will not change the condition numbers
*  (in exact arithmetic) but diagonal scaling will.  For further
*  explanation of balancing, see section 4.10.2 of the LAPACK
*  Users' Guide.
"

	<cdecl: SDWORD 'dgeevx_' char * char * char * char * SDWORD * double * SDWORD * double * double * double * SDWORD * double * SDWORD * double * double * double * double * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD>
	^self invalidCall!

xgehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'dgehrd_'  SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * >
	^self invalidCall!

xgelqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGELQF computes an LQ factorization of a real M-by-N matrix A:
*  A = L * Q.
"

	<cdecl: SDWORD 'dgelqf_' SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xgelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork iwork: iwork info: info 
	"
*  Purpose
*  =======
*  DGELSD computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize 2-norm(| b - A*x |)
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The problem is solved in three steps:
*  (1) Reduce the coefficient matrix A to bidiagonal form with
*      Householder transformations, reducing the original problem
*      into a 'bidiagonal least squares problem' (BLS)
*  (2) Solve the BLS using a divide and conquer approach.
*  (3) Apply back all the Householder tranformations to solve
*      the original least squares problem.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'dgelsd_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * SDWORD * double * SDWORD * SDWORD * SDWORD *>
	^self invalidCall!

xgelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGELSS computes the minimum norm solution to a real linear least
*  squares problem:
*  Minimize 2-norm(| b - A*x |).
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
*  X.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
"

	<cdecl: SDWORD 'dgelss_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * SDWORD * double * SDWORD * SDWORD *>
	^self invalidCall!

xgelsWithtrans: trans m: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb work: work lwork: lwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  DGELS solves overdetermined or underdetermined real linear systems
*  involving an M-by-N matrix A, or its transpose, using a QR or LQ
*  factorization of A.  It is assumed that A has full rank.
*  The following options are provided:
*  1. If TRANS = 'N' and m >= n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A*X ||.
*  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
*     an underdetermined system A * X = B.
*  3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
*     an undetermined system A**T * X = B.
*  4. If TRANS = 'T' and m < n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A**T * X ||.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
"

	<cdecl: SDWORD 'dgels_'  char * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work info: info 
	"
*  Purpose
*  =======
*  This routine is deprecated and has been replaced by routine DGELSY.
*  DGELSX computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
"

	<cdecl: SDWORD 'dgelsx_'  SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * double * SDWORD * double * SDWORD * >
	^self invalidCall!

xgelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGELSY computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
*  This routine is basically identical to the original xGELSX except
*  three differences:
*    o The call to the subroutine xGEQPF has been substituted by the
*      the call to the subroutine xGEQP3. This subroutine is a Blas-3
*      version of the QR factorization with column pivoting.
*    o Matrix B (the right hand side) is updated with Blas-3.
*    o The permutation of matrix B (the right hand side) is faster and
*      more simple.
"

	<cdecl: SDWORD 'dgelsy_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD *>
	^self invalidCall!

xgeql2Withm: m n: n a: a lda: lda tau: tau work: work info: info 
	"
*  Purpose
*  =======
*  DGEQL2 computes a QL factorization of a real m by n matrix A:
*  A = Q * L.
"

	<cdecl: SDWORD 'dgeql2_' SDWORD * SDWORD * double * SDWORD * double * double * SDWORD *>
	^self invalidCall!

xgeqlfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGEQLF computes a QL factorization of a real M-by-N matrix A:
*  A = Q * L.
"

	<cdecl: SDWORD 'dgeqlf_' SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xgeqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGEQP3 computes a QR factorization with column pivoting of a
*  matrix A:  A*P = Q*R  using Level 3 BLAS.
"

	<cdecl: SDWORD 'dgeqp3_' SDWORD * SDWORD * double * SDWORD * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xgeqr2Withm: m n: n a: a lda: lda tau: tau work: work info: info 
	"
*  Purpose
*  =======
*  DGEQR2 computes a QR factorization of a real m by n matrix A:
*  A = Q * R.
"

	<cdecl: SDWORD 'dgeqr2_' SDWORD * SDWORD * double * SDWORD * double * double * SDWORD *>
	^self invalidCall!

xgeqrfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGEQRF computes a QR factorization of a real M-by-N matrix A:
*  A = Q * R.
"

	<cdecl: SDWORD 'dgeqrf_' SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xgerfsWithtrans: trans n: n nrhs: nrhs a: a lda: lda af: af ldaf: ldaf ipiv: ipiv b: b ldb: ldb x: x ldx: ldx ferr: ferr berr: berr work: work iwork: iwork info: info length: lengthArgtrans 
	"
*  Purpose
*  =======
*  DGERFS improves the computed solution to a system of linear
*  equations and provides error bounds and backward error estimates for
*  the solution.
"

	<cdecl: SDWORD 'dgerfs_' char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xgerq2Withm: m n: n a: a lda: lda tau: tau work: work info: info 
	"
*  Purpose
*  =======
*  DGERQ2 computes an RQ factorization of a real m by n matrix A:
*  A = R * Q.
"

	<cdecl: SDWORD 'dgerq2_' SDWORD * SDWORD * double * SDWORD * double * double * SDWORD *>
	^self invalidCall!

xgerqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGERQF computes an RQ factorization of a real M-by-N matrix A:
*  A = R * Q.
"

	<cdecl: SDWORD 'dgerqf_' SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xgesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork iwork: iwork info: info length: lengthOfjobz 
	"
*  Purpose
*  =======
*  DGESDD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and right singular
*  vectors.  If singular vectors are desired, it uses a
*  divide-and-conquer algorithm.
*  The SVD is written
*       A = U * SIGMA * transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns VT = V**T, not V.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'dgesdd_'  char * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork info: info length: lengthOfjobu length: lengthOfjobvt 
	"
*  Purpose
*  =======
*  DGESVD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*       A = U * SIGMA * transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns V**T, not V.
"

	<cdecl: SDWORD 'dgesvd_'  char * char * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgesvWithn: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info 
	"
*  Purpose
*  =======
*  DGESV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*  The LU decomposition with partial pivoting and row interchanges is
*  used to factor A as
*     A = P * L * U,
*  where P is a permutation matrix, L is unit lower triangular, and U is
*  upper triangular.  The factored form of A is then used to solve the
*  system of equations A * X = B.
"

	<cdecl: SDWORD 'dgesv_' SDWORD * SDWORD * double * SDWORD * SDWORD * double * SDWORD * SDWORD *>
	^self invalidCall!

xgesvxWithfact: fact trans: trans n: n nrhs: nrhs a: a lda: lda af: af ldaf: ldaf ipiv: ipiv equed: equed r: r c: c b: b ldb: ldb x: x ldx: ldx rcond: rcond ferr: ferr berr: berr work: work iwork: iwork info: info length: lengthArgfact length: lengthArgtrans length: lengthArgequed 
	"
*  Purpose
*  =======
*  DGESVX uses the LU factorization to compute the solution to a real
*  system of linear equations
*     A * X = B,
*  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*  Error bounds on the solution and a condition estimate are also
*  provided.
"

	<cdecl: SDWORD 'dgesvx_' char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * char * double * double * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD>
	^self invalidCall!

xgetrfWithm: m n: n a: a lda: lda ipiv: ipiv info: info 
	"
*  Purpose
*  =======
*  DGETRF computes an LU factorization of a general M-by-N matrix A
*  using partial pivoting with row interchanges.
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*  This is the right-looking Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'dgetrf_' SDWORD * SDWORD * double * SDWORD * SDWORD * SDWORD *>
	^self invalidCall!

xgetriWithn: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGETRI computes the inverse of a matrix using the LU factorization
*  computed by DGETRF.
*  This method inverts U and then computes inv(A) by solving the system
*  inv(A)*L = inv(U) for inv(A).
"

	<cdecl: SDWORD 'dgetri_' SDWORD * double * SDWORD * SDWORD * double * SDWORD * SDWORD *>
	^self invalidCall!

xgetrsWithtrans: trans n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  DGETRS solves a system of linear equations
*     A * X = B  or  A' * X = B
*  with a general N-by-N matrix A using the LU factorization computed
*  by DGETRF.
"

	<cdecl: SDWORD 'dgetrs_'  char * SDWORD * SDWORD * double * SDWORD * SDWORD * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xggbakWithjob: job side: side n: n ilo: ilo ihi: ihi lscale: lscale rscale: rscale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  DGGBAK forms the right or left eigenvectors of a real generalized
*  eigenvalue problem A*x = lambda*B*x, by backward transformation on
*  the computed eigenvectors of the balanced pair of matrices output by
*  DGGBAL.
"

	<cdecl: SDWORD 'dggbak_'  char * char * SDWORD * SDWORD * SDWORD * double * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggbalWithjob: job n: n a: a lda: lda b: b ldb: ldb ilo: ilo ihi: ihi lscale: lscale rscale: rscale work: work info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  DGGBAL balances a pair of general real matrices (A,B).  This
*  involves, first, permuting A and B by similarity transformations to
*  isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
*  elements on the diagonal; and second, applying a diagonal similarity
*  transformation to rows and columns ILO to IHI to make the rows
*  and columns as close in norm as possible. Both steps are optional.
*  Balancing may reduce the 1-norm of the matrices, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors in the
*  generalized eigenvalue problem A*x = lambda*B*x.
"

	<cdecl: SDWORD 'dggbal_'  char * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD * double * double * double * SDWORD * SDWORD >
	^self invalidCall!

xggesWithjobvsl: jobvsl jobvsr: jobvsr sort: sort delztg: delztg n: n a: a lda: lda b: b ldb: ldb sdim: sdim alphar: alphar alphai: alphai beta: beta vsl: vsl ldvsl: ldvsl vsr: vsr ldvsr: ldvsr work: work lwork: lwork bwork: bwork info: info length: lengthArgjobvsl length: lengthArgjobvsr length: lengthArgsort 
	"
*  Purpose
*  =======
*  DGGES computes for a pair of N-by-N real nonsymmetric matrices (A,B),
*  the generalized eigenvalues, the generalized real Schur form (S,T),
*  optionally, the left and/or right matrices of Schur vectors (VSL and
*  VSR). This gives the generalized Schur factorization
*           (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T )
*  Optionally, it also orders the eigenvalues so that a selected cluster
*  of eigenvalues appears in the leading diagonal blocks of the upper
*  quasi-triangular matrix S and the upper triangular matrix T.The
*  leading columns of VSL and VSR then form an orthonormal basis for the
*  corresponding left and right eigenspaces (deflating subspaces).
*  (If only the generalized eigenvalues are needed, use the driver
*  DGGEV instead, which is faster.)
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
*  or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
*  usually represented as the pair (alpha,beta), as there is a
*  reasonable interpretation for beta=0 or both being zero.
*  A pair of matrices (S,T) is in generalized real Schur form if T is
*  upper triangular with non-negative diagonal and S is block upper
*  triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
*  to real generalized eigenvalues, while 2-by-2 blocks of S will be
*  'standardized' by making the corresponding elements of T have the
*  form:
*          [  a  0  ]
*          [  0  b  ]
*  and the pair of corresponding 2-by-2 blocks in S and T will have a
*  complex conjugate pair of generalized eigenvalues.
"

	<cdecl: SDWORD 'dgges_' char * char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * double * double * double * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD>
	^self invalidCall!

xggesxWithjobvsl: jobvsl jobvsr: jobvsr sort: sort delztg: delztg sense: sense n: n a: a lda: lda b: b ldb: ldb sdim: sdim alphar: alphar alphai: alphai beta: beta vsl: vsl ldvsl: ldvsl vsr: vsr ldvsr: ldvsr rconde: rconde rcondv: rcondv work: work lwork: lwork iwork: iwork liwork: liwork bwork: bwork info: info length: lengthArgjobvsl length: lengthArgjobvsr length: lengthArgsort length: lengthArgsense 
	"
*  Purpose
*  =======
*  DGGESX computes for a pair of N-by-N real nonsymmetric matrices
*  (A,B), the generalized eigenvalues, the real Schur form (S,T), and,
*  optionally, the left and/or right matrices of Schur vectors (VSL and
*  VSR).  This gives the generalized Schur factorization
*       (A,B) = ( (VSL) S (VSR)**T, (VSL) T (VSR)**T )
*  Optionally, it also orders the eigenvalues so that a selected cluster
*  of eigenvalues appears in the leading diagonal blocks of the upper
*  quasi-triangular matrix S and the upper triangular matrix T; computes
*  a reciprocal condition number for the average of the selected
*  eigenvalues (RCONDE); and computes a reciprocal condition number for
*  the right and left deflating subspaces corresponding to the selected
*  eigenvalues (RCONDV). The leading columns of VSL and VSR then form
*  an orthonormal basis for the corresponding left and right eigenspaces
*  (deflating subspaces).
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
*  or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
*  usually represented as the pair (alpha,beta), as there is a
*  reasonable interpretation for beta=0 or for both being zero.
*  A pair of matrices (S,T) is in generalized real Schur form if T is
*  upper triangular with non-negative diagonal and S is block upper
*  triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
*  to real generalized eigenvalues, while 2-by-2 blocks of S will be
*  'standardized' by making the corresponding elements of T have the
*  form:
*          [  a  0  ]
*          [  0  b  ]
*  and the pair of corresponding 2-by-2 blocks in S and T will have a
*  complex conjugate pair of generalized eigenvalues.
"

	<cdecl: SDWORD 'dggesx_' char * char * char * SDWORD * char * SDWORD * double * SDWORD * double * SDWORD * SDWORD * double * double * double * double * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD>
	^self invalidCall!

xggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  DGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
*  the generalized eigenvalues, and optionally, the left and/or right
*  generalized eigenvectors.
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j).
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B .
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'dggev_'  char * char * SDWORD * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork iwork: iwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  DGGEVX computes for a pair of N-by-N real nonsymmetric matrices (A,B)
*  the generalized eigenvalues, and optionally, the left and/or right
*  generalized eigenvectors.
*  Optionally also, it computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
*  the eigenvalues (RCONDE), and reciprocal condition numbers for the
*  right eigenvectors (RCONDV).
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j) .
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B.
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'dggevx_'  char * char * char * char * SDWORD * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * double * SDWORD * double * double * double * double * double * double * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xggglmWithn: n m: m p: p a: a lda: lda b: b ldb: ldb d: d x: x y: y work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGGGLM solves a general Gauss-Markov linear model (GLM) problem:
*          minimize || y ||_2   subject to   d = A*x + B*y
*              x
*  where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
*  given N-vector. It is assumed that M <= N <= M+P, and
*             rank(A) = M    and    rank( A B ) = N.
*  Under these assumptions, the constrained equation is always
*  consistent, and there is a unique solution x and a minimal 2-norm
*  solution y, which is obtained using a generalized QR factorization
*  of A and B.
*  In particular, if matrix B is square nonsingular, then the problem
*  GLM is equivalent to the following weighted linear least squares
*  problem
*               minimize || inv(B)*(d-A*x) ||_2
*                   x
*  where inv(B) denotes the inverse of B.
"

	<cdecl: SDWORD 'dggglm_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xgghrdWithcompq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz info: info length: lengthOfcompq length: lengthOfcompz 
	"
*  Purpose
*  =======
*  DGGHRD reduces a pair of real matrices (A,B) to generalized upper
*  Hessenberg form using orthogonal transformations, where A is a
*  general matrix and B is upper triangular.  The form of the
*  generalized eigenvalue problem is
*     A*x = lambda*B*x,
*  and B is typically made upper triangular by computing its QR
*  factorization and moving the orthogonal matrix Q to the left side
*  of the equation.
*  This subroutine simultaneously reduces A to a Hessenberg matrix H:
*     Q**T*A*Z = H
*  and transforms B to another upper triangular matrix T:
*     Q**T*B*Z = T
*  in order to reduce the problem to its standard form
*     H*y = lambda*T*y
*  where y = Z**T*x.
*  The orthogonal matrices Q and Z are determined as products of Givens
*  rotations.  They may either be formed explicitly, or they may be
*  postmultiplied into input matrices Q1 and Z1, so that
*       Q1 * A * Z1**T = (Q1*Q) * H * (Z1*Z)**T
*       Q1 * B * Z1**T = (Q1*Q) * T * (Z1*Z)**T
*  If Q1 is the orthogonal matrix from the QR factorization of B in the
*  original equation A*x = lambda*B*x, then DGGHRD reduces the original
*  problem to generalized Hessenberg form.
"

	<cdecl: SDWORD 'dgghrd_'  char * char * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGGLSE solves the linear equality-constrained least squares (LSE)
*  problem:
*          minimize || c - A*x ||_2   subject to   B*x = d
*  where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
*  M-vector, and d is a given P-vector. It is assumed that
*  P <= N <= M+P, and
*           rank(B) = P and  rank( ( A ) ) = N.
*                                ( ( B ) )
*  These conditions ensure that the LSE problem has a unique solution,
*  which is obtained using a GRQ factorization of the matrices B and A.
"

	<cdecl: SDWORD 'dgglse_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xggqrfWithn: n m: m p: p a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGGQRF computes a generalized QR factorization of an N-by-M matrix A
*  and an N-by-P matrix B:
*              A = Q*R,        B = Q*T*Z,
*  where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
*  matrix, and R and T assume one of the forms:
*  if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
*                  (  0  ) N-M                         N   M-N
*                     M
*  where R11 is upper triangular, and
*  if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
*                   P-N  N                           ( T21 ) P
*                                                       P
*  where T12 or T21 is upper triangular.
*  In particular, if B is square and nonsingular, the GQR factorization
*  of A and B implicitly gives the QR factorization of inv(B)*A:
*               inv(B)*A = Z'*(inv(T)*R)
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  transpose of the matrix Z.
"

	<cdecl: SDWORD 'dggqrf_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xggrqfWithm: m p: p n: n a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DGGRQF computes a generalized RQ factorization of an M-by-N matrix A
*  and a P-by-N matrix B:
*              A = R*Q,        B = Z*T*Q,
*  where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
*  matrix, and R and T assume one of the forms:
*  if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
*                   N-M  M                           ( R21 ) N
*                                                       N
*  where R12 or R21 is upper triangular, and
*  if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
*                  (  0  ) P-N                         P   N-P
*                     N
*  where T11 is upper triangular.
*  In particular, if B is square and nonsingular, the GRQ factorization
*  of A and B implicitly gives the RQ factorization of A*inv(B):
*               A*inv(B) = (R*inv(T))*Z'
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  transpose of the matrix Z.
"

	<cdecl: SDWORD 'dggrqf_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work iwork: iwork info: info length: lengthOfjobu length: lengthOfjobv length: lengthOfjobq 
	"
*  Purpose
*  =======
*  DGGSVD computes the generalized singular value decomposition (GSVD)
*  of an M-by-N real matrix A and P-by-N real matrix B:
*      U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
*  where U, V and Q are orthogonal matrices, and Z' is the transpose
*  of Z.  Let K+L = the effective numerical rank of the matrix (A',B')',
*  then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and
*  D2 are M-by-(K+L) and P-by-(K+L) 'diagonal' matrices and of the
*  following structures, respectively:
*  If M-K-L >= 0,
*                      K  L
*         D1 =     K ( I  0 )
*                  L ( 0  C )
*              M-K-L ( 0  0 )
*                    K  L
*         D2 =   L ( 0  S )
*              P-L ( 0  0 )
*                  N-K-L  K    L
*    ( 0 R ) = K (  0   R11  R12 )
*              L (  0    0   R22 )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
*    S = diag( BETA(K+1),  ... , BETA(K+L) ),
*    C**2 + S**2 = I.
*    R is stored in A(1:K+L,N-K-L+1:N) on exit.
*  If M-K-L < 0,
*                    K M-K K+L-M
*         D1 =   K ( I  0    0   )
*              M-K ( 0  C    0   )
*                      K M-K K+L-M
*         D2 =   M-K ( 0  S    0  )
*              K+L-M ( 0  0    I  )
*                P-L ( 0  0    0  )
*                     N-K-L  K   M-K  K+L-M
*    ( 0 R ) =     K ( 0    R11  R12  R13  )
*                M-K ( 0     0   R22  R23  )
*              K+L-M ( 0     0    0   R33  )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(M) ),
*    S = diag( BETA(K+1),  ... , BETA(M) ),
*    C**2 + S**2 = I.
*    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
*    ( 0  R22 R23 )
*    in B(M-K+1:L,N+M-K-L+1:N) on exit.
*  The routine computes C, S, R, and optionally the orthogonal
*  transformation matrices U, V and Q.
*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
*  A and B implicitly gives the SVD of A*inv(B):
*                       A*inv(B) = U*(D1*inv(D2))*V'.
*  If ( A',B')' has orthonormal columns, then the GSVD of A and B is
*  also equal to the CS decomposition of A and B. Furthermore, the GSVD
*  can be used to derive the solution of the eigenvalue problem:
*                       A'*A x = lambda* B'*B x.
*  In some literature, the GSVD of A and B is presented in the form
*                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
*  where U and V are orthogonal and X is nonsingular, D1 and D2 are
*  ``diagonal''.  The former GSVD form can be converted to the latter
*  form by taking the nonsingular matrix X as
*                       X = Q*( I   0    )
*                             ( 0 inv(R) ).
"

	<cdecl: SDWORD 'dggsvd_'  char * char * char * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xhesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	^self 
		xsysvWithuplo: uplo
		n: n
		nrhs: nrhs
		a: a
		lda: lda
		ipiv: ipiv
		b: b
		ldb: ldb
		work: work
		lwork: lwork
		info: info
		length: lengthOfuplo!

xhetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: luplo 
	^self 
		xsytrfWithuplo: uplo
		n: n
		a: a
		lda: lda
		ipiv: ipiv
		work: work
		lwork: lwork
		info: info
		length: luplo!

xhetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	^self 
		xsytriWithuplo: uplo
		n: n
		a: a
		lda: lda
		ipiv: ipiv
		work: work
		info: info
		length: lengthOfuplo!

xhetrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	^self 
		xsytrsWithuplo: uplo
		n: n
		nrhs: nrhs
		a: a
		lda: lda
		ipiv: ipiv
		b: b
		ldb: ldb
		info: info
		length: lengthOfuplo!

xhgeqzWithjob: job compq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta q: q ldq: ldq z: z ldz: ldz work: work lwork: lwork info: info length: lengthArgjob length: lengthArgcompq length: lengthArgcompz 
	"
*  Purpose
*  =======
*  DHGEQZ implements a single-/double-shift version of the QZ method for
*  finding the generalized eigenvalues
*  w(j)=(ALPHAR(j) + i*ALPHAI(j))/BETAR(j)   of the equation
*       det( A - w(i) B ) = 0
*  In addition, the pair A,B may be reduced to generalized Schur form:
*  B is upper triangular, and A is block upper triangular, where the
*  diagonal blocks are either 1-by-1 or 2-by-2, the 2-by-2 blocks having
*  complex generalized eigenvalues (see the description of the argument
*  JOB.)
*  If JOB='S', then the pair (A,B) is simultaneously reduced to Schur
*  form by applying one orthogonal tranformation (usually called Q) on
*  the left and another (usually called Z) on the right.  The 2-by-2
*  upper-triangular diagonal blocks of B corresponding to 2-by-2 blocks
*  of A will be reduced to positive diagonal matrices.  (I.e.,
*  if A(j+1,j) is non-zero, then B(j+1,j)=B(j,j+1)=0 and B(j,j) and
*  B(j+1,j+1) will be positive.)
*  If JOB='E', then at each iteration, the same transformations
*  are computed, but they are only applied to those parts of A and B
*  which are needed to compute ALPHAR, ALPHAI, and BETAR.
*  If JOB='S' and COMPQ and COMPZ are 'V' or 'I', then the orthogonal
*  transformations used to reduce (A,B) are accumulated into the arrays
*  Q and Z s.t.:
*       Q(in) A(in) Z(in)* = Q(out) A(out) Z(out)*
*       Q(in) B(in) Z(in)* = Q(out) B(out) Z(out)*
*  Ref: C.B. Moler & G.W. Stewart, 'An Algorithm for Generalized Matrix
*       Eigenvalue Problems', SIAM J. Numer. Anal., 10(1973),
*       pp. 241--256.
"

	<cdecl: SDWORD 'dhgeqz_' char * char * char * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD>
	^self invalidCall!

xhseinWithside: side eigsrc: eigsrc initv: initv select: select n: n h: h ldh: ldh wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr mm: mm m: m work: work ifaill: ifaill ifailr: ifailr info: info length: lengthArgside length: lengthArgeigsrc length: lengthArginitv 
	"
*  Purpose
*  =======
*  DHSEIN uses inverse iteration to find specified right and/or left
*  eigenvectors of a real upper Hessenberg matrix H.
*  The right eigenvector x and the left eigenvector y of the matrix H
*  corresponding to an eigenvalue w are defined by:
*               H * x = w * x,     y**h * H = w * y**h
*  where y**h denotes the conjugate transpose of the vector y.
"

	<cdecl: SDWORD 'dhsein_' char * char * char * SDWORD * SDWORD * double * SDWORD * double * double * double * SDWORD * double * SDWORD * SDWORD * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD>
	^self invalidCall!

xhseqrWithjob: job compz: compz n: n ilo: ilo ihi: ihi h: h ldh: ldh wr: wr wi: wi z: z ldz: ldz work: work lwork: lwork info: info length: lengthArgjob length: lengthArgcompz 
	"
*  Purpose
*  =======
*  DHSEQR computes the eigenvalues of a real upper Hessenberg matrix H
*  and, optionally, the matrices T and Z from the Schur decomposition
*  H = Z T Z**T, where T is an upper quasi-triangular matrix (the Schur
*  form), and Z is the orthogonal matrix of Schur vectors.
*  Optionally Z may be postmultiplied into an input orthogonal matrix Q,
*  so that this routine can give the Schur factorization of a matrix A
*  which has been reduced to the Hessenberg form H by the orthogonal
*  matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
"

	<cdecl: SDWORD 'dhseqr_' char * char * SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD>
	^self invalidCall!

xlabadWithsmall: small large: large 
	"
*  Purpose
*  =======
*  DLABAD takes as input the values computed by DLAMCH for underflow and
*  overflow, and returns the square root of each of these values if the
*  log of LARGE is sufficiently large.  This subroutine is intended to
*  identify machines with a large exponent range, such as the Crays, and
*  redefine the underflow and overflow limits to be the square roots of
*  the values computed by DLAMCH.  This subroutine is needed because
*  DLAMCH does not compensate for poor arithmetic in the upper half of
*  the exponent range, as is found on a Cray.
"

	<cdecl: SDWORD 'dlabad_' double * double *>
	^self invalidCall!

xlaconWithn: n v: v x: x isgn: isgn est: est kase: kase 
	"
*  Purpose
*  =======
*  DLACON estimates the 1-norm of a square, real matrix A.
*  Reverse communication is used for evaluating matrix-vector products.
"

	<cdecl: SDWORD 'dlacon_' SDWORD * double * double * SDWORD * double * SDWORD *>
	^self invalidCall!

xlacpyWithuplo: uplo m: m n: n a: a lda: lda b: b ldb: ldb length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DLACPY copies all or part of a two-dimensional matrix A to another
*  matrix B.
"

	<cdecl: SDWORD 'dlacpy_'  char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD >
	^self invalidCall!

xladivWitha: a b: b c: c d: d p: p q: q 
	"
*  Purpose
*  =======
*  DLADIV performs complex division in  real arithmetic
*                        a + i*b
*             p + i*q = ---------
*                        c + i*d
*  The algorithm is due to Robert L. Smith and can be found
*  in D. Knuth, The art of Computer Programming, Vol.2, p.195
"

	<cdecl: SDWORD 'dladiv_' double * double * double * double * double * double *>
	^self invalidCall!

xlamchWithcmach: cmach length: lengthOfcmach 
	"
*  Purpose
*  =======
*  DLAMCH determines double precision machine parameters.
"

	<cdecl: double 'dlamch_'  char * SDWORD >
	^self invalidCall!

xlangeWithnorm: norm m: m n: n a: a lda: lda work: work length: lengthOfnorm 
	"
*  Purpose
*  =======
*  DLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real matrix A.
"

	<cdecl: double 'dlange_'  char * SDWORD * SDWORD * double * SDWORD * double * SDWORD >
	^self invalidCall!

xlangtWithnorm: norm n: n dl: dl d: d du: du length: lengthArgnorm 
	"
*  Purpose
*  =======
*  DLANGT  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real tridiagonal matrix A.
"

	<cdecl: double 'dlangt_' char * SDWORD * double * double * double * SDWORD>
	^self invalidCall!

xlanhsWithnorm: norm n: n a: a lda: lda work: work length: lengthArgnorm 
	"
*  Purpose
*  =======
*  DLANHS  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  Hessenberg matrix A.
"

	<cdecl: double 'dlanhs_' char * SDWORD * double * SDWORD * double * SDWORD>
	^self invalidCall!

xlanspWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DLANSP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real symmetric matrix A,  supplied in packed form.
"

	<cdecl: double 'dlansp_'  char * char * SDWORD * double * double * SDWORD SDWORD >
	^self invalidCall!

xlansyWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DLANSY  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real symmetric matrix A.
"

	<cdecl: double 'dlansy_'  char * char * SDWORD * double * SDWORD * double * SDWORD SDWORD >
	^self invalidCall!

xlantpWithnorm: norm uplo: uplo diag: diag n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DLANTP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  triangular matrix A, supplied in packed form.
"

	<cdecl: double 'dlantp_'  char * char * char * SDWORD * double * double * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlantrWithnorm: norm uplo: uplo diag: diag m: m n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DLANTR  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  trapezoidal or triangular matrix A.
"

	<cdecl: double 'dlantr_'  char * char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlapllWithn: n x: x incx: incx y: y incy: incy ssmin: ssmin 
	"
*  Purpose
*  =======
*  Given two column vectors X and Y, let
*                       A = ( X Y ).
*  The subroutine first computes the QR factorization of A = Q*R,
*  and then computes the SVD of the 2-by-2 upper triangular matrix R.
*  The smaller singular value of R is returned in SSMIN, which is used
*  as the measurement of the linear dependency of the vectors X and Y.
"

	<cdecl: SDWORD 'dlapll_' SDWORD * double * SDWORD * double * SDWORD * double *>
	^self invalidCall!

xlapy2Withx: x y: y 
	"
*  Purpose
*  =======
*  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
*  overflow.
"

	<cdecl: double 'dlapy2_' double * double *>
	^self invalidCall!

xlapy3Withx: x y: y z: z 
	"
*  Purpose
*  =======
*  DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
*  unnecessary overflow.
"

	<cdecl: double 'dlapy3_' double * double * double *>
	^self invalidCall!

xlarnvWithidist: idist iseed: iseed n: n x: x 
	"
*  Purpose
*  =======
*  DLARNV returns a vector of n random real numbers from a uniform or
*  normal distribution.
"

	<cdecl: SDWORD 'dlarnv_' SDWORD * SDWORD * SDWORD * double *>
	^self invalidCall!

xlaruvWithiseed: iseed n: n x: x 
	"
*  Purpose
*  =======
*  DLARUV returns a vector of n random real numbers from a uniform (0,1)
*  distribution (n <= 128).
*  This is an auxiliary routine called by DLARNV and ZLARNV.
"

	<cdecl: SDWORD 'dlaruv_' SDWORD * SDWORD * double *>
	^self invalidCall!

xlasetWithuplo: uplo m: m n: n alpha: alpha beta: beta a: a lda: lda length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
"

	<cdecl: SDWORD 'dlaset_'  char * SDWORD * SDWORD * double * double * double * SDWORD * SDWORD >
	^self invalidCall!

xorg2lWithm: m n: n k: k a: a lda: lda tau: tau work: work info: info 
	"
*  Purpose
*  =======
*  DORG2L generates an m by n real matrix Q with orthonormal columns,
*  which is defined as the last n columns of a product of k elementary
*  reflectors of order m
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by DGEQLF.
"

	<cdecl: SDWORD 'dorg2l_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD *>
	^self invalidCall!

xorg2rWithm: m n: n k: k a: a lda: lda tau: tau work: work info: info 
	"
*  Purpose
*  =======
*  DORG2R generates an m by n real matrix Q with orthonormal columns,
*  which is defined as the first n columns of a product of k elementary
*  reflectors of order m
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by DGEQRF.
"

	<cdecl: SDWORD 'dorg2r_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD *>
	^self invalidCall!

xorghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'dorghr_'  SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * >
	^self invalidCall!

xorgl2Withm: m n: n k: k a: a lda: lda tau: tau work: work info: info 
	"
*  Purpose
*  =======
*  DORGL2 generates an m by n real matrix Q with orthonormal rows,
*  which is defined as the first m rows of a product of k elementary
*  reflectors of order n
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by DGELQF.
"

	<cdecl: SDWORD 'dorgl2_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD *>
	^self invalidCall!

xorglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DORGLQ generates an M-by-N real matrix Q with orthonormal rows,
*  which is defined as the first M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by DGELQF.
"

	<cdecl: SDWORD 'dorglq_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xorgqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DORGQL generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the last N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by DGEQLF.
"

	<cdecl: SDWORD 'dorgql_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xorgqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DORGQR generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by DGEQRF.
"

	<cdecl: SDWORD 'dorgqr_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xorgr2Withm: m n: n k: k a: a lda: lda tau: tau work: work info: info 
	"
*  Purpose
*  =======
*  DORGR2 generates an m by n real matrix Q with orthonormal rows,
*  which is defined as the last m rows of a product of k elementary
*  reflectors of order n
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by DGERQF.
"

	<cdecl: SDWORD 'dorgr2_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD *>
	^self invalidCall!

xorgrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DORGRQ generates an M-by-N real matrix Q with orthonormal rows,
*  which is defined as the last M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by DGERQF.
"

	<cdecl: SDWORD 'dorgrq_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD *>
	^self invalidCall!

xorgtrWithuplo: uplo n: n a: a lda: lda tau: tau work: work lwork: lwork info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DORGTR generates a real orthogonal matrix Q which is defined as the
*  product of n-1 elementary reflectors of order N, as returned by
*  DSYTRD:
*  if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
*  if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
"

	<cdecl: SDWORD 'dorgtr_' char * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xpoconWithuplo: uplo n: n a: a lda: lda anorm: anorm rcond: rcond work: work iwork: iwork info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DPOCON estimates the reciprocal of the condition number (in the
*  1-norm) of a real symmetric positive definite matrix using the
*  Cholesky factorization A = U**T*U or A = L*L**T computed by DPOTRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'dpocon_' char * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xpoequWithn: n a: a lda: lda s: s scond: scond amax: amax info: info 
	"
*  Purpose
*  =======
*  DPOEQU computes row and column scalings intended to equilibrate a
*  symmetric positive definite matrix A and reduce its condition number
*  (with respect to the two-norm).  S contains the scale factors,
*  S(i) = 1/sqrt(A(i,i)), chosen so that the scaled matrix B with
*  elements B(i,j) = S(i)*A(i,j)*S(j) has ones on the diagonal.  This
*  choice of S puts the condition number of B within a factor N of the
*  smallest possible condition number over all possible diagonal
*  scalings.
"

	<cdecl: SDWORD 'dpoequ_' SDWORD * double * SDWORD * double * double * double * SDWORD *>
	^self invalidCall!

xporfsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda af: af ldaf: ldaf b: b ldb: ldb x: x ldx: ldx ferr: ferr berr: berr work: work iwork: iwork info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DPORFS improves the computed solution to a system of linear
*  equations when the coefficient matrix is symmetric positive definite,
*  and provides error bounds and backward error estimates for the
*  solution.
"

	<cdecl: SDWORD 'dporfs_' char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xposvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DPOSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite matrix and X and B
*  are N-by-NRHS matrices.
*  The Cholesky decomposition is used to factor A as
*     A = U**T* U,  if UPLO = 'U', or
*     A = L * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is a lower triangular
*  matrix.  The factored form of A is then used to solve the system of
*  equations A * X = B.
"

	<cdecl: SDWORD 'dposv_' char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xposvxWithfact: fact uplo: uplo n: n nrhs: nrhs a: a lda: lda af: af ldaf: ldaf equed: equed s: s b: b ldb: ldb x: x ldx: ldx rcond: rcond ferr: ferr berr: berr work: work iwork: iwork info: info length: lengthArgfact length: lengthArguplo length: lengthArgequed 
	"
*  Purpose
*  =======
*  DPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
*  compute the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite matrix and X and B
*  are N-by-NRHS matrices.
*  Error bounds on the solution and a condition estimate are also
*  provided.
"

	<cdecl: SDWORD 'dposvx_' char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * char * double * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD>
	^self invalidCall!

xpotf2Withuplo: uplo n: n a: a lda: lda info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DPOTF2 computes the Cholesky factorization of a real symmetric
*  positive definite matrix A.
*  The factorization has the form
*     A = U' * U ,  if UPLO = 'U', or
*     A = L  * L',  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*  This is the unblocked version of the algorithm, calling Level 2 BLAS.
"

	<cdecl: SDWORD 'dpotf2_' char * SDWORD * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xpotrfWithuplo: uplo n: n a: a lda: lda info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DPOTRF computes the Cholesky factorization of a real symmetric
*  positive definite matrix A.
*  The factorization has the form
*     A = U**T * U,  if UPLO = 'U', or
*     A = L  * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*  This is the block version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'dpotrf_'  char * SDWORD * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xpotriWithuplo: uplo n: n a: a lda: lda info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DPOTRI computes the inverse of a real symmetric positive definite
*  matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
*  computed by DPOTRF.
"

	<cdecl: SDWORD 'dpotri_' char * SDWORD * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xpotrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DPOTRS solves a system of linear equations A*X = B with a symmetric
*  positive definite matrix A using the Cholesky factorization
*  A = U**T*U or A = L*L**T computed by DPOTRF.
"

	<cdecl: SDWORD 'dpotrs_' char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xsyconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work iwork: iwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYCON estimates the reciprocal of the condition number (in the
*  1-norm) of a real symmetric matrix A using the factorization
*  A = U*D*U**T or A = L*D*L**T computed by DSYTRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'dsycon_'  char * SDWORD * double * SDWORD * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsyevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYEVD computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric matrix A. If eigenvectors are desired, it uses a
*  divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*  Because of large use of BLAS of level 3, DSYEVD needs N**2 more
*  workspace than DSYEVX.
"

	<cdecl: SDWORD 'dsyevd_'  char * char * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsyevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYEVR computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix T.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*  Whenever possible, DSYEVR calls DSTEGR to compute the
*  eigenspectrum using Relatively Robust Representations.  DSTEGR
*  computes eigenvalues by the dqds algorithm, while orthogonal
*  eigenvectors are computed from various 'good' L D L^T representations
*  (also known as Relatively Robust Representations). Gram-Schmidt
*  orthogonalization is avoided as far as possible. More specifically,
*  the various steps of the algorithm are as follows. For the i-th
*  unreduced block of T,
*     (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
*          is a relatively robust representation,
*     (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
*         relative accuracy by the dqds algorithm,
*     (c) If there is a cluster of close eigenvalues, 'choose' sigma_i
*         close to the cluster, and go to step (a),
*     (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
*         compute the corresponding eigenvector by forming a
*         rank-revealing twisted factorization.
*  The desired accuracy of the output can be specified by the input
*  parameter ABSTOL.
*  For more details, see 'A new O(n^2) algorithm for the symmetric
*  tridiagonal eigenvalue/eigenvector problem', by Inderjit Dhillon,
*  Computer Science Division Technical Report No. UCB//CSD-97-971,
*  UC Berkeley, May 1997.
*  Note 1 : DSYEVR calls DSTEGR when the full spectrum is requested
*  on machines which conform to the ieee-754 floating point standard.
*  DSYEVR calls DSTEBZ and SSTEIN on non-ieee machines and
*  when partial spectrum requests are made.
*  Normal execution of DSTEGR may create NaNs and infinities and
*  hence may abort due to a floating point exception in environments
*  which do not handle NaNs and infinities in the ieee standard default
*  manner.
"

	<cdecl: SDWORD 'dsyevr_'  char * char * char * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xsyevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYEV computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric matrix A.
"

	<cdecl: SDWORD 'dsyev_'  char * char * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsyevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of indices
*  for the desired eigenvalues.
"

	<cdecl: SDWORD 'dsyevx_'  char * char * char * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xsygvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYGVD computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be symmetric and B is also positive definite.
*  If eigenvectors are desired, it uses a divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'dsygvd_'  SDWORD * char * char * SDWORD * double * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsygvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYGV computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
*  Here A and B are assumed to be symmetric and B is also
*  positive definite.
"

	<cdecl: SDWORD 'dsygv_'  SDWORD * char * char * SDWORD * double * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsygvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYGVX computes selected eigenvalues, and optionally, eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
*  and B are assumed to be symmetric and B is also positive definite.
*  Eigenvalues and eigenvectors can be selected by specifying either a
*  range of values or a range of indices for the desired eigenvalues.
"

	<cdecl: SDWORD 'dsygvx_'  SDWORD * char * char * char * SDWORD * double * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * double * SDWORD * double * double * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xsyrfsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda af: af ldaf: ldaf ipiv: ipiv b: b ldb: ldb x: x ldx: ldx ferr: ferr berr: berr work: work iwork: iwork info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DSYRFS improves the computed solution to a system of linear
*  equations when the coefficient matrix is symmetric indefinite, and
*  provides error bounds and backward error estimates for the solution.
"

	<cdecl: SDWORD 'dsyrfs_' char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xsysvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
*  matrices.
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**T,  if UPLO = 'U', or
*     A = L * D * L**T,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with
*  1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
*  used to solve the system of equations A * X = B.
"

	<cdecl: SDWORD 'dsysv_'  char * SDWORD * SDWORD * double * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsysvxWithfact: fact uplo: uplo n: n nrhs: nrhs a: a lda: lda af: af ldaf: ldaf ipiv: ipiv b: b ldb: ldb x: x ldx: ldx rcond: rcond ferr: ferr berr: berr work: work lwork: lwork iwork: iwork info: info length: lengthArgfact length: lengthArguplo 
	"
*  Purpose
*  =======
*  DSYSVX uses the diagonal pivoting factorization to compute the
*  solution to a real system of linear equations A * X = B,
*  where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
*  matrices.
*  Error bounds on the solution and a condition estimate are also
*  provided.
"

	<cdecl: SDWORD 'dsysvx_' char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD>
	^self invalidCall!

xsytrdWithuplo: uplo n: n a: a lda: lda d: d e: e tau: tau work: work lwork: lwork info: info length: lengthArguplo 
	"
*  Purpose
*  =======
*  DSYTRD reduces a real symmetric matrix A to real symmetric
*  tridiagonal form T by an orthogonal similarity transformation:
*  Q**T * A * Q = T.
"

	<cdecl: SDWORD 'dsytrd_' char * SDWORD * double * SDWORD * double * double * double * double * SDWORD * SDWORD * SDWORD>
	^self invalidCall!

xsytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYTRF computes the factorization of a real symmetric matrix A using
*  the Bunch-Kaufman diagonal pivoting method.  The form of the
*  factorization is
*     A = U*D*U**T  or  A = L*D*L**T
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with
*  1-by-1 and 2-by-2 diagonal blocks.
*  This is the blocked version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'dsytrf_'  char * SDWORD * double * SDWORD * SDWORD * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYTRI computes the inverse of a real symmetric indefinite matrix
*  A using the factorization A = U*D*U**T or A = L*D*L**T computed by
*  DSYTRF.
"

	<cdecl: SDWORD 'dsytri_'  char * SDWORD * double * SDWORD * SDWORD * double * SDWORD * SDWORD >
	^self invalidCall!

xsytrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  DSYTRS solves a system of linear equations A*X = B with a real
*  symmetric matrix A using the factorization A = U*D*U**T or
*  A = L*D*L**T computed by DSYTRF.
"

	<cdecl: SDWORD 'dsytrs_'  char * SDWORD * SDWORD * double * SDWORD * SDWORD * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtgevcWithside: side howmny: howmny select: select n: n a: a lda: lda b: b ldb: ldb vl: vl ldvl: ldvl vr: vr ldvr: ldvr mm: mm m: m work: work info: info length: lengthArgside length: lengthArghowmny 
	"
*  Purpose
*  =======
*  DTGEVC computes some or all of the right and/or left generalized
*  eigenvectors of a pair of real upper triangular matrices (A,B).
*  The right generalized eigenvector x and the left generalized
*  eigenvector y of (A,B) corresponding to a generalized eigenvalue
*  w are defined by:
*          (A - wB) * x = 0  and  y**H * (A - wB) = 0
*  where y**H denotes the conjugate tranpose of y.
*  If an eigenvalue w is determined by zero diagonal elements of both A
*  and B, a unit vector is returned as the corresponding eigenvector.
*  If all eigenvectors are requested, the routine may either return
*  the matrices X and/or Y of right or left eigenvectors of (A,B), or
*  the products Z*X and/or Q*Y, where Z and Q are input orthogonal
*  matrices.  If (A,B) was obtained from the generalized real-Schur
*  factorization of an original pair of matrices
*     (A0,B0) = (Q*A*Z**H,Q*B*Z**H),
*  then Z*X and Q*Y are the matrices of right or left eigenvectors of
*  A.
*  A must be block upper triangular, with 1-by-1 and 2-by-2 diagonal
*  blocks.  Corresponding to each 2-by-2 diagonal block is a complex
*  conjugate pair of eigenvalues and eigenvectors; only one
*  eigenvector of the pair is computed, namely the one corresponding
*  to the eigenvalue with positive imaginary part.
"

	<cdecl: SDWORD 'dtgevc_' char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD * double * SDWORD * SDWORD SDWORD>
	^self invalidCall!

xtgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  DTGEXC reorders the generalized real Schur decomposition of a real
*  matrix pair (A,B) using an orthogonal equivalence transformation
*                 (A, B) = Q * (A, B) * Z',
*  so that the diagonal block of (A, B) with row index IFST is moved
*  to row ILST.
*  (A, B) must be in generalized real Schur canonical form (as returned
*  by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
*  diagonal blocks. B is upper triangular.
*  Optionally, the matrices Q and Z of generalized Schur vectors are
*  updated.
*         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'
*         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'
"

	<cdecl: SDWORD 'dtgexc_' SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD * double * SDWORD * SDWORD *>
	^self invalidCall!

xtgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork info: info 
	"
*  Purpose
*  =======
*  DTGSEN reorders the generalized real Schur decomposition of a real
*  matrix pair (A, B) (in terms of an orthonormal equivalence trans-
*  formation Q' * (A, B) * Z), so that a selected cluster of eigenvalues
*  appears in the leading diagonal blocks of the upper quasi-triangular
*  matrix A and the upper triangular B. The leading columns of Q and
*  Z form orthonormal bases of the corresponding left and right eigen-
*  spaces (deflating subspaces). (A, B) must be in generalized real
*  Schur canonical form (as returned by DGGES), i.e. A is block upper
*  triangular with 1-by-1 and 2-by-2 diagonal blocks. B is upper
*  triangular.
*  DTGSEN also computes the generalized eigenvalues
*              w(j) = (ALPHAR(j) + i*ALPHAI(j))/BETA(j)
*  of the reordered matrix pair (A, B).
*  Optionally, DTGSEN computes the estimates of reciprocal condition
*  numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
*  (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
*  between the matrix pairs (A11, B11) and (A22,B22) that correspond to
*  the selected cluster and the eigenvalues outside the cluster, resp.,
*  and norms of 'projections' onto left and right eigenspaces w.r.t.
*  the selected cluster in the (1,1)-block.
"

	<cdecl: SDWORD 'dtgsen_' SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * double * double * SDWORD * double * SDWORD * SDWORD * double * double * SDWORD * SDWORD * SDWORD * SDWORD *>
	^self invalidCall!

xtgsylWithtrans: trans ijob: ijob m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc d: d ldd: ldd e: e lde: lde f: f ldf: ldf dif: dif scale: scale work: work lwork: lwork iwork: iwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  DTGSYL solves the generalized Sylvester equation:
*              A * R - L * B = scale * C                 (1)
*              D * R - L * E = scale * F
*  where R and L are unknown m-by-n matrices, (A, D), (B, E) and
*  (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
*  respectively, with real entries. (A, D) and (B, E) must be in
*  generalized (real) Schur canonical form, i.e. A, B are upper quasi
*  triangular and D, E are upper triangular.
*  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
*  scaling factor chosen to avoid overflow.
*  In matrix notation (1) is equivalent to solve  Zx = scale b, where
*  Z is defined as
*             Z = [ kron(In, A)  -kron(B', Im) ]         (2)
*                 [ kron(In, D)  -kron(E', Im) ].
*  Here Ik is the identity matrix of size k and X' is the transpose of
*  X. kron(X, Y) is the Kronecker product between the matrices X and Y.
*  If TRANS = 'T', DTGSYL solves the transposed system Z'*y = scale*b,
*  which is equivalent to solve for R and L in
*              A' * R  + D' * L   = scale *  C           (3)
*              R  * B' + L  * E'  = scale * (-F)
*  This case (TRANS = 'T') is used to compute an one-norm-based estimate
*  of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
*  and (B,E), using DLACON.
*  If IJOB >= 1, DTGSYL computes a Frobenius norm-based estimate
*  of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
*  reciprocal of the smallest singular value of Z. See [1-2] for more
*  information.
*  This is a level 3 BLAS algorithm.
"

	<cdecl: SDWORD 'dtgsyl_'  char * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtpconWithnorm: norm uplo: uplo diag: diag n: n ap: ap rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DTPCON estimates the reciprocal of the condition number of a packed
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'dtpcon_'  char * char * char * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtptriWithuplo: uplo diag: diag n: n ap: ap info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DTPTRI computes the inverse of a real upper or lower triangular
*  matrix A stored in packed format.
"

	<cdecl: SDWORD 'dtptri_'  char * char * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtptrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs ap: ap b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DTPTRS solves a triangular system of the form
*     A * X = B  or  A**T * X = B,
*  where A is a triangular matrix of order N stored in packed format,
*  and B is an N-by-NRHS matrix.  A check is made to verify that A is
*  nonsingular.
"

	<cdecl: SDWORD 'dtptrs_'  char * char * char * SDWORD * SDWORD * double * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DTRCON estimates the reciprocal of the condition number of a
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'dtrcon_'  char * char * char * SDWORD * double * SDWORD * double * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrevcWithside: side howmny: howmny select: select n: n t: t ldt: ldt vl: vl ldvl: ldvl vr: vr ldvr: ldvr mm: mm m: m work: work info: info length: lengthArgside length: lengthArghowmny 
	"
*  Purpose
*  =======
*  DTREVC computes some or all of the right and/or left eigenvectors of
*  a real upper quasi-triangular matrix T.
*  The right eigenvector x and the left eigenvector y of T corresponding
*  to an eigenvalue w are defined by:
*               T*x = w*x,     y'*T = w*y'
*  where y' denotes the conjugate transpose of the vector y.
*  If all eigenvectors are requested, the routine may either return the
*  matrices X and/or Y of right or left eigenvectors of T, or the
*  products Q*X and/or Q*Y, where Q is an input orthogonal
*  matrix. If T was obtained from the real-Schur factorization of an
*  original matrix A = Q*T*Q', then Q*X and Q*Y are the matrices of
*  right or left eigenvectors of A.
*  T must be in Schur canonical form (as returned by DHSEQR), that is,
*  block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
*  2-by-2 diagonal block has its diagonal elements equal and its
*  off-diagonal elements of opposite sign.  Corresponding to each 2-by-2
*  diagonal block is a complex conjugate pair of eigenvalues and
*  eigenvectors; only one eigenvector of the pair is computed, namely
*  the one corresponding to the eigenvalue with positive imaginary part.
"

	<cdecl: SDWORD 'dtrevc_' char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD * double * SDWORD * SDWORD SDWORD>
	^self invalidCall!

xtrexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst work: work info: info length: lengthOfcompq 
	"
*  Purpose
*  =======
*  DTREXC reorders the real Schur factorization of a real matrix
*  A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
*  moved to row ILST.
*  The real Schur form T is reordered by an orthogonal similarity
*  transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
*  is updated by postmultiplying it with Z.
*  T must be in Schur canonical form (as returned by DHSEQR), that is,
*  block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
*  2-by-2 diagonal block has its diagonal elements equal and its
*  off-diagonal elements of opposite sign.
"

	<cdecl: SDWORD 'dtrexc_'  char * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD * double * SDWORD * SDWORD >
	^self invalidCall!

xtrrfsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb x: x ldx: ldx ferr: ferr berr: berr work: work iwork: iwork info: info length: lengthArguplo length: lengthArgtrans length: lengthArgdiag 
	"
*  Purpose
*  =======
*  DTRRFS provides error bounds and backward error estimates for the
*  solution to a system of linear equations with a triangular
*  coefficient matrix.
*  The solution matrix X must be computed by DTRTRS or some other
*  means before entering this routine.  DTRRFS does not do iterative
*  refinement because doing so cannot improve the backward error.
"

	<cdecl: SDWORD 'dtrrfs_' char * char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD>
	^self invalidCall!

xtrsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq wr: wr wi: wi m: m s: s sep: sep work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjob length: lengthOfcompq 
	"
*  Purpose
*  =======
*  DTRSEN reorders the real Schur factorization of a real matrix
*  A = Q*T*Q**T, so that a selected cluster of eigenvalues appears in
*  the leading diagonal blocks of the upper quasi-triangular matrix T,
*  and the leading columns of Q form an orthonormal basis of the
*  corresponding right invariant subspace.
*  Optionally the routine computes the reciprocal condition numbers of
*  the cluster of eigenvalues and/or the invariant subspace.
*  T must be in Schur canonical form (as returned by DHSEQR), that is,
*  block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
*  2-by-2 diagonal block has its diagonal elemnts equal and its
*  off-diagonal elements of opposite sign.
"

	<cdecl: SDWORD 'dtrsen_'  char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * double * SDWORD * double * double * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrsylWithtrana: trana tranb: tranb isgn: isgn m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc scale: scale info: info length: lengthOftrana length: lengthOftranb 
	"
*  Purpose
*  =======
*  DTRSYL solves the real Sylvester matrix equation:
*     op(A)*X + X*op(B) = scale*C or
*     op(A)*X - X*op(B) = scale*C,
*  where op(A) = A or A**T, and  A and B are both upper quasi-
*  triangular. A is M-by-M and B is N-by-N; the right hand side C and
*  the solution X are M-by-N; and scale is an output scale factor, set
*  <= 1 to avoid overflow in X.
*  A and B must be in Schur canonical form (as returned by DHSEQR), that
*  is, block upper triangular with 1-by-1 and 2-by-2 diagonal blocks;
*  each 2-by-2 diagonal block has its diagonal elements equal and its
*  off-diagonal elements of opposite sign.
"

	<cdecl: SDWORD 'dtrsyl_'  char * char * SDWORD * SDWORD * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtriWithuplo: uplo diag: diag n: n a: a lda: lda info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DTRTRI computes the inverse of a real upper or lower triangular
*  matrix A.
*  This is the Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'dtrtri_'  char * char * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  DTRTRS solves a triangular system of the form
*     A * X = B  or  A**T * X = B,
*  where A is a triangular matrix of order N, and B is an N-by-NRHS
*  matrix.  A check is made to verify that A is nonsingular.
"

	<cdecl: SDWORD 'dtrtrs_'  char * char * char * SDWORD * SDWORD * double * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall! !
!LapackDLibrary categoriesFor: #cComplexPointerOn:!public! !
!LapackDLibrary categoriesFor: #cElementPointerOn:!public! !
!LapackDLibrary categoriesFor: #cRealPointerOn:!public! !
!LapackDLibrary categoriesFor: #isComplex!public! !
!LapackDLibrary categoriesFor: #isDoublePrecision!public! !
!LapackDLibrary categoriesFor: #schurSelectFunction!public! !
!LapackDLibrary categoriesFor: #xgebakWithjob:side:n:ilo:ihi:scale:m:v:ldv:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xgebalWithjob:n:a:lda:ilo:ihi:scale:info:length:!public! !
!LapackDLibrary categoriesFor: #xgeconWithnorm:n:a:lda:anorm:rcond:work:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xgeequWithm:n:a:lda:r:c:rowcnd:colcnd:amax:info:!public! !
!LapackDLibrary categoriesFor: #xgeesWithjobvs:sort:select:n:a:lda:sdim:wr:wi:vs:ldvs:work:lwork:bwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xgeesxWithjobvs:sort:select:sense:n:a:lda:sdim:wr:wi:vs:ldvs:rconde:rcondv:work:lwork:iwork:liwork:bwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xgeevWithjobvl:jobvr:n:a:lda:wr:wi:vl:ldvl:vr:ldvr:work:lwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xgeevxWithbalanc:jobvl:jobvr:sense:n:a:lda:wr:wi:vl:ldvl:vr:ldvr:scale:abnrm:rconde:rcondv:work:lwork:iwork:info:length:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xgehrdWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgelqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:iwork:info:!public! !
!LapackDLibrary categoriesFor: #xgelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgelsWithtrans:m:n:nrhs:a:lda:b:ldb:work:lwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xgelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:info:!public! !
!LapackDLibrary categoriesFor: #xgelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgeql2Withm:n:a:lda:tau:work:info:!public! !
!LapackDLibrary categoriesFor: #xgeqlfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgeqp3Withm:n:a:lda:jpvt:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgeqr2Withm:n:a:lda:tau:work:info:!public! !
!LapackDLibrary categoriesFor: #xgeqrfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgerfsWithtrans:n:nrhs:a:lda:af:ldaf:ipiv:b:ldb:x:ldx:ferr:berr:work:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xgerq2Withm:n:a:lda:tau:work:info:!public! !
!LapackDLibrary categoriesFor: #xgerqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xgesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xgesvWithn:nrhs:a:lda:ipiv:b:ldb:info:!public! !
!LapackDLibrary categoriesFor: #xgesvxWithfact:trans:n:nrhs:a:lda:af:ldaf:ipiv:equed:r:c:b:ldb:x:ldx:rcond:ferr:berr:work:iwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xgetrfWithm:n:a:lda:ipiv:info:!public! !
!LapackDLibrary categoriesFor: #xgetriWithn:a:lda:ipiv:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgetrsWithtrans:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackDLibrary categoriesFor: #xggbakWithjob:side:n:ilo:ihi:lscale:rscale:m:v:ldv:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xggbalWithjob:n:a:lda:b:ldb:ilo:ihi:lscale:rscale:work:info:length:!public! !
!LapackDLibrary categoriesFor: #xggesWithjobvsl:jobvsr:sort:delztg:n:a:lda:b:ldb:sdim:alphar:alphai:beta:vsl:ldvsl:vsr:ldvsr:work:lwork:bwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xggesxWithjobvsl:jobvsr:sort:delztg:sense:n:a:lda:b:ldb:sdim:alphar:alphai:beta:vsl:ldvsl:vsr:ldvsr:rconde:rcondv:work:lwork:iwork:liwork:bwork:info:length:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xggevWithjobvl:jobvr:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:work:lwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:iwork:bwork:info:length:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xggglmWithn:m:p:a:lda:b:ldb:d:x:y:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xgghrdWithcompq:compz:n:ilo:ihi:a:lda:b:ldb:q:ldq:z:ldz:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xgglseWithm:n:p:a:lda:b:ldb:c:d:x:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xggqrfWithn:m:p:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xggrqfWithm:p:n:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:iwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xhesvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xhetrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xhetriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackDLibrary categoriesFor: #xhetrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackDLibrary categoriesFor: #xhgeqzWithjob:compq:compz:n:ilo:ihi:a:lda:b:ldb:alphar:alphai:beta:q:ldq:z:ldz:work:lwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xhseinWithside:eigsrc:initv:select:n:h:ldh:wr:wi:vl:ldvl:vr:ldvr:mm:m:work:ifaill:ifailr:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xhseqrWithjob:compz:n:ilo:ihi:h:ldh:wr:wi:z:ldz:work:lwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xlabadWithsmall:large:!public! !
!LapackDLibrary categoriesFor: #xlaconWithn:v:x:isgn:est:kase:!public! !
!LapackDLibrary categoriesFor: #xlacpyWithuplo:m:n:a:lda:b:ldb:length:!public! !
!LapackDLibrary categoriesFor: #xladivWitha:b:c:d:p:q:!public! !
!LapackDLibrary categoriesFor: #xlamchWithcmach:length:!public! !
!LapackDLibrary categoriesFor: #xlangeWithnorm:m:n:a:lda:work:length:!public! !
!LapackDLibrary categoriesFor: #xlangtWithnorm:n:dl:d:du:length:!public! !
!LapackDLibrary categoriesFor: #xlanhsWithnorm:n:a:lda:work:length:!public! !
!LapackDLibrary categoriesFor: #xlanspWithnorm:uplo:n:ap:work:length:length:!public! !
!LapackDLibrary categoriesFor: #xlansyWithnorm:uplo:n:a:lda:work:length:length:!public! !
!LapackDLibrary categoriesFor: #xlantpWithnorm:uplo:diag:n:ap:work:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xlantrWithnorm:uplo:diag:m:n:a:lda:work:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xlapllWithn:x:incx:y:incy:ssmin:!public! !
!LapackDLibrary categoriesFor: #xlapy2Withx:y:!public! !
!LapackDLibrary categoriesFor: #xlapy3Withx:y:z:!public! !
!LapackDLibrary categoriesFor: #xlarnvWithidist:iseed:n:x:!public! !
!LapackDLibrary categoriesFor: #xlaruvWithiseed:n:x:!public! !
!LapackDLibrary categoriesFor: #xlasetWithuplo:m:n:alpha:beta:a:lda:length:!public! !
!LapackDLibrary categoriesFor: #xorg2lWithm:n:k:a:lda:tau:work:info:!public! !
!LapackDLibrary categoriesFor: #xorg2rWithm:n:k:a:lda:tau:work:info:!public! !
!LapackDLibrary categoriesFor: #xorghrWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xorgl2Withm:n:k:a:lda:tau:work:info:!public! !
!LapackDLibrary categoriesFor: #xorglqWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xorgqlWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xorgqrWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xorgr2Withm:n:k:a:lda:tau:work:info:!public! !
!LapackDLibrary categoriesFor: #xorgrqWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xorgtrWithuplo:n:a:lda:tau:work:lwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xpoconWithuplo:n:a:lda:anorm:rcond:work:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xpoequWithn:a:lda:s:scond:amax:info:!public! !
!LapackDLibrary categoriesFor: #xporfsWithuplo:n:nrhs:a:lda:af:ldaf:b:ldb:x:ldx:ferr:berr:work:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xposvWithuplo:n:nrhs:a:lda:b:ldb:info:length:!public! !
!LapackDLibrary categoriesFor: #xposvxWithfact:uplo:n:nrhs:a:lda:af:ldaf:equed:s:b:ldb:x:ldx:rcond:ferr:berr:work:iwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xpotf2Withuplo:n:a:lda:info:length:!public! !
!LapackDLibrary categoriesFor: #xpotrfWithuplo:n:a:lda:info:length:!public! !
!LapackDLibrary categoriesFor: #xpotriWithuplo:n:a:lda:info:length:!public! !
!LapackDLibrary categoriesFor: #xpotrsWithuplo:n:nrhs:a:lda:b:ldb:info:length:!public! !
!LapackDLibrary categoriesFor: #xsyconWithuplo:n:a:lda:ipiv:anorm:rcond:work:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xsyevdWithjobz:uplo:n:a:lda:w:work:lwork:iwork:liwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xsyevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:iwork:liwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xsyevWithjobz:uplo:n:a:lda:w:work:lwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xsyevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xsygvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:iwork:liwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xsygvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xsygvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xsyrfsWithuplo:n:nrhs:a:lda:af:ldaf:ipiv:b:ldb:x:ldx:ferr:berr:work:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xsysvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xsysvxWithfact:uplo:n:nrhs:a:lda:af:ldaf:ipiv:b:ldb:x:ldx:rcond:ferr:berr:work:lwork:iwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xsytrdWithuplo:n:a:lda:d:e:tau:work:lwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xsytrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xsytriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackDLibrary categoriesFor: #xsytrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackDLibrary categoriesFor: #xtgevcWithside:howmny:select:n:a:lda:b:ldb:vl:ldvl:vr:ldvr:mm:m:work:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xtgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:work:lwork:info:!public! !
!LapackDLibrary categoriesFor: #xtgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alphar:alphai:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:info:!public! !
!LapackDLibrary categoriesFor: #xtgsylWithtrans:ijob:m:n:a:lda:b:ldb:c:ldc:d:ldd:e:lde:f:ldf:dif:scale:work:lwork:iwork:info:length:!public! !
!LapackDLibrary categoriesFor: #xtpconWithnorm:uplo:diag:n:ap:rcond:work:iwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xtptriWithuplo:diag:n:ap:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xtptrsWithuplo:trans:diag:n:nrhs:ap:b:ldb:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xtrconWithnorm:uplo:diag:n:a:lda:rcond:work:iwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xtrevcWithside:howmny:select:n:t:ldt:vl:ldvl:vr:ldvr:mm:m:work:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xtrexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:work:info:length:!public! !
!LapackDLibrary categoriesFor: #xtrrfsWithuplo:trans:diag:n:nrhs:a:lda:b:ldb:x:ldx:ferr:berr:work:iwork:info:length:length:length:!public! !
!LapackDLibrary categoriesFor: #xtrsenWithjob:compq:select:n:t:ldt:q:ldq:wr:wi:m:s:sep:work:lwork:iwork:liwork:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xtrsylWithtrana:tranb:isgn:m:n:a:lda:b:ldb:c:ldc:scale:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xtrtriWithuplo:diag:n:a:lda:info:length:length:!public! !
!LapackDLibrary categoriesFor: #xtrtrsWithuplo:trans:diag:n:nrhs:a:lda:b:ldb:info:length:length:length:!public! !

LapackSLibrary guid: (GUID fromString: '{E0F3C781-6B79-4E8C-AA37-5CF0F31E6315}')!
LapackSLibrary comment: ''!
!LapackSLibrary categoriesForClass!Unclassified! !
!LapackSLibrary methodsFor!

cComplexPointerOn: aComplex 	^self cFloatComplexPointerOn: aComplex!

cElementPointerOn: aDouble 	^self cRealPointerOn: aDouble!

cRealPointerOn: aDouble 	^self cFloatPointerOn: aDouble!

isComplex	^false!

isDoublePrecision	^false!

schurSelectFunction
	"Answer the descriptor for callback function"

	^ExternalDescriptor fromString: 'cdecl: SDWORD FLOAT* FLOAT*' !

xgebakWithjob: job side: side n: n ilo: ilo ihi: ihi scale: scale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  SGEBAK forms the right or left eigenvectors of a real general matrix
*  by backward transformation on the computed eigenvectors of the
*  balanced matrix output by SGEBAL.
"

	<cdecl: SDWORD 'sgebak_'  char * char * SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgebalWithjob: job n: n a: a lda: lda ilo: ilo ihi: ihi scale: scale info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  SGEBAL balances a general real matrix A.  This involves, first,
*  permuting A by a similarity transformation to isolate eigenvalues
*  in the first 1 to ILO-1 and last IHI+1 to N elements on the
*  diagonal; and second, applying a diagonal similarity transformation
*  to rows and columns ILO to IHI to make the rows and columns as
*  close in norm as possible.  Both steps are optional.
*  Balancing may reduce the 1-norm of the matrix, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors.
"

	<cdecl: SDWORD 'sgebal_'  char * SDWORD * float * SDWORD * SDWORD * SDWORD * float * SDWORD * SDWORD >
	^self invalidCall!

xgeconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm 
	"
*  Purpose
*  =======
*  SGECON estimates the reciprocal of the condition number of a general
*  real matrix A, in either the 1-norm or the infinity-norm, using
*  the LU factorization computed by SGETRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'sgecon_'  char * SDWORD * float * SDWORD * float * float * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgeesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs work: work lwork: lwork bwork: bwork info: info length: lengthOfjobvs length: lengthOfsort 
	"
*  Purpose
*  =======
*  SGEES computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues, the real Schur form T, and, optionally, the matrix of
*  Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
*  Optionally, it also orders the eigenvalues on the diagonal of the
*  real Schur form so that selected eigenvalues are at the top left.
*  The leading columns of Z then form an orthonormal basis for the
*  invariant subspace corresponding to the selected eigenvalues.
*  A matrix is in real Schur form if it is upper quasi-triangular with
*  1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in the
*  form
*          [  a  b  ]
*          [  c  a  ]
*  where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
"

	<cdecl: SDWORD 'sgees_'  char * char * SDWORD * SDWORD * float * SDWORD * SDWORD * float * float * float * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgeevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  SGEEV computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
"

	<cdecl: SDWORD 'sgeev_'  char * char * SDWORD * float * SDWORD * float * float * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'sgehrd_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xgelqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGELQF computes an LQ factorization of a real M-by-N matrix A:
*  A = L * Q.
"

	<cdecl: SDWORD 'sgelqf_'  SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xgelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork iwork: iwork info: info 
	"
*  Purpose
*  =======
*  SGELSD computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize 2-norm(| b - A*x |)
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The problem is solved in three steps:
*  (1) Reduce the coefficient matrix A to bidiagonal form with
*      Householder transformations, reducing the original problem
*      into a 'bidiagonal least squares problem' (BLS)
*  (2) Solve the BLS using a divide and conquer approach.
*  (3) Apply back all the Householder tranformations to solve
*      the original least squares problem.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'sgelsd_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * float * SDWORD * float * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xgelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGELSS computes the minimum norm solution to a real linear least
*  squares problem:
*  Minimize 2-norm(| b - A*x |).
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
*  X.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
"

	<cdecl: SDWORD 'sgelss_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * float * SDWORD * float * SDWORD * SDWORD * >
	^self invalidCall!

xgelsWithtrans: trans m: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb work: work lwork: lwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  SGELS solves overdetermined or underdetermined real linear systems
*  involving an M-by-N matrix A, or its transpose, using a QR or LQ
*  factorization of A.  It is assumed that A has full rank.
*  The following options are provided: 
*  1. If TRANS = 'N' and m >= n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A*X ||.
*  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
*     an underdetermined system A * X = B.
*  3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
*     an undetermined system A**T * X = B.
*  4. If TRANS = 'T' and m < n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A**T * X ||.
*  Several right hand side vectors b and solution vectors x can be 
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution 
*  matrix X.
"

	<cdecl: SDWORD 'sgels_'  char * SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work info: info 
	"
*  Purpose
*  =======
*  This routine is deprecated and has been replaced by routine SGELSY.
*  SGELSX computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be 
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
"

	<cdecl: SDWORD 'sgelsx_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * SDWORD * float * SDWORD * float * SDWORD * >
	^self invalidCall!

xgelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGELSY computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
*  This routine is basically identical to the original xGELSX except
*  three differences:
*    o The call to the subroutine xGEQPF has been substituted by the
*      the call to the subroutine xGEQP3. This subroutine is a Blas-3
*      version of the QR factorization with column pivoting.
*    o Matrix B (the right hand side) is updated with Blas-3.
*    o The permutation of matrix B (the right hand side) is faster and
*      more simple.
"

	<cdecl: SDWORD 'sgelsy_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * SDWORD * float * SDWORD * float * SDWORD * SDWORD * >
	^self invalidCall!

xgeqlfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGEQLF computes a QL factorization of a real M-by-N matrix A:
*  A = Q * L.
"

	<cdecl: SDWORD 'sgeqlf_'  SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xgeqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGEQP3 computes a QR factorization with column pivoting of a
*  matrix A:  A*P = Q*R  using Level 3 BLAS.
"

	<cdecl: SDWORD 'sgeqp3_'  SDWORD * SDWORD * float * SDWORD * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xgeqrfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGEQRF computes a QR factorization of a real M-by-N matrix A:
*  A = Q * R.
"

	<cdecl: SDWORD 'sgeqrf_'  SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xgerqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGERQF computes an RQ factorization of a real M-by-N matrix A:
*  A = R * Q.
"

	<cdecl: SDWORD 'sgerqf_'  SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xgesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork iwork: iwork info: info length: lengthOfjobz 
	"
*  Purpose
*  =======
*  SGESDD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and right singular
*  vectors.  If singular vectors are desired, it uses a
*  divide-and-conquer algorithm.
*  The SVD is written
*       A = U * SIGMA * transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns VT = V**T, not V.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'sgesdd_'  char * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork info: info length: lengthOfjobu length: lengthOfjobvt 
	"
*  Purpose
*  =======
*  SGESVD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*       A = U * SIGMA * transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns V**T, not V.
"

	<cdecl: SDWORD 'sgesvd_'  char * char * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgesvWithn: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info 
	"
*  Purpose
*  =======
*  SGESV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*  The LU decomposition with partial pivoting and row interchanges is
*  used to factor A as
*     A = P * L * U,
*  where P is a permutation matrix, L is unit lower triangular, and U is
*  upper triangular.  The factored form of A is then used to solve the
*  system of equations A * X = B.
"

	<cdecl: SDWORD 'sgesv_'  SDWORD * SDWORD * float * SDWORD * SDWORD * float * SDWORD * SDWORD * >
	^self invalidCall!

xgetrfWithm: m n: n a: a lda: lda ipiv: ipiv info: info 
	"
*  Purpose
*  =======
*  SGETRF computes an LU factorization of a general M-by-N matrix A
*  using partial pivoting with row interchanges.
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*  This is the right-looking Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'sgetrf_'  SDWORD * SDWORD * float * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xgetriWithn: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGETRI computes the inverse of a matrix using the LU factorization
*  computed by SGETRF.
*  This method inverts U and then computes inv(A) by solving the system
*  inv(A)*L = inv(U) for inv(A).
"

	<cdecl: SDWORD 'sgetri_'  SDWORD * float * SDWORD * SDWORD * float * SDWORD * SDWORD * >
	^self invalidCall!

xgetrsWithtrans: trans n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  SGETRS solves a system of linear equations
*     A * X = B  or  A' * X = B
*  with a general N-by-N matrix A using the LU factorization computed
*  by SGETRF.
"

	<cdecl: SDWORD 'sgetrs_'  char * SDWORD * SDWORD * float * SDWORD * SDWORD * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xggbakWithjob: job side: side n: n ilo: ilo ihi: ihi lscale: lscale rscale: rscale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  SGGBAK forms the right or left eigenvectors of a real generalized
*  eigenvalue problem A*x = lambda*B*x, by backward transformation on
*  the computed eigenvectors of the balanced pair of matrices output by
*  SGGBAL.
"

	<cdecl: SDWORD 'sggbak_'  char * char * SDWORD * SDWORD * SDWORD * float * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggbalWithjob: job n: n a: a lda: lda b: b ldb: ldb ilo: ilo ihi: ihi lscale: lscale rscale: rscale work: work info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  SGGBAL balances a pair of general real matrices (A,B).  This
*  involves, first, permuting A and B by similarity transformations to
*  isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
*  elements on the diagonal; and second, applying a diagonal similarity
*  transformation to rows and columns ILO to IHI to make the rows
*  and columns as close in norm as possible. Both steps are optional.
*  Balancing may reduce the 1-norm of the matrices, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors in the
*  generalized eigenvalue problem A*x = lambda*B*x.
"

	<cdecl: SDWORD 'sggbal_'  char * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD * float * float * float * SDWORD * SDWORD >
	^self invalidCall!

xggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  SGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
*  the generalized eigenvalues, and optionally, the left and/or right
*  generalized eigenvectors.
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j).
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B .
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'sggev_'  char * char * SDWORD * float * SDWORD * float * SDWORD * float * float * float * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork iwork: iwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  SGGEVX computes for a pair of N-by-N real nonsymmetric matrices (A,B)
*  the generalized eigenvalues, and optionally, the left and/or right
*  generalized eigenvectors.
*  Optionally also, it computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
*  the eigenvalues (RCONDE), and reciprocal condition numbers for the
*  right eigenvectors (RCONDV).
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j) .
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B.
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'sggevx_'  char * char * char * char * SDWORD * float * SDWORD * float * SDWORD * float * float * float * float * SDWORD * float * SDWORD * float * float * float * float * float * float * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xggglmWithn: n m: m p: p a: a lda: lda b: b ldb: ldb d: d x: x y: y work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGGGLM solves a general Gauss-Markov linear model (GLM) problem:
*          minimize || y ||_2   subject to   d = A*x + B*y
*              x
*  where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
*  given N-vector. It is assumed that M <= N <= M+P, and
*             rank(A) = M    and    rank( A B ) = N.
*  Under these assumptions, the constrained equation is always
*  consistent, and there is a unique solution x and a minimal 2-norm
*  solution y, which is obtained using a generalized QR factorization
*  of A and B.
*  In particular, if matrix B is square nonsingular, then the problem
*  GLM is equivalent to the following weighted linear least squares
*  problem
*               minimize || inv(B)*(d-A*x) ||_2
*                   x
*  where inv(B) denotes the inverse of B.
"

	<cdecl: SDWORD 'sggglm_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * float * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xgghrdWithcompq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz info: info length: lengthOfcompq length: lengthOfcompz 
	"
*  Purpose
*  =======
*  SGGHRD reduces a pair of real matrices (A,B) to generalized upper
*  Hessenberg form using orthogonal transformations, where A is a
*  general matrix and B is upper triangular.  The form of the
*  generalized eigenvalue problem is
*     A*x = lambda*B*x,
*  and B is typically made upper triangular by computing its QR
*  factorization and moving the orthogonal matrix Q to the left side
*  of the equation.
*  This subroutine simultaneously reduces A to a Hessenberg matrix H:
*     Q**T*A*Z = H
*  and transforms B to another upper triangular matrix T:
*     Q**T*B*Z = T
*  in order to reduce the problem to its standard form
*     H*y = lambda*T*y
*  where y = Z**T*x.
*  The orthogonal matrices Q and Z are determined as products of Givens
*  rotations.  They may either be formed explicitly, or they may be
*  postmultiplied into input matrices Q1 and Z1, so that
*       Q1 * A * Z1**T = (Q1*Q) * H * (Z1*Z)**T
*       Q1 * B * Z1**T = (Q1*Q) * T * (Z1*Z)**T
*  If Q1 is the orthogonal matrix from the QR factorization of B in the
*  original equation A*x = lambda*B*x, then SGGHRD reduces the original
*  problem to generalized Hessenberg form.
"

	<cdecl: SDWORD 'sgghrd_'  char * char * SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGGLSE solves the linear equality-constrained least squares (LSE)
*  problem:
*          minimize || c - A*x ||_2   subject to   B*x = d
*  where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
*  M-vector, and d is a given P-vector. It is assumed that
*  P <= N <= M+P, and
*           rank(B) = P and  rank( ( A ) ) = N.
*                                ( ( B ) )
*  These conditions ensure that the LSE problem has a unique solution,
*  which is obtained using a GRQ factorization of the matrices B and A.
"

	<cdecl: SDWORD 'sgglse_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * float * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xggqrfWithn: n m: m p: p a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGGQRF computes a generalized QR factorization of an N-by-M matrix A
*  and an N-by-P matrix B:
*              A = Q*R,        B = Q*T*Z,
*  where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
*  matrix, and R and T assume one of the forms:
*  if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
*                  (  0  ) N-M                         N   M-N
*                     M
*  where R11 is upper triangular, and
*  if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
*                   P-N  N                           ( T21 ) P
*                                                       P
*  where T12 or T21 is upper triangular.
*  In particular, if B is square and nonsingular, the GQR factorization
*  of A and B implicitly gives the QR factorization of inv(B)*A:
*               inv(B)*A = Z'*(inv(T)*R)
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  transpose of the matrix Z.
"

	<cdecl: SDWORD 'sggqrf_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xggrqfWithm: m p: p n: n a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SGGRQF computes a generalized RQ factorization of an M-by-N matrix A
*  and a P-by-N matrix B:
*              A = R*Q,        B = Z*T*Q,
*  where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
*  matrix, and R and T assume one of the forms:
*  if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
*                   N-M  M                           ( R21 ) N
*                                                       N
*  where R12 or R21 is upper triangular, and
*  if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
*                  (  0  ) P-N                         P   N-P
*                     N
*  where T11 is upper triangular.
*  In particular, if B is square and nonsingular, the GRQ factorization
*  of A and B implicitly gives the RQ factorization of A*inv(B):
*               A*inv(B) = (R*inv(T))*Z'
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  transpose of the matrix Z.
"

	<cdecl: SDWORD 'sggrqf_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work iwork: iwork info: info length: lengthOfjobu length: lengthOfjobv length: lengthOfjobq 
	"
*  Purpose
*  =======
*  SGGSVD computes the generalized singular value decomposition (GSVD)
*  of an M-by-N real matrix A and P-by-N real matrix B:
*      U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
*  where U, V and Q are orthogonal matrices, and Z' is the transpose
*  of Z.  Let K+L = the effective numerical rank of the matrix (A',B')',
*  then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and
*  D2 are M-by-(K+L) and P-by-(K+L) 'diagonal' matrices and of the
*  following structures, respectively:
*  If M-K-L >= 0,
*                      K  L
*         D1 =     K ( I  0 )
*                  L ( 0  C )
*              M-K-L ( 0  0 )
*                    K  L
*         D2 =   L ( 0  S )
*              P-L ( 0  0 )
*                  N-K-L  K    L
*    ( 0 R ) = K (  0   R11  R12 )
*              L (  0    0   R22 )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
*    S = diag( BETA(K+1),  ... , BETA(K+L) ),
*    C**2 + S**2 = I.
*    R is stored in A(1:K+L,N-K-L+1:N) on exit.
*  If M-K-L < 0,
*                    K M-K K+L-M
*         D1 =   K ( I  0    0   )
*              M-K ( 0  C    0   )
*                      K M-K K+L-M
*         D2 =   M-K ( 0  S    0  )
*              K+L-M ( 0  0    I  )
*                P-L ( 0  0    0  )
*                     N-K-L  K   M-K  K+L-M
*    ( 0 R ) =     K ( 0    R11  R12  R13  )
*                M-K ( 0     0   R22  R23  )
*              K+L-M ( 0     0    0   R33  )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(M) ),
*    S = diag( BETA(K+1),  ... , BETA(M) ),
*    C**2 + S**2 = I.
*    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
*    ( 0  R22 R23 )
*    in B(M-K+1:L,N+M-K-L+1:N) on exit.
*  The routine computes C, S, R, and optionally the orthogonal
*  transformation matrices U, V and Q.
*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
*  A and B implicitly gives the SVD of A*inv(B):
*                       A*inv(B) = U*(D1*inv(D2))*V'.
*  If ( A',B')' has orthonormal columns, then the GSVD of A and B is
*  also equal to the CS decomposition of A and B. Furthermore, the GSVD
*  can be used to derive the solution of the eigenvalue problem:
*                       A'*A x = lambda* B'*B x.
*  In some literature, the GSVD of A and B is presented in the form
*                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
*  where U and V are orthogonal and X is nonsingular, D1 and D2 are
*  ``diagonal''.  The former GSVD form can be converted to the latter
*  form by taking the nonsingular matrix X as
*                       X = Q*( I   0    )
*                             ( 0 inv(R) ).
"

	<cdecl: SDWORD 'sggsvd_'  char * char * char * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * float * float * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xhesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	^self 
		xsysvWithuplo: uplo
		n: n
		nrhs: nrhs
		a: a
		lda: lda
		ipiv: ipiv
		b: b
		ldb: ldb
		work: work
		lwork: lwork
		info: info
		length: lengthOfuplo!

xhetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: luplo 
	^self 
		xsytrfWithuplo: uplo
		n: n
		a: a
		lda: lda
		ipiv: ipiv
		work: work
		lwork: lwork
		info: info
		length: luplo!

xhetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	^self 
		xsytriWithuplo: uplo
		n: n
		a: a
		lda: lda
		ipiv: ipiv
		work: work
		info: info
		length: lengthOfuplo!

xhetrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	^self 
		xsytrsWithuplo: uplo
		n: n
		nrhs: nrhs
		a: a
		lda: lda
		ipiv: ipiv
		b: b
		ldb: ldb
		info: info
		length: lengthOfuplo!

xlacpyWithuplo: uplo m: m n: n a: a lda: lda b: b ldb: ldb length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SLACPY copies all or part of a two-dimensional matrix A to another
*  matrix B.
"

	<cdecl: SDWORD 'slacpy_'  char * SDWORD * SDWORD * float * SDWORD * float * SDWORD * SDWORD >
	^self invalidCall!

xlamchWithcmach: cmach length: lengthOfcmach 
	"
*  Purpose
*  =======
*  SLAMCH determines single precision machine parameters.
"

	<cdecl: float 'slamch_'  char * SDWORD >
	^self invalidCall!

xlangeWithnorm: norm m: m n: n a: a lda: lda work: work length: lengthOfnorm 
	"
*  Purpose
*  =======
*  SLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real matrix A.
"

	<cdecl: float 'slange_'  char * SDWORD * SDWORD * float * SDWORD * float * SDWORD >
	^self invalidCall!

xlanspWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SLANSP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real symmetric matrix A,  supplied in packed form.
"

	<cdecl: float 'slansp_'  char * char * SDWORD * float * float * SDWORD SDWORD >
	^self invalidCall!

xlansyWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SLANSY  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real symmetric matrix A.
"

	<cdecl: float 'slansy_'  char * char * SDWORD * float * SDWORD * float * SDWORD SDWORD >
	^self invalidCall!

xlantpWithnorm: norm uplo: uplo diag: diag n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  SLANTP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  triangular matrix A, supplied in packed form.
"

	<cdecl: float 'slantp_'  char * char * char * SDWORD * float * float * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlantrWithnorm: norm uplo: uplo diag: diag m: m n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  SLANTR  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  trapezoidal or triangular matrix A.
"

	<cdecl: float 'slantr_'  char * char * char * SDWORD * SDWORD * float * SDWORD * float * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlarnvWithidist: idist iseed: iseed n: n x: x 
	"
*  Purpose
*  =======
*  SLARNV returns a vector of n random real numbers from a uniform or
*  normal distribution.
"

	<cdecl: SDWORD 'slarnv_'  SDWORD * SDWORD * SDWORD * float * >
	^self invalidCall!

xlasetWithuplo: uplo m: m n: n alpha: alpha beta: beta a: a lda: lda length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SLASET initializes an m-by-n matrix A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
"

	<cdecl: SDWORD 'slaset_'  char * SDWORD * SDWORD * float * float * float * SDWORD * SDWORD >
	^self invalidCall!

xorghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'sorghr_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xorglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SORGLQ generates an M-by-N real matrix Q with orthonormal rows,
*  which is defined as the first M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by SGELQF.
"

	<cdecl: SDWORD 'sorglq_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xorgqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SORGQL generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the last N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by SGEQLF.
"

	<cdecl: SDWORD 'sorgql_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xorgqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SORGQR generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by SGEQRF.
"

	<cdecl: SDWORD 'sorgqr_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xorgrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  SORGRQ generates an M-by-N real matrix Q with orthonormal rows,
*  which is defined as the last M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by SGERQF.
"

	<cdecl: SDWORD 'sorgrq_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * >
	^self invalidCall!

xpotrfWithuplo: uplo n: n a: a lda: lda info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SPOTRF computes the Cholesky factorization of a real symmetric
*  positive definite matrix A.
*  The factorization has the form
*     A = U**T * U,  if UPLO = 'U', or
*     A = L  * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*  This is the block version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'spotrf_'  char * SDWORD * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsyconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work iwork: iwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYCON estimates the reciprocal of the condition number (in the
*  1-norm) of a real symmetric matrix A using the factorization
*  A = U*D*U**T or A = L*D*L**T computed by SSYTRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'ssycon_'  char * SDWORD * float * SDWORD * SDWORD * float * float * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsyevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYEVD computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric matrix A. If eigenvectors are desired, it uses a
*  divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*  Because of large use of BLAS of level 3, SSYEVD needs N**2 more
*  workspace than SSYEVX.
"

	<cdecl: SDWORD 'ssyevd_'  char * char * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsyevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYEVR computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix T.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*  Whenever possible, SSYEVR calls SSTEGR to compute the
*  eigenspectrum using Relatively Robust Representations.  SSTEGR
*  computes eigenvalues by the dqds algorithm, while orthogonal
*  eigenvectors are computed from various 'good' L D L^T representations
*  (also known as Relatively Robust Representations). Gram-Schmidt
*  orthogonalization is avoided as far as possible. More specifically,
*  the various steps of the algorithm are as follows. For the i-th
*  unreduced block of T,
*     (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
*          is a relatively robust representation,
*     (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
*         relative accuracy by the dqds algorithm,
*     (c) If there is a cluster of close eigenvalues, 'choose' sigma_i
*         close to the cluster, and go to step (a),
*     (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
*         compute the corresponding eigenvector by forming a
*         rank-revealing twisted factorization.
*  The desired accuracy of the output can be specified by the input
*  parameter ABSTOL.
*  For more details, see 'A new O(n^2) algorithm for the symmetric
*  tridiagonal eigenvalue/eigenvector problem', by Inderjit Dhillon,
*  Computer Science Division Technical Report No. UCB//CSD-97-971,
*  UC Berkeley, May 1997.
*  Note 1 : SSYEVR calls SSTEGR when the full spectrum is requested
*  on machines which conform to the ieee-754 floating point standard.
*  SSYEVR calls SSTEBZ and SSTEIN on non-ieee machines and
*  when partial spectrum requests are made.
*  Normal execution of SSTEGR may create NaNs and infinities and
*  hence may abort due to a floating point exception in environments
*  which do not handle NaNs and infinities in the ieee standard default
*  manner.
"

	<cdecl: SDWORD 'ssyevr_'  char * char * char * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xsyevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYEV computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric matrix A.
"

	<cdecl: SDWORD 'ssyev_'  char * char * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsyevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of indices
*  for the desired eigenvalues.
"

	<cdecl: SDWORD 'ssyevx_'  char * char * char * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xsygvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYGVD computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be symmetric and B is also positive definite.
*  If eigenvectors are desired, it uses a divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'ssygvd_'  SDWORD * char * char * SDWORD * float * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsygvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYGV computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
*  Here A and B are assumed to be symmetric and B is also
*  positive definite.
"

	<cdecl: SDWORD 'ssygv_'  SDWORD * char * char * SDWORD * float * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xsygvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYGVX computes selected eigenvalues, and optionally, eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
*  and B are assumed to be symmetric and B is also positive definite.
*  Eigenvalues and eigenvectors can be selected by specifying either a
*  range of values or a range of indices for the desired eigenvalues.
"

	<cdecl: SDWORD 'ssygvx_'  SDWORD * char * char * char * SDWORD * float * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * float * SDWORD * float * float * SDWORD * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xsysvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
*  matrices.
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**T,  if UPLO = 'U', or
*     A = L * D * L**T,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with 
*  1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
*  used to solve the system of equations A * X = B.
"

	<cdecl: SDWORD 'ssysv_'  char * SDWORD * SDWORD * float * SDWORD * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYTRF computes the factorization of a real symmetric matrix A using
*  the Bunch-Kaufman diagonal pivoting method.  The form of the
*  factorization is
*     A = U*D*U**T  or  A = L*D*L**T
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with 
*  1-by-1 and 2-by-2 diagonal blocks.
*  This is the blocked version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'ssytrf_'  char * SDWORD * float * SDWORD * SDWORD * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYTRI computes the inverse of a real symmetric indefinite matrix
*  A using the factorization A = U*D*U**T or A = L*D*L**T computed by
*  SSYTRF.
"

	<cdecl: SDWORD 'ssytri_'  char * SDWORD * float * SDWORD * SDWORD * float * SDWORD * SDWORD >
	^self invalidCall!

xsytrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  SSYTRS solves a system of linear equations A*X = B with a real
*  symmetric matrix A using the factorization A = U*D*U**T or
*  A = L*D*L**T computed by SSYTRF.
"

	<cdecl: SDWORD 'ssytrs_'  char * SDWORD * SDWORD * float * SDWORD * SDWORD * float * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  STGEXC reorders the generalized real Schur decomposition of a real
*  matrix pair (A,B) using an orthogonal equivalence transformation
*                 (A, B) = Q * (A, B) * Z',
*  so that the diagonal block of (A, B) with row index IFST is moved
*  to row ILST.
*  (A, B) must be in generalized real Schur canonical form (as returned
*  by SGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
*  diagonal blocks. B is upper triangular.
*  Optionally, the matrices Q and Z of generalized Schur vectors are
*  updated.
*         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'
*         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'
"

	<cdecl: SDWORD 'stgexc_'  SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD * float * SDWORD * SDWORD * >
	^self invalidCall!

xtgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork info: info 
	"
*  Purpose
*  =======
*  STGSEN reorders the generalized real Schur decomposition of a real
*  matrix pair (A, B) (in terms of an orthonormal equivalence trans-
*  formation Q' * (A, B) * Z), so that a selected cluster of eigenvalues
*  appears in the leading diagonal blocks of the upper quasi-triangular
*  matrix A and the upper triangular B. The leading columns of Q and
*  Z form orthonormal bases of the corresponding left and right eigen-
*  spaces (deflating subspaces). (A, B) must be in generalized real
*  Schur canonical form (as returned by SGGES), i.e. A is block upper
*  triangular with 1-by-1 and 2-by-2 diagonal blocks. B is upper
*  triangular.
*  STGSEN also computes the generalized eigenvalues
*              w(j) = (ALPHAR(j) + i*ALPHAI(j))/BETA(j)
*  of the reordered matrix pair (A, B).
*  Optionally, STGSEN computes the estimates of reciprocal condition
*  numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
*  (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
*  between the matrix pairs (A11, B11) and (A22,B22) that correspond to
*  the selected cluster and the eigenvalues outside the cluster, resp.,
*  and norms of 'projections' onto left and right eigenspaces w.r.t.
*  the selected cluster in the (1,1)-block.
"

	<cdecl: SDWORD 'stgsen_'  SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * float * float * float * SDWORD * float * SDWORD * SDWORD * float * float * SDWORD * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xtgsylWithtrans: trans ijob: ijob m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc d: d ldd: ldd e: e lde: lde f: f ldf: ldf dif: dif scale: scale work: work lwork: lwork iwork: iwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  STGSYL solves the generalized Sylvester equation:
*              A * R - L * B = scale * C                 (1)
*              D * R - L * E = scale * F
*  where R and L are unknown m-by-n matrices, (A, D), (B, E) and
*  (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
*  respectively, with real entries. (A, D) and (B, E) must be in
*  generalized (real) Schur canonical form, i.e. A, B are upper quasi
*  triangular and D, E are upper triangular.
*  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output
*  scaling factor chosen to avoid overflow.
*  In matrix notation (1) is equivalent to solve  Zx = scale b, where
*  Z is defined as
*             Z = [ kron(In, A)  -kron(B', Im) ]         (2)
*                 [ kron(In, D)  -kron(E', Im) ].
*  Here Ik is the identity matrix of size k and X' is the transpose of
*  X. kron(X, Y) is the Kronecker product between the matrices X and Y.
*  If TRANS = 'T', STGSYL solves the transposed system Z'*y = scale*b,
*  which is equivalent to solve for R and L in
*              A' * R  + D' * L   = scale *  C           (3)
*              R  * B' + L  * E'  = scale * (-F)
*  This case (TRANS = 'T') is used to compute an one-norm-based estimate
*  of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
*  and (B,E), using SLACON.
*  If IJOB >= 1, STGSYL computes a Frobenius norm-based estimate
*  of Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
*  reciprocal of the smallest singular value of Z. See [1-2] for more
*  information.
*  This is a level 3 BLAS algorithm.
"

	<cdecl: SDWORD 'stgsyl_'  char * SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * float * float * float * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtpconWithnorm: norm uplo: uplo diag: diag n: n ap: ap rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  STPCON estimates the reciprocal of the condition number of a packed
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'stpcon_'  char * char * char * SDWORD * float * float * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtptriWithuplo: uplo diag: diag n: n ap: ap info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  STPTRI computes the inverse of a real upper or lower triangular
*  matrix A stored in packed format.
"

	<cdecl: SDWORD 'stptri_'  char * char * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtptrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs ap: ap b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  STPTRS solves a triangular system of the form
*     A * X = B  or  A**T * X = B,
*  where A is a triangular matrix of order N stored in packed format,
*  and B is an N-by-NRHS matrix.  A check is made to verify that A is
*  nonsingular.
"

	<cdecl: SDWORD 'stptrs_'  char * char * char * SDWORD * SDWORD * float * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work iwork: iwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  STRCON estimates the reciprocal of the condition number of a
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'strcon_'  char * char * char * SDWORD * float * SDWORD * float * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst work: work info: info length: lengthOfcompq 
	"
*  Purpose
*  =======
*  STREXC reorders the real Schur factorization of a real matrix
*  A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
*  moved to row ILST.
*  The real Schur form T is reordered by an orthogonal similarity
*  transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
*  is updated by postmultiplying it with Z.
*  T must be in Schur canonical form (as returned by SHSEQR), that is,
*  block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
*  2-by-2 diagonal block has its diagonal elements equal and its
*  off-diagonal elements of opposite sign.
"

	<cdecl: SDWORD 'strexc_'  char * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD * float * SDWORD * SDWORD >
	^self invalidCall!

xtrsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq wr: wr wi: wi m: m s: s sep: sep work: work lwork: lwork iwork: iwork liwork: liwork info: info length: lengthOfjob length: lengthOfcompq 
	"
*  Purpose
*  =======
*  STRSEN reorders the real Schur factorization of a real matrix
*  A = Q*T*Q**T, so that a selected cluster of eigenvalues appears in
*  the leading diagonal blocks of the upper quasi-triangular matrix T,
*  and the leading columns of Q form an orthonormal basis of the
*  corresponding right invariant subspace.
*  Optionally the routine computes the reciprocal condition numbers of
*  the cluster of eigenvalues and/or the invariant subspace.
*  T must be in Schur canonical form (as returned by SHSEQR), that is,
*  block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
*  2-by-2 diagonal block has its diagonal elemnts equal and its
*  off-diagonal elements of opposite sign.
"

	<cdecl: SDWORD 'strsen_'  char * char * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * float * SDWORD * float * float * float * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrsylWithtrana: trana tranb: tranb isgn: isgn m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc scale: scale info: info length: lengthOftrana length: lengthOftranb 
	"
*  Purpose
*  =======
*  STRSYL solves the real Sylvester matrix equation:
*     op(A)*X + X*op(B) = scale*C or
*     op(A)*X - X*op(B) = scale*C,
*  where op(A) = A or A**T, and  A and B are both upper quasi-
*  triangular. A is M-by-M and B is N-by-N; the right hand side C and
*  the solution X are M-by-N; and scale is an output scale factor, set
*  <= 1 to avoid overflow in X.
*  A and B must be in Schur canonical form (as returned by SHSEQR), that
*  is, block upper triangular with 1-by-1 and 2-by-2 diagonal blocks;
*  each 2-by-2 diagonal block has its diagonal elements equal and its
*  off-diagonal elements of opposite sign.
"

	<cdecl: SDWORD 'strsyl_'  char * char * SDWORD * SDWORD * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * float * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtriWithuplo: uplo diag: diag n: n a: a lda: lda info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  STRTRI computes the inverse of a real upper or lower triangular
*  matrix A.
*  This is the Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'strtri_'  char * char * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  STRTRS solves a triangular system of the form
*     A * X = B  or  A**T * X = B,
*  where A is a triangular matrix of order N, and B is an N-by-NRHS
*  matrix.  A check is made to verify that A is nonsingular.
"

	<cdecl: SDWORD 'strtrs_'  char * char * char * SDWORD * SDWORD * float * SDWORD * float * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall! !
!LapackSLibrary categoriesFor: #cComplexPointerOn:!public! !
!LapackSLibrary categoriesFor: #cElementPointerOn:!public! !
!LapackSLibrary categoriesFor: #cRealPointerOn:!public! !
!LapackSLibrary categoriesFor: #isComplex!public! !
!LapackSLibrary categoriesFor: #isDoublePrecision!public! !
!LapackSLibrary categoriesFor: #schurSelectFunction!public! !
!LapackSLibrary categoriesFor: #xgebakWithjob:side:n:ilo:ihi:scale:m:v:ldv:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xgebalWithjob:n:a:lda:ilo:ihi:scale:info:length:!public! !
!LapackSLibrary categoriesFor: #xgeconWithnorm:n:a:lda:anorm:rcond:work:iwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xgeesWithjobvs:sort:select:n:a:lda:sdim:wr:wi:vs:ldvs:work:lwork:bwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xgeevWithjobvl:jobvr:n:a:lda:wr:wi:vl:ldvl:vr:ldvr:work:lwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xgehrdWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgelqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:iwork:info:!public! !
!LapackSLibrary categoriesFor: #xgelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgelsWithtrans:m:n:nrhs:a:lda:b:ldb:work:lwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xgelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:info:!public! !
!LapackSLibrary categoriesFor: #xgelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgeqlfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgeqp3Withm:n:a:lda:jpvt:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgeqrfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgerqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:iwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xgesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xgesvWithn:nrhs:a:lda:ipiv:b:ldb:info:!public! !
!LapackSLibrary categoriesFor: #xgetrfWithm:n:a:lda:ipiv:info:!public! !
!LapackSLibrary categoriesFor: #xgetriWithn:a:lda:ipiv:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgetrsWithtrans:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackSLibrary categoriesFor: #xggbakWithjob:side:n:ilo:ihi:lscale:rscale:m:v:ldv:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xggbalWithjob:n:a:lda:b:ldb:ilo:ihi:lscale:rscale:work:info:length:!public! !
!LapackSLibrary categoriesFor: #xggevWithjobvl:jobvr:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:work:lwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alphar:alphai:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:iwork:bwork:info:length:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xggglmWithn:m:p:a:lda:b:ldb:d:x:y:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xgghrdWithcompq:compz:n:ilo:ihi:a:lda:b:ldb:q:ldq:z:ldz:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xgglseWithm:n:p:a:lda:b:ldb:c:d:x:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xggqrfWithn:m:p:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xggrqfWithm:p:n:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:iwork:info:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xhesvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xhetrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xhetriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackSLibrary categoriesFor: #xhetrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackSLibrary categoriesFor: #xlacpyWithuplo:m:n:a:lda:b:ldb:length:!public! !
!LapackSLibrary categoriesFor: #xlamchWithcmach:length:!public! !
!LapackSLibrary categoriesFor: #xlangeWithnorm:m:n:a:lda:work:length:!public! !
!LapackSLibrary categoriesFor: #xlanspWithnorm:uplo:n:ap:work:length:length:!public! !
!LapackSLibrary categoriesFor: #xlansyWithnorm:uplo:n:a:lda:work:length:length:!public! !
!LapackSLibrary categoriesFor: #xlantpWithnorm:uplo:diag:n:ap:work:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xlantrWithnorm:uplo:diag:m:n:a:lda:work:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xlarnvWithidist:iseed:n:x:!public! !
!LapackSLibrary categoriesFor: #xlasetWithuplo:m:n:alpha:beta:a:lda:length:!public! !
!LapackSLibrary categoriesFor: #xorghrWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xorglqWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xorgqlWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xorgqrWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xorgrqWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xpotrfWithuplo:n:a:lda:info:length:!public! !
!LapackSLibrary categoriesFor: #xsyconWithuplo:n:a:lda:ipiv:anorm:rcond:work:iwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xsyevdWithjobz:uplo:n:a:lda:w:work:lwork:iwork:liwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xsyevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:iwork:liwork:info:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xsyevWithjobz:uplo:n:a:lda:w:work:lwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xsyevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:info:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xsygvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:iwork:liwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xsygvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xsygvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:iwork:ifail:info:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xsysvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xsytrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xsytriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackSLibrary categoriesFor: #xsytrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackSLibrary categoriesFor: #xtgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:work:lwork:info:!public! !
!LapackSLibrary categoriesFor: #xtgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alphar:alphai:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:info:!public! !
!LapackSLibrary categoriesFor: #xtgsylWithtrans:ijob:m:n:a:lda:b:ldb:c:ldc:d:ldd:e:lde:f:ldf:dif:scale:work:lwork:iwork:info:length:!public! !
!LapackSLibrary categoriesFor: #xtpconWithnorm:uplo:diag:n:ap:rcond:work:iwork:info:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xtptriWithuplo:diag:n:ap:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xtptrsWithuplo:trans:diag:n:nrhs:ap:b:ldb:info:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xtrconWithnorm:uplo:diag:n:a:lda:rcond:work:iwork:info:length:length:length:!public! !
!LapackSLibrary categoriesFor: #xtrexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:work:info:length:!public! !
!LapackSLibrary categoriesFor: #xtrsenWithjob:compq:select:n:t:ldt:q:ldq:wr:wi:m:s:sep:work:lwork:iwork:liwork:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xtrsylWithtrana:tranb:isgn:m:n:a:lda:b:ldb:c:ldc:scale:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xtrtriWithuplo:diag:n:a:lda:info:length:length:!public! !
!LapackSLibrary categoriesFor: #xtrtrsWithuplo:trans:diag:n:nrhs:a:lda:b:ldb:info:length:length:length:!public! !

LapackZLibrary guid: (GUID fromString: '{070EEB5A-DB0C-456C-A8FE-309C935E25D6}')!
LapackZLibrary comment: ''!
!LapackZLibrary categoriesForClass!Unclassified! !
!LapackZLibrary methodsFor!

cComplexPointerOn: aComplex 	^self cDoubleComplexPointerOn: aComplex!

cElementPointerOn: aComplex 	^self cComplexPointerOn: aComplex!

cRealPointerOn: aDouble 	^self cDoublePointerOn: aDouble!

isComplex	^true!

isDoublePrecision	^true!

schurSelectFunction
	"Answer the descriptor for callback function"

	^ExternalDescriptor fromString: 'cdecl: SDWORD "DOUBLECOMPLEX" void *' !

xgebakWithjob: job side: side n: n ilo: ilo ihi: ihi scale: scale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  ZGEBAK forms the right or left eigenvectors of a complex general
*  matrix by backward transformation on the computed eigenvectors of the
*  balanced matrix output by ZGEBAL.
"

	<cdecl: SDWORD 'zgebak_'  char * char * SDWORD * SDWORD * SDWORD * double * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgebalWithjob: job n: n a: a lda: lda ilo: ilo ihi: ihi scale: scale info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  ZGEBAL balances a general complex matrix A.  This involves, first,
*  permuting A by a similarity transformation to isolate eigenvalues
*  in the first 1 to ILO-1 and last IHI+1 to N elements on the
*  diagonal; and second, applying a diagonal similarity transformation
*  to rows and columns ILO to IHI to make the rows and columns as
*  close in norm as possible.  Both steps are optional.
*  Balancing may reduce the 1-norm of the matrix, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors.
"

	<cdecl: SDWORD 'zgebal_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD * double * SDWORD * SDWORD >
	^self invalidCall!

xgeconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm 
	"
*  Purpose
*  =======
*  ZGECON estimates the reciprocal of the condition number of a general
*  complex matrix A, in either the 1-norm or the infinity-norm, using
*  the LU factorization computed by ZGETRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'zgecon_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * "ExternalDoubleComplex"void * double * SDWORD * SDWORD >
	^self invalidCall!

xgeesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim w: w vs: vs ldvs: ldvs work: work lwork: lwork rwork: rwork bwork: bwork info: info length: lengthOfjobvs length: lengthOfsort 
	"
*  Purpose
*  =======
*  ZGEES computes for an N-by-N complex nonsymmetric matrix A, the
*  eigenvalues, the Schur form T, and, optionally, the matrix of Schur
*  vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).
*  Optionally, it also orders the eigenvalues on the diagonal of the
*  Schur form so that selected eigenvalues are at the top left.
*  The leading columns of Z then form an orthonormal basis for the
*  invariant subspace corresponding to the selected eigenvalues.
*  A complex matrix is in Schur form if it is upper triangular.
"

	<cdecl: SDWORD 'zgees_'  char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgeevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  ZGEEV computes for an N-by-N complex nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
"

	<cdecl: SDWORD 'zgeev_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgeevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr scale: scale abnrm: abnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  ZGEEVX computes for an N-by-N complex nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*  Optionally also, it computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
*  (RCONDE), and reciprocal condition numbers for the right
*  eigenvectors (RCONDV).
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
*  Balancing a matrix means permuting the rows and columns to make it
*  more nearly upper triangular, and applying a diagonal similarity
*  transformation D * A * D**(-1), where D is a diagonal matrix, to
*  make its rows and columns closer in norm and the condition numbers
*  of its eigenvalues and eigenvectors smaller.  The computed
*  reciprocal condition numbers correspond to the balanced matrix.
*  Permuting rows and columns will not change the condition numbers
*  (in exact arithmetic) but diagonal scaling will.  For further
*  explanation of balancing, see section 4.10.2 of the LAPACK
*  Users' Guide.
"

	<cdecl: SDWORD 'zgeevx_'  char * char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * double * double * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xgehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'zgehrd_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgelqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGELQF computes an LQ factorization of a complex M-by-N matrix A:
*  A = L * Q.
"

	<cdecl: SDWORD 'zgelqf_'  SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork iwork: iwork info: info 
	"
*  Purpose
*  =======
*  ZGELSD computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize 2-norm(| b - A*x |)
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The problem is solved in three steps:
*  (1) Reduce the coefficient matrix A to bidiagonal form with
*      Householder tranformations, reducing the original problem
*      into a 'bidiagonal least squares problem' (BLS)
*  (2) Solve the BLS using a divide and conquer approach.
*  (3) Apply back all the Householder tranformations to solve
*      the original least squares problem.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'zgelsd_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * >
	^self invalidCall!

xgelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork info: info 
	"
*  Purpose
*  =======
*  ZGELSS computes the minimum norm solution to a complex linear
*  least squares problem:
*  Minimize 2-norm(| b - A*x |).
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
*  X.
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
"

	<cdecl: SDWORD 'zgelss_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * >
	^self invalidCall!

xgelsWithtrans: trans m: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb work: work lwork: lwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  ZGELS solves overdetermined or underdetermined complex linear systems
*  involving an M-by-N matrix A, or its conjugate-transpose, using a QR
*  or LQ factorization of A.  It is assumed that A has full rank.
*  The following options are provided:
*  1. If TRANS = 'N' and m >= n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A*X ||.
*  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
*     an underdetermined system A * X = B.
*  3. If TRANS = 'C' and m >= n:  find the minimum norm solution of
*     an undetermined system A**H * X = B.
*  4. If TRANS = 'C' and m < n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A**H * X ||.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
"

	<cdecl: SDWORD 'zgels_'  char * SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work rwork: rwork info: info 
	"
*  Purpose
*  =======
*  This routine is deprecated and has been replaced by routine ZGELSY.
*  ZGELSX computes the minimum-norm solution to a complex linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by unitary transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
"

	<cdecl: SDWORD 'zgelsx_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * double * SDWORD * "ExternalDoubleComplex"void * double * SDWORD * >
	^self invalidCall!

xgelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork rwork: rwork info: info 
	"
*  Purpose
*  =======
*  ZGELSY computes the minimum-norm solution to a complex linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by unitary transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
*  This routine is basically identical to the original xGELSX except
*  three differences:
*    o The permutation of matrix B (the right hand side) is faster and
*      more simple.
*    o The call to the subroutine xGEQPF has been substituted by the
*      the call to the subroutine xGEQP3. This subroutine is a Blas-3
*      version of the QR factorization with column pivoting.
*    o Matrix B (the right hand side) is updated with Blas-3.
"

	<cdecl: SDWORD 'zgelsy_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * double * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * >
	^self invalidCall!

xgeqlfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGEQLF computes a QL factorization of a complex M-by-N matrix A:
*  A = Q * L.
"

	<cdecl: SDWORD 'zgeqlf_'  SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgeqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork rwork: rwork info: info 
	"
*  Purpose
*  =======
*  ZGEQP3 computes a QR factorization with column pivoting of a
*  matrix A:  A*P = Q*R  using Level 3 BLAS.
"

	<cdecl: SDWORD 'zgeqp3_'  SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * >
	^self invalidCall!

xgeqrfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGEQRF computes a QR factorization of a complex M-by-N matrix A:
*  A = Q * R.
"

	<cdecl: SDWORD 'zgeqrf_'  SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgerqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGERQF computes an RQ factorization of a complex M-by-N matrix A:
*  A = R * Q.
"

	<cdecl: SDWORD 'zgerqf_'  SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork iwork: iwork info: info length: lengthOfjobz 
	"
*  Purpose
*  =======
*  ZGESDD computes the singular value decomposition (SVD) of a complex
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors, by using divide-and-conquer method. The SVD is written
*       A = U * SIGMA * conjugate-transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
*  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns VT = V**H, not V.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'zgesdd_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xgesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork info: info length: lengthOfjobu length: lengthOfjobvt 
	"
*  Purpose
*  =======
*  ZGESVD computes the singular value decomposition (SVD) of a complex
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*       A = U * SIGMA * conjugate-transpose(V)
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
*  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*  Note that the routine returns V**H, not V.
"

	<cdecl: SDWORD 'zgesvd_'  char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgesvWithn: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info 
	"
*  Purpose
*  =======
*  ZGESV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*  The LU decomposition with partial pivoting and row interchanges is
*  used to factor A as
*     A = P * L * U,
*  where P is a permutation matrix, L is unit lower triangular, and U is
*  upper triangular.  The factored form of A is then used to solve the
*  system of equations A * X = B.
"

	<cdecl: SDWORD 'zgesv_'  SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgetrfWithm: m n: n a: a lda: lda ipiv: ipiv info: info 
	"
*  Purpose
*  =======
*  ZGETRF computes an LU factorization of a general M-by-N matrix A
*  using partial pivoting with row interchanges.
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*  This is the right-looking Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'zgetrf_'  SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xgetriWithn: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGETRI computes the inverse of a matrix using the LU factorization
*  computed by ZGETRF.
*  This method inverts U and then computes inv(A) by solving the system
*  inv(A)*L = inv(U) for inv(A).
"

	<cdecl: SDWORD 'zgetri_'  SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgetrsWithtrans: trans n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  ZGETRS solves a system of linear equations
*     A * X = B,  A**T * X = B,  or  A**H * X = B
*  with a general N-by-N matrix A using the LU factorization computed
*  by ZGETRF.
"

	<cdecl: SDWORD 'zgetrs_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xggbakWithjob: job side: side n: n ilo: ilo ihi: ihi lscale: lscale rscale: rscale m: m v: v ldv: ldv info: info length: lengthOfjob length: lengthOfside 
	"
*  Purpose
*  =======
*  ZGGBAK forms the right or left eigenvectors of a complex generalized
*  eigenvalue problem A*x = lambda*B*x, by backward transformation on
*  the computed eigenvectors of the balanced pair of matrices output by
*  ZGGBAL.
"

	<cdecl: SDWORD 'zggbak_'  char * char * SDWORD * SDWORD * SDWORD * double * double * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggbalWithjob: job n: n a: a lda: lda b: b ldb: ldb ilo: ilo ihi: ihi lscale: lscale rscale: rscale work: work info: info length: lengthOfjob 
	"
*  Purpose
*  =======
*  ZGGBAL balances a pair of general complex matrices (A,B).  This
*  involves, first, permuting A and B by similarity transformations to
*  isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
*  elements on the diagonal; and second, applying a diagonal similarity
*  transformation to rows and columns ILO to IHI to make the rows
*  and columns as close in norm as possible. Both steps are optional.
*  Balancing may reduce the 1-norm of the matrices, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors in the
*  generalized eigenvalue problem A*x = lambda*B*x.
"

	<cdecl: SDWORD 'zggbal_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD * double * double * double * SDWORD * SDWORD >
	^self invalidCall!

xggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork info: info length: lengthOfjobvl length: lengthOfjobvr 
	"
*  Purpose
*  =======
*  ZGGEV computes for a pair of N-by-N complex nonsymmetric matrices
*  (A,B), the generalized eigenvalues, and optionally, the left and/or
*  right generalized eigenvectors.
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right generalized eigenvector v(j) corresponding to the
*  generalized eigenvalue lambda(j) of (A,B) satisfies
*               A * v(j) = lambda(j) * B * v(j).
*  The left generalized eigenvector u(j) corresponding to the
*  generalized eigenvalues lambda(j) of (A,B) satisfies
*               u(j)**H * A = lambda(j) * u(j)**H * B
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'zggev_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  ZGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
*  (A,B) the generalized eigenvalues, and optionally, the left and/or
*  right generalized eigenvectors.
*  Optionally, it also computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
*  the eigenvalues (RCONDE), and reciprocal condition numbers for the
*  right eigenvectors (RCONDV).
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j) .
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B.
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'zggevx_'  char * char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * double * double * double * double * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xggevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr lscale: lscale rscale: rscale abnrm: abnrm bbnrm: bbnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork iwork: iwork bwork: bwork info: info length: lengthOfbalanc length: lengthOfjobvl length: lengthOfjobvr length: lengthOfsense 
	"
*  Purpose
*  =======
*  ZGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
*  (A,B) the generalized eigenvalues, and optionally, the left and/or
*  right generalized eigenvectors.
*  Optionally, it also computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
*  the eigenvalues (RCONDE), and reciprocal condition numbers for the
*  right eigenvectors (RCONDV).
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   A * v(j) = lambda(j) * B * v(j) .
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*                   u(j)**H * A  = lambda(j) * u(j)**H * B.
*  where u(j)**H is the conjugate-transpose of u(j).
"

	<cdecl: SDWORD 'zggevx_'  char * char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * double * double * double * double * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD SDWORD >
	^self invalidCall!

xggglmWithn: n m: m p: p a: a lda: lda b: b ldb: ldb d: d x: x y: y work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGGGLM solves a general Gauss-Markov linear model (GLM) problem:
*          minimize || y ||_2   subject to   d = A*x + B*y
*              x
*  where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
*  given N-vector. It is assumed that M <= N <= M+P, and
*             rank(A) = M    and    rank( A B ) = N.
*  Under these assumptions, the constrained equation is always
*  consistent, and there is a unique solution x and a minimal 2-norm
*  solution y, which is obtained using a generalized QR factorization
*  of A and B.
*  In particular, if matrix B is square nonsingular, then the problem
*  GLM is equivalent to the following weighted linear least squares
*  problem
*               minimize || inv(B)*(d-A*x) ||_2
*                   x
*  where inv(B) denotes the inverse of B.
"

	<cdecl: SDWORD 'zggglm_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xgghrdWithcompq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz info: info length: lengthOfcompq length: lengthOfcompz 
	"
*  Purpose
*  =======
*  ZGGHRD reduces a pair of complex matrices (A,B) to generalized upper
*  Hessenberg form using unitary transformations, where A is a
*  general matrix and B is upper triangular.  The form of the
*  generalized eigenvalue problem is
*     A*x = lambda*B*x,
*  and B is typically made upper triangular by computing its QR
*  factorization and moving the unitary matrix Q to the left side
*  of the equation.
*  This subroutine simultaneously reduces A to a Hessenberg matrix H:
*     Q**H*A*Z = H
*  and transforms B to another upper triangular matrix T:
*     Q**H*B*Z = T
*  in order to reduce the problem to its standard form
*     H*y = lambda*T*y
*  where y = Z**H*x.
*  The unitary matrices Q and Z are determined as products of Givens
*  rotations.  They may either be formed explicitly, or they may be
*  postmultiplied into input matrices Q1 and Z1, so that
*       Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
*       Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
*  If Q1 is the unitary matrix from the QR factorization of B in the
*  original equation A*x = lambda*B*x, then ZGGHRD reduces the original
*  problem to generalized Hessenberg form.
"

	<cdecl: SDWORD 'zgghrd_'  char * char * SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xgglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGGLSE solves the linear equality-constrained least squares (LSE)
*  problem:
*          minimize || c - A*x ||_2   subject to   B*x = d
*  where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
*  M-vector, and d is a given P-vector. It is assumed that
*  P <= N <= M+P, and
*           rank(B) = P and  rank( ( A ) ) = N.
*                                ( ( B ) )
*  These conditions ensure that the LSE problem has a unique solution,
*  which is obtained using a GRQ factorization of the matrices B and A.
"

	<cdecl: SDWORD 'zgglse_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xggqrfWithn: n m: m p: p a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGGQRF computes a generalized QR factorization of an N-by-M matrix A
*  and an N-by-P matrix B:
*              A = Q*R,        B = Q*T*Z,
*  where Q is an N-by-N unitary matrix, Z is a P-by-P unitary matrix,
*  and R and T assume one of the forms:
*  if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
*                  (  0  ) N-M                         N   M-N
*                     M
*  where R11 is upper triangular, and
*  if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
*                   P-N  N                           ( T21 ) P
*                                                       P
*  where T12 or T21 is upper triangular.
*  In particular, if B is square and nonsingular, the GQR factorization
*  of A and B implicitly gives the QR factorization of inv(B)*A:
*               inv(B)*A = Z'*(inv(T)*R)
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  conjugate transpose of matrix Z.
"

	<cdecl: SDWORD 'zggqrf_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xggrqfWithm: m p: p n: n a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZGGRQF computes a generalized RQ factorization of an M-by-N matrix A
*  and a P-by-N matrix B:
*              A = R*Q,        B = Z*T*Q,
*  where Q is an N-by-N unitary matrix, Z is a P-by-P unitary
*  matrix, and R and T assume one of the forms:
*  if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
*                   N-M  M                           ( R21 ) N
*                                                       N
*  where R12 or R21 is upper triangular, and
*  if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
*                  (  0  ) P-N                         P   N-P
*                     N
*  where T11 is upper triangular.
*  In particular, if B is square and nonsingular, the GRQ factorization
*  of A and B implicitly gives the RQ factorization of A*inv(B):
*               A*inv(B) = (R*inv(T))*Z'
*  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
*  conjugate transpose of the matrix Z.
"

	<cdecl: SDWORD 'zggrqf_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work rwork: rwork iwork: iwork info: info length: lengthOfjobu length: lengthOfjobv length: lengthOfjobq 
	"
*  Purpose
*  =======
*  ZGGSVD computes the generalized singular value decomposition (GSVD)
*  of an M-by-N complex matrix A and P-by-N complex matrix B:
*        U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
*  where U, V and Q are unitary matrices, and Z' means the conjugate
*  transpose of Z.  Let K+L = the effective numerical rank of the
*  matrix (A',B')', then R is a (K+L)-by-(K+L) nonsingular upper
*  triangular matrix, D1 and D2 are M-by-(K+L) and P-by-(K+L) 'diagonal'
*  matrices and of the following structures, respectively:
*  If M-K-L >= 0,
*                      K  L
*         D1 =     K ( I  0 )
*                  L ( 0  C )
*              M-K-L ( 0  0 )
*                    K  L
*         D2 =   L ( 0  S )
*              P-L ( 0  0 )
*                  N-K-L  K    L
*    ( 0 R ) = K (  0   R11  R12 )
*              L (  0    0   R22 )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
*    S = diag( BETA(K+1),  ... , BETA(K+L) ),
*    C**2 + S**2 = I.
*    R is stored in A(1:K+L,N-K-L+1:N) on exit.
*  If M-K-L < 0,
*                    K M-K K+L-M
*         D1 =   K ( I  0    0   )
*              M-K ( 0  C    0   )
*                      K M-K K+L-M
*         D2 =   M-K ( 0  S    0  )
*              K+L-M ( 0  0    I  )
*                P-L ( 0  0    0  )
*                     N-K-L  K   M-K  K+L-M
*    ( 0 R ) =     K ( 0    R11  R12  R13  )
*                M-K ( 0     0   R22  R23  )
*              K+L-M ( 0     0    0   R33  )
*  where
*    C = diag( ALPHA(K+1), ... , ALPHA(M) ),
*    S = diag( BETA(K+1),  ... , BETA(M) ),
*    C**2 + S**2 = I.
*    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
*    ( 0  R22 R23 )
*    in B(M-K+1:L,N+M-K-L+1:N) on exit.
*  The routine computes C, S, R, and optionally the unitary
*  transformation matrices U, V and Q.
*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
*  A and B implicitly gives the SVD of A*inv(B):
*                       A*inv(B) = U*(D1*inv(D2))*V'.
*  If ( A',B')' has orthnormal columns, then the GSVD of A and B is also
*  equal to the CS decomposition of A and B. Furthermore, the GSVD can
*  be used to derive the solution of the eigenvalue problem:
*                       A'*A x = lambda* B'*B x.
*  In some literature, the GSVD of A and B is presented in the form
*                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
*  where U and V are orthogonal and X is nonsingular, and D1 and D2 are
*  ``diagonal''.  The former GSVD form can be converted to the latter
*  form by taking the nonsingular matrix X as
*                        X = Q*(  I   0    )
*                              (  0 inv(R) )
"

	<cdecl: SDWORD 'zggsvd_'  char * char * char * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xheconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHECON estimates the reciprocal of the condition number of a complex
*  Hermitian matrix A using the factorization A = U*D*U**H or
*  A = L*D*L**H computed by ZHETRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'zhecon_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * double * double * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xheevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHEEVD computes all eigenvalues and, optionally, eigenvectors of a
*  complex Hermitian matrix A.  If eigenvectors are desired, it uses a
*  divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'zheevd_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xheevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork rwork: rwork iwork: iwork info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHEEVR computes selected eigenvalues and, optionally, eigenvectors
*  of a complex Hermitian matrix T.  Eigenvalues and eigenvectors can
*  be selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*  Whenever possible, ZHEEVR calls ZSTEGR to compute the
*  eigenspectrum using Relatively Robust Representations.  ZSTEGR
*  computes eigenvalues by the dqds algorithm, while orthogonal
*  eigenvectors are computed from various 'good' L D L^T representations
*  (also known as Relatively Robust Representations). Gram-Schmidt
*  orthogonalization is avoided as far as possible. More specifically,
*  the various steps of the algorithm are as follows. For the i-th
*  unreduced block of T,
*     (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
*          is a relatively robust representation,
*     (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
*         relative accuracy by the dqds algorithm,
*     (c) If there is a cluster of close eigenvalues, 'choose' sigma_i
*         close to the cluster, and go to step (a),
*     (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
*         compute the corresponding eigenvector by forming a
*         rank-revealing twisted factorization.
*  The desired accuracy of the output can be specified by the input
*  parameter ABSTOL.
*  For more details, see 'A new O(n^2) algorithm for the symmetric
*  tridiagonal eigenvalue/eigenvector problem', by Inderjit Dhillon,
*  Computer Science Division Technical Report No. UCB//CSD-97-971,
*  UC Berkeley, May 1997.
*  Note 1 : ZHEEVR calls ZSTEGR when the full spectrum is requested
*  on machines which conform to the ieee-754 floating point standard.
*  ZHEEVR calls DSTEBZ and ZSTEIN on non-ieee machines and
*  when partial spectrum requests are made.
*  Normal execution of ZSTEGR may create NaNs and infinities and
*  hence may abort due to a floating point exception in environments
*  which do not handle NaNs and infinities in the ieee standard default
*  manner.
"

	<cdecl: SDWORD 'zheevr_'  char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * SDWORD * SDWORD * double * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xheevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork info: info length: ljobz length: luplo 
	<cdecl: void 'zheev_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xheevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHEEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a complex Hermitian matrix A.  Eigenvalues and eigenvectors can
*  be selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
"

	<cdecl: SDWORD 'zheevx_'  char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * SDWORD * SDWORD * double * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xhegvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHEGVD computes all the eigenvalues, and optionally, the eigenvectors
*  of a complex generalized Hermitian-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be Hermitian and B is also positive definite.
*  If eigenvectors are desired, it uses a divide and conquer algorithm.
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
"

	<cdecl: SDWORD 'zhegvd_'  SDWORD * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xhegvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork info: info length: lengthOfjobz length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHEGV computes all the eigenvalues, and optionally, the eigenvectors
*  of a complex generalized Hermitian-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
*  Here A and B are assumed to be Hermitian and B is also
*  positive definite.
"

	<cdecl: SDWORD 'zhegv_'  SDWORD * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xhegvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail info: info length: lengthOfjobz length: lengthOfrange length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHEGVX computes selected eigenvalues, and optionally, eigenvectors
*  of a complex generalized Hermitian-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be Hermitian and B is also positive definite.
*  Eigenvalues and eigenvectors can be selected by specifying either a
*  range of values or a range of indices for the desired eigenvalues.
"

	<cdecl: SDWORD 'zhegvx_'  SDWORD * char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * SDWORD * SDWORD * double * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xhesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHESV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
*  matrices.
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**H,  if UPLO = 'U', or
*     A = L * D * L**H,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is Hermitian and block diagonal with
*  1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
*  used to solve the system of equations A * X = B.
"

	<cdecl: SDWORD 'zhesv_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xhetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHETRF computes the factorization of a complex Hermitian matrix A
*  using the Bunch-Kaufman diagonal pivoting method.  The form of the
*  factorization is
*     A = U*D*U**H  or  A = L*D*L**H
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is Hermitian and block diagonal with
*  1-by-1 and 2-by-2 diagonal blocks.
*  This is the blocked version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'zhetrf_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xhetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHETRI computes the inverse of a complex Hermitian indefinite matrix
*  A using the factorization A = U*D*U**H or A = L*D*L**H computed by
*  ZHETRF.
"

	<cdecl: SDWORD 'zhetri_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xhetrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZHETRS solves a system of linear equations A*X = B with a complex
*  Hermitian matrix A using the factorization A = U*D*U**H or
*  A = L*D*L**H computed by ZHETRF.
"

	<cdecl: SDWORD 'zhetrs_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xlacgvWithn: n x: x incx: incx 
	"
*  Purpose
*  =======
*  ZLACGV conjugates a complex vector of length N.
"

	<cdecl: SDWORD 'zlacgv_'  SDWORD * "ExternalDoubleComplex"void * SDWORD * >
	^self invalidCall!

xlacpyWithuplo: uplo m: m n: n a: a lda: lda b: b ldb: ldb length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZLACPY copies all or part of a two-dimensional matrix A to another
*  matrix B.
"

	<cdecl: SDWORD 'zlacpy_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xlangeWithnorm: norm m: m n: n a: a lda: lda work: work length: lengthOfnorm 
	"
*  Purpose
*  =======
*  ZLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex matrix A.
"

	<cdecl: double 'zlange_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD >
	^self invalidCall!

xlanheWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZLANHE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex hermitian matrix A.
"

	<cdecl: double 'zlanhe_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD SDWORD >
	^self invalidCall!

xlanhpWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZLANHP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex hermitian matrix A,  supplied in packed form.
"

	<cdecl: double 'zlanhp_'  char * char * SDWORD * "ExternalDoubleComplex"void * double * SDWORD SDWORD >
	^self invalidCall!

xlanspWithnorm: norm uplo: uplo n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZLANSP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex symmetric matrix A,  supplied in packed form.
"

	<cdecl: double 'zlansp_'  char * char * SDWORD * "ExternalDoubleComplex"void * double * SDWORD SDWORD >
	^self invalidCall!

xlansyWithnorm: norm uplo: uplo n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZLANSY  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex symmetric matrix A.
"

	<cdecl: double 'zlansy_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD SDWORD >
	^self invalidCall!

xlantpWithnorm: norm uplo: uplo diag: diag n: n ap: ap work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZLANTP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  triangular matrix A, supplied in packed form.
"

	<cdecl: double 'zlantp_'  char * char * char * SDWORD * "ExternalDoubleComplex"void * double * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlantrWithnorm: norm uplo: uplo diag: diag m: m n: n a: a lda: lda work: work length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZLANTR  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  trapezoidal or triangular matrix A.
"

	<cdecl: double 'zlantr_'  char * char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD SDWORD SDWORD >
	^self invalidCall!

xlarnvWithidist: idist iseed: iseed n: n x: x 
	"
*  Purpose
*  =======
*  ZLARNV returns a vector of n random complex numbers from a uniform or
*  normal distribution.
"

	<cdecl: SDWORD 'zlarnv_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * >
	^self invalidCall!

xlasetWithuplo: uplo m: m n: n alpha: alpha beta: beta a: a lda: lda length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZLASET initializes a 2-D array A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
"

	<cdecl: SDWORD 'zlaset_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xpotrfWithuplo: uplo n: n a: a lda: lda info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZPOTRF computes the Cholesky factorization of a complex Hermitian
*  positive definite matrix A.
*  The factorization has the form
*     A = U**H * U,  if UPLO = 'U', or
*     A = L  * L**H,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
*  This is the block version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'zpotrf_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xspconWithuplo: uplo n: n ap: ap ipiv: ipiv anorm: anorm rcond: rcond work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSPCON estimates the reciprocal of the condition number (in the
*  1-norm) of a complex symmetric packed matrix A using the
*  factorization A = U*D*U**T or A = L*D*L**T computed by ZSPTRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'zspcon_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xspsvWithuplo: uplo n: n nrhs: nrhs ap: ap ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSPSV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric matrix stored in packed format and X
*  and B are N-by-NRHS matrices.
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**T,  if UPLO = 'U', or
*     A = L * D * L**T,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, D is symmetric and block diagonal with 1-by-1
*  and 2-by-2 diagonal blocks.  The factored form of A is then used to
*  solve the system of equations A * X = B.
"

	<cdecl: SDWORD 'zspsv_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsptrfWithuplo: uplo n: n ap: ap ipiv: ipiv info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSPTRF computes the factorization of a complex symmetric matrix A
*  stored in packed format using the Bunch-Kaufman diagonal pivoting
*  method:
*     A = U*D*U**T  or  A = L*D*L**T
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with
*  1-by-1 and 2-by-2 diagonal blocks.
"

	<cdecl: SDWORD 'zsptrf_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsptriWithuplo: uplo n: n ap: ap ipiv: ipiv work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSPTRI computes the inverse of a complex symmetric indefinite matrix
*  A in packed storage using the factorization A = U*D*U**T or
*  A = L*D*L**T computed by ZSPTRF.
"

	<cdecl: SDWORD 'zsptri_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xsptrsWithuplo: uplo n: n nrhs: nrhs ap: ap ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSPTRS solves a system of linear equations A*X = B with a complex
*  symmetric matrix A stored in packed format using the factorization
*  A = U*D*U**T or A = L*D*L**T computed by ZSPTRF.
"

	<cdecl: SDWORD 'zsptrs_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsyconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSYCON estimates the reciprocal of the condition number (in the
*  1-norm) of a complex symmetric matrix A using the factorization
*  A = U*D*U**T or A = L*D*L**T computed by ZSYTRF.
*  An estimate is obtained for norm(inv(A)), and the reciprocal of the
*  condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
"

	<cdecl: SDWORD 'zsycon_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * double * double * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xsysvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSYSV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
*  matrices.
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**T,  if UPLO = 'U', or
*     A = L * D * L**T,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with
*  1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
*  used to solve the system of equations A * X = B.
"

	<cdecl: SDWORD 'zsysv_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSYTRF computes the factorization of a complex symmetric matrix A
*  using the Bunch-Kaufman diagonal pivoting method.  The form of the
*  factorization is
*     A = U*D*U**T  or  A = L*D*L**T
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with
*  with 1-by-1 and 2-by-2 diagonal blocks.
*  This is the blocked version of the algorithm, calling Level 3 BLAS.
"

	<cdecl: SDWORD 'zsytrf_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xsytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSYTRI computes the inverse of a complex symmetric indefinite matrix
*  A using the factorization A = U*D*U**T or A = L*D*L**T computed by
*  ZSYTRF.
"

	<cdecl: SDWORD 'zsytri_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD >
	^self invalidCall!

xsytrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb info: info length: lengthOfuplo 
	"
*  Purpose
*  =======
*  ZSYTRS solves a system of linear equations A*X = B with a complex
*  symmetric matrix A using the factorization A = U*D*U**T or
*  A = L*D*L**T computed by ZSYTRF.
"

	<cdecl: SDWORD 'zsytrs_'  char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst info: info 
	"
*  Purpose
*  =======
*  ZTGEXC reorders the generalized Schur decomposition of a complex
*  matrix pair (A,B), using an unitary equivalence transformation
*  (A, B) := Q * (A, B) * Z', so that the diagonal block of (A, B) with
*  row index IFST is moved to row ILST.
*  (A, B) must be in generalized Schur canonical form, that is, A and
*  B are both upper triangular.
*  Optionally, the matrices Q and Z of generalized Schur vectors are
*  updated.
*         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'
*         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'
"

	<cdecl: SDWORD 'ztgexc_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xtgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork info: info 
	"
*  Purpose
*  =======
*  ZTGSEN reorders the generalized Schur decomposition of a complex
*  matrix pair (A, B) (in terms of an unitary equivalence trans-
*  formation Q' * (A, B) * Z), so that a selected cluster of eigenvalues
*  appears in the leading diagonal blocks of the pair (A,B). The leading
*  columns of Q and Z form unitary bases of the corresponding left and
*  right eigenspaces (deflating subspaces). (A, B) must be in
*  generalized Schur canonical form, that is, A and B are both upper
*  triangular.
*  ZTGSEN also computes the generalized eigenvalues
*           w(j)= ALPHA(j) / BETA(j)
*  of the reordered matrix pair (A, B).
*  Optionally, the routine computes estimates of reciprocal condition
*  numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
*  (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
*  between the matrix pairs (A11, B11) and (A22,B22) that correspond to
*  the selected cluster and the eigenvalues outside the cluster, resp.,
*  and norms of 'projections' onto left and right eigenspaces w.r.t.
*  the selected cluster in the (1,1)-block.
"

	<cdecl: SDWORD 'ztgsen_'  SDWORD * SDWORD * SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * double * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD * SDWORD * >
	^self invalidCall!

xtgsylWithtrans: trans ijob: ijob m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc d: d ldd: ldd e: e lde: lde f: f ldf: ldf dif: dif scale: scale work: work lwork: lwork iwork: iwork info: info length: lengthOftrans 
	"
*  Purpose
*  =======
*  ZTGSYL solves the generalized Sylvester equation:
*              A * R - L * B = scale * C            (1)
*              D * R - L * E = scale * F
*  where R and L are unknown m-by-n matrices, (A, D), (B, E) and
*  (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,
*  respectively, with complex entries. A, B, D and E are upper
*  triangular (i.e., (A,D) and (B,E) in generalized Schur form).
*  The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1
*  is an output scaling factor chosen to avoid overflow.
*  In matrix notation (1) is equivalent to solve Zx = scale*b, where Z
*  is defined as
*         Z = [ kron(In, A)  -kron(B', Im) ]        (2)
*             [ kron(In, D)  -kron(E', Im) ],
*  Here Ix is the identity matrix of size x and X' is the conjugate
*  transpose of X. Kron(X, Y) is the Kronecker product between the
*  matrices X and Y.
*  If TRANS = 'C', y in the conjugate transposed system Z'*y = scale*b
*  is solved for, which is equivalent to solve for R and L in
*              A' * R + D' * L = scale * C           (3)
*              R * B' + L * E' = scale * -F
*  This case (TRANS = 'C') is used to compute an one-norm-based estimate
*  of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)
*  and (B,E), using ZLACON.
*  If IJOB >= 1, ZTGSYL computes a Frobenius norm-based estimate of
*  Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the
*  reciprocal of the smallest singular value of Z.
*  This is a level-3 BLAS algorithm.
"

	<cdecl: SDWORD 'ztgsyl_'  char * SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtpconWithnorm: norm uplo: uplo diag: diag n: n ap: ap rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZTPCON estimates the reciprocal of the condition number of a packed
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'ztpcon_'  char * char * char * SDWORD * "ExternalDoubleComplex"void * double * "ExternalDoubleComplex"void * double * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtptriWithuplo: uplo diag: diag n: n ap: ap info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZTPTRI computes the inverse of a complex upper or lower triangular
*  matrix A stored in packed format.
"

	<cdecl: SDWORD 'ztptri_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtptrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs ap: ap b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZTPTRS solves a triangular system of the form
*     A * X = B,  A**T * X = B,  or  A**H * X = B,
*  where A is a triangular matrix of order N stored in packed format,
*  and B is an N-by-NRHS matrix.  A check is made to verify that A is
*  nonsingular.
"

	<cdecl: SDWORD 'ztptrs_'  char * char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work rwork: rwork info: info length: lengthOfnorm length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZTRCON estimates the reciprocal of the condition number of a
*  triangular matrix A, in either the 1-norm or the infinity-norm.
*  The norm of A is computed and an estimate is obtained for
*  norm(inv(A)), then the reciprocal of the condition number is
*  computed as
*     RCOND = 1 / ( norm(A) * norm(inv(A)) ).
"

	<cdecl: SDWORD 'ztrcon_'  char * char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * "ExternalDoubleComplex"void * double * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xtrexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst info: info length: lengthOfcompq 
	"
*  Purpose
*  =======
*  ZTREXC reorders the Schur factorization of a complex matrix
*  A = Q*T*Q**H, so that the diagonal element of T with row index IFST
*  is moved to row ILST.
*  The Schur form T is reordered by a unitary similarity transformation
*  Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by
*  postmultplying it with Z.
"

	<cdecl: SDWORD 'ztrexc_'  char * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD * SDWORD * SDWORD >
	^self invalidCall!

xtrsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq w: w m: m s: s sep: sep work: work lwork: lwork info: info length: lengthOfjob length: lengthOfcompq 
	"
*  Purpose
*  =======
*  ZTRSEN reorders the Schur factorization of a complex matrix
*  A = Q*T*Q**H, so that a selected cluster of eigenvalues appears in
*  the leading positions on the diagonal of the upper triangular matrix
*  T, and the leading columns of Q form an orthonormal basis of the
*  corresponding right invariant subspace.
*  Optionally the routine computes the reciprocal condition numbers of
*  the cluster of eigenvalues and/or the invariant subspace.
"

	<cdecl: SDWORD 'ztrsen_'  char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * double * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrsylWithtrana: trana tranb: tranb isgn: isgn m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc scale: scale info: info length: lengthOftrana length: lengthOftranb 
	"
*  Purpose
*  =======
*  ZTRSYL solves the complex Sylvester matrix equation:
*     op(A)*X + X*op(B) = scale*C or
*     op(A)*X - X*op(B) = scale*C,
*  where op(A) = A or A**H, and A and B are both upper triangular. A is
*  M-by-M and B is N-by-N; the right hand side C and the solution X are
*  M-by-N; and scale is an output scale factor, set <= 1 to avoid
*  overflow in X.
"

	<cdecl: SDWORD 'ztrsyl_'  char * char * SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * double * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtriWithuplo: uplo diag: diag n: n a: a lda: lda info: info length: lengthOfuplo length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZTRTRI computes the inverse of a complex upper or lower triangular
*  matrix A.
*  This is the Level 3 BLAS version of the algorithm.
"

	<cdecl: SDWORD 'ztrtri_'  char * char * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD SDWORD >
	^self invalidCall!

xtrtrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb info: info length: lengthOfuplo length: lengthOftrans length: lengthOfdiag 
	"
*  Purpose
*  =======
*  ZTRTRS solves a triangular system of the form
*     A * X = B,  A**T * X = B,  or  A**H * X = B,
*  where A is a triangular matrix of order N, and B is an N-by-NRHS
*  matrix.  A check is made to verify that A is nonsingular.
"

	<cdecl: SDWORD 'ztrtrs_'  char * char * char * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * SDWORD * SDWORD * SDWORD SDWORD SDWORD >
	^self invalidCall!

xunghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork info: info 
	<cdecl: void 'zunghr_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xunglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZUNGLQ generates an M-by-N complex matrix Q with orthonormal rows,
*  which is defined as the first M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(k)' . . . H(2)' H(1)'
*  as returned by ZGELQF.
"

	<cdecl: SDWORD 'zunglq_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xungqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZUNGQL generates an M-by-N complex matrix Q with orthonormal columns,
*  which is defined as the last N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(k) . . . H(2) H(1)
*  as returned by ZGEQLF.
"

	<cdecl: SDWORD 'zungql_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xungqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*        Q  =  H(1) H(2) . . . H(k)
*  as returned by ZGEQRF.
"

	<cdecl: SDWORD 'zungqr_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall!

xungrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork info: info 
	"
*  Purpose
*  =======
*  ZUNGRQ generates an M-by-N complex matrix Q with orthonormal rows,
*  which is defined as the last M rows of a product of K elementary
*  reflectors of order N
*        Q  =  H(1)' H(2)' . . . H(k)'
*  as returned by ZGERQF.
"

	<cdecl: SDWORD 'zungrq_'  SDWORD * SDWORD * SDWORD * "ExternalDoubleComplex"void * SDWORD * "ExternalDoubleComplex"void * "ExternalDoubleComplex"void * SDWORD * SDWORD * >
	^self invalidCall! !
!LapackZLibrary categoriesFor: #cComplexPointerOn:!public! !
!LapackZLibrary categoriesFor: #cElementPointerOn:!public! !
!LapackZLibrary categoriesFor: #cRealPointerOn:!public! !
!LapackZLibrary categoriesFor: #isComplex!public! !
!LapackZLibrary categoriesFor: #isDoublePrecision!public! !
!LapackZLibrary categoriesFor: #schurSelectFunction!public! !
!LapackZLibrary categoriesFor: #xgebakWithjob:side:n:ilo:ihi:scale:m:v:ldv:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xgebalWithjob:n:a:lda:ilo:ihi:scale:info:length:!public! !
!LapackZLibrary categoriesFor: #xgeconWithnorm:n:a:lda:anorm:rcond:work:rwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xgeesWithjobvs:sort:select:n:a:lda:sdim:w:vs:ldvs:work:lwork:rwork:bwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xgeevWithjobvl:jobvr:n:a:lda:w:vl:ldvl:vr:ldvr:work:lwork:rwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xgeevxWithbalanc:jobvl:jobvr:sense:n:a:lda:w:vl:ldvl:vr:ldvr:scale:abnrm:rconde:rcondv:work:lwork:rwork:info:length:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xgehrdWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xgelqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xgelsdWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:iwork:info:!public! !
!LapackZLibrary categoriesFor: #xgelssWithm:n:nrhs:a:lda:b:ldb:s:rcond:rank:work:lwork:rwork:info:!public! !
!LapackZLibrary categoriesFor: #xgelsWithtrans:m:n:nrhs:a:lda:b:ldb:work:lwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xgelsxWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:rwork:info:!public! !
!LapackZLibrary categoriesFor: #xgelsyWithm:n:nrhs:a:lda:b:ldb:jpvt:rcond:rank:work:lwork:rwork:info:!public! !
!LapackZLibrary categoriesFor: #xgeqlfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xgeqp3Withm:n:a:lda:jpvt:tau:work:lwork:rwork:info:!public! !
!LapackZLibrary categoriesFor: #xgeqrfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xgerqfWithm:n:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xgesddWithjobz:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:iwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xgesvdWithjobu:jobvt:m:n:a:lda:s:u:ldu:vt:ldvt:work:lwork:rwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xgesvWithn:nrhs:a:lda:ipiv:b:ldb:info:!public! !
!LapackZLibrary categoriesFor: #xgetrfWithm:n:a:lda:ipiv:info:!public! !
!LapackZLibrary categoriesFor: #xgetriWithn:a:lda:ipiv:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xgetrsWithtrans:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackZLibrary categoriesFor: #xggbakWithjob:side:n:ilo:ihi:lscale:rscale:m:v:ldv:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xggbalWithjob:n:a:lda:b:ldb:ilo:ihi:lscale:rscale:work:info:length:!public! !
!LapackZLibrary categoriesFor: #xggevWithjobvl:jobvr:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:work:lwork:rwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:rwork:bwork:info:length:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xggevxWithbalanc:jobvl:jobvr:sense:n:a:lda:b:ldb:alpha:beta:vl:ldvl:vr:ldvr:lscale:rscale:abnrm:bbnrm:rconde:rcondv:work:lwork:rwork:iwork:bwork:info:length:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xggglmWithn:m:p:a:lda:b:ldb:d:x:y:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xgghrdWithcompq:compz:n:ilo:ihi:a:lda:b:ldb:q:ldq:z:ldz:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xgglseWithm:n:p:a:lda:b:ldb:c:d:x:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xggqrfWithn:m:p:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xggrqfWithm:p:n:a:lda:taua:b:ldb:taub:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xggsvdWithjobu:jobv:jobq:m:n:p:k:l:a:lda:b:ldb:alpha:beta:u:ldu:v:ldv:q:ldq:work:rwork:iwork:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xheconWithuplo:n:a:lda:ipiv:anorm:rcond:work:info:length:!public! !
!LapackZLibrary categoriesFor: #xheevdWithjobz:uplo:n:a:lda:w:work:lwork:rwork:lrwork:iwork:liwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xheevrWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:isuppz:work:lwork:rwork:iwork:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xheevWithjobz:uplo:n:a:lda:w:work:lwork:rwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xheevxWithjobz:range:uplo:n:a:lda:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xhegvdWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:lrwork:iwork:liwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xhegvWithitype:jobz:uplo:n:a:lda:b:ldb:w:work:lwork:rwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xhegvxWithitype:jobz:range:uplo:n:a:lda:b:ldb:vl:vu:il:iu:abstol:m:w:z:ldz:work:lwork:rwork:iwork:ifail:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xhesvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xhetrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xhetriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackZLibrary categoriesFor: #xhetrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackZLibrary categoriesFor: #xlacgvWithn:x:incx:!public! !
!LapackZLibrary categoriesFor: #xlacpyWithuplo:m:n:a:lda:b:ldb:length:!public! !
!LapackZLibrary categoriesFor: #xlangeWithnorm:m:n:a:lda:work:length:!public! !
!LapackZLibrary categoriesFor: #xlanheWithnorm:uplo:n:a:lda:work:length:length:!public! !
!LapackZLibrary categoriesFor: #xlanhpWithnorm:uplo:n:ap:work:length:length:!public! !
!LapackZLibrary categoriesFor: #xlanspWithnorm:uplo:n:ap:work:length:length:!public! !
!LapackZLibrary categoriesFor: #xlansyWithnorm:uplo:n:a:lda:work:length:length:!public! !
!LapackZLibrary categoriesFor: #xlantpWithnorm:uplo:diag:n:ap:work:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xlantrWithnorm:uplo:diag:m:n:a:lda:work:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xlarnvWithidist:iseed:n:x:!public! !
!LapackZLibrary categoriesFor: #xlasetWithuplo:m:n:alpha:beta:a:lda:length:!public! !
!LapackZLibrary categoriesFor: #xpotrfWithuplo:n:a:lda:info:length:!public! !
!LapackZLibrary categoriesFor: #xspconWithuplo:n:ap:ipiv:anorm:rcond:work:info:length:!public! !
!LapackZLibrary categoriesFor: #xspsvWithuplo:n:nrhs:ap:ipiv:b:ldb:info:length:!public! !
!LapackZLibrary categoriesFor: #xsptrfWithuplo:n:ap:ipiv:info:length:!public! !
!LapackZLibrary categoriesFor: #xsptriWithuplo:n:ap:ipiv:work:info:length:!public! !
!LapackZLibrary categoriesFor: #xsptrsWithuplo:n:nrhs:ap:ipiv:b:ldb:info:length:!public! !
!LapackZLibrary categoriesFor: #xsyconWithuplo:n:a:lda:ipiv:anorm:rcond:work:info:length:!public! !
!LapackZLibrary categoriesFor: #xsysvWithuplo:n:nrhs:a:lda:ipiv:b:ldb:work:lwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xsytrfWithuplo:n:a:lda:ipiv:work:lwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xsytriWithuplo:n:a:lda:ipiv:work:info:length:!public! !
!LapackZLibrary categoriesFor: #xsytrsWithuplo:n:nrhs:a:lda:ipiv:b:ldb:info:length:!public! !
!LapackZLibrary categoriesFor: #xtgexcWithwantq:wantz:n:a:lda:b:ldb:q:ldq:z:ldz:ifst:ilst:info:!public! !
!LapackZLibrary categoriesFor: #xtgsenWithijob:wantq:wantz:select:n:a:lda:b:ldb:alpha:beta:q:ldq:z:ldz:m:dif:work:lwork:iwork:liwork:info:!public! !
!LapackZLibrary categoriesFor: #xtgsylWithtrans:ijob:m:n:a:lda:b:ldb:c:ldc:d:ldd:e:lde:f:ldf:dif:scale:work:lwork:iwork:info:length:!public! !
!LapackZLibrary categoriesFor: #xtpconWithnorm:uplo:diag:n:ap:rcond:work:rwork:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xtptriWithuplo:diag:n:ap:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xtptrsWithuplo:trans:diag:n:nrhs:ap:b:ldb:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xtrconWithnorm:uplo:diag:n:a:lda:rcond:work:rwork:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xtrexcWithcompq:n:t:ldt:q:ldq:ifst:ilst:info:length:!public! !
!LapackZLibrary categoriesFor: #xtrsenWithjob:compq:select:n:t:ldt:q:ldq:w:m:s:sep:work:lwork:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xtrsylWithtrana:tranb:isgn:m:n:a:lda:b:ldb:c:ldc:scale:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xtrtriWithuplo:diag:n:a:lda:info:length:length:!public! !
!LapackZLibrary categoriesFor: #xtrtrsWithuplo:trans:diag:n:nrhs:a:lda:b:ldb:info:length:length:length:!public! !
!LapackZLibrary categoriesFor: #xunghrWithn:ilo:ihi:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xunglqWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xungqlWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xungqrWithm:n:k:a:lda:tau:work:lwork:info:!public! !
!LapackZLibrary categoriesFor: #xungrqWithm:n:k:a:lda:tau:work:lwork:info:!public! !

DOUBLECOMPLEX guid: (GUID fromString: '{3AB3F306-CAED-49E0-9F8D-009EC156F9A0}')!
DOUBLECOMPLEX comment: ''!
!DOUBLECOMPLEX categoriesForClass!Unclassified! !
!DOUBLECOMPLEX methodsFor!

asComplex
	"Answer a Complex of the same value as the receiver's contents."

	^self realPart i: self imaginaryPart!

imaginaryPart
	"Answer the receiver's imaginaryPart field as a Smalltalk object."

	^(bytes doubleAtOffset: 8)!

imaginaryPart: anObject
	"Set the receiver's imaginaryPart field to the value of anObject."

	bytes doubleAtOffset: 8 put: anObject!

realPart
	"Answer the receiver's realPart field as a Smalltalk object."

	^(bytes doubleAtOffset: 0)!

realPart: anObject
	"Set the receiver's realPart field to the value of anObject."

	bytes doubleAtOffset: 0 put: anObject!

value
	"Answer the receiver's value field as a Smalltalk object."

	^self asComplex! !
!DOUBLECOMPLEX categoriesFor: #asComplex!converting!public! !
!DOUBLECOMPLEX categoriesFor: #imaginaryPart!**compiled accessors**!public! !
!DOUBLECOMPLEX categoriesFor: #imaginaryPart:!**compiled accessors**!public! !
!DOUBLECOMPLEX categoriesFor: #realPart!**compiled accessors**!public! !
!DOUBLECOMPLEX categoriesFor: #realPart:!**compiled accessors**!public! !
!DOUBLECOMPLEX categoriesFor: #value!public! !

!DOUBLECOMPLEX class methodsFor!

defineFields
	"Define the fields of the DOUBLECOMPLEX structure.
	
		struct doubleComplex {
			double realPart;
			double imaginaryPart;
		} DOUBLECOMPLEX;
"

	self
		defineField: #realPart type: DOUBLEField new offset: 0;
		defineField: #imaginaryPart type: DOUBLEField new offset: 8.
	self byteSize: 16! !
!DOUBLECOMPLEX class categoriesFor: #defineFields!initializing!public! !

FLOATCOMPLEX guid: (GUID fromString: '{A2CEEB33-8D07-489E-AD7B-6410420A8143}')!
FLOATCOMPLEX comment: ''!
!FLOATCOMPLEX categoriesForClass!Unclassified! !
!FLOATCOMPLEX methodsFor!

asComplex
	"Answer a Complex of the same value as the receiver's contents."

	^self realPart i: self imaginaryPart!

imaginaryPart
	"Answer the receiver's imaginaryPart field as a Smalltalk object."

	^(bytes floatAtOffset: 4)!

imaginaryPart: anObject
	"Set the receiver's imaginaryPart field to the value of anObject."

	bytes floatAtOffset: 4 put: anObject!

realPart
	"Answer the receiver's realPart field as a Smalltalk object."

	^(bytes floatAtOffset: 0)!

realPart: anObject
	"Set the receiver's realPart field to the value of anObject."

	bytes floatAtOffset: 0 put: anObject!

value
	"Answer the receiver's value field as a Smalltalk object."

	^self asComplex! !
!FLOATCOMPLEX categoriesFor: #asComplex!converting!public! !
!FLOATCOMPLEX categoriesFor: #imaginaryPart!**compiled accessors**!public! !
!FLOATCOMPLEX categoriesFor: #imaginaryPart:!**compiled accessors**!public! !
!FLOATCOMPLEX categoriesFor: #realPart!**compiled accessors**!public! !
!FLOATCOMPLEX categoriesFor: #realPart:!**compiled accessors**!public! !
!FLOATCOMPLEX categoriesFor: #value!public! !

!FLOATCOMPLEX class methodsFor!

defineFields
	"Define the fields of the FLOATCOMPLEX structure.
	
		struct floatComplex {
			float realPart;
			float imaginaryPart;
		} FLOATCOMPLEX;
"

	self
		defineField: #realPart type: FLOATField new offset: 0;
		defineField: #imaginaryPart type: FLOATField new offset: 4.
	self byteSize: 8! !
!FLOATCOMPLEX class categoriesFor: #defineFields!initializing!public! !

DOUBLECOMPLEXArray guid: (GUID fromString: '{CA7689EF-C36B-4C70-89DA-2ACBD76B4E46}')!
DOUBLECOMPLEXArray comment: ''!
!DOUBLECOMPLEXArray categoriesForClass!Unclassified! !
!DOUBLECOMPLEXArray methodsFor!

castToRealWithArrayOffsetBy: anInteger
	| subArray subAddress |
	(anInteger > (self size * 2) or: [anInteger < 0]) ifTrue: [self error: 'array offset point out of memory'].
	subAddress := bytes yourAddress + (anInteger * self elementSize // 2).
	subArray := DOUBLEArray fromAddress: subAddress length: self size * 2 - anInteger.
	^subArray!

elementClass
	"Answer the class of <ExternalStructure> used to represent elements of the receiver."

	^DOUBLECOMPLEX!

uncheckedAt: anInteger 
	"Private - Answer a Complex number representing the 64-bit double-precision 
	IEEE float real part and 64 bits imaginary part at the specified <integer> index
	in the receiver. No bounds checks are performed on the subscript."

	^(bytes doubleAtOffset: (anInteger - 1) * 16) i: (bytes doubleAtOffset: (anInteger - 1) * 16 + 8)!

uncheckedAt: anInteger put: aComplex
	"Private - Store a Complex number representing the 64-bit double-precision 
	IEEE float real part and 64 bits imaginary part at the specified <integer> index
	in the receiver. No bounds checks are performed on the subscript."

	| tmp |
	tmp := aComplex asComplex.
	bytes doubleAtOffset: (anInteger - 1) * 16 put: tmp realPart asFloat.
	bytes doubleAtOffset: (anInteger - 1) * 16 + 8 put: tmp imaginaryPart asFloat.
	^aComplex! !
!DOUBLECOMPLEXArray categoriesFor: #castToRealWithArrayOffsetBy:!converting!public! !
!DOUBLECOMPLEXArray categoriesFor: #elementClass!constants!public! !
!DOUBLECOMPLEXArray categoriesFor: #uncheckedAt:!accessing!private! !
!DOUBLECOMPLEXArray categoriesFor: #uncheckedAt:put:!accessing!private! !

!DOUBLECOMPLEXArray class methodsFor!

elementSize
	"Private - Answer the size of the receiver's constituent elements."
	
	^16! !
!DOUBLECOMPLEXArray class categoriesFor: #elementSize!constants!private! !

FLOATCOMPLEXArray guid: (GUID fromString: '{91EF9F8C-B6E0-40E8-9981-8D615B6760AD}')!
FLOATCOMPLEXArray comment: ''!
!FLOATCOMPLEXArray categoriesForClass!Unclassified! !
!FLOATCOMPLEXArray methodsFor!

castToRealWithArrayOffsetBy: anInteger
	| subArray subAddress |
	(anInteger > (self size * 2) or: [anInteger < 0]) ifTrue: [self error: 'array offset point out of memory'].
	subAddress := bytes yourAddress + (anInteger * self elementSize // 2).
	subArray := FLOATArray fromAddress: subAddress length: self size * 2 - anInteger.
	^subArray!

elementClass
	"Answer the class of <ExternalStructure> used to represent elements of the receiver."

	^FLOATCOMPLEX!

uncheckedAt: anInteger 
	"Private - Answer a Complex number representing the 32-bit double-precision 
	IEEE float real part and 32 bits imaginary part at the specified <integer> index
	in the receiver. No bounds checks are performed on the subscript."

	^(bytes floatAtOffset: (anInteger - 1) * 8) i: (bytes floatAtOffset: (anInteger - 1) * 8 + 4)!

uncheckedAt: anInteger put: aComplex
	"Private - Store a Complex number representing the 32-bit double-precision 
	IEEE float real part and 64 bits imaginary part at the specified <integer> index
	in the receiver. No bounds checks are performed on the subscript."

	| tmp |
	tmp := aComplex asComplex.
	bytes floatAtOffset: (anInteger - 1) * 8 put: tmp realPart asFloat.
	bytes floatAtOffset: (anInteger - 1) * 8 + 4 put: tmp imaginaryPart asFloat.
	^aComplex! !
!FLOATCOMPLEXArray categoriesFor: #castToRealWithArrayOffsetBy:!converting!public! !
!FLOATCOMPLEXArray categoriesFor: #elementClass!constants!public! !
!FLOATCOMPLEXArray categoriesFor: #uncheckedAt:!accessing!private! !
!FLOATCOMPLEXArray categoriesFor: #uncheckedAt:put:!accessing!private! !

!FLOATCOMPLEXArray class methodsFor!

elementSize
	"Private - Answer the size of the receiver's constituent elements."
	
	^8! !
!FLOATCOMPLEXArray class categoriesFor: #elementSize!constants!private! !

"Binary Globals"!

