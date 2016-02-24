"Filed out from Dolphin Smalltalk 7"!

CMathLibrary subclass: #LapackLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

gebakWithjob: job side: side n: n ilo: ilo ihi: ihi scale: scale m: m v: v ldv: ldv 	"balance back a general matrix	ilo and ihi must be allocated outside this routine"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgebakWithjob: (self cCharPointerOn: job)		side: (self cCharPointerOn: side)		n: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		scale: scale		m: (self cIntegerPointerOn: m)		v: v		ldv: (self cIntegerPointerOn: ldv)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gebalWithjob: job n: n a: a lda: lda ilo: ilo ihi: ihi scale: scale 	"balance a general matrix	ilo and ihi must be allocated outside this routine"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgebalWithjob: (self cCharPointerOn: job)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ilo: ilo		ihi: ihi		scale: scale		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

geconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeconWithnorm: (self cCharPointerOn: norm)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		anorm: (self cElementPointerOn: anorm)		rcond: rcond		work: work		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

geconWithnorm: norm n: n a: a lda: lda anorm: anorm rcond: rcond work: work rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeconWithnorm: (self cCharPointerOn: norm)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		anorm: (self cRealPointerOn: anorm)		rcond: rcond		work: work		rwork: rwork		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim w: w vs: vs ldvs: ldvs 	"schur decomposition (for complex general matrices)	Note: sdim must be allocated outside	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork bwork lwork |	work := self allocateElementArraySize: 1.	bwork := self allocateLogicalArraySize: n.	rwork := self allocateRealArraySize: n.	(self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		w: w		vs: vs		ldvs: ldvs		work: work		lwork: -1		rwork: rwork		bwork: bwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 2 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		w: w		vs: vs		ldvs: ldvs		work: work		lwork: lwork		rwork: rwork		bwork: bwork!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim w: w vs: vs ldvs: ldvs work: work lwork: lwork rwork: rwork bwork: bwork 	"schur decomposition (for complex general matrices)	Note: sdim must be allocated outsied"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self 		xgeesWithjobvs: (self cCharPointerOn: jobvs)		sort: (self cCharPointerOn: sort)		select: select		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		sdim: sdim		w: w		vs: vs		ldvs: (self cIntegerPointerOn: ldvs)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		bwork: bwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs 	"schur decomposition (for real general matrices)	Note: sdim must be allocated outside	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work bwork lwork |	work := self allocateElementArraySize: 1.	bwork := self allocateLogicalArraySize: n.	(self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		wr: wr		wi: wi		vs: vs		ldvs: ldvs		work: work		lwork: -1		bwork: bwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 3 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geesWithjobvs: jobvs		sort: sort		select: select		n: n		a: a		lda: lda		sdim: sdim		wr: wr		wi: wi		vs: vs		ldvs: ldvs		work: work		lwork: lwork		bwork: bwork!

geesWithjobvs: jobvs sort: sort select: select n: n a: a lda: lda sdim: sdim wr: wr wi: wi vs: vs ldvs: ldvs work: work lwork: lwork bwork: bwork 	"schur decomposition (for real general matrices)	Note: sdim must be allocated outsied"	| cARGjobvs cARGsort cARGn cARGlda cARGldvs cARGlwork cARGinfo |	cARGjobvs := self cCharPointerOn: jobvs.	cARGsort := self cCharPointerOn: sort.	cARGn := self cIntegerPointerOn: n.	cARGlda := self cIntegerPointerOn: lda.	cARGldvs := self cIntegerPointerOn: ldvs.	cARGlwork := self cIntegerPointerOn: lwork.	cARGinfo := self cIntegerPointerOn: 0.	self 		xgeesWithjobvs: cARGjobvs		sort: cARGsort		select: select		n: cARGn		a: a		lda: cARGlda		sdim: sdim		wr: wr		wi: wi		vs: vs		ldvs: cARGldvs		work: work		lwork: cARGlwork 		bwork: bwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"eigenvalue/vector decomposition (for complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: 2 * n.	(self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		w: w		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1		rwork: rwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 2 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		w: w		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork		rwork: rwork!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork 	"eigenvalue/vector decomposition (for complex general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"eigenvalue/vector decomposition (for real general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		wr: wr		wi: wi		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		geevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		wr: wr		wi: wi		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork!

geevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda wr: wr wi: wi vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork 	"eigenvalue/vector decomposition (for real general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		wr: wr		wi: wi		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

geevxWithbalanc: balanc jobvl: jobvl jobvr: jobvr sense: sense n: n a: a lda: lda w: w vl: vl ldvl: ldvl vr: vr ldvr: ldvr scale: scale abnrm: abnrm rconde: rconde rcondv: rcondv work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeevxWithbalanc: (self cCharPointerOn: balanc)		jobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		sense: (self cCharPointerOn: sense)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		scale: scale		abnrm: abnrm		rconde: rconde		rcondv: rcondv		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gehrdWithn: n a: a lda: lda tau: tau 	"hessenberg decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork ilo ihi |	work := self allocateElementArraySize: 1.	ilo := self cIntegerPointerOn: 1.	ihi := self cIntegerPointerOn: n.	(self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

gehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau 	"hessenberg decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK	ilo and ihi must be allocated outside"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		gehrdWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

gehrdWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork 	"hessenberg decomposition (for real/complex general matrices)	ilo and ihi must be allocated outside this routine"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgehrdWithn: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelqfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank 	"least squares by SVD divide and conquer algorithm	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork iwork smlsiz nlvl minmn |	minmn := 1 max: (m min: n).	^self isComplex 		ifTrue: 			[			smlsiz := self 						ilaenvWithispec: 9						name: 'CGELSD'						opts: ' '						n1: 0						n2: 0						n3: 0						n4: 0.			nlvl := 1 + (minmn / (smlsiz + 1)) floorLog2.			work := self allocateElementArraySize: 1.			iwork := self allocateIntegerArraySize: 3 * minmn * nlvl + (11 * minmn).			rwork := self allocateRealArraySize: (m >= n 								ifTrue: [8 * n + (2 * n * smlsiz) + (8 * n * nlvl) + (n * nrhs)]								ifFalse: [8 * m + (2 * m * smlsiz) + (8 * m * nlvl) + (m * nrhs)]).			(self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: -1				rwork: rwork				iwork: iwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: 					[lwork := m >= n 								ifTrue: [1 max: n * (2 + nrhs)]								ifFalse: [1 max: m * (2 + nrhs)]].			work := self allocateElementArraySize: lwork.			self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				rwork: rwork				iwork: iwork] 		ifFalse: 			[			smlsiz := self 						ilaenvWithispec: 9						name: 'SGELSD'						opts: ' '						n1: 0						n2: 0						n3: 0						n4: 0.			nlvl := 1 + (minmn / (smlsiz + 1)) floorLog2.			work := self allocateElementArraySize: 1.			iwork := self allocateIntegerArraySize: 3 * minmn * nlvl + (11 * minmn).			(self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				iwork: iwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: 					[lwork := m >= n 								ifTrue: [11 * n + (2 * n * smlsiz) + (8 * n * nlvl) + (n * nrhs)]								ifFalse: [11 * m + (2 * m * smlsiz) + (8 * m * nlvl) + (m * nrhs)]].			work := self allocateElementArraySize: lwork.			self 				gelsdWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				iwork: iwork]!

gelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsdWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelsdWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsdWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank 	"least squares by SVD	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: (5 * (m min: n) max: 1).			(self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 1 max: (m min: n) * 2 + (nrhs max: (m max: n))].			work := self allocateElementArraySize: lwork.			self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: 					[lwork := 1 								max: (m min: n) * 3 + ((nrhs max: (m max: n)) max: 2 * (m min: n))].			work := self allocateElementArraySize: lwork.			self 				gelssWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				s: s				rcond: rcond				rank: rank				work: work				lwork: lwork]!

gelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelssWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelssWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb s: s rcond: rcond rank: rank work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelssWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		s: s		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelsWithtrans: trans m: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsWithtrans: (self cCharPointerOn: trans)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsxWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelsxWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsxWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		rwork: rwork		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank 	"least squares by complete orthogonalization	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| mn work rwork lwork |	mn := m min: n.	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: (2 * n max: 1).			(self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 1 max: mn + (2 * mn max: (n + 1 max: mn + nrhs))].			work := self allocateElementArraySize: lwork.			self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 1 max: (n * 3 + 1 + mn max: mn * 2 + nrhs)].			work := self allocateElementArraySize: lwork.			self 				gelsyWithm: m				n: n				nrhs: nrhs				a: a				lda: lda				b: b				ldb: ldb				jpvt: jpvt				rcond: rcond				rank: rank				work: work				lwork: lwork]!

gelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsyWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cElementPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gelsyWithm: m n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb jpvt: jpvt rcond: rcond rank: rank work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgelsyWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		jpvt: jpvt		rcond: (self cRealPointerOn: rcond)		rank: rank		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

geqlfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqlfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

geqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau 	"qr decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: 2 * n.			(self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := n + 1].			work := self allocateElementArraySize: lwork.			self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 3 * n + 1].			work := self allocateElementArraySize: lwork.			self 				geqp3Withm: m				n: n				a: a				lda: lda				jpvt: jpvt				tau: tau				work: work				lwork: lwork]!

geqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqp3Withm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		jpvt: jpvt		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

geqp3Withm: m n: n a: a lda: lda jpvt: jpvt tau: tau work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqp3Withm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		jpvt: jpvt		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

geqrfWithm: m n: n a: a lda: lda tau: tau 	"qr decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		geqrfWithm: m		n: n		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		geqrfWithm: m		n: n		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

geqrfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	"qr decomposition (for real/complex general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgeqrfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gerqfWithm: m n: n a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgerqfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesddWithjobz: (self cCharPointerOn: jobz)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gesddWithjobz: jobz m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesddWithjobz: (self cCharPointerOn: jobz)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt 	"singular value/vector decomposition (for real/complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	^self isComplex 		ifTrue: 			[			work := self allocateElementArraySize: 1.			rwork := self allocateRealArraySize: 5 * (m min: n).			(self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: -1				rwork: rwork) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 2 * (m min: n) + (m max: n)].			work := self allocateElementArraySize: lwork.			self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: lwork				rwork: rwork]		ifFalse: 			[			work := self allocateElementArraySize: 1.			(self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: -1) = 0 				ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]				ifFalse: [lwork := 3 * (m min: n) + (m max: n) max: 5 * (m min: n)].			work := self allocateElementArraySize: lwork.			self 				gesvdWithjobu: jobu				jobvt: jobvt				m: m				n: n				a: a				lda: lda				s: s				u: u				ldu: ldu				vt: vt				ldvt: ldvt				work: work				lwork: lwork]!

gesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork 	"singular value/vector decomposition (for real general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesvdWithjobu: (self cCharPointerOn: jobu)		jobvt: (self cCharPointerOn: jobvt)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gesvdWithjobu: jobu jobvt: jobvt m: m n: n a: a lda: lda s: s u: u ldu: ldu vt: vt ldvt: ldvt work: work lwork: lwork rwork: rwork 	"singular value/vector decomposition (for complex general matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesvdWithjobu: (self cCharPointerOn: jobu)		jobvt: (self cCharPointerOn: jobvt)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		s: s		u: u		ldu: (self cIntegerPointerOn: ldu)		vt: vt		ldvt: (self cIntegerPointerOn: ldvt)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gesvWithn: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	"find x such that a*x=b, that is solve a linear system of equations	on return:		x is stored in b on return		a contains L and U of P*L*U decomposition (diagonal 1 of L not included)		ipiv contains permutations of rows P of P*L*U decomposition	return:		info contains error code is not 0"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgesvWithn: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

getrfWithm: m n: n a: a lda: lda ipiv: ipiv 	"P*L*U decomposition of a general matrix	return 0 if OK"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgetrfWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

getriWithn: n a: a lda: lda ipiv: ipiv 	"inverse of a general matrix"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		getriWithn: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 10 * n max: 1].	work := self allocateElementArraySize: lwork.	^self 		getriWithn: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: lwork!

getriWithn: n a: a lda: lda ipiv: ipiv work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgetriWithn: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

getrsWithtrans: trans n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	"Solve a general system of equations A*x=B in x"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgetrsWithtrans: (self cCharPointerOn: trans)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

ggbakWithjob: job side: side n: n ilo: ilo ihi: ihi lscale: lscale rscale: rscale m: m v: v ldv: ldv 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggbakWithjob: (self cCharPointerOn: job)		side: (self cCharPointerOn: side)		n: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		lscale: lscale		rscale: rscale		m: (self cIntegerPointerOn: m)		v: v		ldv: (self cIntegerPointerOn: ldv)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

ggbalWithjob: job n: n a: a lda: lda b: b ldb: ldb ilo: ilo ihi: ihi lscale: lscale rscale: rscale work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggbalWithjob: (self cCharPointerOn: job)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		ilo: ilo		ihi: ihi		lscale: lscale		rscale: rscale		work: work		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"generalized eigenvalue/vector decomposition (for complex general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: 8 * n.	(self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alpha: alpha		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1		rwork: rwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 1 max: 2 * n].	work := self allocateElementArraySize: lwork.	^self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alpha: alpha		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork		rwork: rwork!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr 	"generalized eigenvalue/vector decomposition (for real general matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alphar: alphar		alphai: alphai		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 1 max: 8 * n * n].	work := self allocateElementArraySize: lwork.	^self 		ggevWithjobvl: jobvl		jobvr: jobvr		n: n		a: a		lda: lda		b: b		ldb: ldb		alphar: alphar		alphai: alphai		beta: beta		vl: vl		ldvl: ldvl		vr: vr		ldvr: ldvr		work: work		lwork: lwork!

ggevWithjobvl: jobvl jobvr: jobvr n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta vl: vl ldvl: ldvl vr: vr ldvr: ldvr work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggevWithjobvl: (self cCharPointerOn: jobvl)		jobvr: (self cCharPointerOn: jobvr)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alphar: alphar		alphai: alphai		beta: beta		vl: vl		ldvl: (self cIntegerPointerOn: ldvl)		vr: vr		ldvr: (self cIntegerPointerOn: ldvr)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

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

ggglmWithn: n m: m p: p a: a lda: lda b: b ldb: ldb d: d x: x y: y work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggglmWithn: (self cIntegerPointerOn: n)		m: (self cIntegerPointerOn: m)		p: (self cIntegerPointerOn: p)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		d: d		x: x		y: y		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

gghrdWithcompq: compq compz: compz n: n ilo: ilo ihi: ihi a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgghrdWithcompq: (self cCharPointerOn: compq)		compz: (self cCharPointerOn: compz)		n: (self cIntegerPointerOn: n)		ilo: (self cIntegerPointerOn: ilo)		ihi: (self cIntegerPointerOn: ihi)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

gglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x 	"least squares subject to equality constraints by orthogonalization	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"		| work lwork |	work := self allocateElementArraySize: 1.	(self			gglseWithm: m			n: n			p: p			a: a			lda: lda			b: b			ldb: ldb			c: c			d: d			x: x			work: work			lwork: -1)			= 0		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 1 max: m + n + p].	work := self allocateElementArraySize: lwork.	^self		gglseWithm: m		n: n		p: p		a: a		lda: lda		b: b		ldb: ldb		c: c		d: d		x: x		work: work		lwork: lwork!

gglseWithm: m n: n p: p a: a lda: lda b: b ldb: ldb c: c d: d x: x work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xgglseWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		p: (self cIntegerPointerOn: p)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		c: c		d: d		x: x		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

ggqrfWithn: n m: m p: p a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggqrfWithn: (self cIntegerPointerOn: n)		m: (self cIntegerPointerOn: m)		p: (self cIntegerPointerOn: p)		a: a		lda: (self cIntegerPointerOn: lda)		taua: taua		b: b		ldb: (self cIntegerPointerOn: ldb)		taub: taub		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

ggrqfWithm: m p: p n: n a: a lda: lda taua: taua b: b ldb: ldb taub: taub work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggrqfWithm: (self cIntegerPointerOn: m)		p: (self cIntegerPointerOn: p)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		taua: taua		b: b		ldb: (self cIntegerPointerOn: ldb)		taub: taub		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

ggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq	"Generalized singular value/vector decomposition (for real/complex general matrices)	return 0 if OK"	| work rwork iwork |	^self isComplex 		ifTrue: 			[work := self allocateElementArraySize: ((3 * n max: m) max: p)+n.			rwork := self allocateRealArraySize: 2 * n.			iwork := self allocateIntegerArraySize: n.			self 				ggsvdWithjobu: jobu				jobv: jobv				jobq: jobq				m: m				n: n				p: p				k: k				l: l				a: a				lda: lda				b: b				ldb: ldb				alpha: alpha				beta: beta				u: u				ldu: ldu				v: v				ldv: ldv				q: q				ldq: ldq				work: work				rwork: rwork				iwork: iwork]		ifFalse: 			[work := self allocateElementArraySize: ((3*n max: m) max: p)+n.			iwork := self allocateIntegerArraySize: n.			self 				ggsvdWithjobu: jobu				jobv: jobv				jobq: jobq				m: m				n: n				p: p				k: k				l: l				a: a				lda: lda				b: b				ldb: ldb				alpha: alpha				beta: beta				u: u				ldu: ldu				v: v				ldv: ldv				q: q				ldq: ldq				work: work				iwork: iwork]!

ggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggsvdWithjobu: (self cCharPointerOn: jobu)		jobv: (self cCharPointerOn: jobv)		jobq: (self cCharPointerOn: jobq)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		p: (self cIntegerPointerOn: p)		k: k		l: l		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		u: u		ldu: (self cIntegerPointerOn: ldu)		v: v		ldv: (self cIntegerPointerOn: ldv)		q: q		ldq: (self cIntegerPointerOn: ldq)		work: work		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

ggsvdWithjobu: jobu jobv: jobv jobq: jobq m: m n: n p: p k: k l: l a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta u: u ldu: ldu v: v ldv: ldv q: q ldq: ldq work: work rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xggsvdWithjobu: (self cCharPointerOn: jobu)		jobv: (self cCharPointerOn: jobv)		jobq: (self cCharPointerOn: jobq)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		p: (self cIntegerPointerOn: p)		k: k		l: l		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		u: u		ldu: (self cIntegerPointerOn: ldu)		v: v		ldv: (self cIntegerPointerOn: ldv)		q: q		ldq: (self cIntegerPointerOn: ldq)		work: work		rwork: rwork		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

heconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheconWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		anorm: (self cRealPointerOn: anorm)		rcond: rcond		work: work		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

heevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for real hermitian matrices)	use divide and conquer algorithm	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork iwork lwork lrwork liwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: 1.	iwork := self allocateIntegerArraySize: 1.	(self 		heevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1		rwork: rwork		lrwork: -1		iwork: iwork		liwork: -1) = 0 		ifTrue: 			[lwork := self retrieveLengthQueryAnswerFrom: work.			lrwork := self retrieveLengthQueryAnswerFrom: rwork.			liwork := iwork getHandle signedLongAt: 1]		ifFalse: 			[lwork := 1 						max: (jobz = self doComputeVector ifTrue: [n * n + (2 * n)] ifFalse: [n + 1]).			lrwork := 1 max: (jobz = self doComputeVector 								ifTrue: [2 * n * n + (5 * n) + 1]								ifFalse: [n]).			liwork := 1 						max: (jobz = self doComputeVector ifTrue: [5 * n + 3] ifFalse: [1])].	work := self allocateElementArraySize: lwork.	rwork := self allocateRealArraySize: lrwork.	iwork := self allocateIntegerArraySize: liwork.	^self 		heevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork		rwork: rwork		lrwork: lrwork		iwork: iwork		liwork: liwork!

heevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheevdWithjobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		lrwork: (self cIntegerPointerOn: lrwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

heevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork rwork: rwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheevrWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cRealPointerOn: vl)		vu: (self cRealPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cRealPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		isuppz: isuppz		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

heevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for complex hermitian matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work rwork lwork |	work := self allocateElementArraySize: 1.	rwork := self allocateRealArraySize: (3 * n - 2 max: 1).	(self 		heevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1		rwork: rwork) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 2 * n - 1 max: 1].	work := self allocateElementArraySize: lwork.	^self 		heevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork		rwork: rwork!

heevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork rwork: rwork 	"eigenvalue/vector decomposition (for complex hermitian matrices)"	| cARGjobz cARGuplo cARGn cARGlda cARGlwork cARGinfo |	cARGjobz := self cCharPointerOn: jobz.	cARGuplo := self cCharPointerOn: uplo.	cARGn := self cIntegerPointerOn: n.	cARGlda := self cIntegerPointerOn: lda.	cARGlwork := self cIntegerPointerOn: lwork.	cARGinfo := self cIntegerPointerOn: 0.	self 		xheevWithjobz: cARGjobz		uplo: cARGuplo		n: cARGn		a: a		lda: cARGlda		w: w		work: work		lwork: cARGlwork		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

heevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xheevxWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cRealPointerOn: vl)		vu: (self cRealPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cRealPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

hegvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork lrwork: lrwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhegvdWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		lrwork: (self cIntegerPointerOn: lrwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

hegvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhegvWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

hegvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork rwork: rwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhegvxWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		vl: (self cRealPointerOn: vl)		vu: (self cRealPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cRealPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		rwork: rwork		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

hesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	"Solve x such that A*x = B with A hermitian	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		hesvWithuplo: uplo		n: n		nrhs: nrhs		a: a		lda: lda		ipiv: ipiv		b: b		ldb: ldb		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		hesvWithuplo: uplo		n: n		nrhs: nrhs		a: a		lda: lda		ipiv: ipiv		b: b		ldb: ldb		work: work		lwork: lwork!

hesvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork 	"find x such that a*x=b, that is solve a linear system of equations	on return:		x is stored in b on return		a contains L and U of P*L*U decomposition (diagonal 1 of L not included)		ipiv contains permutations of rows P of P*L*U decomposition	return:		info contains error code is not 0"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhesvWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

hetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"P*L*U decomposition of a hermitian matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		hetrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		hetrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: lwork!

hetrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork 	"P*L*U decomposition of a hermitian matrix	return 0 if OK"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhetrfWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

hetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"inverse of a hermitian matrix"	| work |	work := self allocateElementArraySize: n.	^self 		hetriWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work!

hetriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhetriWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

hetrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xhetrsWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

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

orghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		orghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := (ihi getHandle signedLongAt: 1) - (ilo getHandle signedLongAt: 1) max: 1].	work := self allocateElementArraySize: lwork.	^self 		orghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

orghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix	ilo and ihi must be allocated outside"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorghrWithn: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

orglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorglqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

orgqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorgqlWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

orgqrWithm: m n: n k: k a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		orgqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		orgqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

orgqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorgqrWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

orgrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xorgrqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

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

syconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyconWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		anorm: (self cRealPointerOn: anorm)		rcond: rcond		work: work		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

syconWithuplo: uplo n: n a: a lda: lda ipiv: ipiv anorm: anorm rcond: rcond work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyconWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		anorm: (self cElementPointerOn: anorm)		rcond: rcond		work: work		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

syevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for real symmetric matrices)	use divide and conquer algorithm	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work iwork lwork liwork |	work := self allocateElementArraySize: 1.	iwork := self allocateIntegerArraySize: 1.	(self 		syevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1		iwork: iwork		liwork: -1) = 0 		ifTrue: 			[lwork := self retrieveLengthQueryAnswerFrom: work.			liwork := iwork getHandle signedLongAt: 1]		ifFalse: 			[lwork := 1 max: (jobz = self doComputeVector 								ifTrue: [2 * n * n + (6 * n) + 1]								ifFalse: [2 * n + 1]).			liwork := 1 max: (jobz = self doComputeVector ifTrue: [5 * n + 3] ifFalse: [1])].	work := self allocateElementArraySize: lwork.	iwork := self allocateIntegerArraySize: liwork.	^self 		syevdWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork		iwork: iwork		liwork: liwork!

syevdWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevdWithjobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

syevrWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz isuppz: isuppz work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevrWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cElementPointerOn: vl)		vu: (self cElementPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cElementPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		isuppz: isuppz		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

syevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w 	"eigenvalue/vector decomposition (for real symmetric matrices)	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		syevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := 3 * n - 1 max: 1].	work := self allocateElementArraySize: lwork.	^self 		syevWithjobz: jobz		uplo: uplo		n: n		a: a		lda: lda		w: w		work: work		lwork: lwork!

syevWithjobz: jobz uplo: uplo n: n a: a lda: lda w: w work: work lwork: lwork 	"eigenvalue/vector decomposition (for real symmetric matrices)"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevWithjobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

syevxWithjobz: jobz range: range uplo: uplo n: n a: a lda: lda vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsyevxWithjobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		vl: (self cElementPointerOn: vl)		vu: (self cElementPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cElementPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

sygvdWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsygvdWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

sygvWithitype: itype jobz: jobz uplo: uplo n: n a: a lda: lda b: b ldb: ldb w: w work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsygvWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		w: w		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

sygvxWithitype: itype jobz: jobz range: range uplo: uplo n: n a: a lda: lda b: b ldb: ldb vl: vl vu: vu il: il iu: iu abstol: abstol m: m w: w z: z ldz: ldz work: work lwork: lwork iwork: iwork ifail: ifail 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsygvxWithitype: (self cIntegerPointerOn: itype)		jobz: (self cCharPointerOn: jobz)		range: (self cCharPointerOn: range)		uplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		vl: (self cElementPointerOn: vl)		vu: (self cElementPointerOn: vu)		il: (self cIntegerPointerOn: il)		iu: (self cIntegerPointerOn: iu)		abstol: (self cElementPointerOn: abstol)		m: m		w: w		z: z		ldz: (self cIntegerPointerOn: ldz)		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		ifail: ifail		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

sysvWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsysvWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

sytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"P*L*U decomposition of a syrmitian matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		sytrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n * n].	work := self allocateElementArraySize: lwork.	^self 		sytrfWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work		lwork: lwork!

sytrfWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work lwork: lwork 	"P*L*U decomposition of a symmetric matrix	return 0 if OK"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsytrfWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

sytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv 	"inverse of a symmetric matrix"	| work |	work := self allocateElementArraySize: n.	^self 		sytriWithuplo: uplo		n: n		a: a		lda: lda		ipiv: ipiv		work: work!

sytriWithuplo: uplo n: n a: a lda: lda ipiv: ipiv work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsytriWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		work: work		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

sytrsWithuplo: uplo n: n nrhs: nrhs a: a lda: lda ipiv: ipiv b: b ldb: ldb 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xsytrsWithuplo: (self cCharPointerOn: uplo)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		ipiv: ipiv		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

tgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgexcWithwantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		ifst: ifst		ilst: ilst		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

tgexcWithwantq: wantq wantz: wantz n: n a: a lda: lda b: b ldb: ldb q: q ldq: ldq z: z ldz: ldz ifst: ifst ilst: ilst work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgexcWithwantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		ifst: ifst		ilst: ilst		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

tgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alpha: alpha beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgsenWithijob: (self cIntegerPointerOn: ijob)		wantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		select: select		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alpha: alpha		beta: beta		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		m: m		dif: dif		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

tgsenWithijob: ijob wantq: wantq wantz: wantz select: select n: n a: a lda: lda b: b ldb: ldb alphar: alphar alphai: alphai beta: beta q: q ldq: ldq z: z ldz: ldz m: m dif: dif work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgsenWithijob: (self cIntegerPointerOn: ijob)		wantq: (self cLogicalPointerOn: wantq)		wantz: (self cLogicalPointerOn: wantz)		select: select		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		alphar: alphar		alphai: alphai		beta: beta		q: q		ldq: (self cIntegerPointerOn: ldq)		z: z		ldz: (self cIntegerPointerOn: ldz)		m: m		dif: dif		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

tgsylWithtrans: trans ijob: ijob m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc d: d ldd: ldd e: e lde: lde f: f ldf: ldf dif: dif scale: scale work: work lwork: lwork iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtgsylWithtrans: (self cCharPointerOn: trans)		ijob: (self cIntegerPointerOn: ijob)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		c: c		ldc: (self cIntegerPointerOn: ldc)		d: d		ldd: (self cIntegerPointerOn: ldd)		e: e		lde: (self cIntegerPointerOn: lde)		f: f		ldf: (self cIntegerPointerOn: ldf)		dif: dif		scale: scale		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

transposeConjugated	^$C!

transposed	^$T!

trconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work iwork: iwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrconWithnorm: (self cCharPointerOn: norm)		uplo: (self cCharPointerOn: uplo)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		rcond: rcond		work: work		iwork: iwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trconWithnorm: norm uplo: uplo diag: diag n: n a: a lda: lda rcond: rcond work: work rwork: rwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrconWithnorm: (self cCharPointerOn: norm)		uplo: (self cCharPointerOn: uplo)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		rcond: rcond		work: work		rwork: rwork		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrexcWithcompq: (self cCharPointerOn: compq)		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		ifst: (self cIntegerPointerOn: ifst)		ilst: (self cIntegerPointerOn: ilst)		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trexcWithcompq: compq n: n t: t ldt: ldt q: q ldq: ldq ifst: ifst ilst: ilst work: work 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrexcWithcompq: (self cCharPointerOn: compq)		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		ifst: ifst		ilst: ilst		work: work		info: cARGinfo		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq w: w m: m s: s sep: sep work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrsenWithjob: (self cCharPointerOn: job)		compq: (self cCharPointerOn: compq)		select: select		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		w: w		m: m		s: s		sep: sep		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trsenWithjob: job compq: compq select: select n: n t: t ldt: ldt q: q ldq: ldq wr: wr wi: wi m: m s: s sep: sep work: work lwork: lwork iwork: iwork liwork: liwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrsenWithjob: (self cCharPointerOn: job)		compq: (self cCharPointerOn: compq)		select: select		n: (self cIntegerPointerOn: n)		t: t		ldt: (self cIntegerPointerOn: ldt)		q: q		ldq: (self cIntegerPointerOn: ldq)		wr: wr		wi: wi		m: m		s: s		sep: sep		work: work		lwork: (self cIntegerPointerOn: lwork)		iwork: iwork		liwork: (self cIntegerPointerOn: liwork)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trsylWithtrana: trana tranb: tranb isgn: isgn m: m n: n a: a lda: lda b: b ldb: ldb c: c ldc: ldc scale: scale 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrsylWithtrana: (self cCharPointerOn: trana)		tranb: (self cCharPointerOn: tranb)		isgn: (self cIntegerPointerOn: isgn)		m: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		c: c		ldc: (self cIntegerPointerOn: ldc)		scale: scale		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trtriWithuplo: uplo diag: diag n: n a: a lda: lda 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrtriWithuplo: (self cCharPointerOn: uplo)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		a: a		lda: (self cIntegerPointerOn: lda)		info: cARGinfo		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

trtrsWithuplo: uplo trans: trans diag: diag n: n nrhs: nrhs a: a lda: lda b: b ldb: ldb 	"Solve a triangular system of equations A*x=B in x"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xtrtrsWithuplo: (self cCharPointerOn: uplo)		trans: (self cCharPointerOn: trans)		diag: (self cCharPointerOn: diag)		n: (self cIntegerPointerOn: n)		nrhs: (self cIntegerPointerOn: nrhs)		a: a		lda: (self cIntegerPointerOn: lda)		b: b		ldb: (self cIntegerPointerOn: ldb)		info: cARGinfo		length: 1		length: 1		length: 1.	^cARGinfo getHandle signedLongAt: 1!

unghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		unghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := (ihi getHandle signedLongAt: 1) - (ilo getHandle signedLongAt: 1) max: 1].	work := self allocateElementArraySize: lwork.	^self 		unghrWithn: n		ilo: ilo		ihi: ihi		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

unghrWithn: n ilo: ilo ihi: ihi a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix	ilo and ihi must be allocated outside"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xunghrWithn: (self cIntegerPointerOn: n)		ilo: ilo		ihi: ihi		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

unglqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xunglqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

ungqlWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xungqlWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

ungqrWithm: m n: n k: k a: a lda: lda tau: tau 	"generate orthonormal matrix	use LAPACK 3.0 feature to get optimized lwork	return 0 if OK"	| work lwork |	work := self allocateElementArraySize: 1.	(self 		ungqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: -1) = 0 		ifTrue: [lwork := self retrieveLengthQueryAnswerFrom: work]		ifFalse: [lwork := n max: 1].	work := self allocateElementArraySize: lwork.	^self 		ungqrWithm: m		n: n		k: k		a: a		lda: lda		tau: tau		work: work		lwork: lwork!

ungqrWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	"build orthonormal matrix"	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xungqrWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

ungrqWithm: m n: n k: k a: a lda: lda tau: tau work: work lwork: lwork 	| cARGinfo |	cARGinfo := self cIntegerPointerOn: 0.	self		xungrqWithm: (self cIntegerPointerOn: m)		n: (self cIntegerPointerOn: n)		k: (self cIntegerPointerOn: k)		a: a		lda: (self cIntegerPointerOn: lda)		tau: tau		work: work		lwork: (self cIntegerPointerOn: lwork)		info: cARGinfo.	^cARGinfo getHandle signedLongAt: 1!

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

	^'LAPACK30'! !
!LapackLibrary class categoriesFor: #fileName!public! !

