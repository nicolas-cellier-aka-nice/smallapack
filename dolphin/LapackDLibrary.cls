"Filed out from Dolphin Smalltalk 7"!

LapackLibrary subclass: #LapackDLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

