"Filed out from Dolphin Smalltalk 7"!

LapackLibrary subclass: #LapackCLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

