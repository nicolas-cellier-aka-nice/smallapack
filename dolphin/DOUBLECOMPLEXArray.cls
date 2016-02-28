"Filed out from Dolphin Smalltalk 7"!

ExternalArray subclass: #DOUBLECOMPLEXArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

