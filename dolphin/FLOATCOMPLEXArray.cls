"Filed out from Dolphin Smalltalk 7"!

ExternalArray subclass: #FLOATCOMPLEXArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

