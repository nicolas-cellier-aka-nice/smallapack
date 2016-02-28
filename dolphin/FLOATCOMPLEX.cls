"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #FLOATCOMPLEX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

