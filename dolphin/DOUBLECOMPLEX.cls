"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #DOUBLECOMPLEX
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

