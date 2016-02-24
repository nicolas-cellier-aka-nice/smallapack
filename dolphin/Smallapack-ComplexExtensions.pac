| package |
package := Package name: 'Smallapack-ComplexExtensions'.
package paxVersion: 1;
	basicComment: 'This add a few messages to Complex required by Smallapack'.


package methodNames
	add: #Complex -> #conjugated;
	add: #Complex -> #i;
	add: #Number -> #addToComplex:;
	add: #Number -> #i;
	add: #Number -> #i:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Dolphin Smalltalk 5.1\Burning River\Complex\Complex';
	add: '..\..\Documents and Settings\cellier\Mes documents\Dolphin Smalltalk X6\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Complex methodsFor!

conjugated
	^self class real: real imaginary: imag negated!

i
	"Answer the result of multiplying self by 1 i"

	^self class real: imag negated imaginary: real! !
!Complex categoriesFor: #conjugated!arithmetic!public! !
!Complex categoriesFor: #i!arithmetic!public! !

!Number methodsFor!

addToComplex: aComplex
	^aComplex + (Complex real: self)!

i
	"Answer a pure imaginary Complex number"

	^Complex real: self class zero imaginary: self!

i: aNumber
	"Form a complex number with self as realPart and argument as imaginaryPart"

	^Complex real: self imaginary: aNumber! !
!Number categoriesFor: #addToComplex:!double dispatch!public! !
!Number categoriesFor: #i!public! !
!Number categoriesFor: #i:!converting!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

