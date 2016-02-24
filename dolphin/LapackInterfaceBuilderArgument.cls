"Filed out from Dolphin Smalltalk 7"!

Object subclass: #LapackInterfaceBuilderArgument
	instanceVariableNames: 'name inout type dimensions lengthSpec isArray lowercaseName hasExtraLengthArgument'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LapackInterfaceBuilderArgument guid: (GUID fromString: '{9A2AFA20-AEFB-446F-93EB-2CDE50EFD026}')!
LapackInterfaceBuilderArgument comment: ''!
!LapackInterfaceBuilderArgument categoriesForClass!Unclassified! !
!LapackInterfaceBuilderArgument methodsFor!

cAllocatedName
	^'cArg' , self lowercaseName!

dimensions
	^dimensions!

dimensions: anObject
	dimensions := anObject!

extraLengthArgumentKey
	^'length:'!

extraLengthArgumentName
	^'lengthArg' , self lowercaseName!

extraLengthArgumentType
	^'long'!

hasExtraLengthArgument
	hasExtraLengthArgument isNil 
		ifTrue: [hasExtraLengthArgument := LapackInterfaceBuilder characterTypeExpr matchesPrefix: type].
	^hasExtraLengthArgument!

inout
	^inout!

inout: anObject
	inout := anObject!

isArray
	isArray isNil 
		ifTrue: [isArray := LapackInterfaceBuilder arrayIndicatorExpr matches: type].
	^isArray!

isInput
	^LapackInterfaceBuilder inputIndicatorExpr matches: inout!

isInputOutput
	^LapackInterfaceBuilder inputOutputIndicatorExpr matches: inout!

isOutput
	^LapackInterfaceBuilder outputIndicatorExpr matches: inout!

isWorkspace
	^LapackInterfaceBuilder workspaceIndicatorExpr matches: inout!

lengthSpec
	^lengthSpec!

lengthSpec: anObject
	lengthSpec := anObject!

lowercaseName
	lowercaseName isNil ifTrue: [lowercaseName := name asLowercase].
	^lowercaseName!

name
	^name!

name: anObject
	name := anObject.
	lowercaseName := nil!

type
	^type!

type: anObject
	type := anObject! !
!LapackInterfaceBuilderArgument categoriesFor: #cAllocatedName!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #dimensions!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #dimensions:!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #extraLengthArgumentKey!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #extraLengthArgumentName!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #extraLengthArgumentType!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #hasExtraLengthArgument!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #inout!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #inout:!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #isArray!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #isInput!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #isInputOutput!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #isOutput!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #isWorkspace!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #lengthSpec!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #lengthSpec:!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #lowercaseName!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #name!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #name:!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #type!accessing!public! !
!LapackInterfaceBuilderArgument categoriesFor: #type:!accessing!public! !

