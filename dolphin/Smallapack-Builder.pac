| package |
package := Package name: 'Smallapack-Builder'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #LapackInterfaceBuilder;
	add: #LapackInterfaceBuilderArgument;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	add: 'Smallapack-External';
	yourself).

package!

"Class Definitions"!

Object subclass: #LapackInterfaceBuilder
	instanceVariableNames: 'sourceStream type name comment purpose description arguments shortName codeStream'
	classVariableNames: 'ArgumentsExpr ArrayIndicatorExpr CharacterTypeExpr ComplexTypeExpr DescriptionExpr DetailsExpr DoubleComplexTypeExpr DoubleRealTypeExpr InputIndicatorExpr InputOutputIndicatorExpr IntegerTypeExpr LogicalTypeExpr OutputIndicatorExpr PurposeExpr RealTypeExpr ReferencesExpr SeparatorExpr WorkspaceIndicatorExpr'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #LapackInterfaceBuilderArgument
	instanceVariableNames: 'name inout type dimensions lengthSpec isArray lowercaseName hasExtraLengthArgument'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

LapackInterfaceBuilder guid: (GUID fromString: '{A8FA8834-531D-44FF-9B75-C8786FB7F943}')!
LapackInterfaceBuilder comment: ''!
!LapackInterfaceBuilder categoriesForClass!Unclassified! !
!LapackInterfaceBuilder methodsFor!

baseFunctionPattern
	| func |
	func := String new writeStream.
	func nextPutAll: self genericName.
	arguments isEmpty 
		ifFalse: 
			[func nextPutAll: 'With'.
			arguments do: [:arg | self codeStream: func addPatternArgument: arg].
			arguments do: 
					[:arg | 
					arg hasExtraLengthArgument 
						ifTrue: 
							[func
								nextPutAll: arg extraLengthArgumentKey;
								space;
								nextPutAll: arg extraLengthArgumentName;
								space]]].
	^func contents!

cAllocationMethodFor: anInterfaceArgument 
	(IntegerTypeExpr matchesPrefix: anInterfaceArgument type) 
		ifTrue: [^#cIntegerPointerOn:].
	(RealTypeExpr matchesPrefix: anInterfaceArgument type) 
		ifTrue: 
			[^self isComplex ifTrue: [#cRealPointerOn:] ifFalse: [#cElementPointerOn:]].
	(ComplexTypeExpr matchesPrefix: anInterfaceArgument type) 
		ifTrue: 
			[^self isComplex 
				ifTrue: [#cElementPointerOn:]
				ifFalse: [#cComplexPointerOn:]].
	(DoubleRealTypeExpr matchesPrefix: anInterfaceArgument type) 
		ifTrue: 
			[^self isComplex ifTrue: [#cRealPointerOn:] ifFalse: [#cElementPointerOn:]].
	(DoubleComplexTypeExpr matchesPrefix: anInterfaceArgument type) 
		ifTrue: 
			[^self isComplex 
				ifTrue: [#cElementPointerOn:]
				ifFalse: [#cComplexPointerOn:]].
	(CharacterTypeExpr matchesPrefix: anInterfaceArgument type) 
		ifTrue: [^#cCharPointerOn:].
	(LogicalTypeExpr matchesPrefix: anInterfaceArgument type) 
		ifTrue: [^#cLogicalPointerOn:].
	^anInterfaceArgument type!

codeStream: code addPatternArgument: anInterfaceArgument 
	code
		nextPutAll: anInterfaceArgument lowercaseName;
		nextPut: $:;
		space;
		nextPutAll: anInterfaceArgument lowercaseName;
		space!

codeStream: code allocate: anInterfaceArgument 
	code
		nextPutAll: anInterfaceArgument cAllocatedName;
		nextPutAll: '  := self ';
		nextPutAll: (self cAllocationMethodFor: anInterfaceArgument);
		space;
		nextPutAll: anInterfaceArgument lowercaseName;
		nextPut: $.!

codeStream: code free: anInterfaceArgument 
	code
		nextPutAll: 'self free: ';
		nextPutAll: anInterfaceArgument cAllocatedName;
		nextPutAll: '.'!

compileCode: code forClass: aClass classified: aProtocol 
	"use old chunk format"

	codeStream
		cr;
		nextPut: $!!;
		print: aClass;
		space;
		nextPutAll: ' methodsFor';
		nextPut: $!!;
		cr; cr.
	self nextChunkPut: code.
	codeStream
		space;
		nextPut: $!!;
		cr.
	false 
		ifTrue: 
			[aClass 
				compile: code
				classified: aProtocol]!

cTypeFor: aString 
	(IntegerTypeExpr matchesPrefix: aString) ifTrue: [^'long'].
	(CharacterTypeExpr matchesPrefix: aString) ifTrue: [^'char'].
	(DoubleRealTypeExpr matchesPrefix: aString) ifTrue: [^'double'].
	(DoubleComplexTypeExpr matchesPrefix: aString) ifTrue: [^'ExternalDoubleComplex'].
	(RealTypeExpr matchesPrefix: aString) ifTrue: [^'float'].
	(ComplexTypeExpr matchesPrefix: aString) ifTrue: [^'ExternalFloatComplex'].
	(LogicalTypeExpr matchesPrefix: aString) ifTrue: [^'long'].
	^aString!

destinationSubclass
	"The class in which we should compile code"

	^Smalltalk at: ('Lapack' , (Array with: name first) , 'Library') asSymbol ifAbsent: [self class lapackLibrary]!

generateAbstractBaseFunction
	| code |
	code := String new writeStream.
	code nextPutAll: self baseFunctionPattern.
	code cr; crtab; nextPutAll: 'self subclassResponsibility'.
	self 
		compileCode: code contents
		forClass: self class lapackLibrary
		classified: 'procedures'!

generateCode
	self generateAbstractBaseFunction.
	self generateConcreteBaseFunction.
	self generateWrapperFunction!

generateConcreteBaseFunction
	self generateConcreteBaseFunctionForDolphin!

generateConcreteBaseFunctionForDolphin
	| code |
	code := String new writeStream.
	code
		nextPutAll: self baseFunctionPattern;
		crtab; nextPut: $"; cr;
		nextPutAll: (purpose contents copyReplaceAll: '"' with: '''');
		nextPut: $";
		cr; crtab;
		nextPutAll: '<cdecl: ';
		nextPutAll: (self cTypeFor: type);
		space;
		nextPutAll: (name asLowercase , '_') printString;
		space.
	arguments do: 
			[:arg | 
			code
				nextPutAll: (self cTypeFor: arg type);
				nextPutAll: ' *']
		separatedBy: [code nextPutAll: ' '].
	arguments do: 
			[:arg | 
			arg hasExtraLengthArgument 
				ifTrue: 
					[code
						nextPutAll: ' ';
						nextPutAll: arg extraLengthArgumentType]].
	code
		nextPutAll: '>';
		crtab;
		nextPutAll: '^self invalidCall'.
	self 
		compileCode: code contents
		forClass: self destinationSubclass
		classified: 'procedures'!

generateConcreteBaseFunctionForSqueak
	| code |
	code := String new writeStream.
	code
		nextPutAll: self baseFunctionPattern;
		crtab; nextPut: $"; cr;
		nextPutAll: (purpose contents copyReplaceAll: '"' with: '''');
"		nextPutAll: (description contents replaceAll: $"" with: $');"
		nextPut: $";
		cr; crtab;
		nextPutAll: '<cdecl: ';
		nextPutAll: (self cTypeFor: type);
		space;
		nextPutAll: (name asLowercase , '_') printString;
		nextPutAll: ' ('.
	arguments do: 
			[:arg | 
			code
				nextPutAll: (self cTypeFor: arg type);
				nextPutAll: ' *']
		separatedBy: [code nextPutAll: ' '].
	arguments do: 
			[:arg | 
			arg hasExtraLengthArgument 
				ifTrue: 
					[code
						nextPutAll: ' ';
						nextPutAll: arg extraLengthArgumentType]].
	code
		nextPutAll: ')>';
		crtab;
		nextPutAll: '^self externalCallFailed'.
	self 
		compileCode: code contents
		forClass: self destinationSubclass
		classified: 'procedures'!

generateWrapperFunction
	"dispatch according to Smalltalk dialect"

	self generateWrapperFunctionForDolphin!

generateWrapperFunctionForDolphin
	"at this level, we only allocate non array input variables and info output variable"

	| pattern locals alloc call code lenArg retval |
	pattern := String new writeStream.
	locals := String new writeStream.
	alloc := String new writeStream.
	call := String new writeStream.
	lenArg := String new writeStream.
	retval := String new writeStream.
	pattern
		nextPutAll: self shortName;
		nextPutAll: 'With'.
	call
		nextPutAll: 'self';
		crtab: 2;
		nextPutAll: self genericName;
		nextPutAll: 'With'.
	arguments do: 
			[:arg | 
			call
				nextPutAll: arg lowercaseName;
				nextPut: $:;
				space.
			(arg name = 'INFO' and: [arg isOutput]) 
				ifTrue: 
					[locals
						space;
						nextPutAll: arg cAllocatedName.
					alloc
						nextPutAll: arg cAllocatedName;
						nextPutAll: ' := self cIntegerPointerOn: 0.';
						crtab.
					call nextPutAll: arg cAllocatedName.
					retval
						nextPut: $.;
						crtab;
						nextPut: $^;
						nextPutAll: arg cAllocatedName;
						nextPutAll: ' sdwordAtOffset: 0']
				ifFalse: 
					[self codeStream: pattern addPatternArgument: arg.
					(arg isInput and: [arg isArray not]) 
						ifTrue: 
							[locals
								space;
								nextPutAll: arg cAllocatedName.
							self codeStream: alloc allocate: arg.
							alloc crtab.
							call nextPutAll: arg cAllocatedName]
						ifFalse: [call nextPutAll: arg lowercaseName]].
			arg hasExtraLengthArgument 
				ifTrue: 
					[lenArg
						crtab: 2;
						nextPutAll: arg extraLengthArgumentKey;
						nextPutAll: ' 1']]
		separatedBy: [call crtab: 2].
	code := String new writeStream.
	code
		nextPutAll: pattern contents;
		crtab;
		nextPut: $|;
		nextPutAll: locals contents;
		space;
		nextPut: $|;
		crtab;
		nextPutAll: alloc contents;
		nextPutAll: call contents;
		nextPutAll: lenArg contents;
		nextPutAll: retval contents.
	self 
		compileCode: code contents
		forClass: self destinationSubclass
		classified: 'wrapper'!

generateWrapperFunctionForSqueak
	"at this level, we only allocate non array input variables and info output variable"

	| pattern locals alloc call free code lenArg retval |
	pattern := String new writeStream.
	locals := String new writeStream.
	alloc := String new writeStream.
	call := String new writeStream.
	lenArg := String new writeStream.
	retval := String new writeStream.
	free := String new writeStream.
	pattern
		nextPutAll: self shortName;
		nextPutAll: 'With'.
	call
		nextPutAll: 'self';
		crtab: 2;
		nextPutAll: self genericName;
		nextPutAll: 'With'.
	arguments do: 
			[:arg | 
			call
				nextPutAll: arg lowercaseName;
				nextPut: $:;
				space.
			(arg name = 'INFO' and: [arg isOutput]) 
				ifTrue: 
					[locals
						space;
						nextPutAll: arg cAllocatedName.
					alloc
						nextPutAll: arg cAllocatedName;
						nextPutAll: ' := self cIntegerPointerOn: 0.';
						crtab.
					call nextPutAll: arg cAllocatedName.
					self codeStream: free free: arg.
					free crtab: 2.
					retval
						nextPut: $.;
						crtab;
						nextPutAll: arg cAllocatedName;
						nextPutAll: ' getHandle signedLongAt: 1']
				ifFalse: 
					[self codeStream: pattern addPatternArgument: arg.
					(arg isInput and: [arg isArray not]) 
						ifTrue: 
							[locals
								space;
								nextPutAll: arg cAllocatedName.
							self codeStream: alloc allocate: arg.
							alloc crtab.
							call nextPutAll: arg cAllocatedName.
							self codeStream: free free: arg.
							free crtab: 2]
						ifFalse: [call nextPutAll: arg lowercaseName]].
			arg hasExtraLengthArgument 
				ifTrue: 
					[lenArg
						crtab: 2;
						nextPutAll: arg extraLengthArgumentKey;
						nextPutAll: ' 1']]
		separatedBy: [call crtab: 2].
	code := String new writeStream.
	code
		nextPutAll: pattern contents;
		crtab;
		nextPut: $|;
		nextPutAll: locals contents;
		space;
		nextPut: $|;
		crtab;
		nextPut: $^;
		crtab;
		nextPut: $[;
		nextPutAll: alloc contents;
		nextPutAll: call contents;
		nextPutAll: lenArg contents;
		nextPutAll: retval contents;
		nextPutAll: '] ensure: [';
		nextPutAll: free contents;
		nextPut: $].
	self 
		compileCode: code contents
		forClass: self destinationSubclass
		classified: 'wrapper'!

generateWrapperFunctionForVW
	"at this level, we only allocate non array input variables and info output variable"

	| pattern locals alloc call free code lenArg retval |
	pattern := String new writeStream.
	locals := String new writeStream.
	alloc := String new writeStream.
	call := String new writeStream.
	lenArg := String new writeStream.
	retval := String new writeStream.
	free := String new writeStream.
	pattern
		nextPutAll: self shortName;
		nextPutAll: 'With'.
	call
		nextPutAll: 'self';
		crtab: 2;
		nextPutAll: self genericName;
		nextPutAll: 'With'.
	arguments do: 
			[:arg | 
			call
				nextPutAll: arg lowercaseName;
				nextPut: $:;
				space.
			(arg name = 'INFO' and: [arg isOutput]) 
				ifTrue: 
					[locals
						space;
						nextPutAll: arg cAllocatedName.
					alloc
						nextPutAll: arg cAllocatedName;
						nextPutAll: ' := self cIntegerPointerOn: 0.';
						crtab.
					call nextPutAll: arg cAllocatedName.
					self codeStream: free free: arg.
					free crtab: 2.
					retval
						nextPut: $.;
						crtab;
						nextPutAll: arg cAllocatedName;
						nextPutAll: ' contents']
				ifFalse: 
					[self codeStream: pattern addPatternArgument: arg.
					(arg isInput and: [arg isArray not]) 
						ifTrue: 
							[locals
								space;
								nextPutAll: arg cAllocatedName.
							self codeStream: alloc allocate: arg.
							alloc crtab.
							call nextPutAll: arg cAllocatedName.
							self codeStream: free free: arg.
							free crtab: 2]
						ifFalse: [call nextPutAll: arg lowercaseName]].
			arg hasExtraLengthArgument 
				ifTrue: 
					[lenArg
						crtab: 2;
						nextPutAll: arg extraLengthArgumentKey;
						nextPutAll: ' 1']]
		separatedBy: [call crtab: 2].
	code := String new writeStream.
	code
		nextPutAll: pattern contents;
		crtab;
		nextPut: $|;
		nextPutAll: locals contents;
		space;
		nextPut: $|;
		crtab;
		nextPut: $^;
		crtab;
		nextPut: $[;
		nextPutAll: alloc contents;
		nextPutAll: call contents;
		nextPutAll: lenArg contents;
		nextPutAll: retval contents;
		nextPutAll: '] ensure: [';
		nextPutAll: free contents;
		nextPut: $].
	self 
		compileCode: code contents
		forClass: self destinationSubclass
		classified: 'wrapper'!

generateWrapperWrapperFunction
	"at this level, we allocate workspace arrays
	We also handle LWORK=-1 trick of LAPACK 3.0"

	(arguments anySatisfy: [:arg | arg isWorkspace]) ifFalse: [^self]!

genericName
	^'x' , self shortName!

isComplex
	^'CZ' includes: name first!

isDoublePrecision
	^'DZ' includes: name first!

isReal
	^'DS' includes: name first!

isSinglePrecision
	^'CS' includes: name first!

nextChunkPut: aString

	(aString identityIncludes: $!!) 
		ifTrue: 
			[aString do: 
					[:character | 
					codeStream nextPut: character.
					"Double up embedded chunk markers"
					character == $!! ifTrue: [codeStream nextPut: $!!]]]
		ifFalse: [codeStream nextPutAll: aString].
	codeStream nextPut: $!!!

parseArguments
	| declPattern args line lengthPattern |
	arguments := arguments contents readStream.
	declPattern := '\*  ([A-Z]+) +\(([a-z /]+)\) +([A-Za-z0-9(,)*+-/ ]+)' asRegex.
	args := OrderedCollection new.
	
	[line := arguments nextLine.
	(declPattern matches: line) 
		ifTrue: 
			[(args add: LapackInterfaceBuilderArgument new)
				name: (declPattern subexpression: 2);
				inout: (declPattern subexpression: 3);
				type: (declPattern subexpression: 4)]
		ifFalse: 
			[args isEmpty 
				ifFalse: 
					[lengthPattern := ('.*' , args last name , ' >= (.*)') asRegex.
					(lengthPattern matches: line) 
						ifTrue: [args last lengthSpec: (lengthPattern subexpression: 2)]]].
	arguments atEnd] 
			whileFalse.
	arguments := args!

parseComment
	| line collector |
	comment := comment contents readStream.
	purpose := String new writeStream.
	description := String new writeStream.
	arguments := String new writeStream.
	collector := String new writeStream.
	[comment atEnd] whileFalse: 
			[line := comment nextLine.
			(PurposeExpr matches: line) ifTrue: [collector := purpose].
			(DescriptionExpr matches: line) ifTrue: [collector := description].
			(ArgumentsExpr matches: line) ifTrue: [collector := arguments].
			(DetailsExpr matches: line) ifTrue: [collector := String new writeStream].
			(ReferencesExpr matches: line) 
				ifTrue: [collector := String new writeStream].
			(SeparatorExpr matches: line) 
				ifTrue: [collector := String new writeStream].
			collector
				nextPutAll: line;
				cr]!

parseFileNamed: aString 
	| file |
	file := File open: aString mode: #read.
	sourceStream := file readWriteStream contents withNormalizedLineDelimiters readStream.
	file close.
	codeStream :=(File open: 'AutoGen_LapackLibrary.st' mode: #append check: false) readWriteStream.
	
	[codeStream setToEnd.
	self parseInformations.
	self parseComment.
	self parseArguments.
	self generateCode] 
			ensure: [codeStream close]!

parseInformations
	| line functionPattern subroutinePattern commentPattern endPattern |
	sourceStream reset.
	line := sourceStream nextLine.
	functionPattern := ' +((DOUBLE *)?[A-Z]+) +FUNCTION +([A-Z][A-Z0-9]*) *\(..*' 
				asRegex.
	subroutinePattern := ' +SUBROUTINE +([A-Z][A-Z0-9]*) *\(..*' asRegex.
	endPattern := ' +END *' asRegex.
	commentPattern := '\* .*' asRegex.
	(functionPattern matches: line) 
		ifTrue: 
			[type := functionPattern subexpression: 2.
			name := functionPattern subexpression: 4].
	(subroutinePattern matches: line) 
		ifTrue: 
			[type := 'long'.	"on Dolphin, int would not be understood"
			name := subroutinePattern subexpression: 2].
	comment := String new writeStream.
	[sourceStream atEnd] whileFalse: 
			[line := sourceStream nextLine.
			(commentPattern matches: line) 
				ifTrue: 
					[comment
						nextPutAll: line;
						cr].
			(endPattern matches: line) 
				ifTrue: [sourceStream position: sourceStream size]]!

shortName
	shortName isNil 
		ifTrue: [shortName := name asLowercase copyFrom: 2 to: name size].
	^shortName! !
!LapackInterfaceBuilder categoriesFor: #baseFunctionPattern!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #cAllocationMethodFor:!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #codeStream:addPatternArgument:!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #codeStream:allocate:!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #codeStream:free:!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #compileCode:forClass:classified:!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #cTypeFor:!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #destinationSubclass!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateAbstractBaseFunction!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateCode!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateConcreteBaseFunction!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateConcreteBaseFunctionForDolphin!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateConcreteBaseFunctionForSqueak!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateWrapperFunction!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateWrapperFunctionForDolphin!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateWrapperFunctionForSqueak!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateWrapperFunctionForVW!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #generateWrapperWrapperFunction!code generation!public! !
!LapackInterfaceBuilder categoriesFor: #genericName!accessing!public! !
!LapackInterfaceBuilder categoriesFor: #isComplex!accessing!public! !
!LapackInterfaceBuilder categoriesFor: #isDoublePrecision!accessing!public! !
!LapackInterfaceBuilder categoriesFor: #isReal!accessing!public! !
!LapackInterfaceBuilder categoriesFor: #isSinglePrecision!accessing!public! !
!LapackInterfaceBuilder categoriesFor: #nextChunkPut:!private! !
!LapackInterfaceBuilder categoriesFor: #parseArguments!parsing!public! !
!LapackInterfaceBuilder categoriesFor: #parseComment!parsing!public! !
!LapackInterfaceBuilder categoriesFor: #parseFileNamed:!parsing!public! !
!LapackInterfaceBuilder categoriesFor: #parseInformations!parsing!public! !
!LapackInterfaceBuilder categoriesFor: #shortName!accessing!public! !

!LapackInterfaceBuilder class methodsFor!

arrayIndicatorExpr 
	^ArrayIndicatorExpr !

characterTypeExpr
	^CharacterTypeExpr!

initialize
	"LapackInterfaceBuilder initialize"

	IntegerTypeExpr := 'INTEGER' asRegexIgnoringCase.
	RealTypeExpr := 'REAL' asRegexIgnoringCase.
	ComplexTypeExpr := 'COMPLEX' asRegexIgnoringCase.
	DoubleRealTypeExpr := 'DOUBLE *PRECISION|REAL *\* *8' asRegexIgnoringCase.
	DoubleComplexTypeExpr := 'DOUBLE *COMPLEX|COMPLEX *\* *16' asRegexIgnoringCase.
	CharacterTypeExpr := 'CHARACTER' asRegexIgnoringCase.
	LogicalTypeExpr := 'LOGICAL' asRegexIgnoringCase.

	ArrayIndicatorExpr := '.*array.*' asRegex.
	InputIndicatorExpr := 'input' asRegex.
	OutputIndicatorExpr := 'output' asRegex.
	InputOutputIndicatorExpr := 'input( *(or|/)? *)output' asRegex.
	WorkspaceIndicatorExpr := 'workspace(( *(or|/)? *)output)?' asRegex.
	
	PurposeExpr := '\* *Purpose *' asRegex.
	DescriptionExpr := '\* *Description *' asRegex.
	ArgumentsExpr := '\* *Arguments *' asRegex.
	DetailsExpr := '\* *Further Details *' asRegex.
	ReferencesExpr := '\* *References *' asRegex.
	SeparatorExpr := '\* *====================+ *' asRegex.!

inputIndicatorExpr
	^InputIndicatorExpr!

inputOutputIndicatorExpr  
	^InputOutputIndicatorExpr  !

lapackLibrary
	^LapackLibrary!

outputIndicatorExpr
	^OutputIndicatorExpr!

testParser
	"LapackInterfaceBuilder testParser"

	(File exists: 'AutoGen_LapackLibrary.st')
		ifTrue: [File delete: 'AutoGen_LapackLibrary.st'].

	File forAll: '*.f' in: 'C:\Library\lapack3.0\src\fort' do: [:e | (self new) parseFileNamed: e path]!

workspaceIndicatorExpr   
	^WorkspaceIndicatorExpr   ! !
!LapackInterfaceBuilder class categoriesFor: #arrayIndicatorExpr!accessing!public! !
!LapackInterfaceBuilder class categoriesFor: #characterTypeExpr!accessing!public! !
!LapackInterfaceBuilder class categoriesFor: #initialize!class initialization!public! !
!LapackInterfaceBuilder class categoriesFor: #inputIndicatorExpr!accessing!public! !
!LapackInterfaceBuilder class categoriesFor: #inputOutputIndicatorExpr!accessing!public! !
!LapackInterfaceBuilder class categoriesFor: #lapackLibrary!accessing!public! !
!LapackInterfaceBuilder class categoriesFor: #outputIndicatorExpr!accessing!public! !
!LapackInterfaceBuilder class categoriesFor: #testParser!examples!public! !
!LapackInterfaceBuilder class categoriesFor: #workspaceIndicatorExpr!accessing!public! !

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

"Binary Globals"!

