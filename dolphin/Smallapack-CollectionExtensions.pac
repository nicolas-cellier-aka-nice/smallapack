| package |
package := Package name: 'Smallapack-CollectionExtensions'.
package paxVersion: 1;
	basicComment: 'This adds arithmetic iterators on collections, like
- computing the sum, the product
- extracting the smallest/greatest element

This also add the possibility to re-index a collection.'.


package classNames
	add: #IndirectCollection;
	yourself.

package methodNames
	add: #Collection -> #atAll;
	add: #Collection -> #count:;
	add: #Collection -> #productOf:;
	add: #Collection -> #reindexedThrough:;
	add: #Collection -> #sumOf:;
	add: #Dictionary -> #rejectMask:;
	add: #Dictionary -> #selectMask:;
	add: #Integer -> #checkAsIndexWithLimit:;
	add: #Integer -> #indexAccessInto:;
	add: #Integer -> #indexAccessInto:put:;
	add: #Interval -> #indexAccessInto:;
	add: #Object -> #assignAllCollection:;
	add: #Object -> #assignCollection:atBooleanCollectionIndex:;
	add: #Object -> #assignCollection:atIntegerCollectionIndex:;
	add: #Object -> #checkAsIndexWithLimit:;
	add: #Point -> #indexAccessInto:;
	add: #Point -> #indexAccessInto:put:;
	add: #SequenceableCollection -> #assignAllCollection:;
	add: #SequenceableCollection -> #assignCollection:atBooleanCollectionIndex:;
	add: #SequenceableCollection -> #assignCollection:atIntegerCollectionIndex:;
	add: #SequenceableCollection -> #atBooleanCollection:;
	add: #SequenceableCollection -> #atBooleanCollection:putCollection:;
	add: #SequenceableCollection -> #atBooleanCollection:replicate:;
	add: #SequenceableCollection -> #atInteger:;
	add: #SequenceableCollection -> #atInteger:put:;
	add: #SequenceableCollection -> #atIntegerCollection:;
	add: #SequenceableCollection -> #atIntegerCollection:putCollection:;
	add: #SequenceableCollection -> #atIntegerCollection:replicate:;
	add: #SequenceableCollection -> #atIntervalFrom:to:by:;
	add: #SequenceableCollection -> #checkAsIndexWithLimit:;
	add: #SequenceableCollection -> #cumulativeMaxOf:;
	add: #SequenceableCollection -> #cumulativeMinOf:;
	add: #SequenceableCollection -> #cumulativeProductOf:;
	add: #SequenceableCollection -> #cumulativeSumOf:;
	add: #SequenceableCollection -> #findAll:;
	add: #SequenceableCollection -> #findMaxOf:;
	add: #SequenceableCollection -> #findMinOf:;
	add: #SequenceableCollection -> #generalizedAt:;
	add: #SequenceableCollection -> #generalizedAt:put:;
	add: #SequenceableCollection -> #indexAccessInto:;
	add: #SequenceableCollection -> #indexAccessInto:put:;
	add: #SequenceableCollection -> #maxOf:;
	add: #SequenceableCollection -> #minOf:;
	add: #SequenceableCollection -> #productOf:;
	add: #SequenceableCollection -> #rejectMask:;
	add: #SequenceableCollection -> #selectMask:;
	add: #SequenceableCollection -> #sumOf:;
	add: #Symbol -> #checkAsIndexWithLimit:;
	add: #Symbol -> #indexAccessInto:;
	add: #Symbol -> #indexAccessInto:put:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Collection subclass: #IndirectCollection
	instanceVariableNames: 'indices targetCollection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Collection methodsFor!

atAll
	"Answer a subCollection with all the elements of self.
	That is a copy."

	^self copy!

count: aBlock 
	"Count the number of elements for which a Block is true.
	This is equivalent to :
		(self select: aBlock) size,
		(self collect: aBlock) occurencesOf: true,
	but prevent a useless memory allocation..."

	| count |
	count := 0.
	self do: [:e | (aBlock value: e) ifTrue: [count := count + 1]].
	^count!

productOf: aBlock
	"Answer the product of all elements after applying a Block."

	^self inject: 1 into: [:e :sum | sum * (aBlock value: e)]!

reindexedThrough: anIndexTable 
	"Create an indirect collection operating on self with a different set of indices..."

	^IndirectCollection new pointOn: self through: anIndexTable!

sumOf: aBlock
	"Answer the sum of all elements after applying a Block."

	^self inject: 0 into: [:e :sum | sum + (aBlock value: e)]! !
!Collection categoriesFor: #atAll!accessing-generalized!public! !
!Collection categoriesFor: #count:!enumerating!public! !
!Collection categoriesFor: #productOf:!arithmetic!public! !
!Collection categoriesFor: #reindexedThrough:!converting!public! !
!Collection categoriesFor: #sumOf:!arithmetic!public! !

!Dictionary methodsFor!

rejectMask: aBooleanDictionary 
	"Reject the keys of self which are associated to true in aBooleanDictionary."

	| newCollection |
	newCollection := self species new.
	self associationsDo: [:each | (aBooleanDictionary at: each key ifAbsent: [false])
			ifFalse: [newCollection add: each]].
	^newCollection!

selectMask: aBooleanDictionary 
	"Select only the keys of self which are associated to true in aBooleanDictionary."

	| newCollection |
	newCollection := self species new.
	self associationsDo: [:each | (aBooleanDictionary at: each key ifAbsent: [false])
			ifTrue: [newCollection add: each]].
	^newCollection! !
!Dictionary categoriesFor: #rejectMask:!enumerating!public! !
!Dictionary categoriesFor: #selectMask:!enumerating!public! !

!Integer methodsFor!

checkAsIndexWithLimit: maxDim 
	^(self between: 1 and: maxDim) 
		ifTrue: [self]
		ifFalse: 
			[self 
				error: 'Expected an integer between 1 and ' , maxDim printString.
			nil]!

indexAccessInto: aCollection

	^aCollection atInteger: self!

indexAccessInto: aCollection put: aValue

	^aCollection atInteger: self put: aValue! !
!Integer categoriesFor: #checkAsIndexWithLimit:!accessing-generalized!public! !
!Integer categoriesFor: #indexAccessInto:!accessing-generalized!public! !
!Integer categoriesFor: #indexAccessInto:put:!accessing-generalized!public! !

!Interval methodsFor!

indexAccessInto: aCollection 
	"I am used as an index to access aCollection"

	^aCollection atIntervalFrom: start to: stop by: step! !
!Interval categoriesFor: #indexAccessInto:!accessing-generalized!public! !

!Object methodsFor!

assignAllCollection: aCollection 
	"Used in a generalized access of the form A( B ) := C
		A is aCollection
		B is #all
		C is self
	in this case, fill the whole collection with self (Beware, not a copy !!)"

	^aCollection atAllPut: self!

assignCollection: aCollection atBooleanCollectionIndex: indexCollection 
	"Used in a generalized access of the form A( B ) := C
		A is aCollection
		B is indexCollection
		C is self
	in this case, fill a part of aCollection with self (Beware, not a copy !!)"

	^aCollection atBooleanCollection: indexCollection replicate: self!

assignCollection: aCollection atIntegerCollectionIndex: indexCollection 
	"Used in a generalized access of the form A( B ) := C
		A is aCollection
		B is indexCollection
		C is self
	in this case, fill a part of aCollection with self (Beware, not a copy !!)"

	^aCollection atIntegerCollection: indexCollection replicate: self!

checkAsIndexWithLimit: maxDim 
	self 
		error: 'This object cannot be used as a generalized index into a collection '.
	^nil! !
!Object categoriesFor: #assignAllCollection:!accessing-generalized!public! !
!Object categoriesFor: #assignCollection:atBooleanCollectionIndex:!accessing-generalized!public! !
!Object categoriesFor: #assignCollection:atIntegerCollectionIndex:!accessing-generalized!public! !
!Object categoriesFor: #checkAsIndexWithLimit:!accessing-generalized!public! !

!Point methodsFor!

indexAccessInto: aCollection

	^aCollection atPoint: self!

indexAccessInto: aCollection put: aValue

	^aCollection atPoint: self put: aValue! !
!Point categoriesFor: #indexAccessInto:!accessing-generalized!public! !
!Point categoriesFor: #indexAccessInto:put:!accessing-generalized!public! !

!SequenceableCollection methodsFor!

assignAllCollection: aCollection 
	"Used in a generalized access of the form A( B ) := C
		A is aCollection
		B is #all
		C is self
	in this case, fill aCollection with a copy of self"

	self size = aCollection size 
		ifFalse: 
			[self 
				error: 'In generalized access A( : ) := C, size of A and  C should match'].
	^self 
		replaceFrom: 1
		to: aCollection size
		with: aCollection
		startingAt: 1!

assignCollection: aCollection atBooleanCollectionIndex: indexCollection 
	self size = indexCollection size 
		ifFalse: 
			[self 
				error: 'in generalized access : A( B ) := C, sizes of B and C should match'].
	^aCollection atBooleanCollection: indexCollection putCollection: self!

assignCollection: aCollection atIntegerCollectionIndex: indexCollection 
	self size = indexCollection size 
		ifFalse: 
			[self 
				error: 'in generalized access : A( B ) := C, sizes of B and C should match'].
	^aCollection atIntegerCollection: indexCollection putCollection: self!

atBooleanCollection: maskCollection 
	"Answer a subCollection of self using a mask."

	| res sz |
	sz := maskCollection size.
	res := (self species new: sz) writeStream.
	1 to: sz
		do: [:i | (maskCollection at: i) ifTrue: [res nextPut: (self at: i)]].
	^res contents!

atBooleanCollection: maskCollection putCollection: replacementCollection 
	"Assign a subCollection of self with a replacement collection;
	only elements for which corresponding mask is true are set.
	The maskCollection and replacementCollection should be Sequenceable of same size
	and their size should not be greater than self size."

	| sz |
	sz := maskCollection size.
	1 to: sz
		do: 
			[:i | 
			(maskCollection at: i) 
				ifTrue: [self at: i put: (replacementCollection at: i)]].
	^replacementCollection!

atBooleanCollection: maskCollection replicate: anObject 
	"Assign a subCollection of self with a replicated object"

	| sz |
	sz := maskCollection size.
	1 to: sz
		do: [:i | (maskCollection at: i) ifTrue: [self at: i put: anObject]].
	^anObject!

atInteger: anInteger
	^self at: anInteger!

atInteger: anInteger put: anObject 
	^self at: anInteger put: anObject!

atIntegerCollection: indexCollection 
	"Answer a subCollection of self"

	| res sz |
	sz := indexCollection size.
	res := self species ofSize: sz.
	1 to: sz do: [:i | res at: i put: (self at: (indexCollection atInteger: i))].
	^res!

atIntegerCollection: indexCollection putCollection: replacementCollection 
	"Assign a subCollection of self with a replacement collection"

	1 to: indexCollection size
		do: 
			[:i | 
			self at: (indexCollection at: i)
				put: (replacementCollection at: i)].
	^replacementCollection!

atIntegerCollection: indexCollection replicate: anObject 
	"Assign a subCollection of self with a replicated object"

	| sz |
	sz := indexCollection size.
	1 to: sz do: [:i | self at: (indexCollection at: i) put: anObject].
	^anObject!

atIntervalFrom: interStart to: interStop by: interStep 
	"accessing self( interStart to: interStop by: interStep )"

	^interStep = 1 
		ifTrue: [self copyFrom: interStart to: interStop]
		ifFalse: [self atIntegerCollection: (interStart to: interStop by: interStep)]!

checkAsIndexWithLimit: maxDim 
	^(self isEmpty or: [self anySatisfy: [:e | (e isKindOf: Boolean) not]]) 
		ifTrue: 
			[(self anySatisfy: [:e | e isInteger not or: [e < 1 or: [e > maxDim]]]) 
				ifTrue: 
					[self error: 'Expected a collection of integers between 1 and ' 
								, maxDim printString.
					nil]
				ifFalse: [self]]
		ifFalse: 
			[(self size <= maxDim or: [maxDim = 0]) 
				ifTrue: [(1 to: self size) selectMask: self]
				ifFalse: 
					[self error: 'Expected a collection of booleans of size ' 
								, maxDim printString.
					nil]]!

cumulativeMaxOf: aBlock 
	"Answer with a collection accumulating the max of all preceding elements after applying a Block"

	| res cumul |
	res := (self species new: self size) writeStream.
	self isEmpty ifTrue: [^res contents].
	res nextPut: (cumul := aBlock value: (self at: 1)).
	2 to: self size
		do: [:i | res nextPut: (cumul := cumul max: (aBlock value: (self at: i)))].
	^res contents!

cumulativeMinOf: aBlock 
	"Answer with a collection accumulating the min of all preceding elements after applying a Block"

	| res cumul |
	res := (self species new: self size) writeStream.
	self isEmpty ifTrue: [^res contents].
	res nextPut: (cumul := aBlock value: (self at: 1)).
	2 to: self size
		do: [:i | res nextPut: (cumul := cumul min: (aBlock value: (self at: i)))].
	^res contents!

cumulativeProductOf: aBlock 
	"Answer with a collection accumulating the product of all preceding elements after applying a Block"

	| res cumul |
	res := (self species new: self size) writeStream.
	self isEmpty ifTrue: [^res contents].
	res nextPut: (cumul := aBlock value: (self at: 1)).
	2 to: self size do: [:i | res nextPut: (cumul := cumul * (aBlock value: (self at: i)))].
	^res contents!

cumulativeSumOf: aBlock 
	"Answer with a collection accumulating the sum of all preceding elements after applying a Block"

	| res cumul |
	res := (self species new: self size) writeStream.
	self isEmpty ifTrue: [^res contents].
	res nextPut: (cumul := aBlock value: (self at: 1)).
	2 to: self size
		do: [:i | res nextPut: (cumul := cumul + (aBlock value: (self at: i)))].
	^res contents!

findAll: aBlock 
	"select all indices for which a block evaluates to true"

	^(1 to: self size) select: [:i | aBlock value: (self at: i)]!

findMaxOf: aBlock
	"Answer the index of the max of all elements after applying aBlock.
	Roughly equivalent to:
		^(self collect: aBlock) findMax
	But within a single loop and without allocation"

	| min index |
	self isEmpty ifTrue: [^0].
	index := 1.
	min := aBlock value: (self at: 1).
	2 to: self size
		do: 
			[:i | 
			| tmp |
			(tmp := aBlock value: (self at: i)) > min 
				ifTrue: 
					[min := tmp.
					index := i]].
	^index!

findMinOf: aBlock
	"Answer the index of the min of all elements after applying aBlock.
	Roughly equivalent to:
		^(self collect: aBlock) findMin
	But within a single loop and without allocation"

	| min index |
	self isEmpty ifTrue: [^0].
	index := 1.
	min := aBlock value: (self at: 1).
	2 to: self size
		do: 
			[:i | 
			| tmp |
			(tmp := aBlock value: (self at: i)) < min 
				ifTrue: 
					[min := tmp.
					index := i]].
	^index!

generalizedAt: anObject
	"Generalized access enable subCollection accessing.
	Use a double dispatching technique."

	^anObject indexAccessInto: self!

generalizedAt: anObject put: another
	"Generalized access enable subCollection accessing.
	Use a double dispatching technique."

	^anObject indexAccessInto: self put: another!

indexAccessInto: aCollection 
	"I am used as an index to access into aCollection"

	(self allSatisfy: 
			[:e | 
			(e isInteger 
				or: [e respondsToArithmetic and: [e toMinimumGenerality isInteger]]) 
					and: [e >= 1 and: [e <= aCollection size]]]) 
		ifTrue: [^aCollection atIntegerCollection: self].
	((self allSatisfy: [:e | e isKindOf: Boolean]) 
		and: [self size <= aCollection size]) 
			ifTrue: [^aCollection atBooleanCollection: self].
	^self 
		error: 'In generalized access A( B ), attempt to access a collection A with an invalid collection of indices B'!

indexAccessInto: aCollection put: anObject 
	"I am used as an index to access into aCollection"

	(self allSatisfy: 
			[:e | 
			(e isInteger 
				or: [e respondsToArithmetic and: [e toMinimumGenerality isInteger]]) 
					and: [e >= 1 and: [e <= aCollection size]]]) 
		ifTrue: 
			[^anObject assignCollection: aCollection atIntegerCollectionIndex: self].
	((self allSatisfy: [:e | e isKindOf: Boolean]) 
		and: [self size <= aCollection size]) 
			ifTrue: 
				[^anObject assignCollection: aCollection atBooleanCollectionIndex: self].
	^self 
		error: 'In generalized access A( B ) := C, attempt to access a collection A with an invalid collection of indices B'!

maxOf: aBlock 
	"answer the greatest of all elements after applying a Block"

	| max |
	self isEmpty ifTrue: [^nil].
	max := aBlock value: (self at: 1).
	2 to: self size do: [:i | max := max max: (aBlock value: (self at: i))].
	^max!

minOf: aBlock 
	"answer the smallest of all elements after applying a Block"

	| min |
	self isEmpty ifTrue: [^nil].
	min := aBlock value: (self at: 1).
	2 to: self size do: [:i | min := min min: (aBlock value: (self at: i))].
	^min!

productOf: aBlock 
	"answer the product of all elements after applying a Block"

	| product |
	self isEmpty ifTrue: [^1].
	product := aBlock value: (self at: 1).
	2 to: self size do: [:i | product := product * (aBlock value: (self at: i))].
	^product!

rejectMask: aBooleanCollection 
	"Select elements of self for which corresponding index in aBooleanCollection is false..."

	| sz aStream |
	sz := self size min: aBooleanCollection size.
	aStream := WriteStream on: (self species withSize: self size).
	1 to: sz do: [:index | (aBooleanCollection at: index)
			ifFalse: [aStream nextPut: (self at: index)]].
	sz + 1 to: self size do: [:index | aStream nextPut: (self at: index)].
	^aStream contents!

selectMask: aBooleanCollection 
	"Select elements of self for which corresponding index in aBooleanCollection is true..."

	| sz aStream |
	sz := self size min: aBooleanCollection size.
	aStream := WriteStream on: (self species withSize: sz).
	1 to: sz do: [:index | (aBooleanCollection at: index)
			ifTrue: [aStream nextPut: (self at: index)]].
	^aStream contents!

sumOf: aBlock 
	"answer the sum of all elements after applying a Block"

	| sum |
	self isEmpty ifTrue: [^0].
	sum := aBlock value: (self at: 1).
	2 to: self size do: [:i | sum := sum + (aBlock value: (self at: i))].
	^sum! !
!SequenceableCollection categoriesFor: #assignAllCollection:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #assignCollection:atBooleanCollectionIndex:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #assignCollection:atIntegerCollectionIndex:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atBooleanCollection:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atBooleanCollection:putCollection:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atBooleanCollection:replicate:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atInteger:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atInteger:put:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atIntegerCollection:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atIntegerCollection:putCollection:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atIntegerCollection:replicate:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #atIntervalFrom:to:by:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #checkAsIndexWithLimit:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #cumulativeMaxOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #cumulativeMinOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #cumulativeProductOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #cumulativeSumOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #findAll:!enumerating!public! !
!SequenceableCollection categoriesFor: #findMaxOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #findMinOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #generalizedAt:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #generalizedAt:put:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #indexAccessInto:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #indexAccessInto:put:!accessing-generalized!public! !
!SequenceableCollection categoriesFor: #maxOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #minOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #productOf:!arithmetic!public! !
!SequenceableCollection categoriesFor: #rejectMask:!enumerating!public! !
!SequenceableCollection categoriesFor: #selectMask:!enumerating!public! !
!SequenceableCollection categoriesFor: #sumOf:!arithmetic!public! !

!Symbol methodsFor!

checkAsIndexWithLimit: maxDim 
	^self == #all 
		ifTrue: [self]
		ifFalse: 
			[self == #end 
				ifTrue: [maxDim]
				ifFalse: 
					[self 
						error: 'This object cannot be used as a generalized index into a collection '.
					nil]]!

indexAccessInto: aCollection 
	"I am used as an index"

	self == #all ifTrue: [^aCollection atAll].
	self == #end ifTrue: [^aCollection last].
	^super indexAccessInto: aCollection!

indexAccessInto: aCollection put: anObject 
	"I am used as an index"

	self == #all ifTrue: [^anObject assignAllCollection: aCollection].
	self == #end 
		ifTrue: [^aCollection atInteger: aCollection size put: anObject].
	^super indexAccessInto: aCollection put: anObject! !
!Symbol categoriesFor: #checkAsIndexWithLimit:!accessing-generalized!public! !
!Symbol categoriesFor: #indexAccessInto:!accessing-generalized!public! !
!Symbol categoriesFor: #indexAccessInto:put:!accessing-generalized!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

IndirectCollection guid: (GUID fromString: '{263B6D99-0FD0-464F-AC84-E214F9EC40B1}')!
IndirectCollection comment: 'IndirectCollection is a wrapper Collection on a target collection.

It is used to access a sub collection of targetCollection, or view it with different keys.
It works like a pointer : if elements are modified, they also are modified in the target collection.

Instance Variables
	indices	<Collection>	give the index correspondance table : selfIndices -> targetIndices
	targetCollection	<Collection>	the original target collection accessed with : targetIndices -> targetElement

'!
!IndirectCollection categoriesForClass!Kernel-Objects! !
!IndirectCollection methodsFor!

add: newObject 
	self shouldNotImplement!

at: anInteger 
	^targetCollection at: (indices at: anInteger)!

at: anInteger put: anObject 
	^targetCollection at: (indices at: anInteger) put: anObject!

collect: aBlock 
	^indices collect: [:e | aBlock value: (targetCollection at: e)]!

copyFrom: start to: stop 
	"Beware : it is a copy of the pointer,
	but we keep modifying the same targetCollection"

	^targetCollection reindexedThrough: (indices copyFrom: start to: stop)!

do: aBlock 
	indices do: [:e | aBlock value: (targetCollection at: e)]!

isSequenceable
	^indices isSequenceable!

pointOn: aTargetList through: anIndexTable 
	targetCollection := aTargetList.
	indices := anIndexTable!

reject: aBlock 
	^targetCollection reindexedThrough: (indices 
				reject: [:e | aBlock value: (targetCollection at: e)])!

rejectMask: aBooleanCollection 
	^targetCollection 
		reindexedThrough: (indices rejectMask: [:e | aBooleanCollection])!

remove: oldObject ifAbsent: anExceptionBlock 
	self shouldNotImplement!

replaceFrom: start to: stop with: aCollection 
	start to: stop do: [:i | self at: i put: (aCollection at: i)]!

select: aBlock 
	^targetCollection reindexedThrough: (indices 
				select: [:e | aBlock value: (targetCollection at: e)])!

selectMask: aBooleanCollection 
	^targetCollection 
		reindexedThrough: (indices selectMask: [:e | aBooleanCollection])!

size
	"Since i am a partial view of targetCollection,
	i have my own size"

	^indices size!

species
	^indices species! !
!IndirectCollection categoriesFor: #add:!adding!public! !
!IndirectCollection categoriesFor: #at:!accessing!public! !
!IndirectCollection categoriesFor: #at:put:!accessing!public! !
!IndirectCollection categoriesFor: #collect:!enumerating!public! !
!IndirectCollection categoriesFor: #copyFrom:to:!copying!public! !
!IndirectCollection categoriesFor: #do:!enumerating!public! !
!IndirectCollection categoriesFor: #isSequenceable!public!testing! !
!IndirectCollection categoriesFor: #pointOn:through:!initialize/release!public! !
!IndirectCollection categoriesFor: #reject:!enumerating!public! !
!IndirectCollection categoriesFor: #rejectMask:!enumerating!public! !
!IndirectCollection categoriesFor: #remove:ifAbsent:!public!removing! !
!IndirectCollection categoriesFor: #replaceFrom:to:with:!copying!public! !
!IndirectCollection categoriesFor: #select:!enumerating!public! !
!IndirectCollection categoriesFor: #selectMask:!enumerating!public! !
!IndirectCollection categoriesFor: #size!accessing!public! !
!IndirectCollection categoriesFor: #species!private! !

"Binary Globals"!

