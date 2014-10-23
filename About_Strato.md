0 Introduction
==============

I had an idea that I've carried with me 'in the back' for a long time. In that place where you leave idea's to sit and perhaps grow into an even better idea. Perhaps even with an extra idea about how to make it into something real. And like many ideas, it started with something easily explained:

I want to make a programming language without any reserved words.

There are pretty significant differences between the list of reserved words of say C, Pascal and SQL. There's this special status of them with any Lisp-related language. There's [Qalb](http://nas.sr/%D9%82%D9%84%D8%A8/) and [UOF](https://en.wikipedia.org/wiki/Uniform_Office_Format). Ever used formulas in Microsoft Excel in another language? Reserved words tie a programming language to one and only one natural language, and already start limiting you as a programming since they are no longer available to you as identifiers.

I agree that reading written code is important. I agree that code should read as if it is by itself explaining what it's supposed to do. That is very strongly so with COBOL and SQL; it goes for BASIC; a little less for Pascal and Fortran, bringing with it tidbits of mathematical notation; and sometimes totally not so for for example C and Perl. But code in these exert the virtue of being concise. I want both. When reading code it's important to get a good view of the structure and flow, exactly what punctuation is for. Well chosen identifiers will tell you what the code is about, but it's the perators that make it all happen.

1 Basics
========

The three main basics in programming are sequence, selection and iteration. Sequence has stopped being an issue since around the advent of virtual memory. We no longer work (much) with punchcards. Editors have advanced greatly since edlin. With 16-bits (20-bits addressing) and 32-bits you're certain to get a big enough block of contiguous (virtual) memory to keep a good chunk of code in. So in the code you can silently assume things get done in the same order as they're written there, unless execution control structures tell otherwise. Among execution control structures are function calls, exception handling and on a higher level threading, but let's look at selection and iteration first.

Selection is so important I would propose to have it everywhere, which saves us the use of reserved words 'if', 'then' and 'else'. By leaving a 'stray' expression that evaluates to a boolean in front of two statements or code blocks, it controls the execution of the latter. Selection by juxtaposition if you will. By not having text there to clearly mark which is which, it is true this may hamper readability, but that's why I would require every selection to have an else-clause. Using indentation and comments you're still free to clearly mark else-clauses. 

I personally like 'case' or 'switch', but I'd like to keep this first version straight-forward to get things going, and might introduce one in a later version.

Iteration is something I don't propose to infer only from code, so I assign it a character: "&". Followed by an boolean-expression and a statement or code block, which would work like a while-loop. Or the other way round, with the statement up front, and get a repeat-until-loop. Most langauges offer a 'for' structure, but in practice this are two extra statements, one that is executed once before the loop starts (i=0), and one that is executed before each evaluation of the boolean expression (i++), so I propose an advanced structure:
	& ( Statement BooleanExpression Statement ) Statement
And a code block also counts as a statement, so I image I might see code like this one day:
	&({BuildRequest;WriteRequest;}Line=EndOfMessage{Line:=ReadLine;}){ProcessResponseLine;}

Exception handling in its basic form comes down to indicating which code to execute when something gets thrown, optionally depending on this something. Write ":::" to denote where to start catching exceptions from (try) and "???" followed by a code block that will handle them (catch). Or "???(e:x)" to handle only exceptions of type x. Write "!!!" to throw an exception, or to re-throw the exception from a exception-handling-block.

To defer code to the end of the current code block, write ">>>"

2 Types
=======

Code files don't start with execution. Leaving interpreted languages behind us, we start code files with declarations and the basic structure to execute code with.

Start a code file with the namespace name.

Declare a type by writing it's name (identifier), "=" and the name of an other type. Depending on the run-time library I suppose there will be basic types like "int8", "uint32" etc.

Declare a record (struct) by writing it's identifier, "=", "{" and none or more field definitions, close with "}". By default the members of the structure are placed in memory in sequence (//TODO: alignment), but this can be overriden by "@" followed by a numerical literal for the relative position to the start of the record.

Declare an enumeration by writing an identifier, "=", "(" and none or more enumeration labels, optionally set to a specific value with "=" and a numerical literal. Comma's to separate labels are optional. Close with ")".

Declare an array by refering to a type and suffix it with "[]".

Declare a function by writing an identifier, "(", none or more arguments, ")", and optionally ":" and a return value type. Then either write a semocolon (";") to define a function signature by that name, or start a code block ("{", "}") with none or more statements to perform when the function gets called. Optionally precede the option with a record type, which will be available inside of the function's code block with the 'this' reference ("@@").

Multiple functions with the same name but different argument lists can be declared.
Access the result value of the function with the function name or "??".

3 Object oriented programming
=============================

Ah, good old polymorphic design. It had a good run. Slowly changing things, first with SmallTalk in the 60's (!). Then C++ and later Java and every other language. But then by the end of the century, the poly-inheritance discussion led to interfaces and duck-typing. In practice generics and unit-tests also changed a lot about the way we work.

So in theory it's a kind of a step back: What's important is that the data exists in collections of the same build-up (struct, record), and poly-morphism still lets us extend the definitions with specializations. But the functions or methods that operate on the data exist in a different realm, only taking a reference to pieces of those classes (this, self). And sets of those define the behaviour of the entities (interface, vtable) with their own pedigree of inheritance and specialization.

At the base there's still this thing called class. Defining a class is much similar to defining a struct/record, but follow the identifier with "(" and a base class to inherit from, ")", then "=", "{" and none or more data members. Close with "}".

An important difference is that values of this 'type' will actually hold a reference to the actual place in memory where the object 'lives'. (Objects live on the heap, never on the stack.) Assignments to values of this type are counted, so that the instance is released when the last reference is removed.

To define a constructor, write a function of the same name. A class can have several constructors with different argument lists. //TODO: call inherited constructor
To define a destructor, write "-", the class name, "(", ")", "{" for the code block to execute as destructor, close with "}".

Technics
==========

Identifiers
-----------
Identifier
Literal
String
	'x'
	'x''x'
Comment
	// ...
	/* ... */


Declaration
-----------
NameSpace (start of file)
	Identifier
Const
	Identifier = Literal ;
Type
	Identifier = Identifier ;
	Identifier = Identifier[] ;
	Identifier = Signature
Variable
	Identifier : Identifier ;
Interface
	? Identifier [ : Identifier ] = {
		Signature*
	}
Struct
	Identifier [ : Identifier ] = {
		Variable
		Identifier : Identifier @ Offset ;
		Signature
	}

Signature
	[Identifier .] Identifier ( [Identifier [, Identifier]*] ) [: Identifier];
Function
	Signature CodeBlock
	Signature CodeBlockX

Execution
---------
Expression
	Identifier
	( Expression )
	Expression Operator Expression
	Operator Expression
	Expression Operator
Statement
	Expression ;
	CodeBlock
CodeBlock
	{ Statement* }
CodeBlockX
	{ Statement* Expression }

Selection
	Expression Statement [Statement]
	();;
	(){}{}

Iteration
	& [Expression:bool] Statement [Expression:bool]
	& ( [Expression;] Expression:bool [; Expression]) Statement
	&();
	&{}()
	&(;;){}
	&({}{}){}
	&(Identifier [ : Identifier ] Identifier){}

Operator:
	namespace . Identifier
	left . right
	variable [ Expression ]
	:=
	+ - * / % << >> && ||
	<> != =
	? x	type-of x
	=?	is
	:	as
	@	address of

Specials
	??	result (like return, but not breaking!)
	@@	this
	@@@	inherited
	!!!	raise/break
	:::	try
	>>>	defer Statement
	???	catch
	??? ( Indentifier )

Environment
	<<<	import/include

Other
=======
interpreter
debugger
compiler
(in that order)
to js? LLIR? C? D?
ref counting
weak ref

TODO
====

* heap (malloc, check stack overflow)
* strings on heap, strings from/to array of chars
* struct initial values
* arguments: read-only (except when by ref)
* function variable, also as result x(x)(x) etc.
* JSON, array literals (requires 'standard' key-value storage solution)
* syscall loadlibrary, COM/OLE/ActiveX...
* output LLVMIR? GCC? C?
* initialization/finalization per unit/module/namespace
* threads (or whichever fancy async thing in vogue by then)
* thread local vars
* types, classes! (proper polymorphism)
* constructors, destructor
* getters, setters
* protected, private (-, -- prefix?)
* interfaces
* field not found? "[]"(x:string) method? parse into call
* accept object reference as selection boolean
* accept "0" as null reference
* foreach like &(item=collection) Enumerable interface
* calling conventions?
* something to avoid pre-processors (function versions like in D?)
* disallow adding to namespaces from 'outside'
* call stacks on exception
* detect 'declared but never used'
* detect variable unassigned or no initial value prior to first use
* generics/templates
* something like 'parameters into' a code block (i.e. lambda)
* enable self-referencing classes (forward?)
* 'check' method to call on each varindex (stateful objects)
* switch,case syntax
* proper docs/guide/specs/tutorials
* conditional compilation

IDEA'S
======

* dump: store errors in sphere data file and show inbetween things
* merge xsu's?
* still to InheritedFrom on ttRecord?

Considerations
==============

Case-sensitive or not
---------------------

I have written far more code in Pascal/Delphi than in C/C++, but enough of both to compare what it's like to have a language that's case-sensitive or not. I personally find it an advantage of Pascal that it's case-insensitive, but in practice write code as if it was not, very strictly so. But it's more of a personal code hygene thing I don't get compiler support for. C/C++ has a history of trying to get the compiler as performant as possible on old hardware. It's why a single equal sign was chosen for assigment and a double equal sign to compare values. So I guess it figured to stick with case sensitivity since it is what you get with binary comparing strings. But I very much regret it got used in codebases that have seen wide adoption, where constants exists in all-caps, but important variables have the same names in miniscules.

So for this new language I select case-sensitivity, but very strongly urge to avoid declaring multilpe things with the same name with different capitalization. I thought about having the language prevent this, or have case-insensitivity anyway to prevent this and keep to virtual case-sensitivity myself, but I don't like languages that take freedom away. So you have the freedom to, but the warm, friendly advice not to.

Prefix increment/decrement
--------------------------

I never use them. I think they're never good for readability. "i++;" I can live with. "for(i=0;i<x;i++)" is clear in one glance what's going on. Or in the new form: "&({i=0}i<x{i++})". Therefore I don't have them here. Use postfix "++" and "--" only.

Reflection
----------

Reflection is not something I was looking for in a first version. It could get added later, if you want to go CLR or JVM then it's possibly required. To get a good first feel how the language is to work with I guess we'll do just fine with code that can't query it's own build-up. Hashes (kay-value stores) and/or JSON objects should provide enough of what we need.

The Name
--------

When taking on a project, selecting the programming language is a strategic choice. One of many, but still, part of the strategy. That's mainly where the name comes from. If I have a quick search of the web for "Strato" there's a philosopher and an Indo-Greek king, and nothing really computer-related (except a hosting provoder in Germany, hmm). I think it passes the "bar exam" (where you test the understandability of a brand name by shouting it to someone in a crowded bar), should try that, though, one day.

Why
---

Of course people are going to ask why. I have that with many projects I undertake. I have no plans to take over the world, none at all. This is something I needed to do for myself, to deepen the knowledge of the theoretics of programming languages. It helps me appreciate even more the effort that went into the more established languages I use for the day to day work. It's a way to vent for me. I took a chance to put anything in I want for a language, and leave out what I don't like about languages, and see if the result would be anything workable. I enjoy creating something, putting some work in and have a usable result. I share this so it may mean something to someone else as well, and perhaps, just maybe, it will grow into something more with the help of anyone that gets inspired by my work.