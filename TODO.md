TODO
====

* lookup b-tree's (instead of linked list)
* resolve operators just like virtual methods
* read-only function arguments (or even just read-only) (except by ref)
* polyvalent by context '=','&','|'
* interfaces, properties (extensive testing/dogfooding)
* prevent namespace-crossing (also: ttPrivate)

* heap (malloc, check stack overflow)
* strings on heap, strings from/to array of chars
* struct initial values
* infer untyped local when first used as output argument
* function variable, also as result x(x)(x) etc.
* JSON, array literals (requires 'standard' key-value storage solution)
* syscall loadlibrary, COM/OLE/ActiveX...
* output LLVMIR? GCC? C?
* chained comparison operators (as in 2 < x < 8 )
* "---" denoting 'implementation': declarations below line not accessible from 'outside'
* data hiding (no access to values of class from 'outside' of class? doc!)
* protected, private (-, -- prefix?)
* threads (or whichever fancy async thing in vogue by then)
* thread local vars
* lazy evaluation logic operators (and,or)
* abstract methods (just with missing implementation?)
* interfaces
* 'default' property "" for "[]" on instance
* field not found? "[]"(x:string) method? parse into call
* accept object reference as selection boolean
* accept "0" as null reference
* foreach like &(item=collection) Enumerable interface
* calling conventions?
* replace syscall with genuine interrupt calls
* something to avoid pre-processors (function versions like in D?)
* disallow adding to namespaces from 'outside'
* call-stack on exception
* detect/fake access violations (e.g. with null pointer)
* detect 'declared but never used'
* detect variable unassigned or no initial value prior to first use
* generics/templates
* something like 'parameters into' a code block (i.e. lambda)
* enable self-referencing classes (forward?)
* 'check' method to call on each varindex (stateful objects)
* switch,case syntax
* proper docs/guide/specs/tutorials
* conditional compilation
* xsView: status-bar with 'path' of node (build from parents with an index)

IDEA'S
======

* dump: store errors in sphere data file and show inbetween things
* merge xsu's? or:
* 'link' hsu's by prefix ID, map links to hsu with relative prefixes.
* still to InheritedFrom on ttRecord?
* all offsets into TypeDecl_object negative to instance pointers
* switch to allow pointer arith, default off! (also 'dirty' pointer casts?)

DO OR DON'T?
============

* namespace prefix: more namespace defs in one file? (anonymous namespace?)
