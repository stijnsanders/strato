TODO
====

* heap (malloc, check stack overflow)
* strings on heap, strings from/to array of chars
* struct initial values
* arguments: read-only (except when by ref)
* infer untyped local when first used as output argument
* function variable, also as result x(x)(x) etc.
* JSON, array literals (requires 'standard' key-value storage solution)
* syscall loadlibrary, COM/OLE/ActiveX...
* output LLVMIR? GCC? C?
* initialization/finalization per unit/module/namespace
* chained comparison operators (as in 2 < x < 8 )
* [or] "---" denoting 'implementation': declarations below line not accessible from 'outside'
* threads (or whichever fancy async thing in vogue by then)
* thread local vars
* types, classes! (proper polymorphism)
* constructors, destructor
* virtual methods (as it currently stands, inheriters but pointer to base class don't jump to overriden overload)
* data hiding (no access to values of class from 'outside' of class? doc!)
* getters, setters (with parameters [])
* inherited (@@@) on property getters/setters
* protected, private (-, -- prefix?)
* abstract methods (just with missing implementation?)
* interfaces
* 'default' property "" for "[]" on instance
* field not found? "[]"(x:string) method? parse into call
* accept object reference as selection boolean
* accept "0" as null reference
* foreach like &(item=collection) Enumerable interface
* calling conventions?
* something to avoid pre-processors (function versions like in D?)
* disallow adding to namespaces from 'outside'
* call-stack on exception
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
* xsView: right-pane with source-file line:pos highlighted
* xsView: look-up, global and local(=relative)

IDEA'S
======

* dump: store errors in sphere data file and show inbetween things
* merge xsu's? or:
* 'link' hsu's by prefix ID, map links to hsu with relative prefixes.
* still to InheritedFrom on ttRecord?

DO OR DON'T?
============

* namespace prefix: more namespace defs in one file? (anonymous namespace?)
