hw
//hello world example program

sig1(x:number):number;

ttt1:number;

sig1(x,y:number):number;


ttt2:number;

sig1(x:number):number{
	__writeln("first sig, decl later");
}

ttt3:number;

sig1(x,y:number):number{
}

ttt4:number;

object:={
	_basetype:pointer;//class
	_refcount:number;
	//implemented interfaces?
}

object(){
	_basetype:=0:pointer;
	_refcount:=0;
}

-object(){
	//assert _refcount=0
}

object._destroy(){
	//TODO dealloc
}

object._addref(){
	__xinc(_refcount);
}

object._release(){
	(__xdec(_refcount)==0) {-@@();} {}
}

/*
object."[]"(x:string):string{
	??:="["+x+"]";
}
*/

test1:object={
	_value:number;
}

test1(){
	_value:=1;
}

-test1(){
	__writeln("test1 free");
}

test1(x:number){
	_value:=x;
}

test1.x:number{??:=_value;}{_value:=??;};
test1.y:number{??:=_value;};

/*
test2:test1={}
test2(){
	@@@(123);
}
-test2(){
	__writeln("pre dtor");
	@@@;
	__writeln("post dtor");
}
*/

//test1.xx[y:number]:number{??:=_value*y;}{_value:=??/y;};
//test1.""[x:number]:number{}{};

<<< shell
{
/*
	x:object;
	y:type;
	y:=test1;
	(?x=y)Shell.write('test');;
*/
	x:object;
	x:=test1(11);
	Shell.write((x:test1).x:string);
	(x:test1).x:=100;
}

/*
<<< shell

?someintf{
	Wait();
	Next():string;
}

test={
	value:number;
	field1:number;
}

test.someop(x:number):string;

test.test1(y:number):string{
	test1:="test"+(y+5):string;
}

test.Wait(){
	__writeln("Wait called");
}

test.Next():string{
	Next:=value++:string;
}

{
	x:someintf;
	x.Wait();
	
	y:test;
	y.value:=12;
	
	xx:test.someop;
	xx:=test.test1;
	
	Shell.write(y.xx(1));
	
}{
	Shell.write("all done");
}
*/