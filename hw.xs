hw
//hello world example program

/*
object={
	_basetype:pointer;//class
	_refcount:number;
	//implemented interfaces?
}

object._addref(){
	__xinc(_refcount);
}

object._release(){
	__xdec(_refcount)==0 _destroy(); ;
}

object(){
	_basetype:=0;
	_refcount:=0;
}

-object(){
	//assert _refcount=0
}

object."[]"(x:string):string{
	??:="["+x+"]";
}


test1=(object){
	_value:number;
}

test1(){
	_value:=1;
}

test1.x:number{_value:=??;}{??:=_value;};

<<< shell
main(p:shell){
	x:object;
	y:type;
	y:=test1;
	(?x=y)p.write('test');;
}
*/


//test(x,^y:number){y:=x+5;}

<<<shell
main(p:shell){
	x:=200;
	y:=__xinc(x);
	p.write(x:string);
	p.write(y:string);
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

main(p:shell){
	p.write("Hello world!");

	x:someintf;
	x.Wait();
	
	y:test;
	y.value:=12;
	
	xx:test.someop;
	xx:=test.test1;
	
	p.write(y.xx(1));
	
}

*/