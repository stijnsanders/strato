hw
//hello world example program

object:={
	_basetype:pointer;//class
	_refcount:number;
	//implemented interfaces?
}

object(){
	_basetype:=0;
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

//test1.xx[y:number]:number{??:=_value*y;}{_value:=??/y;};
//test1.""[x:number]:number{}{};

test1Size=@?test1;

<<< shell
main(p:shell){
/*
	x:object;
	y:type;
	y:=test1;
	(?x=y)p.write('test');;
*/

	p.write(@?object:string);
	p.write(@?test1:string+" "+test1Size:string);

	x:object;
	x:=test1(11);
	p.write((x:test1).x:string);

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
	x:someintf;
	x.Wait();
	
	y:test;
	y.value:=12;
	
	xx:test.someop;
	xx:=test.test1;
	
	p.write(y.xx(1));
	
}
*/