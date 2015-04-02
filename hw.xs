hw
//hello world example program

<<< oo

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
	@@@();
	_value:=x;
}

test1.vtest(x:number):number{
	vtest:=x+2;
}

test1.x:number{??:=_value;}{_value:=??;};
test1.y:number{??:=_value;};

test2:test1={}
test2(){
	@@@(123);
}
-test2(){
	__writeln("pre dtor");
	@@@();
	__writeln("post dtor");
}

test2.vtest(x:number):number{
	vtest:=@@@(x+2)+2;
}

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

	yy:test1;
	yy:=test2(111);
	Shell.write(yy.vtest(222):string);
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
