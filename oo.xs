oo

object:={
	_basetype:type@-@?0;//class
	_refcount:number@-@?0*2;
	//implemented interfaces?
}

object(){
	@ @@:pointer:=__malloc(@?@@);
	_basetype:=?@@;
	_refcount:=0;
}

-object(){
	//assert _refcount=0
	//__dealloc(@(@@));
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
