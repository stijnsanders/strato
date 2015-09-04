oo @ 8000

object:={
	_basetype:?object@-@?0;//class
	_refcount:number@-@?0*2;
	//implemented interfaces?
}

object_base_size=@?0*2;

object(){
	@ @@:pointer:=__malloc(@?@@+object_base_size);
	@ @@:number+=object_base_size;
	_basetype:=?@@;
	_refcount:=0;
}

-object(){
	//assert _refcount=0
	:x:pointer:=@ @@:pointer;
	x:number-=object_base_size;
	__mfree(x);
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
