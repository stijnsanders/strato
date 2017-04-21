Strato

:variant @16;//TODO: OLE compatible variants
:i8  @1;
:i16 @2;
:i32 @4;
:i64 @8;
:u8  @1;
:u16 @2;
:u32 @4;
:u64 @8;
:f32 @4;//TODO: floating-point support
:f64 @8;

:hash;//TODO:

__xinc(a:^number):number{1}
__xdec(a:^number):number{2}
__writeln(a:string){$200}
__malloc(a:number):pointer{$100}
__realloc(d:pointer;a:number):pointer{$101}
__mfree(d:pointer){$102}
