%string_type = type { i64, [0 x i8] }
%T1 = type { i64, i64 }
%qwe = type { i64, i64 }

@string_9 = global [3 x i8] c"hii"
@string_struct_10 = global { i64, [3 x i8]* } {i64 3, [3 x i8]* @string_9}

declare void @print_integer(i64)
declare i64 @read_integer()
declare i1 @compare_strings(%string_type*, %string_type*)
declare i8* @allocate_record(i32)
declare i8* @allocate_array(i32, i64, i8*)

define i64 @dolphin_fun_main () {
 %z23 = alloca %qwe*
 %s16 = alloca i1
 %m12 = alloca %string_type*
 %p8 = alloca i64*
 %y1 = alloca i64
 %call0 = call i64 @test ()
 store i64 %call0, i64* %y1
 %size_ptr2 = getelementptr i64*, i64** null, i64 1
 %size3 = ptrtoint i64** %size_ptr2 to i32
 %default_val_4 = alloca i64*
 store i64* null, i64** %default_val_4
 %bitcast5 = bitcast i64** %default_val_4 to i8*
 %malloc_ptr6 = call i8* @allocate_array (i32 %size3, i64 2, i8* %bitcast5)
 %bitcast7 = bitcast i8* %malloc_ptr6 to i64*
 store i64* %bitcast7, i64** %p8
 %bitcast11 = bitcast { i64, [3 x i8]* }* @string_struct_10 to %string_type*
 store %string_type* %bitcast11, %string_type** %m12
 %load13 = load %string_type*, %string_type** %m12
 %load14 = load %string_type*, %string_type** %m12
 %streq15 = call i1 @compare_strings ({ i64, [0 x i8] }* %load13, { i64, [0 x i8] }* %load14)
 store i1 %streq15, i1* %s16
 %size_ptr17 = getelementptr %qwe, %qwe* null, i32 1
 %size18 = ptrtoint %qwe* %size_ptr17 to i32
 %malloc_ptr19 = call i8* @allocate_record (i32 %size18)
 %qwe_ptr20 = bitcast i8* %malloc_ptr19 to %qwe*
 %gep21 = getelementptr %qwe, %qwe* %qwe_ptr20, i32 0, i32 0
 store i64 1, i64* %gep21
 %gep22 = getelementptr %qwe, %qwe* %qwe_ptr20, i32 0, i32 1
 store i64 2, i64* %gep22
 store %qwe* %qwe_ptr20, %qwe** %z23
 %load24 = load i64, i64* %y1
 ret i64 %load24
}

define i64 @test () {
 ret i64 21
}
