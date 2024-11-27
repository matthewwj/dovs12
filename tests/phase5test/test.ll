%string_type = type { i64, [0 x i8] }
%T1 = type { i64, i64 }
%qwe = type { i64, i64 }

@dolphin_rc_empty_string = external global %string_type

@string_16 = global [3 x i8] c"hii"
@string_struct_17 = global { i64, [3 x i8]* } {i64 3, [3 x i8]* @string_16}

declare void @print_integer(i64)
declare i64 @read_integer()
declare i1 @compare_strings(%string_type*, %string_type*)
declare i8* @allocate_record(i32)
declare i8* @allocate_array(i32, i64, i8*)
declare i64 @string_length(%string_type**)

define i64 @dolphin_fun_main () {
 %z30 = alloca %qwe*
 %s23 = alloca i1
 %m19 = alloca %string_type*
 %a15 = alloca %string_type**
 %p8 = alloca i64*
 %y1 = alloca i64
 %call0 = call i64 @test ()
 store i64 %call0, i64* %y1
 %size_ptr2 = getelementptr i64, i64* null, i64 1
 %size3 = ptrtoint i64* %size_ptr2 to i32
 %default_val_4 = alloca i64
 store i64 0, i64* %default_val_4
 %bitcast5 = bitcast i64* %default_val_4 to i8*
 %malloc_ptr6 = call i8* @allocate_array (i32 %size3, i64 2, i8* %bitcast5)
 %bitcast7 = bitcast i8* %malloc_ptr6 to i64*
 store i64* %bitcast7, i64** %p8
 %size_ptr9 = getelementptr %string_type*, %string_type** null, i64 1
 %size10 = ptrtoint %string_type** %size_ptr9 to i32
 %default_val_11 = alloca %string_type*
 store %string_type* @dolphin_rc_empty_string, %string_type** %default_val_11
 %bitcast12 = bitcast %string_type** %default_val_11 to i8*
 %malloc_ptr13 = call i8* @allocate_array (i32 %size10, i64 3, i8* %bitcast12)
 %bitcast14 = bitcast i8* %malloc_ptr13 to %string_type**
 store %string_type** %bitcast14, %string_type*** %a15
 %bitcast18 = bitcast { i64, [3 x i8]* }* @string_struct_17 to %string_type*
 store %string_type* %bitcast18, %string_type** %m19
 %load20 = load %string_type*, %string_type** %m19
 %load21 = load %string_type*, %string_type** %m19
 %streq22 = call i1 @compare_strings ({ i64, [0 x i8] }* %load20, { i64, [0 x i8] }* %load21)
 store i1 %streq22, i1* %s23
 %size_ptr24 = getelementptr %qwe, %qwe* null, i32 1
 %size25 = ptrtoint %qwe* %size_ptr24 to i32
 %malloc_ptr26 = call i8* @allocate_record (i32 %size25)
 %qwe_ptr27 = bitcast i8* %malloc_ptr26 to %qwe*
 %gep28 = getelementptr %qwe, %qwe* %qwe_ptr27, i32 0, i32 0
 store i64 1, i64* %gep28
 %gep29 = getelementptr %qwe, %qwe* %qwe_ptr27, i32 0, i32 1
 store i64 2, i64* %gep29
 store %qwe* %qwe_ptr27, %qwe** %z30
 %load31 = load %string_type**, %string_type*** %a15
 %gep_idx32 = getelementptr %string_type**, %string_type*** %load31, i64 1
 %load33 = load %string_type*, %string_type** %gep_idx32
 %str_size34 = call i64 @string_length (%string_type* %load33)
 ret i64 %str_size34
}

define i64 @test () {
 ret i64 21
}
