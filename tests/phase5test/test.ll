%string_type = type { i64, [0 x i8] }
%dolphin_record_stream = type {  }
%T1 = type { i64, i64 }
%qwe = type { i64, i64 }

@dolphin_rc_empty_string = external global %string_type

@string_16 = global [3 x i8] c"hii"
@string_struct_17 = global { i64, [3 x i8]* } {i64 3, [3 x i8]* @string_16}

declare i64 @compare_strings(%string_type**, %string_type**)
declare i8* @allocate_record(i32)
declare i8* @raw_allocate_on_heap(i32)
declare i8* @allocate_array(i32, i64, i8*)
declare void @report_error_array_index_out_of_bounds()
declare void @report_error_nil_access()
declare void @report_error_division_by_zero()
declare i64 @string_length(%string_type**)
declare i64 @read_integer()
declare void @print_integer(i64)
declare %string_type* @bytes_array_to_string(i8*)
declare i8* @string_to_bytes_array(%string_type*)
declare i64 @byte_to_int_unsigned(i8)
declare i64 @byte_to_int_signed(i8)
declare i8 @int_to_byte_unsigned(i64)
declare i8 @int_to_byte_signed(i64)
declare i64 @ascii_ord(%string_type*)
declare %string_type* @ascii_chr(i64)
declare %string_type** @string_concat(%string_type*, %string_type*)
declare %string_type* @substring(%string_type*, i64, i64)
declare %string_type* @int_to_string(i64)
declare i64 @string_to_int(%string_type*)
declare i64 @input_byte(%dolphin_record_stream*)
declare i1 @output_byte(i8, %dolphin_record_stream**)
declare i8* @input_bytes_array(i64, %dolphin_record_stream**)
declare void @output_bytes_array(i8*, %dolphin_record_stream**)
declare void @output_string(i8*, %dolphin_record_stream**)
declare i1 @seek_in_file(i64, i1, %dolphin_record_stream**)
declare i64 @pos_in_file(%dolphin_record_stream**)
declare i1 @close_file(%dolphin_record_stream**)
declare i1 @flush_file(%dolphin_record_stream**)
declare i1 @error_in_file(%dolphin_record_stream**)
declare i1 @end_of_file(%dolphin_record_stream**)
declare i64 @get_eof()
declare %dolphin_record_stream** @open_file(%string_type*, %string_type*)
declare %dolphin_record_stream** @get_stdin()
declare %dolphin_record_stream** @get_stderr()
declare %dolphin_record_stream** @get_stdout()
declare %string_type** @get_cmd_args()
declare void @exit(i64)

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
 %call35 = call i64 @read_integer ()
 ret i64 %call35
}
