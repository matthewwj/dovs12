%string_type = type { i64, [0 x i8] }
%dolphin_record_stream = type {  }
%T1 = type { i64, i64 }

@dolphin_rc_empty_string = external global %string_type

declare i64 @compare_strings(%string_type*, %string_type*)
declare i8* @allocate_record(i32)
declare i8* @raw_allocate_on_heap(i32)
declare i8* @allocate_array(i32, i64, i8*)
declare void @report_error_array_index_out_of_bounds()
declare void @report_error_nil_access()
declare void @report_error_division_by_zero()
declare i64 @string_length(%string_type*)
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
declare void @output_string(%string_type*, %dolphin_record_stream*)
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
declare %dolphin_record_stream* @get_stdout()
declare %string_type** @get_cmd_args()
declare void @exit(i64)

define i64 @dolphin_fun_main () {
 %a6 = alloca %T1*
 %size_ptr0 = getelementptr %T1, %T1* null, i32 1
 %size1 = ptrtoint %T1* %size_ptr0 to i32
 %malloc_ptr2 = call i8* @allocate_record (i32 %size1)
 %T1_ptr3 = bitcast i8* %malloc_ptr2 to %T1*
 %gep4 = getelementptr %T1, %T1* %T1_ptr3, i32 0, i32 0
 store i64 2, i64* %gep4
 %gep5 = getelementptr %T1, %T1* %T1_ptr3, i32 0, i32 1
 store i64 0, i64* %gep5
 store %T1* %T1_ptr3, %T1** %a6
 ret i64 1
}
