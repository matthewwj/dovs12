%dolphin_record_udp_recvfrom_result = type { %array_type*, %dolphin_record_socket_address* }
%dolphin_record_accepted_connection = type { %dolphin_record_socket*, %dolphin_record_socket_address* }
%dolphin_record_socket = type {  }
%dolphin_record_socket_address = type {  }
%dolphin_record_ip_address = type {  }
%dolphin_record_ip_version = type {  }
%dolphin_record_connection_type = type {  }
%dolphin_record_stream = type {  }
%array_type = type { i64, [0 x i8] }

@str0 = global { i64, [12 x i8] } {i64 12, [12 x i8] c"Hello World\0A"}

declare i64 @compare_strings(%array_type*, %array_type*)
declare i8* @allocate_record(i32)
declare i8* @raw_allocate_on_heap(i32)
declare %array_type* @allocate_array(i32, i64, i8*)
declare void @report_error_array_index_out_of_bounds()
declare void @report_error_nil_access()
declare void @report_error_division_by_zero()
declare %dolphin_record_udp_recvfrom_result* @socket_recvfrom_udp(%dolphin_record_socket*)
declare i64 @socket_sendto_udp(%dolphin_record_socket*, %dolphin_record_socket_address*, %array_type*)
declare i1 @socket_close(%dolphin_record_socket*)
declare i1 @socket_activate_udp(%dolphin_record_socket*)
declare i1 @socket_connect(%dolphin_record_socket*, %dolphin_record_socket_address*)
declare %dolphin_record_accepted_connection* @socket_accept(%dolphin_record_socket*)
declare i1 @socket_listen(%dolphin_record_socket*, i64)
declare i1 @socket_bind(%dolphin_record_socket*, %dolphin_record_socket_address*)
declare i64 @get_port_of_socket_address(%dolphin_record_socket_address*)
declare %dolphin_record_ip_address* @get_ip_address_of_socket_address(%dolphin_record_socket_address*)
declare %dolphin_record_socket_address* @create_socket_address(%dolphin_record_ip_address*, i64)
declare %array_type* @ip_address_to_string(%dolphin_record_ip_address*)
declare %dolphin_record_ip_address* @string_to_ip_address(%dolphin_record_ip_version*, %array_type*)
declare %dolphin_record_stream* @socket_get_output_stream(%dolphin_record_socket*)
declare %dolphin_record_stream* @socket_get_input_stream(%dolphin_record_socket*)
declare %dolphin_record_socket* @create_socket(%dolphin_record_ip_version*, %dolphin_record_connection_type*)
declare %dolphin_record_ip_address* @get_ipv6_address_any()
declare %dolphin_record_ip_address* @get_ipv4_address_any()
declare %dolphin_record_ip_version* @get_ipv6()
declare %dolphin_record_ip_version* @get_ipv4()
declare %dolphin_record_connection_type* @get_tcp_connection_type()
declare %dolphin_record_connection_type* @get_udp_connection_type()
declare %array_type* @bytes_array_to_string(%array_type*)
declare %array_type* @string_to_bytes_array(%array_type*)
declare i64 @byte_to_int_unsigned(i8)
declare i64 @byte_to_int_signed(i8)
declare i8 @int_to_byte_unsigned(i64)
declare i8 @int_to_byte_signed(i64)
declare i64 @ascii_ord(%array_type*)
declare %array_type* @ascii_chr(i64)
declare %array_type* @string_concat(%array_type*, %array_type*)
declare %array_type* @substring(%array_type*, i64, i64)
declare %array_type* @int_to_string(i64)
declare i64 @string_to_int(%array_type*)
declare i64 @input_byte(%dolphin_record_stream*)
declare i1 @output_byte(i8, %dolphin_record_stream*)
declare %array_type* @input_bytes_array(i64, %dolphin_record_stream*)
declare void @output_bytes_array(%array_type*, %dolphin_record_stream*)
declare void @output_string(%array_type*, %dolphin_record_stream*)
declare i1 @seek_in_file(i64, i1, %dolphin_record_stream*)
declare i64 @pos_in_file(%dolphin_record_stream*)
declare i1 @close_file(%dolphin_record_stream*)
declare i1 @flush_file(%dolphin_record_stream*)
declare i1 @error_in_file(%dolphin_record_stream*)
declare i1 @end_of_file(%dolphin_record_stream*)
declare i64 @get_eof()
declare %dolphin_record_stream* @open_file(%array_type*, %array_type*)
declare %dolphin_record_stream* @get_stdin()
declare %dolphin_record_stream* @get_stderr()
declare %dolphin_record_stream* @get_stdout()
declare %array_type* @get_cmd_args()
declare void @exit(i64)


define i64 @dolphin_main () {
 %test2 = alloca %array_type*
 %str_bitcast1 = bitcast { i64, [12 x i8] }* @str0 to %array_type*
 store %array_type* %str_bitcast1, %array_type** %test2
 ret i64 1
}