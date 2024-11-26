%array_type = type { i64, [0 x i8] }

@str0 = global { i64, [12 x i8] } {i64 12, [12 x i8] c"Hello World\0A"}

declare void @print_integer(i64)
declare i64 @read_integer()
declare i64 @compare_strings(%array_type*, %array_type*)

define i64 @dolphin_main () {
 %y4 = alloca %array_type*
 %test2 = alloca %array_type*
 %str_bitcast1 = bitcast { i64, [12 x i8] }* @str0 to %array_type*
 store %array_type* %str_bitcast1, %array_type** %test2
 %load3 = load %array_type*, %array_type** %test2
 store %array_type* %load3, %array_type** %y4
 ret i64 125
}
