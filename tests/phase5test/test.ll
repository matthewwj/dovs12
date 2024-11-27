%array_type = type { i64, [0 x i8] }

@str0 = global { i64, [2 x i8] } {i64 2, [2 x i8] c"hi"}
@T1 = global { i64, i64 } {i64 0, i64 0}
@qwe = global { i64, i64 } {i64 0, i64 0}

declare void @print_integer(i64)
declare i64 @read_integer()
declare i1 @compare_strings(%array_type*, %array_type*)

define i64 @dolphin_fun_main () {
 %z6 = alloca %array_type*
 %m2 = alloca %array_type*
 %str_bitcast1 = bitcast { i64, [2 x i8] }* @str0 to %array_type*
 store %array_type* %str_bitcast1, %array_type** %m2
 %load3 = load %array_type*, %array_type** %m2
 %load4 = load %array_type*, %array_type** %m2
 %streq5 = call i1 @compare_strings ({ i64, [0 x i8] }* %load3, { i64, [0 x i8] }* %load4)
 store i1 %streq5, i1* %z6
 ret i64 2
}
