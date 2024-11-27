%string_type = type { i64, [0 x i8] }

@string_0 = global [2 x i8] c"hi"
@string_struct_1 = global { i64, [2 x i8]* } {i64 2, [2 x i8]* @string_0}
@T1 = global { i64, i64 } {i64 0, i64 0}
@qwe = global { i64, i64 } {i64 0, i64 0}

declare void @print_integer(i64)
declare i64 @read_integer()
declare i1 @compare_strings(%string_type**, %string_type**)

define i64 @dolphin_fun_main () {
 %z7 = alloca %string_type*
 %m3 = alloca %string_type*
 %bitcast2 = bitcast { i64, [2 x i8]* }* @string_struct_1 to %string_type*
 store %string_type* %bitcast2, %string_type** %m3
 %load4 = load %string_type*, %string_type** %m3
 %load5 = load %string_type*, %string_type** %m3
 %streq6 = call i1 @compare_strings ({ i64, [0 x i8] }* %load4, { i64, [0 x i8] }* %load5)
 store i1 %streq6, i1* %z7
 ret i64 2
}
