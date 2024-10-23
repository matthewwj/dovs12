declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %c67 = alloca i1
 %b66 = alloca i64
 %a64 = alloca i64
 store i64 1, i64* %a64
 %temp_name65 = add i64 2, 3
 store i64 %temp_name65, i64* %b66
 store i1 1, i1* %c67
 %load68 = load i64, i64* %a64
 %load69 = load i64, i64* %b66
 %temp_name70 = add i64 %load68, %load69
 ret i64 %temp_name70
}
