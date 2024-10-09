declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x7 = alloca i64
 %x6 = alloca i64
 store i64 5, i64* %x6
 store i64 10, i64* %x7
 %load8 = load i64, i64* %x7
 %temp_name9 = add i64 %load8, 2
 %load10 = load i64, i64* %x6
 %temp_name11 = add i64 %load10, 2
 %temp_name12 = add i64 2, 2
 ret i64 %temp_name12
}
