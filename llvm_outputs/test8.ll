declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x10 = alloca i64
 %x9 = alloca i64
 store i64 5, i64* %x9
 store i64 10, i64* %x10
 %load11 = load i64, i64* %x10
 %temp_name12 = add i64 %load11, 2
 %load13 = load i64, i64* %x9
 %temp_name14 = add i64 %load13, 2
 %temp_name15 = add i64 2, 2
 ret i64 %temp_name15
}
