declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x13 = alloca i64
 store i64 5, i64* %x13
 store i64 10, i64* %x13
 %temp_name14 = add i64 2, 2
 ret i64 %temp_name14
}
