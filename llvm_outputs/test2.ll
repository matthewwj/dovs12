declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x0 = alloca i64
 store i64 5, i64* %x0
 %temp_name1 = add i64 2, 2
 ret i64 %temp_name1
}
