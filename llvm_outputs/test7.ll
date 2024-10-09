declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x2 = alloca i64
 store i64 10, i64* %x2
 %load3 = load i64, i64* %x2
 %call4 = call void @print_integer (i64 %load3)
 %temp_name5 = add i64 2, 2
 ret i64 %temp_name5
}
