declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x2 = alloca i64
 store i64 10, i64* %x2
 %call3 = call i64 @read_integer ()
 %temp_name4 = add i64 2, 2
 ret i64 %temp_name4
}
