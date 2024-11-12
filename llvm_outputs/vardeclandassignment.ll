declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x84 = alloca i64
 store i64 5, i64* %x84
 store i64 10, i64* %x84
 %temp_name85 = add i64 2, 2
 ret i64 %temp_name85
}
