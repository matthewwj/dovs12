declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x74 = alloca i64
 store i64 10, i64* %x74
 %load75 = load i64, i64* %x74
 call void @print_integer (i64 %load75)
 %temp_name76 = add i64 2, 2
 ret i64 %temp_name76
}
