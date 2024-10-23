declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %y31 = alloca i64
 %x28 = alloca i64
 store i64 10, i64* %x28
 %load29 = load i64, i64* %x28
 %temp_name30 = add i64 %load29, 5
 store i64 %temp_name30, i64* %y31
 %load32 = load i64, i64* %x28
 %load33 = load i64, i64* %y31
 %temp_name34 = add i64 %load32, %load33
 ret i64 %temp_name34
}
