declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x78 = alloca i64
 %x77 = alloca i64
 store i64 5, i64* %x77
 store i64 10, i64* %x78
 %load79 = load i64, i64* %x78
 %temp_name80 = add i64 %load79, 2
 %load81 = load i64, i64* %x77
 %temp_name82 = add i64 %load81, 2
 %temp_name83 = add i64 2, 2
 ret i64 %temp_name83
}
