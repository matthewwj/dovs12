declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %a72 = alloca i64
 %a71 = alloca i64
 store i64 1, i64* %a71
 store i64 2, i64* %a72
 %load73 = load i64, i64* %a72
 ret i64 %load73
}
