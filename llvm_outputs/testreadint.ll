declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x62 = alloca i64
 %call61 = call i64 @read_integer ()
 store i64 %call61, i64* %x62
 %load63 = load i64, i64* %x62
 ret i64 %load63
}
