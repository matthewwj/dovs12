declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x72 = alloca i64
 %call71 = call i64 @read_integer ()
 store i64 %call71, i64* %x72
 %load73 = load i64, i64* %x72
 ret i64 %load73
}
