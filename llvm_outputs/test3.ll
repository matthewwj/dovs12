declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %y41 = alloca i64
 %x38 = alloca i64
 store i64 10, i64* %x38
 %load39 = load i64, i64* %x38
 %temp_name40 = add i64 %load39, 5
 store i64 %temp_name40, i64* %y41
 %load42 = load i64, i64* %x38
 %load43 = load i64, i64* %y41
 %temp_name44 = add i64 %load42, %load43
 ret i64 %temp_name44
}
