declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x1 = alloca i64
 %temp_name0 = add i64 2, 2
 store i64 %temp_name0, i64* %x1
 br label %for_continue2
for_continue2:
 %load5 = load i64, i64* %x1
 %temp_name6 = icmp sgt i64 %load5, 0
 br i1 %temp_name6, label %for_body3, label %exit_for_loop4
for_body3:
 %load7 = load i64, i64* %x1
 %temp_name8 = sub i64 %load7, 1
 store i64 %temp_name8, i64* %x1
 br label %for_continue2
exit_for_loop4:
 %load9 = load i64, i64* %x1
 ret i64 %load9
}
