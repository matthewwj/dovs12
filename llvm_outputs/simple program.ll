declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x1 = alloca i64
 %temp_name0 = add i64 2, 2
 store i64 %temp_name0, i64* %x1
 br label %while_continue2
while_continue2:
 %load5 = load i64, i64* %x1
 %temp_name6 = icmp sgt i64 %load5, 0
 br i1 %temp_name6, label %while_body3, label %exit_while_loop4
while_body3:
 br label %while_continue7
while_continue7:
 %load10 = load i64, i64* %x1
 %temp_name11 = icmp sgt i64 %load10, 0
 br i1 %temp_name11, label %while_body8, label %exit_while_loop9
while_body8:
 %load12 = load i64, i64* %x1
 %temp_name13 = sub i64 %load12, 1
 store i64 %temp_name13, i64* %x1
 br label %while_continue7
exit_while_loop9:
 br label %while_continue2
exit_while_loop4:
 br label %for_continue14
for_continue14:
 %load17 = load i64, i64* %x1
 %temp_name18 = icmp sgt i64 %load17, 0
 br i1 %temp_name18, label %for_body15, label %exit_for_loop16
for_body15:
 %load19 = load i64, i64* %x1
 %temp_name20 = sub i64 %load19, 1
 store i64 %temp_name20, i64* %x1
 br label %for_continue14
exit_for_loop16:
 br label %while_continue21
while_continue21:
 %load24 = load i64, i64* %x1
 %temp_name25 = icmp sgt i64 %load24, 0
 br i1 %temp_name25, label %while_body22, label %exit_while_loop23
while_body22:
 br label %exit_while_loop23
post_break26:
 br label %while_continue21
exit_while_loop23:
 %temp_name27 = add i64 2, 2
 ret i64 %temp_name27
}
