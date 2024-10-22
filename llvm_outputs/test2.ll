declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x11 = alloca i64
 %temp_name10 = add i64 2, 2
 store i64 %temp_name10, i64* %x11
 br label %while_continue12
while_continue12:
 %load15 = load i64, i64* %x11
 %temp_name16 = icmp sgt i64 %load15, 0
 br i1 %temp_name16, label %while_body13, label %exit_while_loop14
while_body13:
 br label %while_continue17
while_continue17:
 %load20 = load i64, i64* %x11
 %temp_name21 = icmp sgt i64 %load20, 0
 br i1 %temp_name21, label %while_body18, label %exit_while_loop19
while_body18:
 %load22 = load i64, i64* %x11
 %temp_name23 = sub i64 %load22, 1
 store i64 %temp_name23, i64* %x11
 br label %while_continue17
exit_while_loop19:
 br label %while_continue12
exit_while_loop14:
 br label %for_continue24
for_continue24:
 %load27 = load i64, i64* %x11
 %temp_name28 = icmp sgt i64 %load27, 0
 br i1 %temp_name28, label %for_body25, label %exit_for_loop26
for_body25:
 %load29 = load i64, i64* %x11
 %temp_name30 = sub i64 %load29, 1
 store i64 %temp_name30, i64* %x11
 br label %for_continue24
exit_for_loop26:
 br label %while_continue31
while_continue31:
 %load34 = load i64, i64* %x11
 %temp_name35 = icmp sgt i64 %load34, 0
 br i1 %temp_name35, label %while_body32, label %exit_while_loop33
while_body32:
 br label %exit_while_loop33
post_break36:
 br label %while_continue31
exit_while_loop33:
 %temp_name37 = add i64 2, 2
 ret i64 %temp_name37
}
