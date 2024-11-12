declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x35 = alloca i64
 store i64 5, i64* %x35
 br label %while_continue36
while_continue36:
 %load39 = load i64, i64* %x35
 %temp_name40 = icmp sgt i64 %load39, 0
 br i1 %temp_name40, label %while_body37, label %exit_while_loop38
while_body37:
 br label %while_continue41
while_continue41:
 %load44 = load i64, i64* %x35
 %temp_name45 = icmp eq i64 %load44, 3
 br i1 %temp_name45, label %while_body42, label %exit_while_loop43
while_body42:
 br label %exit_while_loop43
post_break46:
 br label %while_continue41
exit_while_loop43:
 %load47 = load i64, i64* %x35
 %temp_name48 = sub i64 %load47, 1
 store i64 %temp_name48, i64* %x35
 br label %while_continue36
exit_while_loop38:
 %load49 = load i64, i64* %x35
 ret i64 %load49
}
