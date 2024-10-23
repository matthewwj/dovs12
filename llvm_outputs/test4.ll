declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %x45 = alloca i64
 store i64 5, i64* %x45
 br label %while_continue46
while_continue46:
 %load49 = load i64, i64* %x45
 %temp_name50 = icmp sgt i64 %load49, 0
 br i1 %temp_name50, label %while_body47, label %exit_while_loop48
while_body47:
 br label %while_continue51
while_continue51:
 %load54 = load i64, i64* %x45
 %temp_name55 = icmp eq i64 %load54, 3
 br i1 %temp_name55, label %while_body52, label %exit_while_loop53
while_body52:
 br label %exit_while_loop53
post_break56:
 br label %while_continue51
exit_while_loop53:
 %load57 = load i64, i64* %x45
 %temp_name58 = sub i64 %load57, 1
 store i64 %temp_name58, i64* %x45
 br label %while_continue46
exit_while_loop48:
 %load59 = load i64, i64* %x45
 ret i64 %load59
}
