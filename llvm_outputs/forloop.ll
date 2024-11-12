declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %i50 = alloca i64
 store i64 1, i64* %i50
 store i64 0, i64* %i50
 br label %for_continue51
for_continue51:
 %load54 = load i64, i64* %i50
 %temp_name55 = icmp slt i64 %load54, 10
 br i1 %temp_name55, label %for_body52, label %exit_for_loop53
for_body52:
 %load56 = load i64, i64* %i50
 call void @print_integer (i64 %load56)
 %load57 = load i64, i64* %i50
 %temp_name58 = add i64 %load57, 1
 store i64 %temp_name58, i64* %i50
 br label %for_continue51
exit_for_loop53:
 %load59 = load i64, i64* %i50
 call void @print_integer (i64 %load59)
 %load60 = load i64, i64* %i50
 ret i64 %load60
}
