declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %i60 = alloca i64
 store i64 1, i64* %i60
 store i64 0, i64* %i60
 br label %for_continue61
for_continue61:
 %load64 = load i64, i64* %i60
 %temp_name65 = icmp slt i64 %load64, 10
 br i1 %temp_name65, label %for_body62, label %exit_for_loop63
for_body62:
 %load66 = load i64, i64* %i60
 call void @print_integer (i64 %load66)
 %load67 = load i64, i64* %i60
 %temp_name68 = add i64 %load67, 1
 store i64 %temp_name68, i64* %i60
 br label %for_continue61
exit_for_loop63:
 %load69 = load i64, i64* %i60
 call void @print_integer (i64 %load69)
 %load70 = load i64, i64* %i60
 ret i64 %load70
}
