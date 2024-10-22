declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %x1 = alloca i64
 %temp_name0 = add i64 2, 2
 store i64 %temp_name0, i64* %x1
 br label %while continue2
while continue2:
 %load5 = load i64, i64* %x1
 %temp_name6 = icmp sgt i64 %load5, 0
 br i1 %temp_name6, label %while body3, label %exit while loop4
while body3:
 %load7 = load i64, i64* %x1
 %temp_name8 = sub i64 %load7, 1
 store i64 %temp_name8, i64* %x1
 br label %while continue2
exit while loop4:
 br label %while continue9
while continue9:
 %load12 = load i64, i64* %x1
 %temp_name13 = icmp sgt i64 %load12, 0
 br i1 %temp_name13, label %while body10, label %exit while loop11
while body10:
 br label %exit while loop11
post break14:
 br label %while continue9
exit while loop11:
 %temp_name15 = add i64 2, 2
 ret i64 %temp_name15
}
