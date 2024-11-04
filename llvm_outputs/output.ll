declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %condSym14 = alloca i1
 %condSym7 = alloca i1
 %y1 = alloca i64
 %temp_name0 = add i64 2, 2
 store i64 %temp_name0, i64* %y1
 %load5 = load i64, i64* %y1
 %temp_name6 = icmp sgt i64 %load5, 3
 store i1 %temp_name6, i1* %condSym7
 %condValue8 = load i1, i1* %condSym7
 br i1 %condValue8, label %then2, label %else3
then2:
 %load12 = load i64, i64* %y1
 %temp_name13 = icmp slt i64 %load12, 3
 store i1 %temp_name13, i1* %condSym14
 %condValue15 = load i1, i1* %condSym14
 br i1 %condValue15, label %then9, label %else10
then9:
 %load16 = load i64, i64* %y1
 store i64 %load16, i64* %y1
 %temp_name17 = add i64 %load16, 2
 br label %final11
else10:
 %load18 = load i64, i64* %y1
 store i64 %load18, i64* %y1
 %temp_name19 = add i64 %load18, 3
 br label %final11
final11:
 br label %final4
else3:
 store i64 5, i64* %y1
 br label %final4
final4:
 %load20 = load i64, i64* %y1
 ret i64 %load20
}
