declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %condSym7 = alloca i1
 %x1 = alloca i64
 %temp_name0 = add i64 1, 1
 store i64 %temp_name0, i64* %x1
 %load5 = load i64, i64* %x1
 %temp_name6 = icmp sgt i64 %load5, 3
 store i1 %temp_name6, i1* %condSym7
 %condValue8 = load i1, i1* %condSym7
 br i1 %condValue8, label %then2, label %else3
then2:
 store i64 3, i64* %x1
 br label %final4
else3:
 store i64 5, i64* %x1
 br label %final4
final4:
 %load9 = load i64, i64* %x1
 ret i64 %load9
}
