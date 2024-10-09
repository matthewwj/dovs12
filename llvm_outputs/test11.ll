declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %condSym24 = alloca i1
 %b19 = alloca i64
 %a18 = alloca i64
 store i64 0, i64* %a18
 store i64 0, i64* %b19
 %temp_name23 = icmp slt i64 5, 10
 store i1 %temp_name23, i1* %condSym24
 %condValue25 = load i1, i1* %condSym24
 br i1 %condValue25, label %then20, label %else21
then20:
 store i64 5, i64* %a18
 br label %final22
else21:
 store i64 10, i64* %b19
 br label %final22
final22:
 %temp_name26 = add i64 2, 2
 ret i64 %temp_name26
}
