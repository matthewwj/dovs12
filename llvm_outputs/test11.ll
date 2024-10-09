declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %condSym21 = alloca i1
 %b16 = alloca i64
 %a15 = alloca i64
 store i64 0, i64* %a15
 store i64 0, i64* %b16
 %temp_name20 = icmp slt i64 5, 10
 store i1 %temp_name20, i1* %condSym21
 %condValue22 = load i1, i1* %condSym21
 br i1 %condValue22, label %then17, label %else18
then17:
 store i64 5, i64* %a15
 br label %final19
else18:
 store i64 10, i64* %b16
 br label %final19
final19:
 %temp_name23 = add i64 2, 2
 ret i64 %temp_name23
}
