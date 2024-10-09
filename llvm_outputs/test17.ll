declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %y33 = alloca i1
 %x32 = alloca i1
 store i1 1, i1* %x32
 store i1 1, i1* %y33
 %load37 = load i1, i1* %x32
 br i1 %load37, label %logic_next35, label %logic_short_circ34
logic_short_circ34:
 br label %logic_end36
logic_next35:
 %load38 = load i1, i1* %y33
 br label %logic_end36
logic_end36:
 %logic_res39 = phi i1 [%load38, %logic_next35], [0, %logic_short_circ34]
 ret i64 0
}
