declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @main () {
 %y25 = alloca i1
 %x24 = alloca i1
 store i1 0, i1* %x24
 store i1 1, i1* %y25
 %load29 = load i1, i1* %x24
 br i1 %load29, label %logic_next27, label %logic_short_circ26
logic_short_circ26:
 br label %logic_end28
logic_next27:
 %load30 = load i1, i1* %y25
 br label %logic_end28
logic_end28:
 %logic_res31 = phi i1 [%load30, %logic_next27], [0, %logic_short_circ26]
 ret i64 0
}
