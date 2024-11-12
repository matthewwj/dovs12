declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %y87 = alloca i1
 %x86 = alloca i1
 store i1 0, i1* %x86
 store i1 1, i1* %y87
 %load91 = load i1, i1* %x86
 br i1 %load91, label %logic_next89, label %logic_short_circ88
logic_short_circ88:
 br label %logic_end90
logic_next89:
 %load92 = load i1, i1* %y87
 br label %logic_end90
logic_end90:
 %logic_res93 = phi i1 [%load92, %logic_next89], [0, %logic_short_circ88]
 ret i64 0
}
