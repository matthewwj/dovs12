module TAst = TypedAst
module Sym = Symbol

open Ll
  
let make_ident name = TAst.Ident {sym = Sym.symbol name}
  


let string_type = 
  Struct [I64; Ptr I8]
  
let dolphin_record_stream = Struct[]
  
let dolphin_rc_empty_string = string_type

let make_ident name = TAst.Ident {sym = Sym.symbol name}
let array_type : Ll.ty = Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)]
let array_type_name = Sym.symbol "array_type"
let array_type : Ll.ty = Ll.Namedt array_type_name

let library_functions =
  [
    (Sym.symbol "read_integer", TAst.FunTyp { ret = Int; params = [] });
    (Sym.symbol "print_integer", TAst.FunTyp {
      ret = Void;
      params = [TAst.Param { paramname = make_ident "value"; typ = Int }]
    });
    (Sym.symbol "bytes_array_to_string", FunTyp {
      ret = TypedAst.Str;
      params = [Param { paramname = make_ident "bytes_array"; typ =  TAst.Ptr TAst.Int8 }]
    });
    (Sym.symbol "string_to_bytes_array", TAst.FunTyp {
      ret =  (TAst.Ptr TAst.Int8);
      params = [TAst.Param { paramname = make_ident "string"; typ =  TypedAst.Str }]
    });

    (Sym.symbol "byte_to_int_unsigned", TAst.FunTyp {
      ret = Int;
      params = [TAst.Param { paramname = make_ident "byte"; typ = Int8 }]
    });
    (Sym.symbol "byte_to_int_signed", TAst.FunTyp {
      ret = Int;
      params = [TAst.Param { paramname = make_ident "byte"; typ = Int8 }]
    });
    (Sym.symbol "int_to_byte_unsigned", TAst.FunTyp {
      ret = Int8;
      params = [TAst.Param { paramname = make_ident "int_val"; typ = Int }]
    });
    (Sym.symbol "int_to_byte_signed", TAst.FunTyp {
      ret = Int8;
      params = [TAst.Param { paramname = make_ident "int_val"; typ = Int }]
    });
    (Sym.symbol "ascii_ord", TAst.FunTyp {
      ret = Int;
      params = [TAst.Param { paramname = make_ident "string"; typ = TypedAst.Str }]
    });
    (Sym.symbol "ascii_chr", TAst.FunTyp {
      ret = TypedAst.Str;
      params = [TAst.Param { paramname = make_ident "code"; typ = Int }]
    });

    (Sym.symbol "string_concat", TAst.FunTyp {
      ret = Ptr TypedAst.Str;
      params = [
        TAst.Param { paramname = make_ident "str1"; typ = TypedAst.Str };
        TAst.Param { paramname = make_ident "str2"; typ = TypedAst.Str }
      ]
    });
    (Sym.symbol "substring", TAst.FunTyp {
      ret = TypedAst.Str;
      params = [
        TAst.Param { paramname = make_ident "string"; typ = TypedAst.Str };
        TAst.Param { paramname = make_ident "start"; typ = Int };
        TAst.Param { paramname = make_ident "length"; typ = Int }
      ]
    });
    (Sym.symbol "int_to_string", TAst.FunTyp {
      ret = TypedAst.Str;
      params = [TAst.Param { paramname = make_ident "int_val"; typ = Int }]
    });
    (Sym.symbol "string_to_int", TAst.FunTyp {
      ret = Int;
      params = [TAst.Param { paramname = make_ident "string"; typ = TypedAst.Str }]
    });
    (Sym.symbol "input_byte", TAst.FunTyp {
      ret = Int;
      params = [TAst.Param { paramname = make_ident "stream"; typ = (Struct "dolphin_record_stream") }]
    });
    (Sym.symbol "output_byte", TAst.FunTyp {
      ret = Bool;
      params = [
        TAst.Param { paramname = make_ident "byte"; typ = Int8 };
        TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }
      ]
    });
    (Sym.symbol "input_bytes_array", TAst.FunTyp {
      ret = TAst.Ptr TAst.Int8;
      params = [
        TAst.Param { paramname = make_ident "length"; typ = Int };
        TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }
      ]
    });
    (Sym.symbol "output_bytes_array", TAst.FunTyp {
      ret = Void;
      params = [
        TAst.Param { paramname = make_ident "bytes_array"; typ = TAst.Ptr TAst.Int8 };
        TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }
      ]
    });
    
    (Sym.symbol "output_string", TAst.FunTyp {
      ret = Void;
      params = [
        TAst.Param { paramname = make_ident "string"; typ = TAst.Ptr TAst.Int8 };
        TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }
      ]
    });
    (Sym.symbol "seek_in_file", TAst.FunTyp {
      ret = Bool;
      params = [
        TAst.Param { paramname = make_ident "position"; typ = Int };
        TAst.Param { paramname = make_ident "relative"; typ = Bool };
        TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }
      ]
    });
    (Sym.symbol "pos_in_file", TAst.FunTyp {
      ret = Int;
      params = [TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }]
    });
    (Sym.symbol "close_file", TAst.FunTyp {
      ret = Bool;
      params = [TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }]
    });
    (Sym.symbol "flush_file", TAst.FunTyp {
      ret = Bool;
      params = [TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }]
    });
    (Sym.symbol "error_in_file", TAst.FunTyp {
      ret = Bool;
      params = [TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }]
    });
    (Sym.symbol "end_of_file", TAst.FunTyp {
      ret = Bool;
      params = [TAst.Param { paramname = make_ident "stream"; typ = Ptr (Struct "dolphin_record_stream") }]
    });
    (Sym.symbol "get_eof", TAst.FunTyp {
      ret = Int;
      params = []
    });
    (Sym.symbol "open_file", TAst.FunTyp {
      ret = Ptr (Struct "dolphin_record_stream");
      params = [
        TAst.Param { paramname = make_ident "filename"; typ = TypedAst.Str };
        TAst.Param { paramname = make_ident "mode"; typ = TypedAst.Str }
      ]
    });
    (Sym.symbol "get_stdin", TAst.FunTyp {
      ret = Ptr (Struct "dolphin_record_stream");
      params = []
    });
    (Sym.symbol "get_stderr", TAst.FunTyp {
      ret = Ptr (Struct "dolphin_record_stream");
      params = []
    });
    (Sym.symbol "get_stdout", TAst.FunTyp {
      ret = Ptr (Struct "dolphin_record_stream");
      params = []
    });
    (Sym.symbol "get_cmd_args", TAst.FunTyp {
      ret = TAst.Ptr TypedAst.Str;
      params = []
    });
    (Sym.symbol "exit", TAst.FunTyp {
      ret = Void;
      params = [TAst.Param { paramname = make_ident "status"; typ = Int }]
    });
  ]



let user_inaccessible_functions = [
    (Sym.symbol "compare_strings", I64, [Ptr string_type; Ptr string_type]);
    (Sym.symbol "allocate_record", Ptr I8, [I32]);
    (Sym.symbol "raw_allocate_on_heap", Ptr I8, [I32]);
    (Sym.symbol "allocate_array", Ptr I8, [I32; I64; Ptr I8]);
    (Sym.symbol "report_error_array_index_out_of_bounds", Void, []);
    (Sym.symbol "report_error_nil_access", Void, []);
    (Sym.symbol "report_error_division_by_zero", Void, []);
    (Sym.symbol "string_length", I64, [Ptr string_type]);
  ]
