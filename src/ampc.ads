with Interfaces.C.Strings;
--  with Interfaces.C.Pointers;
with System;
--  with System.Address_To_Access_Conversions;

package AMPC is
   package C renames Interfaces.C;

   --  States - mpc_state_t
   type States is
   record
      Position : C.long;
      Row      : C.long;
      Column   : C.long;
      Term     : C.int;
   end record with
     Convention => C;

   type States_Ptr is access all States with
     Convention => C;

   --  Errors - mpc_err_t
   type Errors is
   record
      State        : States;
      Expected_Num : C.int;
      Filename     : C.Strings.chars_ptr;
      Failure      : C.Strings.chars_ptr;
      Expected     : System.Address;
      Recieved     : C.char;
   end record with
     Convention => C;

   type Errors_Ptr is access all Errors with
     Convention => C;

   procedure Put (Error : in Errors_Ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "mpc_err_print";

   procedure Free (Error : in Errors_Ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "mpc_err_delete";

   --  Values - mpc_val_t
   type Values is null record with
     Convention => C;

   type Values_Ptr is access all Values;

   type Results (Success : Boolean) is
   record
      case Success is
         when False =>
            Error : Errors_Ptr;

         when True =>
            Output : Values_Ptr;
      end case;
   end record with
     Convention      => C_Pass_By_Copy,
     Unchecked_Union => True;

   type Results_Ptr is access all Results with
     Convention => C;

   --  Parsers - mpc_parser_t
   type Parsers is null record with
     Convention => C;

   type Parsers_Ptr is access all Parsers with
     Convention => C;

   function Parse (Input    : in String;
                   Parser   : in Parsers_Ptr;
                   Result   : in Results_Ptr;
                   Filename : in String := "<stdin>") return Boolean;

   function New_Parser (Name : in String) return Parsers_Ptr;

   procedure Free (Parser : in Parsers_Ptr);
   procedure Free (Parser_1 : in Parsers_Ptr;
                   Parser_2 : in Parsers_Ptr);
   procedure Free (Parser_1 : in Parsers_Ptr;
                   Parser_2 : in Parsers_Ptr;
                   Parser_3 : in Parsers_Ptr);
   procedure Free (Parser_1 : in Parsers_Ptr;
                   Parser_2 : in Parsers_Ptr;
                   Parser_3 : in Parsers_Ptr;
                   Parser_4 : in Parsers_Ptr);

   type Language_Flags is (Default, Predictive, Whitespace_Sensitive) with
     Convention => C;

   function Language (Flags : in Language_Flags; Grammar : in String; Parser : in Parsers_Ptr) return Errors_Ptr;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr) return Errors_Ptr;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr;
                      Parser_3 : in Parsers_Ptr) return Errors_Ptr;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr;
                      Parser_3 : in Parsers_Ptr;
                      Parser_4 : in Parsers_Ptr) return Errors_Ptr;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr;
                      Parser_3 : in Parsers_Ptr;
                      Parser_4 : in Parsers_Ptr;
                      Parser_5 : in Parsers_Ptr) return Errors_Ptr;

   --  AST - mpc_ast_t
   type ASTs;

   type AST_Ptr is access all ASTs with
     Convention => C;

   --  Flat array of pointers without bounds.
   type ASTs_Ptr_Array is array (C.size_t) of AST_Ptr with
     Convention => C;

   type ASTs_Ptr_Array_Ptr is access all ASTs_Ptr_Array with
     Convention => C;

   type ASTs is
   record
      Tag                : C.Strings.chars_ptr;
      Contents           : C.Strings.chars_ptr;
      State              : States;
      Number_Of_Children : C.int;
      Children           : ASTs_Ptr_Array_Ptr;
   end record with
     Convention => C;

   Null_AST : constant ASTs := (others => <>);

   function To_AST (Value : in Values_Ptr) return AST_Ptr with
     Inline => True;

   function Tag (Self : in ASTs) return String is (C.Strings.Value (Self.Tag));
   function Contents (Self : in ASTs) return String is (C.Strings.Value (Self.Contents));

   --  NOTE: Always substract 1 from the result in loops.
   --    e.g. C.size_t'First .. Number_Of_Children - 1
   function Number_Of_Children (Self : in ASTs) return C.size_t is (C.size_t (Self.Number_Of_Children));

   procedure Put (AST : in AST_Ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "mpc_ast_print";

   procedure Free (AST : in AST_Ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "mpc_ast_delete";

   function Total_Nodes (AST : in AST_Ptr) return Natural;
end AMPC;
