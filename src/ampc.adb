package body AMPC is
   use type C.int;

   function Parse (Input    : in String;
                   Parser   : in Parsers_Ptr;
                   Result   : in Results_Ptr;
                   Filename : in String := "<stdin>") return Boolean is
      function Parse (Filename : in C.Strings.chars_ptr;
                      Input    : in C.Strings.chars_ptr;
                      Parser   : in Parsers_Ptr;
                      Result   : in Results_Ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "mpc_parse";

      C_Filename : C.Strings.chars_ptr := C.Strings.New_String (Filename);
      C_Input    : C.Strings.chars_ptr := C.Strings.New_String (Input);
      Success    : Boolean             := (Parse (C_Filename, C_Input, Parser, Result) = 1);
   begin
      C.Strings.Free (C_Filename);
      C.Strings.Free (C_Input);

      return Success;
   end Parse;

   function New_Parser (Name : in String) return Parsers_Ptr is
      function New_Parser (Name : in C.Strings.chars_ptr) return Parsers_Ptr with
        Import        => True,
        Convention    => C,
        External_Name => "mpc_new";

      C_Name : C.Strings.chars_ptr := C.Strings.New_String (Name);
      Result : Parsers_Ptr         := New_Parser (C_Name);
   begin
      C.Strings.Free (C_Name);

      return Result;
   end New_Parser;


   procedure Free (Parser : in Parsers_Ptr) is
      procedure Cleanup (Total : in Positive; Parser : in Parsers_Ptr) with
        Import        => True,
        Convention    => C,
        External_Name => "mpc_cleanup";
   begin
      Cleanup (1, Parser);
   end Free;

   procedure Free (Parser_1 : in Parsers_Ptr;
                   Parser_2 : in Parsers_Ptr) is
      procedure Cleanup (Total    : in Positive;
                         Parser_1 : in Parsers_Ptr;
                         Parser_2 : in Parsers_Ptr) with
        Import        => True,
        Convention    => C,
        External_Name => "mpc_cleanup";
   begin
      Cleanup (2, Parser_1, Parser_2);
   end Free;

   procedure Free (Parser_1 : in Parsers_Ptr;
                   Parser_2 : in Parsers_Ptr;
                   Parser_3 : in Parsers_Ptr) is
      procedure Cleanup (Total    : in Positive;
                         Parser_1 : in Parsers_Ptr;
                         Parser_2 : in Parsers_Ptr;
                         Parser_3 : in Parsers_Ptr) with
        Import        => True,
        Convention    => C,
        External_Name => "mpc_cleanup";
   begin
      Cleanup (3, Parser_1, Parser_2, Parser_3);
   end Free;

   procedure Free (Parser_1 : in Parsers_Ptr;
                   Parser_2 : in Parsers_Ptr;
                   Parser_3 : in Parsers_Ptr;
                   Parser_4 : in Parsers_Ptr) is
      procedure Cleanup (Total    : in Positive;
                         Parser_1 : in Parsers_Ptr;
                         Parser_2 : in Parsers_Ptr;
                         Parser_3 : in Parsers_Ptr;
                         Parser_4 : in Parsers_Ptr) with
        Import        => True,
        Convention    => C,
        External_Name => "mpc_cleanup";
   begin
      Cleanup (4, Parser_1, Parser_2, Parser_3, Parser_4);
   end Free;

   function Language (Flags   : in Language_Flags;
                      Grammar : in String;
                      Parser  : in Parsers_Ptr) return Errors_Ptr is
      function Language (Flags   : in Language_Flags;
                         Grammar : in C.Strings.chars_ptr;
                         Parser  : in Parsers_Ptr) return Errors_Ptr with
      Import        => True,
      Convention    => C,
      External_Name => "helper_language2";

      C_Grammar : C.Strings.chars_ptr := C.Strings.New_String (Grammar);
      Result    : Errors_Ptr          := Language (Flags, C_Grammar, Parser);
   begin
      C.Strings.Free (C_Grammar);

      return Result;
   end Language;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr) return Errors_Ptr is
      function Language (Flags    : in Language_Flags;
                         Grammar  : in C.Strings.chars_ptr;
                         Parser_1 : in Parsers_Ptr;
                         Parser_2 : in Parsers_Ptr) return Errors_Ptr with
      Import        => True,
      Convention    => C,
      External_Name => "helper_language2";

      C_Grammar : C.Strings.chars_ptr := C.Strings.New_String (Grammar);
      Result    : Errors_Ptr          := Language
        (Flags, C_Grammar, Parser_1, Parser_2);
   begin
      C.Strings.Free (C_Grammar);

      return Result;
   end Language;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr;
                      Parser_3 : in Parsers_Ptr) return Errors_Ptr is
      function Language (Flags    : in Language_Flags;
                         Grammar  : in C.Strings.chars_ptr;
                         Parser_1 : in Parsers_Ptr;
                         Parser_2 : in Parsers_Ptr;
                         Parser_3 : in Parsers_Ptr) return Errors_Ptr with
      Import        => True,
      Convention    => C,
      External_Name => "helper_language3";

      C_Grammar : C.Strings.chars_ptr := C.Strings.New_String (Grammar);
      Result    : Errors_Ptr          := Language
        (Flags, C_Grammar, Parser_1, Parser_2, Parser_3);
   begin
      C.Strings.Free (C_Grammar);

      return Result;
   end Language;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr;
                      Parser_3 : in Parsers_Ptr;
                      Parser_4 : in Parsers_Ptr) return Errors_Ptr is
      function Language (Flags    : in Language_Flags;
                         Grammar  : in C.Strings.chars_ptr;
                         Parser_1 : in Parsers_Ptr;
                         Parser_2 : in Parsers_Ptr;
                         Parser_3 : in Parsers_Ptr;
                         Parser_4 : in Parsers_Ptr) return Errors_Ptr with
      Import        => True,
      Convention    => C,
      External_Name => "helper_language4";

      C_Grammar : C.Strings.chars_ptr := C.Strings.New_String (Grammar);
      Result    : Errors_Ptr          := Language
        (Flags, C_Grammar, Parser_1, Parser_2, Parser_3, Parser_4);
   begin
      C.Strings.Free (C_Grammar);

      return Result;
   end Language;

   function Language (Flags    : in Language_Flags;
                      Grammar  : in String;
                      Parser_1 : in Parsers_Ptr;
                      Parser_2 : in Parsers_Ptr;
                      Parser_3 : in Parsers_Ptr;
                      Parser_4 : in Parsers_Ptr;
                      Parser_5 : in Parsers_Ptr) return Errors_Ptr is
      function Language (Flags    : in Language_Flags;
                         Grammar  : in C.Strings.chars_ptr;
                         Parser_1 : in Parsers_Ptr;
                         Parser_2 : in Parsers_Ptr;
                         Parser_3 : in Parsers_Ptr;
                         Parser_4 : in Parsers_Ptr;
                         Parser_5 : in Parsers_Ptr) return Errors_Ptr with
      Import        => True,
      Convention    => C,
      External_Name => "helper_language5";

      C_Grammar : C.Strings.chars_ptr := C.Strings.New_String (Grammar);
      Result    : Errors_Ptr          := Language
        (Flags, C_Grammar, Parser_1, Parser_2, Parser_3, Parser_4, Parser_5);
   begin
      C.Strings.Free (C_Grammar);

      return Result;
   end Language;
end AMPC;

