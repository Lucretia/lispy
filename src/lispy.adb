with Ada.Text_IO;
with Ada.Unchecked_Conversion;
--  with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
--  with System;
--  with System.Address_To_Access_Conversions;
with Edit_Line;
with AMPC;

procedure Lispy is
   package IO renames Ada.Text_IO;
   --  package L1 renames Ada.Characters.Latin_1;
   package C renames Interfaces.C;

   use type AMPC.Errors_Ptr;
   use type C.Strings.chars_ptr;

   Version_Number : constant String := "0.0.1";

   Number     : AMPC.Parsers_Ptr := AMPC.New_Parser ("number");
   Operator   : AMPC.Parsers_Ptr := AMPC.New_Parser ("operator");
   Expression : AMPC.Parsers_Ptr := AMPC.New_Parser ("expr");
   Lispy      : AMPC.Parsers_Ptr := AMPC.New_Parser ("lispy");
   Error      : AMPC.Errors_Ptr  := AMPC.Language (
     AMPC.Default,
     "number   : /-?[0-9]+/ ;" &
     "operator : '+' | '-' | '*' | '/' ;" &
     "expr     : <number> | '(' <operator> <expr>+ ')' ;" &
     "lispy    : /^/ <operator> <expr>+ /$/ ;",
     Number, Operator, Expression, Lispy);

   --  Pressing Ctrl+C will quit the application there and then, so no cleanup occurs, we have to catch the signal,
   --  from there, do the cleanup and then raise and exception which will then gracefully exit.
   --  TODO: There's got to be a better way to do this.
   type Signal_Handler is access procedure (Dummy : C.int) with
     Convention => C;

   type Signals is (Sig_Hup, Sig_Int);
   for Signals use (Sig_Hup => 1, Sig_Int => 2);

   procedure Signal (Signal : in Signals; Callback : in Signal_Handler) with
     Import        => True,
     Convention    => C,
     External_Name => "signal";

   Finished : exception;

   procedure Handler (Dummy : in C.int) with
     Convention => C;

   procedure Handler (Dummy : in C.int) is
   begin
      raise Finished;
   end Handler;

   function To_AST is new Ada.Unchecked_Conversion (Source => AMPC.Values_Ptr, Target => AMPC.AST_Ptr);

   function Check_Error (Error : in AMPC.Errors_Ptr) return Boolean is
   begin
      if Error /= null then
         IO.Put_Line ("Failed to create parser.");

         IO.Put_Line ("  >> Filename: " & C.Strings.Value (Error.Filename));
         IO.Put_Line ("  >> Failure : " &
           (if Error.Failure = C.Strings.Null_Ptr then "" else C.Strings.Value (Error.Failure)));
         IO.Put_Line ("  >> Position: " & C.long'Image (Error.State.Position));
         IO.Put_Line ("  >> At      : " & C.long'Image (Error.State.Row) & ", " & C.long'Image (Error.State.Column));
         IO.Put_Line ("  >> Term    : " & C.int'Image (Error.State.Term));

         return True; -- Exit Lispy.
      end if;

      return False;
   end Check_Error;
begin
   if Check_Error (Error) then
      return;
   end if;

   Signal (Sig_Int, Handler'Access);

   IO.Put_Line ("Lispy version v" & Version_Number);
   IO.Put_Line ("Press Ctrl+C to exit");

   REPL :
      loop
         declare
            Result  : aliased AMPC.Results := (Success => False, others => <>);
            Input   : String  := Edit_Line.Read_Line ("Lispy> ");
            Success : Boolean := False;
         begin
            Edit_Line.Add_History (Input);

            --  Unrestricted_Access is a hack due to GNAT not accepting Access onm Unchecked_Unions, GNAT seems to think
            --  there is no "aliased" keyword when there is.
            Success := AMPC.Parse (Input, Lispy, Result'Unrestricted_Access);

            if Success then
               AMPC.Put (To_AST (Result.Output));

               declare
                  AST : AMPC.AST_Ptr := AMPC.To_AST (Result.Output);

                  use type C.int;

               begin
                  IO.Put_Line ("Children :=> " & C.int'Image (AST.Number_Of_Children));

                  for I in C.size_t'First .. C.size_t (AST.Number_Of_Children - 1) loop
                     if AST.Children (I).Contents /= C.Strings.Null_Ptr then
                        IO.Put_Line ("  > " & C.Strings.Value (AST.Children (I).Contents));
                     end if;
                  end loop;
               end;

               AMPC.Free (To_AST (Result.Output));
            else
               AMPC.Put (Result.Error);
               AMPC.Free (Result.Error);
            end if;

            --  Echo the input back at the user.
            IO.Put_Line (Input);
         end;
      end loop REPL;
exception
   when Finished =>
      IO.New_Line;
      IO.Put_Line ("Exiting Lispy...");

      AMPC.Free (Number, Operator, Expression, Lispy);
end Lispy;
