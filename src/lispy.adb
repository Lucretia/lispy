with Ada.Text_IO;
with Ada.Unchecked_Conversion;
--  with Ada.Characters.Latin_1;
with Interfaces.C;
with Edit_Line;
with AMPC;

procedure Lispy is
   package IO renames Ada.Text_IO;
   --  package L1 renames Ada.Characters.Latin_1;
   package C renames Interfaces.C;

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
      IO.New_Line;
      IO.Put_Line ("Exiting Lispy...");

      AMPC.Free (Number, Operator, Expression, Lispy);

      raise Finished;
   end Handler;

   function To_AST is new Ada.Unchecked_Conversion (Source => AMPC.Values_Ptr, Target => AMPC.AST_Ptr);
begin
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

            Success := AMPC.Parse (Input, Lispy, Result'Access);

            if Success then
               AMPC.Put (To_AST (Result.Output));
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
      null;
end Lispy;
