with Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Lispy is
   package IO renames Ada.Text_IO;
   package L1 renames Ada.Characters.Latin_1;

   Version_Number    : constant String := "0.0.1";
   Max_Buffer_Length : constant        := 2048;
   Input_Length      : Positive        := Positive'First;
   Input             : String (1 .. Max_Buffer_Length);

   --  Get input from the user, 1 character at a time.
   procedure Get_Input (Buffer : in out String; Count : in out Positive) is
      Input_Char : Character;
   begin
      Count := Positive'First;

      loop
         IO.Get (Input_Char);

         Buffer (Count) := Input_Char;

         if IO.End_Of_Line then
            IO.Skip_Line;

            exit;
         end if;

         Count := Count + 1;
      end loop;
   end Get_Input;
begin
   IO.Put_Line ("Lispy version v" & Version_Number);
   IO.Put_Line ("Press Ctrl+C to exit");

   loop
      IO.Put ("Lispy> ");

      Get_Input (Input, Input_Length);

      --  Echo the input back at the user.
      IO.Put_Line (Input (1 .. Input_Length));
   end loop;
end Lispy;
