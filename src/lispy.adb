with Ada.Text_IO;
--  with Ada.Characters.Latin_1;
with Edit_Line;

procedure Lispy is
   package IO renames Ada.Text_IO;
   --  package L1 renames Ada.Characters.Latin_1;

   Version_Number : constant String := "0.0.1";
begin
   IO.Put_Line ("Lispy version v" & Version_Number);
   IO.Put_Line ("Press Ctrl+C to exit");

   loop
      REPL :
         declare
            Input : String := Edit_Line.Read_Line ("Lispy> ");
         begin
            Edit_Line.Add_History (Input);

            --  Echo the input back at the user.
            IO.Put_Line (Input);
         end REPL;
   end loop;
end Lispy;
