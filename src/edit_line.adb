with Interfaces.C.Strings;

package body Edit_Line is
   package C renames Interfaces.C;

   use type C.Strings.chars_ptr;

   function Read_Line (Prompt : in String) return String is
      function Read_Line (Prompt : in C.Strings.chars_ptr) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "readline";

      C_Prompt : C.Strings.chars_ptr := C.Strings.New_String (Prompt);
      C_Result : C.Strings.chars_ptr := Read_Line (C_Prompt);
   begin
      C.Strings.Free (C_Prompt);

      if C_Result /= C.Strings.Null_Ptr then
         return S : String := C.Strings.Value (C_Result) do
            C.Strings.Free (C_Result);
         end return;
      end if;

      return "";
   end Read_Line;


   procedure Add_History (Input : in String) is
      procedure Add_History (Input : in C.Strings.chars_ptr) with
        Import        => True,
        Convention    => C,
        External_Name => "add_history";

      C_Input : C.Strings.chars_ptr := C.Strings.New_String (Input);
   begin
      Add_History (C_Input);

      C.Strings.Free (C_Input);
   end Add_History;
end Edit_Line;
