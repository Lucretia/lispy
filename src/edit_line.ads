package Edit_Line is
   function Read_Line (Prompt : in String) return String;

   procedure Add_History (Input : in String);
end Edit_Line;