with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces.C;

package body Eval is
   package IO renames Ada.Text_IO;
   package Fixed renames Ada.Strings.Fixed;
   package C renames Interfaces.C;

   use type C.size_t;
   use type C.int;

   function "+" (L, R : in Lisp_Values) return Lisp_Values is
   begin
      return Make_Lisp_Value (Number_Value => L.Number + R.Number);
   end "+";

   function "-" (L, R : in Lisp_Values) return Lisp_Values is
   begin
      return Make_Lisp_Value (Number_Value => L.Number - R.Number);
   end "-";

   function "*" (L, R : in Lisp_Values) return Lisp_Values is
   begin
      return Make_Lisp_Value (Number_Value => L.Number * R.Number);
   end "*";

   function "/" (L, R : in Lisp_Values) return Lisp_Values is
   begin
      if R.Number = 0 then
         return Make_Lisp_Value (Error_Value => Divide_By_Zero);
      else
         return Make_Lisp_Value (Number_Value => L.Number / R.Number);
      end if;
   end "/";

   function "rem" (L, R : in Lisp_Values) return Lisp_Values is
   begin
      return Make_Lisp_Value (Number_Value => L.Number rem R.Number);
   end "rem";

   function "**" (L, R : in Lisp_Values) return Lisp_Values is
   begin
      return Make_Lisp_Value (Number_Value => L.Number ** Natural (R.Number));
   end "**";

   procedure Put (Item : in Lisp_Values) is
   begin
      case Item.Which is
         when Number =>
            IO.Put (Fixed.Trim (Long_Integer'Image (Item.Number), Ada.Strings.Left));

         when Error =>
            case Item.Error is
               when None =>
                  null;

               when Divide_By_Zero =>
                  IO.Put ("Error: Division by zero!");

               when Bad_Operator =>
                  IO.Put ("Error: Invalid operator!");

               when Bad_Number =>
                  IO.Put ("Error: Invalid number!");
            end case;
      end case;
   end Put;

   procedure Put_Line (Item : in Lisp_Values) is
   begin
      Put (Item);

      IO.New_Line;
   end Put_Line;

   function Evaluate (Tree : in AMPC.AST_Ptr) return Lisp_Values is
   begin
      Find_A_Number : begin
         if Fixed.Index (Source => AMPC.Tag (Tree.all), Pattern => "number") >= 1 then
            return Make_Lisp_Value (Number_Value => Long_Integer'Value (AMPC.Contents (Tree.all)));
         end if;
      exception
         --  If we get a number that's too big to handle.
         when Constraint_Error =>
            return Make_Lisp_Value (Error_Value => Bad_Number);
      end Find_A_Number;

      declare
         Op_String : constant String      := AMPC.Contents (Tree.Children (1).all);
         Op        : constant Character   := Op_String (Op_String'First);
         X         :          Lisp_Values := Evaluate (Tree.Children (2));
         Index     :          C.size_t    := 3;
      begin
         if Tree.Number_Of_Children = 4 and Op = '-' then
            X.Number := -X.Number;
         end if;

         while Fixed.Index (Source => AMPC.Tag (Tree.Children (Index).all), Pattern => "expr") >= 1 loop
            X := Evaluate_Operator (X        => X,
                                    Operator => Op,
                                    Y        => Evaluate (Tree.Children (Index)));

            Index := Index + 1;
         end loop;

         return X;
      end;
   end Evaluate;

   function Evaluate_Operator (X, Y : in Lisp_Values; Operator : in Character) return Lisp_Values is
   begin
      if X.Which = Error then
         return X;
      end if;

      if Y.Which = Error then
         return Y;
      end if;

      case Operator is
         when '+' =>
            return X + Y;

         when '-' =>
            return X - Y;

         when '*' =>
            return X * Y;

         when '/' =>
            return X / Y;

         when '%' =>
            return X rem Y;

         when '^' =>
            return X ** Y;

         when others =>
            return Make_Lisp_Value (Number_Value => 0);
      end case;
   end Evaluate_Operator;
end Eval;