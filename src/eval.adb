with Ada.Strings.Fixed;
with Interfaces.C;

package body Eval is
   package Fixed renames Ada.Strings.Fixed;
   package C renames Interfaces.C;

   use type C.size_t;

   function Evaluate (Tree : in AMPC.AST_Ptr) return Long_Integer is
   begin
      if Fixed.Index (Source => AMPC.Tag (Tree.all), Pattern => "number") >= 1 then
         return Long_Integer'Value (AMPC.Contents (Tree.all));
      end if;

      declare
         Op_String : constant String       := AMPC.Contents (Tree.Children (1).all);
         Op        : constant Character    := Op_String (Op_String'First);
         X         :          Long_Integer := Evaluate (Tree.Children (2));
         Index     :          C.size_t     := 3;
      begin
         while Fixed.Index (Source => AMPC.Tag (Tree.Children (Index).all), Pattern => "expr") >= 1 loop
            X := Evaluate_Operator (X        => X,
                                    Operator => Op,
                                    Y        => Evaluate (Tree.Children (Index)));

            Index := Index + 1;
         end loop;

         return X;
      end;
      -- return 1;
   end Evaluate;

   function Evaluate_Operator (X, Y : in Long_Integer; Operator : in Character) return Long_Integer is
   begin
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

         when others =>
            return 0;
      end case;
   end Evaluate_Operator;
end Eval;