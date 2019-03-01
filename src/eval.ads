with AMPC;

package Eval is
   type Lisp_Value_Types is (Number, Error);
   type Lisp_Errors is (Divide_By_Zero, Bad_Operator, Bad_Number);

   type Lisp_Values (Which : Lisp_Value_Types) is
   record
      case Which is
         when Number =>
            Number : Long_Integer;

         when Error =>
            Error : Lisp_Errors;
      end case;
   end record;

   function Make_Lisp_Value (Number_Value : in Long_Integer) return Lisp_Values is
     (Lisp_Values'(Which => Number, Number => Number_Value));

   function Make_Lisp_Value (Error_Value : in Lisp_Errors) return Lisp_Values is
     (Lisp_Values'(Which => Error, Error => Error_Value));

   procedure Put (Item : in Lisp_Values);
   procedure Put_Line (Item : in Lisp_Values);

   function Evaluate (Tree : in AMPC.AST_Ptr) return Long_Integer;

   function Evaluate_Operator (X, Y : in Long_Integer; Operator : in Character) return Long_Integer;
end Eval;