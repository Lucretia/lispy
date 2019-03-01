with AMPC;

package Eval is
   type Lisp_Value_Types is (Number, Error);
   type Lisp_Errors is (None, Divide_By_Zero, Bad_Operator, Bad_Number);

   --  Cannot make this a variant type as X in the Evaluate function cannot change it's variant in the assignemtn.
   type Lisp_Values is
   record
      Which  : Lisp_Value_Types;
      Number : Long_Integer;
      Error  : Lisp_Errors;
   end record;

   function Make_Lisp_Value (Number_Value : in Long_Integer) return Lisp_Values is
     (Lisp_Values'(Which => Number, Number => Number_Value, Error => None));

   function Make_Lisp_Value (Error_Value : in Lisp_Errors) return Lisp_Values is
     (Lisp_Values'(Which => Error, Number => -1, Error => Error_Value));

   function "+" (L, R : in Lisp_Values) return Lisp_Values;
   function "-" (L, R : in Lisp_Values) return Lisp_Values;
   function "*" (L, R : in Lisp_Values) return Lisp_Values;
   function "/" (L, R : in Lisp_Values) return Lisp_Values;
   function "rem" (L, R : in Lisp_Values) return Lisp_Values;
   function "**" (L, R : in Lisp_Values) return Lisp_Values;

   procedure Put (Item : in Lisp_Values);
   procedure Put_Line (Item : in Lisp_Values);

   function Evaluate (Tree : in AMPC.AST_Ptr) return Lisp_Values;

   function Evaluate_Operator (X, Y : in Lisp_Values; Operator : in Character) return Lisp_Values;
end Eval;