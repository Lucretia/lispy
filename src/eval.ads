with AMPC;

package Eval is
   function Evaluate (Tree : in AMPC.AST_Ptr) return Long_Integer;

   function Evaluate_Operator (X, Y : in Long_Integer; Operator : in Character) return Long_Integer;
end Eval;