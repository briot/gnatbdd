package Calculator is

   type Integer_Array is array (Natural range <>) of Integer;

   --  @given ^an empty calculator$
   procedure Reset;
   --  An actual subprogram, that can be used directly from a step.

   procedure Enter (Value : String);
   function Peek return Integer;
   function Peek_Stack return Integer_Array;

end Calculator;
