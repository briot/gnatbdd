package Calculator is

   type Integer_Array is array (Natural range <>) of Integer;

   procedure Reset;
   procedure Enter (Value : String);
   function Peek return Integer;
   function Peek_Stack return Integer_Array;

end Calculator;
