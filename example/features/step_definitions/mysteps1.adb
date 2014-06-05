with BDD.Asserts;  use BDD.Asserts;
with BDD.Tables;   use BDD.Tables;
with Calculator;   use Calculator;

package body MySteps1 is

   procedure When_I_Enter (Value : String) is
   begin
      Enter (Value);
   end When_I_Enter;

   procedure Then_I_Should_Read (Result : Integer) is
   begin
      Assert (Result, Peek);
   end Then_I_Should_Read;

   procedure Then_The_Stack_Should_Contain (Expected : BDD.Tables.Table) is
      Actual : Table := Create;
      Stack  : constant Integer_Array := Peek_Stack;
   begin
      for S in Stack'Range loop
         Actual.Put (Column => 1, Row => S, Value => Stack (S)'Img);
      end loop;

      Assert (Expected => Expected, Actual => Actual);
   end Then_The_Stack_Should_Contain;

end MySteps1;
