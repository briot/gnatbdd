with BDD.Asserts;  use BDD.Asserts;
with Calculator;   use Calculator;

package body MySteps1 is

   procedure When_I_Enter (Value : String) is
   begin
      Enter (Value);
   end When_I_Enter;

   procedure Then_I_Should_Read (Result : Integer) is
   begin
      Assert (Result, Peek, "checking top of the stack");
   end Then_I_Should_Read;

end MySteps1;
