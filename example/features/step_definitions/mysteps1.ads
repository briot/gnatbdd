with BDD.Tables;

package MySteps1 is

   --  @when ^I enter "(.*)"$
   procedure When_I_Enter (Value : String)
      with Pre => Value /= "";

   --  @then ^I should read %integer$
   procedure Then_I_Should_Read (Result : Integer);

   --  @and ^the stack should contain$
   procedure Then_The_Stack_Should_Contain (Expected : BDD.Tables.Table);

end MySteps1;
