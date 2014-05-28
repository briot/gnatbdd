package MySteps1 is

   --  @when ^I enter "(.*)"$
   procedure When_I_Enter (Value : String)
      with Pre => Value /= "";

   --  @then ^I should read (\d+)$
   procedure Then_I_Should_Read (Result : Integer);

end MySteps1;
