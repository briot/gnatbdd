package MySteps1 is

   --  @given ^an empty calculator$
   procedure Given_An_Empty_Calculator;

   --  @when ^I enter "(.*)"$
   procedure When_I_Enter (Value : String)
      with Pre => Value /= "";

   --  @then ^I should read (\d+)$
   procedure Then_I_Should_Read (Result : Integer);

end MySteps1;
