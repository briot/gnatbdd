with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Calculator is

   Stack : array (1 .. 5) of Integer;
   Stack_Top : Integer := Stack'First;

   procedure Enter (Value : String) is
   begin
      if Is_Digit (Value (Value'First)) then
         if Stack_Top <= Stack'Last then
            Stack (Stack_Top) := Integer'Value (Value);
            Stack_Top := Stack_Top + 1;
         else
            raise Constraint_Error with "Calculator stack overflow";
         end if;

      else
         if Value (Value'First) = '+' then
            if Stack_Top < Stack'First + 2 then
               raise Constraint_Error with "Calculator stack underflow";
            else
               Stack (Stack_Top - 2) :=
                  Stack (Stack_Top - 2) + Stack (Stack_Top - 1);
               Stack_Top := Stack_Top - 1;
            end if;
         else
            raise Program_Error with "Unknown operation: " & Value;
         end if;
      end if;
   end Enter;

   function Peek return Integer is
   begin
      if Stack_Top = Stack'First then
         raise Constraint_Error with "Calculator stack is empty";
      end if;
      return Stack (Stack_Top - 1);
   end Peek;

end Calculator;
