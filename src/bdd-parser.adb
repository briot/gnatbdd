-----------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Traces;     use GNATCOLL.Traces;
with GNATCOLL.Utils;      use GNATCOLL.Utils;

package body BDD.Parser is
   Me : constant Trace_Handle := Create ("BDD.PARSER");

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self   : in out Feature_Parser;
      File   : GNATCOLL.VFS.Virtual_File;
      Runner : in out Abstract_Feature_Runner'Class)
   is
      type State_Type is (None, In_Feature,
                          In_Scenario,
                          In_String,
                          In_Outline, In_Examples);
      State  : State_Type := None;
      F      : Feature;
      Scenar : Scenario;
      Buffer : GNAT.Strings.String_Access := File.Read_File;
      Seen_Scenar : Boolean := False; --  Whether we have one scenario in F
      Index  : Integer := Buffer'First;
      Line   : Natural := 0;
      Index_In_Feature : Natural := 0;
      String_Indent  : Natural := 0;  --  indentation of the mutli-line string
      Line_S, Line_E : Integer;       --  extents of the current line
      First_Char     : Integer;       --  first non-blank character in line
      String_Line_Start : Integer;    --  first line of multi-line string

      procedure Finish_Scenario;
      procedure Finish_Feature;
      --  Called when the end of a scenario or a feature are seen

      function Get_Line_End (After : Positive) return String;
      --  Return the line text after the given character

      ---------------------
      -- Finish_Scenario --
      ---------------------

      procedure Finish_Scenario is
      begin
         case State is
            when In_Scenario | In_Outline | In_Examples =>
               Runner.Scenario_End (F, Scenar);
               Free (Scenar);
               State := In_Feature;
               Seen_Scenar := True;

            when others =>
               null;
         end case;
      end Finish_Scenario;

      --------------------
      -- Finish_Feature --
      --------------------

      procedure Finish_Feature is
      begin
         Finish_Scenario;
         if State /= None then
            Runner.Feature_End (F);
            Free (F);
         end if;

         State := None;
         Seen_Scenar := False;
      end Finish_Feature;

      ------------------
      -- Get_Line_End --
      ------------------

      function Get_Line_End (After : Positive) return String is
      begin
         if After <= Line_E then
            return Buffer (After .. Line_E);
         else
            return "";
         end if;
      end Get_Line_End;

   begin
      Trace (Me, "Parsing " & File.Display_Full_Name);

      while Index < Buffer'Last loop
         Line_S := Index;
         Line_E := Line_End (Buffer.all, Line_S);

         Index := Line_E + 1;
         if Buffer (Index) = ASCII.CR then
            Index := Index + 1;
         end if;
         if Buffer (Index) = ASCII.LF then
            Index := Index + 1;
         end if;

         Line   := Line + 1;

         if Active (Me) then
            Trace (Me, "Line " & Image (Line, 3, Padding => ' ')
                   & " " & Buffer (Line_S .. Line_E));
         end if;

         First_Char := Line_S;
         Skip_Blanks (Buffer (Line_S .. Line_E), First_Char);

         if Starts_With (Buffer (First_Char .. Line_E), """""""") then
            if State = In_String then
               State := In_Scenario;
            elsif State = In_Scenario then
               State := In_String;
               String_Indent := First_Char - Line_S;
               String_Line_Start := Line;
            else
               raise Syntax_Error with "Multi-line strings only allowed in"
                 & " steps, at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

         elsif State = In_String then
            if First_Char  < Line_S + String_Indent - 1 then
               Trace (Me, "MANU String="
                      & Buffer (First_Char .. Line_E));
            elsif Line_S + String_Indent - 1 <= Line_E then
               Trace (Me, "MANU String="
                      & Buffer (Line_S + String_Indent - 1 .. Line_E));
            else
               Trace (Me, "MANU String=");
            end if;

         elsif First_Char > Line_E then
            --  ignore blank lines
            null;

         elsif Buffer (First_Char) = '|' then
            case State is
               when In_Scenario | In_Outline =>
                  Trace (Me, "MANU Table=" & Buffer (First_Char .. Line_E));

               when In_Examples =>
                  Trace (Me, "MANU Example=" & Buffer (First_Char .. Line_E));

               when others =>
                  raise Syntax_Error with "Tables only allowed in"
                    & " steps, at " & File.Display_Full_Name & ":"
                    & Image (Line, 1);
            end case;

         elsif Buffer (First_Char) = '@' then
            --  ??? parse tags
            null;

         elsif Buffer (First_Char) = '#' then
            --  Ignore comment lines
            null;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Features) then
            Finish_Feature;
            F.Set_Unique_Id (Self.F_Id);
            Self.F_Id := Self.F_Id + 1;
            F.Set_File (File);
            F.Set_Name (Buffer (First_Char + Cst_Features'Length .. Line_E));
            Runner.Feature_Start (F);
            Index_In_Feature := 0;
            State := In_Feature;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Background) then
            if State = None then
               raise Syntax_Error with "Background must be defined within a"
                 & " Feature at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

            if Seen_Scenar then
               raise Syntax_Error with "Background must be defined before all"
                 & " Scenario, at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

            Finish_Scenario;

            Scenar := new Scenario_Record;
            Scenar.Set_Attributes
              (Name  => Get_Line_End (First_Char + Cst_Background'Length),
               Kind  => Kind_Background,
               Line  => Line,
               Index => Positive'Last);
            Runner.Scenario_Start (F, Scenar);
            State := In_Scenario;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Scenario) then
            if State = None then
               raise Syntax_Error with "Scenario must be defined within a"
                 & " Feature at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

            Finish_Scenario;
            Index_In_Feature := Index_In_Feature + 1;

            Scenar := new Scenario_Record;
            Scenar.Set_Attributes
              (Name  => Get_Line_End (First_Char + Cst_Scenario'Length),
               Kind  => Kind_Scenario,
               Line  => Line,
               Index => Index_In_Feature);
            Runner.Scenario_Start (F, Scenar);
            State := In_Scenario;

         elsif Starts_With (Buffer (First_Char .. Line_E),
                            Cst_Scenario_Outline)
         then
            if State = None then
               raise Syntax_Error with
                 "Scenario Outline must be defined within a Feature at "
                 & File.Display_Full_Name & ":" & Image (Line, 1);
            end if;

            Finish_Scenario;
            Index_In_Feature := Index_In_Feature + 1;

            Scenar := new Scenario_Record;
            Scenar.Set_Attributes
              (Name => Get_Line_End (First_Char + Cst_Scenario_Outline'Length),
               Kind  => Kind_Outline,
               Line  => Line,
               Index => Index_In_Feature);
            Runner.Scenario_Start (F, Scenar);
            State := In_Outline;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Scenarios) then
            if State /= In_Outline then
               raise Syntax_Error with
                 "Scenarios must be defined within a Scenario Outline, at "
                 & File.Display_Full_Name & ":" & Image (Line, 1);
            end if;
            State := In_Examples;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Examples) then
            if State /= In_Outline then
               raise Syntax_Error with
                 "Examples must be defined within a Scenario Outline, at "
                 & File.Display_Full_Name & ":" & Image (Line, 1);
            end if;
            State := In_Examples;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Given)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_And)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_Then)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_But)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_When)
         then
            if State /= In_Scenario
              and then State /= In_Outline
            then
               raise Syntax_Error with "Step must be defined with in a"
                 & " Scenario at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

            --  ??? Should handle the step
            null;

         else
            if State = None then
               raise Syntax_Error with "Expected line starting with "
                 & Cst_Features & " at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);

            elsif State = In_Feature then
               F.Add_Description (Buffer (First_Char .. Line_E));

            elsif State = In_Scenario
              or else State = In_Outline
            then
               raise Syntax_Error with
                 "Expected line starting with "
                 & Cst_Given & "/"
                 & Cst_When & "/"
                 & Cst_Then & "/"
                 & Cst_And & "/"
                 & Cst_But & ", at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;
         end if;
      end loop;

      if State = In_String then
         raise Syntax_Error with "Multi-line string must end with """""" at "
           & File.Display_Full_Name & ":" & Image (String_Line_Start, 1);
      end if;

      Finish_Feature;
      Free (Buffer);
   end Parse;

end BDD.Parser;
