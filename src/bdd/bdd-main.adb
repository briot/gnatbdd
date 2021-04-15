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

with BDD.Formatters;    use BDD.Formatters;
with BDD.Media;         use BDD.Media;
with BDD.Parser;        use BDD.Parser;
with BDD.Runner;        use BDD.Runner;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNATCOLL.Traces;   use GNATCOLL.Traces;

package body BDD.Main is

   ----------
   -- Main --
   ----------

   procedure Main (Self : in out BDD.Runner.Feature_Runner'Class)
   is
      Parser   : BDD.Parser.Feature_Parser;
      Format   : BDD.Formatters.Formatter_Access;
      Media    : Media_Writer_Access;
   begin
      GNATCOLL.Traces.Parse_Config_File;
      BDD.Command_Line_Switches;

      Self.Discover (+BDD.Features_File_Ext.all, BDD.Features_Directory);

      Format := BDD.Formatters.Create_Formatter;

      if BDD.Output_File = No_File then
         Media := Open_Stdout;
      elsif BDD.Output_File.File_Extension = ".html" then
         Media  := Open_HTML (Open_File (BDD.Output_File));
      else
         Media := Open_File (BDD.Output_File);
      end if;

      Format.Init (Media);

      Self.Run (Format, Parser);

   exception
      when GNAT.Command_Line.Exit_From_Command_Line
         | GNAT.Command_Line.Invalid_Switch
         | GNAT.Command_Line.Invalid_Parameter
         =>
         null;
   end Main;

end BDD.Main;
