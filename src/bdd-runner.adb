-----------------------------------------------------------------------------
--                             g N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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

package body BDD.Runner is

   --------------
   -- Discover --
   --------------

   procedure Discover
     (Self      : in out Feature_Runner;
      Extension : Filesystem_String         := ".feature";
      Directory : GNATCOLL.VFS.Virtual_File := Create_From_Base ("features"))
   is
      Files : File_Array_Access;
   begin
      if Directory.Is_Directory then
         Files := Directory.Read_Dir (Files_Only);
         for F in Files'Range loop
            if Files (F).File_Extension = Extension then
               Self.Register (Files (F));
            end if;
         end loop;
         Unchecked_Free (Files);

         Files := Directory.Read_Dir (Dirs_Only);
         for F in Files'Range loop
            if Files (F).Base_Name /= "."
              and then Files (F).Base_Name /= ".."
            then
               Self.Discover (Extension, Files (F));
            end if;
         end loop;
         Unchecked_Free (Files);
      end if;
   end Discover;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self      : in out Feature_Runner;
      File      : GNATCOLL.VFS.Virtual_File) is
   begin
      Append (Self.Files, File);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self      : in out Feature_Runner;
      Files     : GNATCOLL.VFS.File_Array) is
   begin
      Append (Self.Files, Files);
   end Register;

   ---------
   -- Run --
   ---------

   procedure Run
     (Self   : in out Feature_Runner;
      Format : not null access BDD.Formatters.Formatter'Class;
      Parser : in out BDD.Parser.Feature_Parser'Class)
   is
   begin
      if Self.Files /= null then
         Self.Format := Format;
         Sort (Self.Files.all);
         for F in Self.Files'Range loop
            Parser.Parse (Self.Files (F), Self);
         end loop;
         Self.Format := null;
      end if;
   end Run;

   --------------------
   -- Scenario_Start --
   --------------------

   overriding procedure Scenario_Start
     (Self     : in out Feature_Runner;
      Feature  : in out BDD.Features.Feature'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
   begin
      if Scenario.Kind = Kind_Scenario then
         Self.Format.Scenario_Start (Feature, Scenario);
      end if;
   end Scenario_Start;

   ------------------
   -- Scenario_End --
   ------------------

   overriding procedure Scenario_End
     (Self     : in out Feature_Runner;
      Feature  : in out BDD.Features.Feature'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
      Status : Scenario_Status;
   begin
      if Scenario.Kind = Kind_Scenario then
         Status := Status_Failed;

         delay 1.0;

         Self.Format.Scenario_Completed (Feature, Scenario, Status);
      end if;
   end Scenario_End;

end BDD.Runner;
