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

with BDD.Parser;  use BDD.Parser;

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

   --------------
   -- For_Each --
   --------------

   procedure For_Each
     (Self      : in out Feature_Runner;
      Callback  : access procedure (F : Feature))
   is
      Parser : Feature_Parser;
   begin
      if Self.Files /= null then
         Sort (Self.Files.all);
         for F in Self.Files'Range loop
            Parser.Parse (Self.Files (F), Callback);
         end loop;
      end if;
   end For_Each;

end BDD.Runner;
