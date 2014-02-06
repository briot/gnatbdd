-----------------------------------------------------------------------------
--                             g N A T C O L L                              --
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

--  Manipulating features files

with BDD.Features;    use BDD.Features;
with GNATCOLL.VFS;    use GNATCOLL.VFS;

package BDD.Runner is

   type Feature_Runner is tagged private;
   --  This type is responsible for running each of the features that are
   --  registered.
   --  You can either register features files explicitly, or by using the
   --  Discover procedure below.

   procedure Discover
     (Self      : in out Feature_Runner;
      Extension : Filesystem_String := ".feature";
      Directory : GNATCOLL.VFS.Virtual_File := Create_From_Base ("features"));
   --  Analyze Directory recursively to find all features files.
   --  Extension is the file extension for such files.

   procedure Register
     (Self      : in out Feature_Runner;
      File      : GNATCOLL.VFS.Virtual_File);
   procedure Register
     (Self      : in out Feature_Runner;
      Files     : GNATCOLL.VFS.File_Array);
   --  Register one or more features file explicitly.

   procedure For_Each
     (Self      : in out Feature_Runner;
      Callback  : access procedure (F : Feature));
   --  Calls Callback for each of the registered features file.

private
   type Feature_Runner is tagged record
      Files : GNATCOLL.VFS.File_Array_Access;
   end record;

end BDD.Runner;
