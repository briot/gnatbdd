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

--  A parser for the features files

with BDD.Features;     use BDD.Features;
with GNATCOLL.VFS;     use GNATCOLL.VFS;

package BDD.Parser is

   Syntax_Error : exception;
   --  Raised when reading one of the features file raises a syntax error.

   type Feature_Parser is tagged private;

   procedure Parse
     (Self     : Feature_Parser;
      File     : GNATCOLL.VFS.Virtual_File;
      Callback : access procedure (F : BDD.Features.Feature));
   --  Parses a .feature file.
   --  Calls Callback for each of the features found.
   --  Raises Syntax_Error when the file does not contain valid syntax.

private
   type Feature_Parser is tagged record
      null;
   end record;

end BDD.Parser;
