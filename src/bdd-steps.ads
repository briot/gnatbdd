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

--  This package provides support for matching steps with actual subprograms
--  registered by the user

with BDD.Features;     use BDD.Features;

package BDD.Steps is

   procedure Run_Step
     (Step    : not null access BDD.Features.Step_Record'Class;
      Text    : String;
      Execute : Boolean);
   --  Run a step, and sets its status.
   --
   --  If Execute is False, then we only check whether the step is known, but
   --  it is not run. No exception is raised in this mode.
   --
   --  Text must be Step.Text, minus the leading 'Given|Then|...' words.
   --
   --  This procedure is expected to raise exceptions when a test fails.

end BDD.Steps;
