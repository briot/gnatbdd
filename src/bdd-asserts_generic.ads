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

--  An assertion library for use with GNATBDD

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with BDD.Media;             use BDD.Media;
with GNAT.Source_Info;
with GNATCOLL.Refcount;     use GNATCOLL.Refcount;

package BDD.Asserts_Generic is

   -------------------
   --  Assert_Error --
   -------------------
   --  Steps might fail in several ways. For instance, they might raise an
   --  unexpected exception, or perform explicit tests.
   --  When such a test fails, it should raise an exception. We encourage you
   --  to raise an exception via the following API, which allows you to embed
   --  details about the failure, as well as provide a nice way to display them
   --  to the user.
   --  The reason to use the API below is that Ada exception messages are
   --  limited in length, and thus cannot embed the rich information necessary
   --  to understand the error without launching a debugger.

   Unexpected_Result : exception;

   type Assert_Error is tagged private;
   No_Error : constant Assert_Error;
   --  A reference counted-type that describes the details for an error.
   --  This type is tagged only so that the dot notation can be used for calls.
   --  If you need to store the details of an exception, this is the type that
   --  should be stored, not the underlying Error_Details_Access, which might
   --  be freed at any point if no Assert_Error still exists.

   function Get (E : Exception_Occurrence) return Assert_Error;
   --  Retrieve the details from the exception (which should have been
   --  raises by Raise_Assert_Error above, or one of the Assert procedures
   --  below).

   type Error_Details is tagged private;
   type Error_Details_Access is access all Error_Details'Class;
   --  the details stored in an assert_Error.
   --  You are encouraged to extend this type if you need to provide additional
   --  details about errors.

   procedure Free (Self : in out Error_Details) is null;
   --  Free the memory used by Self.
   --  This should not be called directly.

   function Details (Self : Assert_Error) return Error_Details_Access;
   --  Returns the actual details of the exception.
   --  Do not store the result access, which might be freed when Self is no
   --  longer referenced by your application.

   procedure Set_Details
     (Self     : not null access Error_Details;
      Details  : String := "";
      Msg      : String := "";
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity);
   --  Store basic information in Self.
   --
   --  Msg is in general a static string provided by the user, to help pinpoint
   --  which particular assert failed. It will often be left to the empty
   --  string, since there is already quite a lot of context when the error is
   --  displayed to the user.
   --
   --  Details should be used to describe the expected and actual values.
   --
   --  Location and Entity are used to point to the code location where the
   --  error is raised.

   procedure Raise_Exception (Self : not null access Error_Details);
   pragma No_Return (Raise_Exception);
   --  Wraps Self in an Assert_Error, and raise the Unexpected_Result
   --  exception.
   --  When this exception is handled, one can use Get to retrieve the
   --  Assert_Error, and then Details to get access to Self again.

   procedure Display
     (Self   : Assert_Error;
      Output : not null access BDD.Media.Media_Writer'Class;
      Status : Scenario_Status;
      Prefix : String := "");
   procedure Display
     (Self   : not null access Error_Details;
      Output : not null access BDD.Media.Media_Writer'Class;
      Status : Scenario_Status;
      Prefix : String := "");
   --  Display the details on File, using Term to set appropriate colors.

   function From_Exception (E : Exception_Occurrence) return Assert_Error;
   --  Create from the information contained in the exception

   --------------
   -- Generics --
   --------------
   --  The following API provides convenient ways to perform tests and raise
   --  appropriate exceptions. A number of predefined instances are provided
   --  in the BDD.Asserts package.

   generic
      type T (<>) is limited private;
      with function Image (V : T) return String;
      with function Operator (V1, V2 : T) return Boolean;
      Not_Operator_Image : String;

   package Asserts is

      procedure Assert
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity);
      --  Compare two elements for equality.
      --  You could provide a detailed message through Msg, which will help
      --  understand the error later on.
      --  You are not expected to provide actual values for Location and
      --  Entity, which are used to automatically retrieve the location where
      --  the error occurred.

   end Asserts;

private

   type Error_Details is new Refcounted with record
      Details  : Unbounded_String;
      Msg      : Unbounded_String;
      Location : Unbounded_String;
   end record;

   package Errors is new GNATCOLL.Refcount.Smart_Pointers (Error_Details);
   type Assert_Error is new Errors.Ref with null record;

   No_Error : constant Assert_Error := (Errors.Null_Ref with null record);

end BDD.Asserts_Generic;
