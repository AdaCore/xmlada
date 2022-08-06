------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2016, Nicolas Boulenguez               --
--                     Copyright (C) 2016-2022, AdaCore                     --
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

pragma Ada_2012;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Translators is

   type A_Translator is abstract tagged private;

   type An_Exception (Replacement_Length : Natural) is record
      Replacement : String (1 .. Replacement_Length);
      Used        : Natural := 0;
   end record;

   package Exception_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, An_Exception, Ada.Strings.Hash, "=");

   type A_Translation (Name_Length : Natural) is record
      Name     : String (1 .. Name_Length);
      Position : Exception_Maps.Cursor;
   end record;

   function Valid_Ada_Identifier (Name : String) return Boolean;

   function New_Translation (Translator : in out A_Translator;
                             Original   :        String)
                            return A_Translation;

   function Translated (Translator  : A_Translator;
                        Translation : A_Translation)
                       return String
     with Post => Valid_Ada_Identifier (Translated'Result)
     or Translated'Result = "";

   function Is_Exception (Translation : A_Translation) return Boolean;

   function Original (Translation : A_Translation) return String
     with Pre => Is_Exception (Translation);

   procedure Iterate_On_Unused_Exceptions
     (Translator : A_Translator;
      Process    : not null access procedure (Replaced    : String;
                                              Replacement : String));

   --  This declaration is intended for child units, not for users.
   procedure Set_Exceptions (Translator : in out A_Translator) is abstract;

   function Default_Translation (Translator : A_Translator;
                                 Original   : String)
                                return String is abstract;

private

   type A_Translator is abstract tagged record
        Exceptions : Exception_Maps.Map;
   end record;

end Translators;
