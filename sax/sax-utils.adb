with Unicode.CES;               use Unicode, Unicode.CES;
with Sax.Encodings;             use Sax.Encodings;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;

package body Sax.Utils is

   ----------------------------
   -- Is_Valid_Language_Name --
   ----------------------------

   function Is_Valid_Language_Name
     (Lang : Unicode.CES.Byte_Sequence) return Boolean
   is
      C, C2 : Unicode_Char;
      Index : Natural := Lang'First;
   begin
      Encoding.Read (Lang, Index, C2);

      if not (C2 in Latin_Small_Letter_A .. Latin_Small_Letter_Z
              or else C2 in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
        or else Index > Lang'Last
      then
         return False;
      end if;

      Encoding.Read (Lang, Index, C);
      if C in Latin_Small_Letter_A .. Latin_Small_Letter_Z
        or else C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z
      then
         if Index <= Lang'Last then
            Encoding.Read (Lang, Index, C);
         end if;

      elsif C2 /= Latin_Small_Letter_I
        and then C2 /= Latin_Capital_Letter_I
        and then C2 /= Latin_Small_Letter_X
        and then C2 /= Latin_Capital_Letter_X
      then
         return False;
      end if;

      if C = Hyphen_Minus and then Index > Lang'Last then
         return False;
      end if;

      while Index <= Lang'Last loop
         if C /= Hyphen_Minus
           or else Index > Lang'Last
         then
            return False;
         end if;

         loop
            Encoding.Read (Lang, Index, C);

            exit when Index > Lang'Last
              or else not
              (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z
                or else C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z);
         end loop;
      end loop;

      return True;
   end Is_Valid_Language_Name;

   ------------------------
   -- Is_Valid_Name_Char --
   ------------------------

   function Is_Valid_Name_Char (Char : Unicode.Unicode_Char) return Boolean is
   begin
      return Char = Period
        or else Char = Hyphen_Minus
        or else Char = Spacing_Underscore
        or else Is_Digit (Char)
        or else Is_Letter (Char)
        or else Is_Combining_Char (Char)
        or else Is_Extender (Char);
   end Is_Valid_Name_Char;

   ----------------------
   -- Is_Valid_Nmtoken --
   ----------------------

   function Is_Valid_Nmtoken
     (Nmtoken : Unicode.CES.Byte_Sequence) return Boolean
   is
      C     : Unicode_Char;
      Index : Natural := Nmtoken'First;
   begin
      while Index <= Nmtoken'Last loop
         Encoding.Read (Nmtoken, Index, C);
         if not Is_Valid_Name_Char (C) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Nmtoken;

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name
     (Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      C     : Unicode_Char;
      Index : Natural := Name'First;
   begin
      Encoding.Read (Name, Index, C);

      if C /= Spacing_Underscore
        and then C /= Colon
        and then not Is_Letter (C)
      then
         return False;
      end if;

      return Is_Valid_Nmtoken (Name (Index .. Name'Last));
   end Is_Valid_Name;

   ---------------------
   -- Is_Valid_NCname --
   ---------------------

   function Is_Valid_NCname
     (Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      C     : Unicode_Char;
      Index : Natural := Name'First;
   begin
      Encoding.Read (Name, Index, C);

      if C /= Spacing_Underscore and then not Is_Letter (C) then
         return False;
      end if;

      while Index <= Name'Last loop
         Encoding.Read (Name, Index, C);

         if C = Colon or else not Is_Valid_Name_Char (C) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_NCname;

end Sax.Utils;
