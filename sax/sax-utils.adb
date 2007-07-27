-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                    Copyright (C) 2005-2007, AdaCore               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces;                use Interfaces;
with Unicode.CES;               use Unicode, Unicode.CES;
with Sax.Encodings;             use Sax.Encodings;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;

package body Sax.Utils is

   type Unichar_Boolean_Array is array (Unicode_Char range <>) of Boolean;
   pragma Pack (Unichar_Boolean_Array);

   Valid_URI_Characters : constant Unichar_Boolean_Array
     (Exclamation_Mark .. Tilde) :=
     (Digit_Zero .. Digit_Nine => True,
      Latin_Capital_Letter_A .. Latin_Capital_Letter_Z => True,
      Latin_Small_Letter_A .. Latin_Small_Letter_Z => True,
      Opening_Parenthesis | Closing_Parenthesis => True,
      Percent_Sign       => True,
      Plus_Sign          => True,
      Comma              => True,
      Hyphen_Minus       => True,
      Dot                => True,
      Colon              => True,
      Equals_Sign        => True,
      Commercial_At      => True,
      Semicolon          => True,
      Dollar_Sign        => True,
      Spacing_Underscore => True,
      Exclamation_Mark   => True,
      Star               => True,
      Apostrophe         => True,
      Question_Mark      => True,
      Slash              => True,
      Pound_Sign         => True,
      Tilde              => True,
      others => False);
   --  Rules based on RFC 2141, at http://rfc.net/rfc2141.html,
   --  completed with rules from Uniformed Resource Identifier at
   --  http://www.gbiv.com/protocols/uri/rfc/rfc3986.html

   ----------------------------
   -- Is_Valid_Language_Name --
   ----------------------------

   function Is_Valid_Language_Name
     (Lang : Unicode.CES.Byte_Sequence) return Boolean
   is
      C     : Unicode_Char := Space;
      Index : Natural := Lang'First;
      Count : Natural := 0;
      Subtag : Natural := 1;
      Allow_Digit : Boolean := False;
   begin
      --  See http://www.ietf.org/rfc/rfc3066.tx
      --    Language-Tag = Primary-subtag *( "-" Subtag )
      --    Primary-subtag = 1*8ALPHA
      --    Subtag = 1*8(ALPHA / DIGIT)
      --  In addition, it seems that the length of subtags is not necessarily
      --  limited to 8 characters, given the XML conformance testsuite test
      --  sun/valid/v-lang04.xml

      while Index <= Lang'Last loop
         Encoding.Read (Lang, Index, C);

         if C = Hyphen_Minus then
            if Count = 0 or else (Subtag <= 2 and Count > 8) then
               --  Too many characters
               return False;

            else
               Allow_Digit := True;
               Count := 0;
               Subtag := Subtag + 1;
            end if;

         elsif C not in Latin_Small_Letter_A .. Latin_Small_Letter_Z
           and then C not in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z
           and then (not Allow_Digit
                     or else C not in Digit_Zero .. Digit_Nine)
         then
            return False;

         else
            Count := Count + 1;
         end if;
      end loop;

      if Count = 0 or else (Subtag <= 2 and Count > 8) then
         --  Too many characters
         return False;
      end if;

      return True;
   end Is_Valid_Language_Name;

   ------------------------
   -- Is_Valid_Name_Char --
   ------------------------

   function Is_Valid_Name_Char (Char : Unicode.Unicode_Char) return Boolean is
   begin
      --  ??? Should we create a single lookup table for all of these, that
      --  would be more efficient
      return Char = Period
        or else Char = Hyphen_Minus
        or else Char = Spacing_Underscore
        or else Char = Colon
        or else Is_Digit (Char)
        or else Is_Letter (Char)
        or else Is_Combining_Char (Char)
        or else Is_Extender (Char);
   end Is_Valid_Name_Char;

   --------------------------
   -- Is_Valid_NCname_Char --
   --------------------------

   function Is_Valid_NCname_Char
     (Char : Unicode.Unicode_Char) return Boolean is
   begin
      return Char = Period
        or else Char = Hyphen_Minus
        or else Char = Spacing_Underscore
        or else Is_Digit (Char)
        or else Is_Letter (Char)
        or else Is_Combining_Char (Char)
        or else Is_Extender (Char);
   end Is_Valid_NCname_Char;

   ----------------------
   -- Is_Valid_Nmtoken --
   ----------------------

   function Is_Valid_Nmtoken
     (Nmtoken     : Unicode.CES.Byte_Sequence) return Boolean
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

   -----------------------
   -- Is_Valid_Nmtokens --
   -----------------------

   function Is_Valid_Nmtokens
     (Nmtokens : Unicode.CES.Byte_Sequence) return Boolean
   is
      C     : Unicode_Char;
      Index : Natural := Nmtokens'First;
   begin
      if Nmtokens'Length = 0 then
         return False;
      end if;

      while Index <= Nmtokens'Last loop
         Encoding.Read (Nmtokens, Index, C);
         if C /= Space and then not Is_Valid_Name_Char (C) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Nmtokens;

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name
     (Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      C     : Unicode_Char;
      Index : Natural := Name'First;
   begin
      if Name = "" then
         return False;
      end if;

      Encoding.Read (Name, Index, C);

      if C /= Spacing_Underscore
        and then C /= Colon
        and then not Is_Letter (C)
      then
         return False;
      end if;

      return Is_Valid_Nmtoken (Name (Index .. Name'Last));
   end Is_Valid_Name;

   --------------------
   -- Is_Valid_Names --
   --------------------

   function Is_Valid_Names
     (Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      C             : Unicode_Char;
      Index         : Natural := Name'First;
      First_In_Name : Boolean := True;
   begin
      if Name'Length = 0 then
         return False;
      end if;

      while Index <= Name'Last loop
         Encoding.Read (Name, Index, C);

         if C = Space then
            First_In_Name := True;

         elsif First_In_Name then
            if C /= Spacing_Underscore
              and then C /= Colon
              and then not Is_Letter (C)
            then
               return False;
            end if;
            First_In_Name := False;

         elsif not Is_Valid_Name_Char (C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_Names;

   ----------------------
   -- Is_Valid_NCnames --
   ----------------------

   function Is_Valid_NCnames
     (Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      C             : Unicode_Char;
      Index         : Natural := Name'First;
      First_In_Name : Boolean := True;
   begin
      if Name'Length = 0 then
         return False;
      end if;

      while Index <= Name'Last loop
         Encoding.Read (Name, Index, C);

         if C = Space then
            First_In_Name := True;

         elsif First_In_Name then
            if C /= Spacing_Underscore
              and then not Is_Letter (C)
            then
               return False;
            end if;
            First_In_Name := False;

         elsif not Is_Valid_NCname_Char (C) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_NCnames;

   ---------------------
   -- Is_Valid_NCname --
   ---------------------

   function Is_Valid_NCname
     (Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      C     : Unicode_Char;
      Index : Natural := Name'First;
   begin
      if Name'Length = 0 then
         return False;
      end if;

      Encoding.Read (Name, Index, C);

      if C /= Spacing_Underscore and then not Is_Letter (C) then
         return False;
      end if;

      while Index <= Name'Last loop
         Encoding.Read (Name, Index, C);
         if not Is_Valid_NCname_Char (C) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_NCname;

   --------------------
   -- Is_Valid_QName --
   --------------------

   function Is_Valid_QName
     (Name : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      for N in Name'Range loop
         if Name (N) = ':' then
            return N /= Name'Last
              and then Is_Valid_NCname (Name (Name'First .. N - 1))
              and then Is_Valid_NCname (Name (N + 1 .. Name'Last));
         end if;
      end loop;
      return Is_Valid_NCname (Name);

   end Is_Valid_QName;

   ----------
   -- Hash --
   ----------

   function Hash
     (Key : Unicode.CES.Byte_Sequence) return Interfaces.Unsigned_32
   is
      type Uns is mod 2 ** 32;
      function Rotate_Left (Value : Uns; Amount : Natural) return Uns;
      pragma Import (Intrinsic, Rotate_Left);

      Tmp : Uns := 0;
   begin
      for J in Key'Range loop
         Tmp := Rotate_Left (Tmp, 1) + Character'Pos (Key (J));
      end loop;

      return Interfaces.Unsigned_32 (Tmp);
   end Hash;

   -----------------
   -- Split_Qname --
   -----------------

   function Split_Qname (Qname : Unicode.CES.Byte_Sequence) return Integer is
   begin
      --  ??? This function assumes we are using UTF8 internally
      for Q in Qname'Range loop
         if Qname (Q) = ':' then
            return Q;
         end if;
      end loop;
      return Qname'First - 1;
   end Split_Qname;

   ---------------
   -- Check_URI --
   ---------------

   function Check_URI
     (Name : Unicode.CES.Byte_Sequence) return URI_Type
   is
      Index      : Integer := Name'First;
      C          : Unicode_Char;
      Has_Scheme : Boolean := False;
      Has_Hash   : Boolean := False;
   begin
      --  This is RFC 3986 which obsoletes RFC 2396.
      --  URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
      --  hier-part     = "//" authority path-abempty
      --                / path-absolute
      --                / path-rootless
      --                / path-empty
      --  URI-reference = URI / relative-ref
      --  absolute-URI  = scheme ":" hier-part [ "?" query ]
      --  relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
      --  relative-part = "//" authority path-abempty
      --                / path-absolute
      --                / path-noscheme
      --                / path-empty
      --  scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
      --  authority     = [ userinfo "@" ] host [ ":" port ]
      --  userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
      --  host          = IP-literal / IPv4address / reg-name
      --  port          = *DIGIT
      --  IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
      --  reg-name      = *( unreserved / pct-encoded / sub-delims )
      --  path          = path-abempty    ; begins with "/" or is empty
      --                / path-absolute   ; begins with "/" but not "//"
      --                / path-noscheme   ; begins with a non-colon segment
      --                / path-rootless   ; begins with a segment
      --                / path-empty      ; zero characters
      --  path-abempty  = *( "/" segment )
      --  path-absolute = "/" [ segment-nz *( "/" segment ) ]
      --  path-noscheme = segment-nz-nc *( "/" segment )
      --  path-rootless = segment-nz *( "/" segment )
      --  path-empty    = 0<pchar>
      --  segment       = *pchar
      --  segment-nz    = 1*pchar
      --  segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
      --                ; non-zero-length segment without any colon ":"
      --  pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
      --  query         = *( pchar / "/" / "?" )
      --  fragment      = *( pchar / "/" / "?" )
      --  pct-encoded   = "%" HEXDIG HEXDIG
      --  unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
      --  reserved      = gen-delims / sub-delims
      --  gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
      --  sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
      --                / "*" / "+" / "," / ";" / "="

      --  The relativeURI rule has been replaced with relative-ref.

      --  Find and test the scheme. If there is no scheme, we might have a
      --  relative URI

      while Index <= Name'Last loop
         Encoding.Read (Name, Index, C);
         if C = Colon then
            Has_Scheme := True;
            exit;

         elsif C not in Character'Pos ('a') .. Character'Pos ('z')
           and then C not in Character'Pos ('A') .. Character'Pos ('Z')
           and then C not in Character'Pos ('0') .. Character'Pos ('9')
           and then C /= Character'Pos ('+')
           and then C /= Character'Pos ('-')
           and then C /= Character'Pos ('.')
         then
            Has_Scheme := False;
            exit;
         end if;
      end loop;

      --  For a mailto:, nothing else to check
      if Has_Scheme
        and then Name (Name'First .. Index - 1) = Mailto_Sequence
      then
         return URI_Absolute;
      end if;

      --  Check the rest of the URI. We currently go for a fast check, and do
      --  not check each of the components specifically.

      while Index <= Name'Last loop
         Encoding.Read (Name, Index, C);
         if C = Unicode.Names.Basic_Latin.Hash then
            if Has_Hash then
               --  Two hashes => Invalid URI
               return URI_None;
            end if;
            Has_Hash := True;

         elsif C not in Valid_URI_Characters'Range
           or else not Valid_URI_Characters (C)
         then
            return URI_None;
         end if;
      end loop;

      if Has_Scheme then
         return URI_Absolute;
      else
         return URI_Relative_Ref;
      end if;
   end Check_URI;

   ------------------
   -- Is_Valid_URI --
   ------------------

   function Is_Valid_URI
     (Name : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      return Check_URI (Name) /= URI_None;
   end Is_Valid_URI;

   ------------------
   -- Is_Valid_URN --
   ------------------

   function Is_Valid_URN
     (Name : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      --  format is  "urn:" <NID> ":" <NSS>
      --  NID: Namespace Identifier
      --  NSS: Namespace Specific String

      --  ??? Note that this implementation makes <NSS> optional as it is
      --  often the case in current usage.

      --  Leading sequence should be case insensitive
      if Name'Length < URN_Sequence'Length
        or else Name (Name'First .. Name'First + URN_Sequence'Length - 1) /=
        URN_Sequence
      then
         return False;
      end if;

      return True;
   end Is_Valid_URN;

   ------------------
   -- Is_Valid_IRI --
   ------------------

   function Is_Valid_IRI
     (Name : Unicode.CES.Byte_Sequence) return Boolean is
   begin
      return Check_URI (Name) = URI_Absolute or else Is_Valid_URN (Name);
   end Is_Valid_IRI;

   ---------------------------
   -- Contains_URI_Fragment --
   ---------------------------

   function Contains_URI_Fragment
     (Name : Unicode.CES.Byte_Sequence) return Boolean
   is
      Index : Integer := Name'First;
      C     : Unicode_Char;
   begin
      while Index <= Name'Last loop
         Encoding.Read (Name, Index, C);
         if C = Pound_Sign then
            return True;
         end if;
      end loop;
      return False;
   end Contains_URI_Fragment;

   ------------------------
   -- Is_Valid_HexBinary --
   ------------------------

   function Is_Valid_HexBinary
     (Str : Unicode.CES.Byte_Sequence) return Boolean
   is
      Index : Integer := Str'First;
      C     : Unicode_Char;
   begin
      while Index <= Str'Last loop
         Encoding.Read (Str, Index, C);
         if C not in Character'Pos ('0') .. Character'Pos ('9')
           and then C not in Character'Pos ('a') .. Character'Pos ('f')
           and then C not in Character'Pos ('A') .. Character'Pos ('F')
         then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_HexBinary;

end Sax.Utils;
