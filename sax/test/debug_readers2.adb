with Ada.Text_IO;    use Ada.Text_IO;
with Readers;        use Readers;
with Sax.Exceptions; use Sax.Exceptions;
with Sax.Locators;   use Sax.Locators;
with Sax.Attributes; use Sax.Attributes;
with Unicode.CES;    use Unicode.CES;
with Unicode;        use Unicode;
with Encodings;      use Encodings;

package body Debug_Readers2 is
   -------------
   -- Warning --
   -------------

   procedure Warning
     (Handler : in out Debug_Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Put_Line ("Sax.Warning ("
                & Get_Message (Except) & ", at "
                & To_String (Get_Locator (Except)) & ')');
   end Warning;

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Put_Line ("Sax.Error ("
                & Get_Message (Except) & ", at "
                & To_String (Get_Locator (Except)) & ')');
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Handler : in out Debug_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Put_Line ("Sax.Fatal_Error ("
                & Get_Message (Except) & ", at "
                & To_String (Get_Locator (Except)) & ')');
      Fatal_Error (Reader (Handler), Except);
   end Fatal_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Debug_Reader;
      Loc     : access Sax.Locators.Locator'Class) is
   begin
      Put_Line ("Sax.Set_Document_Locator ()");
      Handler.Locator := Locator_Access (Loc);
   end Set_Document_Locator;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Debug_Reader) is
   begin
      Put_Line ("Sax.Start_Document ()");
   end Start_Document;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Debug_Reader) is
   begin
      Put_Line ("Sax.End_Document ()");
   end End_Document;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Handler : in out Debug_Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Start_Prefix_Mapping (" & Prefix & ", " & URI & ")");
   end Start_Prefix_Mapping;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   procedure End_Prefix_Mapping
     (Handler : in out Debug_Reader; Prefix : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.End_Prefix_Mapping (" & Prefix & ") at "
                & To_String (Handler.Locator.all));
   end End_Prefix_Mapping;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is
   begin
      Put ("Sax.Start_Element ("
           & Namespace_URI & ", " & Local_Name & ", " & Qname);
      for J in 0 .. Get_Length (Atts) - 1 loop
         Put (", " & Get_Qname (Atts, J) & "='" & Get_Value (Atts, J) & ''');
      end loop;
      Put_Line (") at " & To_String (Handler.Locator.all));
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler : in out Debug_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is
   begin
      Put_Line ("Sax.End_Element (" & Namespace_URI & ", "
                & Local_Name & ", " & Qname & ") at "
                & To_String (Handler.Locator.all));
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Characters (" & Ch & ','
                & Integer'Image (Ch'Length) & ") at "
                & To_String (Handler.Locator.all));
   end Characters;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
      Index : Natural := Ch'First;
      C : Unicode_Char;
   begin
      Put ("Sax.Ignorable_Whitespace (");
      while Index <= Ch'Last loop
         C := Encoding.Read (Ch, Index);
         Index := Index + Encoding.Width (C);
         Put (Unicode_Char'Image (C));
      end loop;
      Put_Line (','
                & Integer'Image (Ch'Length) & ") at "
                & To_String (Handler.Locator.all));
   end Ignorable_Whitespace;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   procedure Processing_Instruction
     (Handler : in out Debug_Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Processing instruction (" & Target & ", '" & Data
                & "') at " & To_String (Handler.Locator.all));
   end Processing_Instruction;

   --------------------
   -- Skipped_Entity --
   --------------------

   procedure Skipped_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Skipped_Entity (" & Name & ") at "
                & To_String (Handler.Locator.all));
   end Skipped_Entity;

   -------------
   -- Comment --
   -------------

   procedure Comment
     (Handler : in out Debug_Reader; Ch : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Comment (" & Ch & ") at "
                & To_String (Handler.Locator.all));
   end Comment;

   -----------------
   -- Start_Cdata --
   -----------------

   procedure Start_Cdata (Handler : in out Debug_Reader) is
   begin
      Put_Line ("Sax.Start_Cdata () at " & To_String (Handler.Locator.all));
   end Start_Cdata;

   ---------------
   -- End_Cdata --
   ---------------

   procedure End_Cdata (Handler : in out Debug_Reader) is
   begin
      Put_Line ("Sax.End_Cdata () at " & To_String (Handler.Locator.all));
   end End_Cdata;

   ------------------
   -- Start_Entity --
   ------------------

   procedure Start_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Start_Entity (" & Name & ") at "
                & To_String (Handler.Locator.all));
   end Start_Entity;

   ----------------
   -- End_Entity --
   ----------------

   procedure End_Entity
     (Handler : in out Debug_Reader; Name : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.End_Entity (" & Name & ") at "
                & To_String (Handler.Locator.all));
   end End_Entity;

   ---------------
   -- Start_DTD --
   ---------------

   procedure Start_DTD
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "") is
   begin
      Put_Line ("Sax.Start_DTD (" & Name
                & ", " & Public_Id
                & ", " & System_Id & ") at "
                & To_String (Handler.Locator.all));
   end Start_DTD;

   -------------
   -- End_DTD --
   -------------

   procedure End_DTD (Handler : in out Debug_Reader) is
   begin
      Put_Line ("Sax.End_DTD () at " & To_String (Handler.Locator.all));
   end End_DTD;

   --------------------------
   -- Internal_Entity_Decl --
   --------------------------

   procedure Internal_Entity_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Internal_Entity_Decl ("
                & Name & ", " & Value
                & ") at " & To_String (Handler.Locator.all));
   end Internal_Entity_Decl;

   --------------------------
   -- External_Entity_Decl --
   --------------------------

   procedure External_Entity_Decl
     (Handler   : in out Debug_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.External_Entity_Decl ("
                & Name & ", " & Public_Id
                & ", " & System_Id
                & ") at " & To_String (Handler.Locator.all));
   end External_Entity_Decl;

   --------------------------
   -- Unparsed_Entity_Decl --
   --------------------------

   procedure Unparsed_Entity_Decl
     (Handler       : in out Debug_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence;
      Notation_Name : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Unparsed_Entity_Decl ("
                & Name & ", " & System_Id
                & ", " & Notation_Name
                & ") at " & To_String (Handler.Locator.all));
   end Unparsed_Entity_Decl;

   ------------------
   -- Element_Decl --
   ------------------

   procedure Element_Decl
     (Handler : in out Debug_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Unicode.CES.Byte_Sequence;
      Parsed_Model : Element_Model_Ptr) is
   begin
      Put_Line ("Sax.Element_Decl ("
                & Name & ", " & Model
                & ") at " & To_String (Handler.Locator.all));
   end Element_Decl;

   -------------------
   -- Notation_Decl --
   -------------------

   procedure Notation_Decl
     (Handler       : in out Debug_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Notation_Decl ("
                & Name & ", " & Public_Id
                & ", " & System_Id & ") at "
                & To_String (Handler.Locator.all));
   end Notation_Decl;

   --------------------
   -- Attribute_Decl --
   --------------------

   procedure Attribute_Decl
     (Handler : in out Debug_Reader;
      Ename   : Unicode.CES.Byte_Sequence;
      Aname   : Unicode.CES.Byte_Sequence;
      Typ     : Unicode.CES.Byte_Sequence;
      Value_Default : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence) is
   begin
      Put_Line ("Sax.Attribute_Decl ("
                & Ename & ", " & Aname
                & ", " & Typ & ", " & Value_Default
                & ", " & Value & ")");
   end Attribute_Decl;

end Debug_Readers2;
