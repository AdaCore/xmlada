with Unicode.CES;       use Unicode.CES;
with Sax.Attributes;    use Sax.Attributes;
with Sax.Exceptions;    use Sax.Exceptions;
with Sax.Locators;      use Sax.Locators;
with Schema.Validators; use Schema.Validators;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Schema.Validators; use Schema.Validators;
with GNAT.IO; use GNAT.IO;

package body Schema.Readers is

   Debug : Boolean := False;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Validator_List_Record, Validator_List);

   procedure Push  (List      : in out Validator_List;
                    Validator : XML_Type;
                    Data      : Validator_Data);
   procedure Pop   (List : in out Validator_List);
   procedure Clear (List : in out Validator_List);
   --  Push or remove validators from the list

   pragma Unreferenced (Clear);

   ----------------------
   -- Set_Debug_Output --
   ----------------------

   procedure Set_Debug_Output (Output : Boolean) is
   begin
      Debug := Output;
   end Set_Debug_Output;

   -----------------
   -- Set_Grammar --
   -----------------

   procedure Set_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar) is
   begin
      Reader.Grammar := Grammar;
   end Set_Grammar;

   ----------
   -- Push --
   ----------

   procedure Push
     (List      : in out Validator_List;
      Validator : XML_Type;
      Data      : Validator_Data) is
   begin
      List := new Validator_List_Record'
        (Validator => Validator,
         Data      => Data,
         Next      => List);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (List : in out Validator_List) is
      Tmp : Validator_List := List;
   begin
      if List /= null then
         Free (List.Data);
         List := List.Next;

         Unchecked_Free (Tmp);
      end if;
   end Pop;

   -----------
   -- Clear --
   -----------

   procedure Clear (List : in out Validator_List) is
   begin
      while List /= null loop
         Pop (List);
      end loop;
   end Clear;

   ------------------
   -- Current_Type --
   ------------------

--     function Current_Type
--       (Reader : Validating_Reader) return Schema.Validators.XML_Type is
--     begin
--        if Reader.Validators /= null then
--           return Reader.Validators.Validator;
--        else
--           return No_Type;
--        end if;
--     end Current_Type;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Validating_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Qname);
      Validator : XML_Type;
      Data      : Validator_Data;
      G         : XML_Grammar_NS;
   begin
      if Debug then
         Put_Line (ASCII.ESC & "[33m"
                   & "Start_Element: " & Local_Name
                   & ASCII.ESC & "[39m");
      end if;

      if Handler.Validators /= null then
         Validate_Start_Element
           (Get_Validator (Handler.Validators.Validator), Local_Name,
            Handler.Validators.Data, Validator);
      else
         if Debug then
            Put_Line ("MANU: Getting element definition from grammar "
                      & Namespace_URI & " " & Local_Name);
         end if;
         Get_NS (Handler.Grammar, Namespace_URI, Result => G);
         Validator := Get_Type (Lookup_Element (G, Local_Name));
      end if;

      if Validator = No_Type then
         Raise_Exception
           (XML_Validation_Error'Identity,
            "No data type definition for element " & String (Local_Name));
      end if;

      Data := Create_Validator_Data (Get_Validator (Validator));
      Validate_Attributes (Get_Validator (Validator), Atts, Data);
      Push (Handler.Validators, Validator, Data);
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Validating_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI, Local_Name);
   begin
      if Handler.Validators /= null then
         Validate_End_Element
           (Get_Validator (Handler.Validators.Validator),
            Qname,
            Handler.Validators.Data);
      end if;
      Pop (Handler.Validators);
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Validating_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      if Handler.Validators /= null then
         Validate_Characters
           (Get_Validator (Handler.Validators.Validator), Ch);
      end if;
   end Characters;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class)
   is
      Loc : aliased Locator_Impl;
   begin
      Sax.Readers.Parse (Sax.Readers.Reader (Parser), Input);
      Unref (Parser.Locator.all);

   exception
      when E : XML_Validation_Error =>
         Copy (Loc, Parser.Locator.all);
         Validation_Error
           (Parser, Create (Byte_Sequence (Exception_Message (E)),
                            Loc'Unchecked_Access));
         Unref (Parser.Locator.all);
   end Parse;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Reader : in out Validating_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
   begin
      Raise_Exception
        (XML_Validation_Error'Identity,
         To_String (Get_Locator (Except)) & ": "
         & String (Get_Message (Except)));
   end Validation_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Validating_Reader;
      Loc     : access Sax.Locators.Locator'Class) is
   begin
      Handler.Locator := Locator_Access (Loc);
      Ref (Handler.Locator.all);
   end Set_Document_Locator;

end Schema.Readers;
