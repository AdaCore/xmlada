with Unicode;         use Unicode;
with Unicode.CES;     use Unicode.CES;
with Sax.Attributes;  use Sax.Attributes;
with Sax.Encodings;   use Sax.Encodings;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;  use Ada.Exceptions;
with Sax.Utils;       use Sax.Utils;
with GNAT.IO; use GNAT.IO;
with Ada.Tags; use Ada.Tags;
with Schema.Validators.UR_Type;      use Schema.Validators.UR_Type;
with Schema.Validators.Facets;       use Schema.Validators.Facets;
with Schema.Validators.Extensions;   use Schema.Validators.Extensions;
with Schema.Validators.Restrictions; use Schema.Validators.Restrictions;
with Schema.Validators.Simple_Types; use Schema.Validators.Simple_Types;

package body Schema.Validators is

   Debug : Boolean := False;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Element_List, Element_List_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (XML_Attribute_Group_Record, XML_Attribute_Group);

   procedure Initialize_Sequence
     (Seq  : access Sequence_Record'Class;
      Data : Sequence_Data_Access);
   --  Reset the sequence to point to its first item

   procedure Create_NS_Grammar
     (Grammar       : in out XML_Grammar;
      Namespace_URI : Unicode.CES.Byte_Sequence);
   --  Create a new namespace in the grammar

   procedure Free (Element : in out XML_Element_Record);
   --  Free Element

   function Move_To_Next_Particle
     (Seq   : access Sequence_Record'Class;
      Data  : Sequence_Data_Access;
      Force : Boolean := False) return Boolean;
   --  Move to the next particle to match in the sequence, or stay on the
   --  current one if it still can match (its maxOccurs hasn't been reached
   --  for instance).

   procedure Check_Nested
     (Parent      : access Group_Model_Record'Class;
      Nested      : access Group_Model_Record'Class;
      Data        : access Group_Model_Data_Record'Class;
      Local_Name  : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element;
      Skip_Current      : out Boolean);
   --  Check whether Nested matches Local_Name.
   --  If Nested should match but there is an error, XML_Validator_Record is
   --  raised.
   --  If Nested cannot match Local_Name, Element_Validator is set to null on
   --  exit.
   --  Data should be the parent's data.
   --  Skip_Current is set to True if Nested didn't match Local_Name, but
   --  if it should be considered as terminated successfully (and thus we
   --  should hand out Local_Name to the next validator in the list)

   procedure Run_Nested
     (Validator         : access Group_Model_Record'Class;
      Data              : access Group_Model_Data_Record'Class;
      Local_Name        : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element);
   --  Run the nested group of Validator, if there is any.
   --  On exit, Element_Validator is set to No_Element if either the nested
   --  group didn't match, or there was no nested group.

   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Type;
   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Element;
   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Group;
   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return Attribute_Validator;
   --  Register a type, the definition of which is not know at that point.
   --  The definition must be provided before the grammar is fully filled, or
   --  this is an error.

   function Check_Substitution_Groups
     (Element    : XML_Element;
      Local_Name : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence) return XML_Element;
   --  Check whether any element in the substitution group of Validator can
   --  be used to match Local_Name. This also check whether Element itself
   --  matches.
   --  This also raises an XML_Validator_Record if the matching element is
   --  in fact abstract

   function Extension_Of
     (Base      : XML_Type;
      Extension : XML_Validator := null) return XML_Validator
     renames Schema.Validators.Extensions.Create_Extension_Of;

   function Extension_Of
     (Base       : XML_Type;
      Group      : XML_Group;
      Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return XML_Validator
      renames Schema.Validators.Extensions.Create_Extension_Of;

   function Restriction_Of
     (Base        : XML_Type;
      Restriction : XML_Validator := null) return XML_Validator
      renames Schema.Validators.Restrictions.Create_Restriction_Of;

   ---------------------
   -- Debug_Validator --
   ---------------------
   --  This accepts anything, but prints error messages
   type Debug_Validator_Record is new XML_Validator_Record with null record;
   --  Validates a "SimpleType" XML datatype, ie accepts any contents but
   --  elements and attributes

   procedure Validate_Start_Element
     (Validator         : access Debug_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element);
   procedure Validate_Characters
     (Validator     : access Debug_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);

   ------------------------------
   -- Attribute_Validator_List --
   ------------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attribute_Validator_List, Attribute_Validator_List_Access);

   procedure Free (List : in out Attribute_Validator_List_Access);
   --  Free the contents of List, including contained

   procedure Append
     (List      : in out Attribute_Validator_List_Access;
      Validator : access Attribute_Validator_Record'Class;
      Override  : Boolean);
   procedure Append
     (List      : in out Attribute_Validator_List_Access;
      Group     : XML_Attribute_Group);
   --  Append a new value to List.
   --  If a similar attribute already exists in the list, Validator will either
   --  be ignored (Override is False), or replace the existing definition
   --  (Override is True).

   --------------------
   -- List_Validator --
   --------------------

   type List_Validator_Record is new Any_Simple_XML_Validator_Record with
      record
         Base : XML_Type;
      end record;

   procedure Validate_Characters
     (Validator     : access List_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean);
   --  See doc from inherited subprogram

   -------------------------
   -- Get_Attribute_Lists --
   -------------------------

   procedure Get_Attribute_Lists
     (Validator   : access XML_Validator_Record;
      List        : out Attribute_Validator_List_Access;
      Dependency1 : out XML_Validator;
      Dependency2 : out XML_Validator) is
   begin
      List := Validator.Attributes;
      Dependency1 := null;
      Dependency2 := null;
   end Get_Attribute_Lists;

   ----------------------
   -- Set_Debug_Output --
   ----------------------

   procedure Set_Debug_Output (Output : Boolean) is
   begin
      Debug := Output;
   end Set_Debug_Output;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error (Message : String) is begin
      Raise_Exception (XML_Validation_Error'Identity, Message);
   end Validation_Error;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Attribute_Validator_List_Access) is
   begin
      if List /= null then
         for L in List'Range loop
            if not List (L).Is_Group then
               Free (List (L).Attr);
            end if;
         end loop;
         Unchecked_Free (List);
      end if;
   end Free;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Attribute : Named_Attribute_Validator_Record;
      Attr2     : Attribute_Validator_Record'Class)
     return Boolean is
   begin
      return Attr2 in Named_Attribute_Validator_Record'Class
        and then Attribute.NS = Attr2.NS
        and then Attribute.Local_Name.all =
          Named_Attribute_Validator_Record (Attr2).Local_Name.all;
   end Is_Equal;

   function Is_Equal
     (Attribute : Any_Attribute_Validator;
      Attr2     : Attribute_Validator_Record'Class)
     return Boolean is
   begin
      return Attr2 in Any_Attribute_Validator'Class
        and then Attribute.NS = Attr2.NS
        and then Attribute.Kind = Any_Attribute_Validator (Attr2).Kind;
   end Is_Equal;

   ------------
   -- Append --
   ------------

   procedure Append
     (List      : in out Attribute_Validator_List_Access;
      Validator : access Attribute_Validator_Record'Class;
      Override  : Boolean)
   is
      L : Attribute_Validator_List_Access;
   begin
      if List /= null then
         for A in List'Range loop
            if not List (A).Is_Group
              and then Is_Equal (List (A).Attr.all, Validator.all)
            then
               if Override then
                  --  ??? Should we free the previous value => We are sharing
                  --  the attribute definition through Restriction_Of...
                  List (A) :=
                    (Is_Group => False,
                     Attr     => Attribute_Validator (Validator));
               end if;
               return;
            end if;
         end loop;

         L := new Attribute_Validator_List'
           (List.all & Attribute_Or_Group'
              (Is_Group => False,
               Attr     => Attribute_Validator (Validator)));
         Unchecked_Free (List);
         List := L;
      else
         List := new Attribute_Validator_List'
           (1 => Attribute_Or_Group'
              (Is_Group => False, Attr => Attribute_Validator (Validator)));
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (List      : in out Attribute_Validator_List_Access;
      Group     : XML_Attribute_Group)
   is
      L : Attribute_Validator_List_Access;
   begin
      if List /= null then
         for A in List'Range loop
            if List (A).Is_Group and then List (A).Group = Group then
               return;
            end if;
         end loop;

         L := new Attribute_Validator_List'
           (List.all & Attribute_Or_Group'(Is_Group => True, Group => Group));
         Unchecked_Free (List);
         List := L;
      else
         List := new Attribute_Validator_List'
           (1 => Attribute_Or_Group'(Is_Group => True, Group => Group));
      end if;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (List    : in out Element_List_Access;
      Element : XML_Element)
   is
      L : Element_List_Access;
   begin
      if List /= null then
         L := new Element_List'(List.all & Element);
         Unchecked_Free (List);
         List := L;
      else
         List := new Element_List'(1 => Element);
      end if;
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Named_Attribute_Validator_Record) is
   begin
      Free (Validator.Local_Name);
      Free (Validator.Value);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Attribute_Validator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Validator_Record'Class, Attribute_Validator);
   begin
      Free (Validator.all);
      Unchecked_Free (Validator);
   end Free;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Validator : access XML_Validator_Record;
      Attribute : access Attribute_Validator_Record'Class) is
   begin
      Append (Validator.Attributes, Attribute, Override => True);
   end Add_Attribute;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Group : in out XML_Attribute_Group;
      Attr  : access Attribute_Validator_Record'Class) is
   begin
      Append (Group.Attributes, Attribute_Validator (Attr), Override => True);
   end Add_Attribute;

   -------------------------
   -- Add_Attribute_Group --
   -------------------------

   procedure Add_Attribute_Group
     (Validator : access XML_Validator_Record;
      Group     : XML_Attribute_Group) is
   begin
      Append (Validator.Attributes, Group);
   end Add_Attribute_Group;

   ----------------------------
   -- Create_Attribute_Group --
   ----------------------------

   function Create_Attribute_Group
     (Local_Name : Unicode.CES.Byte_Sequence) return XML_Attribute_Group is
   begin
      return new XML_Attribute_Group_Record'
        (Local_Name => new Unicode.CES.Byte_Sequence'(Local_Name),
         Attributes => null);
   end Create_Attribute_Group;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Validator : access XML_Validator_Record'Class) return String is
   begin
      if Validator.Debug_Name = null then
         return External_Tag (Validator'Tag);
      else
         return Validator.Debug_Name.all;
      end if;
   end Get_Name;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Any_Simple_XML_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Validator, Data, Namespace_URI, Grammar);
   begin
      Validation_Error
        ("Must be a simple type, no <" & Local_Name & "> child allowed");
      Element_Validator := No_Element;
   end Validate_Start_Element;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Debug_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Validator, Data, Namespace_URI, Grammar);
   begin
      Debug_Output (ASCII.ESC & "[36m"
                    & "****** Start_Element: DebugType validator for "
                    & Local_Name & ASCII.ESC & "[39m");
      Element_Validator := No_Element;
      Validation_Error ("Don't know how to validate " & Local_Name);
   end Validate_Start_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Debug_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Validator, Empty_Element);
   begin
      Debug_Output (ASCII.ESC & "[36m"
                    & "****** Charactes: DebugType validator for " & Ch
                    & ASCII.ESC & "[39m");
      Validation_Error ("Don't know how to validate characters " & Ch);
   end Validate_Characters;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access XML_Validator_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Validator, Data, Namespace_URI, Grammar);
   begin
      Validation_Error ("No definition found for """ & Local_Name & """");
      Element_Validator := No_Element;
   end Validate_Start_Element;

   -----------------------
   -- Set_Mixed_Content --
   -----------------------

   procedure Set_Mixed_Content
     (Validator : access XML_Validator_Record;
      Mixed     : Boolean) is
   begin
      Validator.Mixed_Content := Mixed;
   end Set_Mixed_Content;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (Validator         : access XML_Validator_Record;
      Atts              : Sax.Attributes.Attributes'Class;
      Id_Table          : in out Id_Htable_Access;
      Nillable          : Boolean;
      Is_Nil            : out Boolean)
   is
      Length : constant Natural := Get_Length (Atts);
      Seen   : array (0 .. Length - 1) of Boolean := (others => False);

      use Attributes_Htable;
      Visited : Attributes_Htable.HTable (101);

      function Find_Attribute
        (Named : Named_Attribute_Validator) return Integer;
      --  Chech whether Named appears in Atts

      procedure Check_Id (Named : Named_Attribute_Validator;
                          Index : Integer);
      --  Check whether the Index-th attribute in Atts, corresponding to Named,
      --  is an ID.

      procedure Recursive_Check (Validator : XML_Validator);
      procedure Recursive_Check (List : Attribute_Or_Group);
      --  Check recursively the attributes provided by Validator

      procedure Check_Named_Attribute (Named : Named_Attribute_Validator);
      procedure Check_Any_Attribute
        (Any : Any_Attribute_Validator; Index : Integer);
      --  Check a named attribute or a wildcard attribute

      ---------------------------
      -- Check_Named_Attribute --
      ---------------------------

      procedure Check_Named_Attribute (Named : Named_Attribute_Validator) is
         Found : Integer;
      begin
         if Get (Visited, Named.Local_Name.all) = null then
            Set (Visited, Named);
            Found := Find_Attribute (Named);

            if Found = -1 then
               case Named.Attribute_Use is
                  when Required =>
                     Validation_Error
                       ("Attribute """ & Named.Local_Name.all
                        & """ is required in this context");
                  when Prohibited | Optional | Default | Fixed =>
                     null;
               end case;

            else
               Seen (Found) := True;

               case Named.Attribute_Use is
                  when Prohibited =>
                     Validation_Error
                       ("Attribute """ & Named.Local_Name.all
                        & """ is prohibited in this context");

                  when Fixed =>
                     if Named.Value.all /=
                       Get_Value (Atts, Found)
                     then
                        Validation_Error
                          ("Attribute """ & Named.Local_Name.all
                           & """ must have the value """
                           & Named.Value.all & """");
                     end if;

                  when Optional | Required | Default =>
                     null;
               end case;

               Check_Id (Named, Found);

               begin
                  Validate_Attribute (Named.all, Atts, Found);
               exception
                  when E : XML_Validation_Error =>
                     Validation_Error
                       ("Attribute """ & Get_Qname (Atts, Found)
                        & """: " & Exception_Message (E));
               end;
            end if;
         end if;
      end Check_Named_Attribute;

      -------------------------
      -- Check_Any_Attribute --
      -------------------------

      procedure Check_Any_Attribute
        (Any : Any_Attribute_Validator; Index : Integer) is
      begin
         Validate_Attribute (Any, Atts, Index);
      exception
         when E : XML_Validation_Error =>
            Validation_Error
              ("Attribute """ & Get_Qname (Atts, Index)
               & """: " & Exception_Message (E));
      end Check_Any_Attribute;

      --------------------
      -- Find_Attribute --
      --------------------

      function Find_Attribute
        (Named : Named_Attribute_Validator) return Integer is
      begin
         for A in 0 .. Length - 1 loop
            Debug_Output ("Check attribute: "
                          & Named.NS.Namespace_URI.all
                          & ':' & Named.Local_Name.all);
            if not Seen (A)
              and then Get_Local_Name (Atts, A) = Named.Local_Name.all
              and then (Get_URI (Atts, A) = ""
                        or else Get_URI (Atts, A) = Named.NS.Namespace_URI.all)
            then
               return A;
            end if;
         end loop;
         return -1;
      end Find_Attribute;

      --------------
      -- Check_Id --
      --------------

      procedure Check_Id
        (Named : Named_Attribute_Validator; Index : Integer) is
      begin
         if Named.Is_Id then
            if Id_Table = null then
               Id_Table := new Id_Htable.HTable (101);
            else
               if Id_Htable.Get (Id_Table.all, Get_Value (Atts, Index)) /=
                 No_Id
               then
                  Validation_Error
                    ("ID """ & Get_Value (Atts, Index)
                     & """ already defined");
               end if;
            end if;

            Id_Htable.Set
              (Id_Table.all,
               Id_Ref'(Key => new Byte_Sequence'(Get_Value (Atts, Index))));
         end if;
      end Check_Id;

      ---------------------
      -- Recursive_Check --
      ---------------------

      procedure Recursive_Check (List : Attribute_Or_Group) is
      begin
         if List.Is_Group then
            if List.Group.Attributes /= null then
               for L in List.Group.Attributes'Range loop
                  Recursive_Check (List.Group.Attributes (L));
               end loop;
            end if;

         elsif List.Attr.all in Named_Attribute_Validator_Record'Class then
            Check_Named_Attribute (Named_Attribute_Validator (List.Attr));
         end if;
      end Recursive_Check;

      ---------------------
      -- Recursive_Check --
      ---------------------

      procedure Recursive_Check (Validator : XML_Validator) is
         List   : Attribute_Validator_List_Access;
         Dep1, Dep2 : XML_Validator;
      begin
         Get_Attribute_Lists (Validator, List, Dep1, Dep2);
         if List /= null then
            for L in List'Range loop
               Recursive_Check (List (L));
            end loop;

            for L in List'Range loop
               if not List (L).Is_Group
                 and then List (L).Attr.all in Any_Attribute_Validator'Class
               then
                  for A in 0 .. Length - 1 loop
                     if not Seen (A) then
                        Seen (A) := True;
                        Check_Any_Attribute
                          (Any_Attribute_Validator (List (L).Attr.all), A);
                     end if;
                  end loop;
               end if;
            end loop;
         end if;

         if Dep1 /= null then
            Recursive_Check (Dep1);
         end if;

         if Dep2 /= null then
            Recursive_Check (Dep2);
         end if;
      end Recursive_Check;

   begin
      Debug_Output ("Validate_Attributes " & Get_Name (Validator));

      Recursive_Check (XML_Validator (Validator));

      Is_Nil := False;

      for S in Seen'Range loop
         if not Seen (S) then
            if Nillable
              and then Get_URI (Atts, S) = XML_Instance_URI
              and then Get_Local_Name (Atts, S) = "nil"
            then
               Is_Nil := Get_Value_As_Boolean (Atts, S);

            elsif Get_URI (Atts, S) = XML_Instance_URI
              and then Get_Local_Name (Atts, S) = "type"
            then
               null;

            else
               Debug_Output ("Invalid attribute "
                             & Get_URI (Atts, S) & " -> "
                             & Get_Qname (Atts, S));

               Validation_Error
                 ("Attribute """ & Get_Qname (Atts, S) & """ invalid for this"
                  & " element");
            end if;
         end if;
      end loop;
   end Validate_Attributes;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access Any_Simple_XML_Validator_Record;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      null;
   end Validate_End_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator      : access XML_Validator_Record;
      Local_Name     : Unicode.CES.Byte_Sequence;
      Data           : Validator_Data)
   is
      pragma Unreferenced (Validator, Local_Name, Data);
   begin
      null;
   end Validate_End_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator      : access XML_Validator_Record;
      Ch             : Unicode.CES.Byte_Sequence;
      Empty_Element  : Boolean) is
   begin
      Debug_Output ("Validate_Character for unknown " & Get_Name (Validator)
                    & " --" & Ch & "--" & Boolean'Image (Empty_Element));
   end Validate_Characters;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access List_Validator_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      Start : Integer := Ch'First;
   begin
      Debug_Output ("Validate_Characters for list --" & Ch & "--"
                    & Get_Name (Validator));

      --  Ch has already been normalized by the SAX parser
      for C in Ch'Range loop
         if Ch (C) = ' ' then
            if C /= Ch'First then
               Validate_Characters
                 (Get_Validator (Validator.Base), Ch (Start .. C - 1),
                  Empty_Element);
            end if;
            Start := C + 1;
         end if;
      end loop;

      if Start <= Ch'Last then
         Validate_Characters
           (Get_Validator (Validator.Base), Ch (Start .. Ch'Last),
            Empty_Element);
      end if;
   end Validate_Characters;

   -------------
   -- List_Of --
   -------------

   function List_Of (Typ : XML_Type) return XML_Type is
      Tmp : XML_Validator;
   begin
      Tmp := new List_Validator_Record'
        (Any_Simple_XML_Validator_Record with Base => Typ);
      return Create_Type ("List of " & Typ.Local_Name.all, Tmp);
   end List_Of;

   ---------------
   -- Add_Union --
   ---------------

   procedure Add_Union
     (Validator : access XML_Union_Record;
      Part      : XML_Type) is
   begin
      Append
        (Validator.Unions, XML_Particle'
           (Typ        => Particle_XML_Type,
            Type_Descr => Part,
            Next       => null,
            Min_Occurs => 1,
            Max_Occurs => 1));
   end Add_Union;

   ------------------
   -- Create_Union --
   ------------------

   function Create_Union return XML_Union is
   begin
      return new XML_Union_Record;
   end Create_Union;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Union         : access XML_Union_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      Iter : Particle_Iterator;
      Valid : XML_Validator;
   begin
      Debug_Output ("Validate_Characters for union --" & Ch & "--"
                    & Get_Name (Union));

      if Union.Unions = null then
         if Empty_Element then
            return;
         else
            Validation_Error ("No content allowed for this union");
         end if;
      end if;

      Iter := Start (Union.Unions);
      while Get (Iter) /= null loop
         begin
            Valid := Get_Validator (Get (Iter).Type_Descr);
            if Valid /= null then
               Validate_Characters (Valid, Ch, Empty_Element);
            end if;

            --  No error ? => Everything is fine
            return;

         exception
            when XML_Validation_Error =>
               null;
         end;

         Next (Iter);
      end loop;

      Validation_Error ("Invalid value """ & Ch & """");
   end Validate_Characters;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Validator   : access XML_Validator_Record;
      Facet_Name  : Unicode.CES.Byte_Sequence;
      Facet_Value : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Validator);
   begin
      if Facet_Name = "whiteSpace" then
         if Facet_Value /= "collapse" then
            Validation_Error
              ("Invalid value for restriction whiteSpace: " & Facet_Value);
         end if;
      else
         Validation_Error ("Invalid restriction: " & Facet_Name);
      end if;
   end Add_Facet;

   ----------------------
   -- Create_Attribute --
   ----------------------

   function Create_Attribute
     (Local_Name     : Unicode.CES.Byte_Sequence;
      NS             : XML_Grammar_NS;
      Attribute_Type : XML_Type                  := No_Type;
      Attribute_Form : Form_Type                 := Qualified;
      Attribute_Use  : Attribute_Use_Type        := Optional;
      Value          : Unicode.CES.Byte_Sequence := "";
      Is_ID          : Boolean := False)
      return Attribute_Validator
   is
   begin
      return new Named_Attribute_Validator_Record'
        (Local_Name     => new Unicode.CES.Byte_Sequence'(Local_Name),
         NS             => NS,
         Attribute_Type => Attribute_Type,
         Attribute_Form => Attribute_Form,
         Attribute_Use  => Attribute_Use,
         Value          => new Unicode.CES.Byte_Sequence'(Value),
         Is_Id          => Is_ID);
   end Create_Attribute;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Validator : Named_Attribute_Validator_Record;
      Atts      : Sax.Attributes.Attributes'Class;
      Index     : Natural) is
   begin
      Debug_Output ("Checking attribute "
                    & Validator.Local_Name.all
                    & "=" & Get_Value (Atts, Index) & "--");

      if Validator.Attribute_Type /= No_Type then
         Validate_Characters (Get_Validator (Validator.Attribute_Type),
                              Get_Value (Atts, Index),
                              Empty_Element => False);
      end if;
   end Validate_Attribute;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out XML_Validator_Record) is
   begin
      Free (Validator.Debug_Name);
      Free (Validator.Attributes);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Typ : in out XML_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Type_Record, XML_Type);
   begin
      if Typ /= null then
         Free (Typ.Local_Name);
         Unchecked_Free (Typ);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out XML_Validator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Validator_Record'Class, XML_Validator);
   begin
      Free (Validator.all);
      Unchecked_Free (Validator);
   end Free;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Type
   is
      Result : XML_Type := Types_Htable.Get (Grammar.Types.all, Local_Name);
   begin
      if Result = No_Type then
         Result := Register_Forward (Grammar, Local_Name);
      end if;
      return Result;
   end Lookup;

   --------------------
   -- Lookup_Element --
   --------------------

   function Lookup_Element
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence;
      Create_If_Needed : Boolean := True) return XML_Element
   is
      Result : constant XML_Element_Access := Elements_Htable.Get
        (Grammar.Elements.all, Local_Name);
   begin
      if Result = null then
         if Create_If_Needed then
            Debug_Output ("Lookup_Element: creating forward "
                          & Grammar.Namespace_URI.all & " : "
                          & Local_Name);
            return (Elem   => Register_Forward (Grammar, Local_Name).Elem,
                    Is_Ref => True);
         else
            return No_Element;
         end if;
      end if;
      return (Elem => Result, Is_Ref => True);
   end Lookup_Element;

   ------------------
   -- Lookup_Group --
   ------------------

   function Lookup_Group
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Group
   is
      Result : XML_Group := Groups_Htable.Get (Grammar.Groups.all, Local_Name);
   begin
      if Result = No_XML_Group then
         Result := Register_Forward (Grammar, Local_Name);
      end if;
      return Result;
   end Lookup_Group;

   ----------------------------
   -- Lookup_Attribute_Group --
   ----------------------------

   function Lookup_Attribute_Group
     (Grammar       : XML_Grammar_NS;
      Local_Name    : Unicode.CES.Byte_Sequence) return XML_Attribute_Group
   is
      Result : constant XML_Attribute_Group :=
        Attribute_Groups_Htable.Get (Grammar.Attribute_Groups.all, Local_Name);
      Group : XML_Attribute_Group;
   begin
      if Result = Empty_Attribute_Group then
         Group := Create_Attribute_Group (Local_Name);
         Register (Grammar, Group);
         return Group;
      end if;

      return Result;
   end Lookup_Attribute_Group;

   ----------------------
   -- Lookup_Attribute --
   ----------------------

   function Lookup_Attribute
     (Grammar       : XML_Grammar_NS;
      Local_Name    : Unicode.CES.Byte_Sequence) return Attribute_Validator
   is
      Result : constant Named_Attribute_Validator :=
        Attributes_Htable.Get (Grammar.Attributes.all, Local_Name);
   begin
      if Result = null then
         return Register_Forward (Grammar, Local_Name);
      end if;
      return Attribute_Validator (Result);
   end Lookup_Attribute;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name
     (Element : XML_Element) return Unicode.CES.Byte_Sequence is
   begin
      return Element.Elem.Local_Name.all;
   end Get_Local_Name;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name (Typ : XML_Type) return Unicode.CES.Byte_Sequence is
   begin
      if Typ = null or else Typ.Local_Name = null then
         return "No_Type";
      else
         return Typ.Local_Name.all;
      end if;
   end Get_Local_Name;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Element : XML_Element) return XML_Type is
   begin
      if Element.Elem = null then
         return null;
      else
         return Element.Elem.Of_Type;
      end if;
   end Get_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Attr      : access Attribute_Validator_Record;
      Attr_Type : XML_Type)
   is
      pragma Unreferenced (Attr, Attr_Type);
   begin
      null;
   end Set_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (Attr      : access Named_Attribute_Validator_Record;
      Attr_Type : XML_Type) is
   begin
      Attr.Attribute_Type := Attr_Type;
   end Set_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type (Element : XML_Element; Element_Type : XML_Type) is
   begin
      if Element /= No_Element then
         if Element.Is_Ref then
            Validation_Error
              ("Cannot mix complexType definition and ""ref"" attribute");
         end if;

         Element.Elem.Of_Type := Element_Type;

         --  3.3 Element Declaration details:  Validation Rule 3.1
         --  The "default" attribute of element must match the validation rule
         --  for that element

         if Element.Elem.Default /= null then
            Validate_Characters
              (Get_Validator (Element_Type), Element.Elem.Default.all,
               Empty_Element => False);
         end if;

         --  3.3 Element Declaration details:  Validation Rule 3.1
         --  The "fixed" attribute of element must match the validation rule
         --  for that element

         if Element.Elem.Fixed /= null then
            Validate_Characters
              (Get_Validator (Element_Type), Element.Elem.Fixed.all,
               Empty_Element => False);
         end if;
      end if;
   end Set_Type;

   ------------
   -- Get_NS --
   ------------

   procedure Get_NS
     (Grammar       : in out XML_Grammar;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Result        : out XML_Grammar_NS)
   is
   begin
      if Grammar.Grammars /= null then
         for G in Grammar.Grammars'Range loop
            if Grammar.Grammars (G).Namespace_URI.all = Namespace_URI then
               Result := Grammar.Grammars (G);
               return;
            end if;
         end loop;
      end if;

      Create_NS_Grammar (Grammar, Namespace_URI);
      Result := Grammar.Grammars (Grammar.Grammars'Last);
   end Get_NS;

   -----------------------
   -- Create_NS_Grammar --
   -----------------------

   procedure Create_NS_Grammar
     (Grammar       : in out XML_Grammar;
      Namespace_URI : Unicode.CES.Byte_Sequence)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Grammar_NS_Array, Grammar_NS_Array_Access);
      Tmp : Grammar_NS_Array_Access;
   begin
      if Grammar.Grammars = null then
         Grammar.Grammars := new Grammar_NS_Array (1 .. 1);
      else
         Tmp := Grammar.Grammars;
         Grammar.Grammars := new Grammar_NS_Array (1 .. Tmp'Length + 1);
         Grammar.Grammars (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      Debug_Output ("Create_NS_Grammar: " & Namespace_URI);

      Grammar.Grammars (Grammar.Grammars'Last) := new XML_Grammar_NS_Record'
        (Namespace_URI    => new Byte_Sequence'(Namespace_URI),
         Types            => new Types_Htable.HTable (101),
         Elements         => new Elements_Htable.HTable (101),
         Groups           => new Groups_Htable.HTable (101),
         Attributes       => new Attributes_Htable.HTable (101),
         Attribute_Groups => new Attribute_Groups_Htable.HTable (101),
         Element_Form_Default => Unqualified);
   end Create_NS_Grammar;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Grammar : in out XML_Grammar) is
      Tmp      : Common_Simple_Validator;
      Tmp2     : XML_Validator;
      G, XML_G, XML_IG : XML_Grammar_NS;
      Created  : XML_Type;

   begin
      Get_NS (Grammar, XML_URI,          Result => XML_G);
      Get_NS (Grammar, XML_Schema_URI,   Result => G);
      Get_NS (Grammar, XML_Instance_URI, Result => XML_IG);

      Created := Get_Type (Get_UR_Type_Element (Process_Skip));
      Register (G, Created);

      Tmp2 := new XML_Validator_Record;
      Created := Create_Type ("anyType", Tmp2);
      Register (G, Created);

      Tmp2 := new Any_Simple_XML_Validator_Record;
      Created := Create_Type ("anySimpleType", Tmp2);
      Register (G, Created);

      Tmp := new Boolean_Validator_Record;
      Created := Create_Type ("boolean", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Pattern) := True;
      Tmp.Facets.Pattern_String := new Byte_Sequence'("\d\d\d\d-\d\d-\d\d");
      Created := Create_Type ("date", Tmp);
      Register (G, Created);


      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Created := Create_Type ("string", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_QName'Access;
      Created := Create_Type ("QName", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Replace;
      Created := Create_Type ("normalizeString", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Created := Create_Type ("token", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_Language_Name'Access;
      Created := Create_Type ("language", Tmp);
      Register (G, Created);
      Register (Create_Attribute ("lang", XML_G, Lookup (G, "language")));

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_Nmtoken'Access;
      Created := Create_Type ("NMTOKEN", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_Name'Access;
      Created := Create_Type ("Name", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_NCname'Access;
      Created := Create_Type ("NCName", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_NCname'Access;
      Created := Create_Type ("ID", Tmp);
      Register (G, Created);
      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_NCname'Access;
      Created := Create_Type ("IDREF", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := String_Facets;
      Tmp.Facets.Mask (Facet_Whitespace) := True;
      Tmp.Facets.Whitespace := Collapse;
      Tmp.Facets.Mask (Facet_Implicit_Enumeration) := True;
      Tmp.Facets.Implicit_Enumeration := Is_Valid_NCname'Access;
      Created := Create_Type ("ENTITY", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      --  ??? Hack to make it understood as an integer type, not a string
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := Natural'Last;
      Created := Create_Type ("decimal", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Created := Create_Type ("integer", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := 0;
      Created := Create_Type ("nonPositiveInteger", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := -1;
      Created := Create_Type ("negativeInteger", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := +9_223_372_036_854_775_807;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := -9_223_372_036_854_775_808;
      Created := Create_Type ("long", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := +2_147_483_647;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := -2_147_483_648;
      Created := Create_Type ("int", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := +32_767;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := -32_768;
      Created := Create_Type ("short", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := +127;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := -128;
      Created := Create_Type ("byte", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := 0;
      Created := Create_Type ("nonNegativeInteger", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := 1;
      Created := Create_Type ("positiveInteger", Tmp);
      Register (G, Created);

--        Tmp := new Common_Simple_XML_Validator;
--        Tmp.Facets.Settable := Integer_Facets;
--        Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
--        Tmp.Facets.Fraction_Digits := 0;
--        Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
--        Tmp.Facets.Max_Inclusive := +18_446_744_073_709_551_615;
--        Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
--        Tmp.Facets.Min_Inclusive := 0;
--        Register (G, Create_Type ("unsignedLong", Tmp));

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := +4_294_967_295;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := 0;
      Created := Create_Type ("unsignedInt", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := +65_535;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := 0;
      Created := Create_Type ("unsignedShort", Tmp);
      Register (G, Created);

      Tmp := new Common_Simple_XML_Validator;
      Tmp.Facets.Settable := Integer_Facets;
      Tmp.Facets.Mask (Facet_Fraction_Digits) := True;
      Tmp.Facets.Fraction_Digits := 0;
      Tmp.Facets.Mask (Facet_Max_Inclusive) := True;
      Tmp.Facets.Max_Inclusive := +255;
      Tmp.Facets.Mask (Facet_Min_Inclusive) := True;
      Tmp.Facets.Min_Inclusive := 0;
      Created := Create_Type ("unsignedByte", Tmp);
      Register (G, Created);
   end Initialize;

   --------------------
   -- Create_Element --
   --------------------

   function Create_Element
     (Local_Name : Unicode.CES.Byte_Sequence;
      Of_Type    : XML_Type;
      Form       : Form_Type) return XML_Element
   is
      Result : XML_Element;
   begin
      Result :=
        (Elem => new XML_Element_Record'
           (Local_Name          => new Unicode.CES.Byte_Sequence'(Local_Name),
            Substitution_Groups => null,
            Of_Type             => Of_Type,
            Default             => null,
            Is_Abstract         => False,
            Nillable            => False,
            Final_Restriction   => False,
            Final_Extension     => False,
            Block_Restriction   => False,
            Block_Extension     => False,
            Is_Global           => False,
            Form                => Form,
            Fixed               => null),
         Is_Ref => False);
      return Result;
   end Create_Element;

   -------------------
   -- Redefine_Type --
   -------------------

   function Redefine_Type
     (Grammar : XML_Grammar_NS; Local_Name : Byte_Sequence) return XML_Type
   is
      Old : constant XML_Type := Types_Htable.Get
        (Grammar.Types.all, Local_Name);
      Result : XML_Type;
   begin
      if Old /= No_Type then
         Result := Create_Type
           ("@redefine_" & Local_Name, Get_Validator (Old));
         Old.Validator := new Debug_Validator_Record;
         return Result;
      end if;
      return No_Type;
   end Redefine_Type;

   --------------------
   -- Redefine_Group --
   --------------------

   function Redefine_Group
     (Grammar : XML_Grammar_NS; Local_Name : Byte_Sequence) return XML_Group
   is
      Old : constant XML_Group := Groups_Htable.Get
        (Grammar.Groups.all, Local_Name);
      Result : XML_Group;
   begin
      if Old /= No_XML_Group then
         Result := Create_Group ("@redefine_" & Local_Name);
         Result.all := Old.all;
         Old.all := (Local_Name => Old.Local_Name,
                     Particles  => Empty_Particle_List);
         return Result;
      end if;
      return No_XML_Group;
   end Redefine_Group;

   --------------
   -- Register --
   --------------

   procedure Register
     (Grammar : XML_Grammar_NS; Element : in out XML_Element)
   is
      Old : constant XML_Element_Access := Elements_Htable.Get
        (Grammar.Elements.all, Element.Elem.Local_Name.all);
   begin
      if Old /= null then
         if Get_Validator (Old.Of_Type) = null
           or else Get_Validator (Old.Of_Type).all not in
             Debug_Validator_Record'Class
         then
            Validation_Error
              ("Element """ & Element.Elem.Local_Name.all
               & """ has already been declared");
         end if;

         Debug_Output ("Overriding forward decl for element "
                       & Element.Elem.Local_Name.all);

         Free (Old.all);
         Old.all := Element.Elem.all;
         Element := (Elem => Old, Is_Ref => False);
      else
         Elements_Htable.Set (Grammar.Elements.all, Element.Elem);
      end if;

      Element.Elem.Is_Global := True;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Grammar : XML_Grammar_NS; Typ : in out XML_Type) is
      Old : constant XML_Type := Types_Htable.Get
        (Grammar.Types.all, Typ.Local_Name.all);
   begin
      if Old /= No_Type then
         if Get_Validator (Old).all not in Debug_Validator_Record'Class then
            Validation_Error
              ("Type has already been declared: "
               & Typ.Local_Name.all);
         end if;

         Debug_Output ("Overriding forward type " & Typ.Local_Name.all);

         Old.Validator := Typ.Validator;
      else
         Types_Htable.Set (Grammar.Types.all, Typ);

         if Debug then
            if Typ.Validator /= null
              and then Typ.Validator.Debug_Name = null
            then
               Typ.Validator.Debug_Name := new String'(Typ.Local_Name.all);
            end if;
         end if;
      end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Attr : Attribute_Validator) is
      A   : Named_Attribute_Validator;
      Old : Named_Attribute_Validator;
   begin
      if Attr.all in Named_Attribute_Validator_Record'Class then
         A := Named_Attribute_Validator (Attr);

         Old := Attributes_Htable.Get
           (A.NS.Attributes.all, A.Local_Name.all);

         if Old /= null then
            if Get_Validator (Old.Attribute_Type) = null
              or else Get_Validator (Old.Attribute_Type).all not in
                 Debug_Validator_Record'Class
            then
               Validation_Error
                 ("Attribute has already been declared: "
                  & A.Local_Name.all);
            end if;

            Old.all := A.all;
         else
            Attributes_Htable.Set (Attr.NS.Attributes.all, A);
         end if;
      end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Grammar : XML_Grammar_NS; Group : in out XML_Group) is
      Old : constant XML_Group :=
        Groups_Htable.Get (Grammar.Groups.all, Group.Local_Name.all);
   begin
      if Old /= No_XML_Group then
         if Old.Particles.First /= null then
            Validation_Error
              ("Group has already been declared: " & Group.Local_Name.all);
         end if;

         Old.Particles := Group.Particles;

         --  ??? Free Group
         Group := Old;
      else
         Groups_Htable.Set (Grammar.Groups.all, Group);
      end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Grammar : XML_Grammar_NS; Group : in out XML_Attribute_Group)
   is
      Old : constant XML_Attribute_Group := Attribute_Groups_Htable.Get
        (Grammar.Attribute_Groups.all, Group.Local_Name.all);
   begin
      if Old /= Empty_Attribute_Group then
         if Old.Attributes /= null then
            Validation_Error
              ("Attribute group has already been declared: "
               & Group.Local_Name.all);
         end if;
         Old.all := Group.all;
         Unchecked_Free (Group);
         Group := Old;
      else
         Attribute_Groups_Htable.Set (Grammar.Attribute_Groups.all, Group);
      end if;
   end Register;

   ----------------------
   -- Register_Forward --
   ----------------------

   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Type
   is
      Invalid : XML_Type :=
        Types_Htable.Get (Grammar.Types.all, "@@invalid@@");
      Typ : XML_Type;
      Tmp : XML_Validator;
   begin
      if Debug then
         --  Always create a new one for debugging
         Tmp := new Debug_Validator_Record;
         Set_Debug_Name (Tmp, "Forward for type " & Local_Name);
         Invalid := Create_Type ("@@invalid@@", Tmp);

      else
         if Invalid = No_Type then
            Tmp := new Debug_Validator_Record;
            Invalid := Create_Type ("@@invalid@@", Tmp);
            Register (Grammar, Invalid);
         end if;
      end if;

      Debug_Output ("Forward type decl: " & Local_Name);

      Typ := Create_Type (Local_Name, Get_Validator (Invalid));
      Types_Htable.Set (Grammar.Types.all, Typ);
      return Typ;
   end Register_Forward;

   ----------------------
   -- Register_Forward --
   ----------------------

   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Element
   is
      Invalid : XML_Type := Lookup (Grammar, "@@invalid@@");
      Elem    : XML_Element;
      Tmp     : XML_Validator;
   begin
      if Debug then
         --  Always create a new one
         Tmp := new Debug_Validator_Record;
         Set_Debug_Name (Tmp, "Forward for element " & Local_Name);
         Invalid := Create_Type ("@@invalid@@", Tmp);

      else
         if Invalid = No_Type then
            Tmp := new Debug_Validator_Record;
            Invalid := Create_Type ("@@invalid@@", Tmp);
            Register (Grammar, Invalid);
         end if;
      end if;

      Debug_Output ("Forward element decl: " & Local_Name);

      Elem := Create_Element (Local_Name, Invalid, Qualified);
      Register (Grammar, Elem);
      return Elem;
   end Register_Forward;

   ----------------------
   -- Register_Forward --
   ----------------------

   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return Attribute_Validator
   is
      Invalid : XML_Type := Lookup (Grammar, "@@invalid@@");
      Attr    : Attribute_Validator;
      Tmp     : XML_Validator;
   begin
      if Debug then
         --  Always create a new one
         Tmp := new Debug_Validator_Record;
         Set_Debug_Name (Tmp, "Forward for attribute " & Local_Name);
         Invalid := Create_Type ("@@invalid@@", Tmp);

      else
         if Invalid = No_Type then
            Tmp := new Debug_Validator_Record;
            Invalid := Create_Type ("@@invalid@@", Tmp);
            Register (Grammar, Invalid);
         end if;
      end if;

      Debug_Output ("Forward attribute decl: " & Local_Name);

      Attr := Create_Attribute (Local_Name, Grammar, Invalid);
      Register (Attr);
      return Attr;
   end Register_Forward;

   ----------------------
   -- Register_Forward --
   ----------------------

   function Register_Forward
     (Grammar    : XML_Grammar_NS;
      Local_Name : Unicode.CES.Byte_Sequence) return XML_Group
   is
      Gr : XML_Group := Create_Group (Local_Name);
   begin
      Debug_Output ("Forward group decl: " & Local_Name);

      Register (Grammar, Gr);
      return Gr;
   end Register_Forward;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Grammar_NS_Array, Grammar_NS_Array_Access);
   begin
      if Grammar.Grammars /= null then
         for G in Grammar.Grammars'Range loop
            Free (Grammar.Grammars (G));
         end loop;
         Unchecked_Free (Grammar.Grammars);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar_NS) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Types_Htable.HTable, Types_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Elements_Htable.HTable, Elements_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Groups_Htable.HTable, Groups_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Grammar_NS_Record, XML_Grammar_NS);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attributes_Htable.HTable, Attributes_Htable_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Attribute_Groups_Htable.HTable, Attribute_Groups_Htable_Access);

      use Attributes_Htable;
      Att_Iter : Attributes_Htable.Iterator;
      Att      : Named_Attribute_Validator;
   begin
      if Grammar /= null then
         Elements_Htable.Reset (Grammar.Elements.all);
         Unchecked_Free (Grammar.Elements);
         Types_Htable.Reset (Grammar.Types.all);
         Unchecked_Free (Grammar.Types);
         Groups_Htable.Reset (Grammar.Groups.all);
         Unchecked_Free (Grammar.Groups);
         Attribute_Groups_Htable.Reset (Grammar.Attribute_Groups.all);
         Unchecked_Free (Grammar.Attribute_Groups);

         --  We need to explicitly reset this table, since the Free subprogram
         --  does nothing here

         Att_Iter := Attributes_Htable.First (Grammar.Attributes.all);
         while Att_Iter /= Attributes_Htable.No_Iterator loop
            Att := Attributes_Htable.Current (Att_Iter);
            Attributes_Htable.Next (Grammar.Attributes.all, Att_Iter);
            Free (Attribute_Validator (Att));
         end loop;
         Attributes_Htable.Reset (Grammar.Attributes.all);
         Unchecked_Free (Grammar.Attributes);

         Free (Grammar.Namespace_URI);
         Unchecked_Free (Grammar);
      end if;
   end Free;

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Att : in out Named_Attribute_Validator) is
      pragma Unreferenced (Att);
   begin
      null;
   end Do_Nothing;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Element : XML_Element; Default : Unicode.CES.Byte_Sequence) is
   begin
      --  3.3 Element Declaration details: Can't have both
      --  "default" and "fixed"

      if Element.Elem.Fixed /= null then
         Validation_Error
           ("Attributes ""fixed"" and ""default"" conflict with each other");
      end if;

      --  3.3 Element Declaration details:  Validation Rule 3.1
      --  The "default" attribute of element must match the validation rule
      --  for that element

      if Element.Elem.Of_Type /= No_Type then
         Validate_Characters (Get_Validator (Element.Elem.Of_Type), Default,
                              Empty_Element => False);
      end if;

      Free (Element.Elem.Default);
      Element.Elem.Default := new Byte_Sequence'(Default);
   end Set_Default;

   -----------------
   -- Has_Default --
   -----------------

   function Has_Default (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Default /= null;
   end Has_Default;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default
     (Element : XML_Element) return Byte_Sequence_Access is
   begin
      return Element.Elem.Default;
   end Get_Default;

   ---------------
   -- Set_Fixed --
   ---------------

   procedure Set_Fixed
     (Element : XML_Element; Fixed : Unicode.CES.Byte_Sequence) is
   begin
      --  3.3 Element Declaration details: Can't have both
      --  "default" and "fixed"

      if Element.Elem.Default /= null then
         Validation_Error
           ("Attributes ""fixed"" and ""default"" conflict with each other");
      end if;

      --  3.3 Element Declaration details:  Validation Rule 3.1
      --  The "fixed" attribute of element must match the validation rule
      --  for that element

      if Element.Elem.Of_Type /= No_Type then
         Validate_Characters (Get_Validator (Element.Elem.Of_Type), Fixed,
                              Empty_Element => False);
      end if;


      Free (Element.Elem.Fixed);
      Element.Elem.Fixed := new Byte_Sequence'(Fixed);
   end Set_Fixed;

   ---------------
   -- Has_Fixed --
   ---------------

   function Has_Fixed (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Fixed /= null;
   end Has_Fixed;

   ---------------
   -- Get_Fixed --
   ---------------

   function Get_Fixed
     (Element : XML_Element) return Byte_Sequence_Access is
   begin
      return Element.Elem.Fixed;
   end Get_Fixed;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out XML_Element_Record) is
   begin
      --  ??? Should free Of_Type only if it isn't a named type
      Free (Element.Local_Name);
      Free (Element.Default);
      Free (Element.Fixed);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Element : in out XML_Element_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Element_Record, XML_Element_Access);
   begin
      Free (Element.all);
      Unchecked_Free (Element);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Group : in out XML_Group) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XML_Group_Record, XML_Group);
   begin
      Free (Group.Local_Name);
      Unchecked_Free (Group);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Att : in out XML_Attribute_Group) is
   begin
      Free (Att.Local_Name);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Att : XML_Attribute_Group) return Unicode.CES.Byte_Sequence is
   begin
      return Att.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Element : XML_Element_Access) return Unicode.CES.Byte_Sequence is
   begin
      return Element.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Typ : XML_Type) return Unicode.CES.Byte_Sequence is
   begin
      return Typ.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Group : XML_Group) return Unicode.CES.Byte_Sequence is
   begin
      return Group.Local_Name.all;
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (Att : Named_Attribute_Validator) return Unicode.CES.Byte_Sequence is
   begin
      return Att.Local_Name.all;
   end Get_Key;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Sequence_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return new Sequence_Data'
        (Group_Model_Data_Record with
         Current               => No_Iter,
         Num_Occurs_Of_Current => 0);
   end Create_Validator_Data;

   -------------------------------
   -- Check_Substitution_Groups --
   -------------------------------

   function Check_Substitution_Groups
     (Element       : XML_Element;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence) return XML_Element
   is
      Groups : constant Element_List_Access :=
        Element.Elem.Substitution_Groups;
      Result : XML_Element := No_Element;
   begin
      Debug_Output ("++ Testing element xsd=" & Element.Elem.Local_Name.all
                    & " xml=" & Local_Name);

      if Element.Elem.Local_Name.all = Local_Name then
         Result := Element;

      elsif Groups /= null then
         for S in Groups'Range loop
            Debug_Output ("Check_Substitution group: "
                          & Element.Elem.Local_Name.all & " -> "
                          & Groups (S).Elem.Local_Name.all);

            Result := Check_Substitution_Groups
              (Groups (S), Local_Name, Namespace_URI);
            exit when Result /= No_Element;
         end loop;
      end if;

      if Result /= No_Element and then Is_Abstract (Result) then
         Validation_Error
           ("""" & Result.Elem.Local_Name.all & """ is abstract");
         Result := No_Element;
      end if;

      return Result;
   end Check_Substitution_Groups;

   -------------------------
   -- Initialize_Sequence --
   -------------------------

   procedure Initialize_Sequence
     (Seq  : access Sequence_Record'Class;
      Data : Sequence_Data_Access) is
   begin
      Data.Current    := Start (Seq.Particles);

      Data.Num_Occurs := Data.Num_Occurs + 1;

      if Get (Data.Current) = null then
         Validation_Error ("No child authorized for this sequence");
      end if;

      if Seq.Max_Occurs /= Unbounded
        and then Data.Num_Occurs > Seq.Max_Occurs
      then
         Debug_Output ("Too many occurrences of " & Get_Name (Seq));

         Validation_Error
           ("Too many occurrences of sequence. Expecting at most"
            & Integer'Image (Seq.Max_Occurs));
      end if;
   end Initialize_Sequence;

   ---------------------------
   -- Move_To_Next_Particle --
   ---------------------------

   function Move_To_Next_Particle
     (Seq   : access Sequence_Record'Class;
      Data  : Sequence_Data_Access;
      Force : Boolean := False) return Boolean
   is
      Curr : XML_Particle_Access;
   begin
      Data.Num_Occurs_Of_Current := Data.Num_Occurs_Of_Current + 1;

      Curr := Get (Data.Current);
      if Force
        or else
          (Get_Max_Occurs (Data.Current) /= Unbounded
           and then Data.Num_Occurs_Of_Current >=
             Get_Max_Occurs (Data.Current))
      then
         Debug_Output ("Goto next particle in " & Get_Name (Seq)
                       & " Occurs of Current="
                       & Data.Num_Occurs_Of_Current'Img);

         Next (Data.Current);
         Data.Num_Occurs_Of_Current := 0;

         --  No more element possible in this sequence ?
         return not
           (Get (Data.Current) = null
            and then Data.Parent /= null
            and then Data.Num_Occurs >= Seq.Min_Occurs
            and then (Seq.Max_Occurs /= Unbounded
                      and then Data.Num_Occurs <= Seq.Max_Occurs));
      end if;

      return True;
   end Move_To_Next_Particle;

   ------------------
   -- Check_Nested --
   ------------------

   procedure Check_Nested
     (Parent            : access Group_Model_Record'Class;
      Nested            : access Group_Model_Record'Class;
      Data              : access Group_Model_Data_Record'Class;
      Local_Name        : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element;
      Skip_Current      : out Boolean)
   is
      Applies : Boolean;
   begin
      Debug_Output ("++ Testing nested " & Get_Name (Nested));

      Applies_To_Tag
        (Nested, Local_Name, Namespace_URI, Grammar, Applies, Skip_Current);

      if Applies then
         Data.Nested      := Group_Model (Nested);
         Data.Nested_Data := Create_Validator_Data (Nested);
         Group_Model_Data (Data.Nested_Data).Parent := Group_Model (Parent);

         Validate_Start_Element
           (Data.Nested, Local_Name, Namespace_URI,
            Data.Nested_Data, Grammar, Element_Validator);

         if Element_Validator /= No_Element then
            Debug_Output
              ("++ nested Matched " & Get_Name (Data.Nested) & ' '
               & Get_Local_Name (Element_Validator.Elem.Of_Type));
         else
            Validate_End_Element (Nested, Local_Name, Data.Nested_Data);

            Free_Nested_Group (Group_Model_Data (Data));
         end if;
      else
         Element_Validator := No_Element;
      end if;
   end Check_Nested;

   ----------------
   -- Run_Nested --
   ----------------

   procedure Run_Nested
     (Validator         : access Group_Model_Record'Class;
      Data              : access Group_Model_Data_Record'Class;
      Local_Name        : Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element) is
   begin
      Debug_Output ("++ Executing nested: " & Get_Name (Validator)
                    & " -> " & Get_Name (Data.Nested));

      Validate_Start_Element
        (Data.Nested, Local_Name, Namespace_URI,
         Data.Nested_Data, Grammar, Element_Validator);

      if Element_Validator = No_Element then
         Free_Nested_Group (Group_Model_Data (Data));
      end if;
   end Run_Nested;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      D         : constant Sequence_Data_Access := Sequence_Data_Access (Data);
      Start_Elem : XML_Particle_Access;
      First_Iter : Boolean := True;
      Curr       : XML_Particle_Access;
      Tmp        : Boolean;
      pragma Unreferenced (Tmp);
      Skip_Current : Boolean;

   begin
      if D.Nested /= null then
         Run_Nested
           (Validator, D, Local_Name, Namespace_URI, Grammar,
            Element_Validator);
         if Element_Validator /= No_Element
           or else not Move_To_Next_Particle (Validator, D)
         then
            return;
         end if;
      end if;

      Start_Elem := Get (D.Current);

      Debug_Output ("++ Start sequence " & Get_Name (Validator)
                    & " occurs=(" & Validator.Min_Occurs'Img
                    & " <=" & D.Num_Occurs'Img & " <="
                    & Validator.Max_Occurs'Img & ')');

      while First_Iter or else Get (D.Current) /= Start_Elem loop
         First_Iter := False;

         if Get (D.Current) = null then
            Initialize_Sequence (Validator, D);
         end if;

         Curr := Get (D.Current);

         case Curr.Typ is
            when Particle_Element =>
               Element_Validator := Check_Substitution_Groups
                 (Curr.Element, Local_Name, Namespace_URI);

               if Element_Validator /= No_Element then
                  Debug_Output
                    ("++ element Matched: "
                     & Element_Validator.Elem.Local_Name.all & ' '
                     & Get_Local_Name (Element_Validator.Elem.Of_Type));

                  Tmp := Move_To_Next_Particle (Validator, D, Force => False);
                  Check_Qualification (Element_Validator, Namespace_URI);

               elsif D.Num_Occurs_Of_Current < Get_Min_Occurs (D.Current) then
                  Validation_Error
                    ("Expecting at least"
                     & Integer'Image (Get_Min_Occurs (D.Current))
                     & " occurrences of current particle");
                  return;
               end if;

            when Particle_Any =>
               Validate_Start_Element
                 (Curr.Any, Local_Name, Namespace_URI,
                  null, Grammar, Element_Validator);
               if Element_Validator /= No_Element then
                  Tmp := Move_To_Next_Particle (Validator, D, Force => False);
               end if;

            when Particle_Nested =>
               Check_Nested
                 (Validator, Curr.Validator, D, Local_Name,
                  Namespace_URI, Grammar, Element_Validator, Skip_Current);

               if Element_Validator = No_Element
                 and then not Skip_Current
               then
                  Validation_Error
                    ("Expecting at least 1 occurrence of a nested Schema "
                     & " sequence");
               end if;

            when Particle_Group | Particle_XML_Type =>
               --  Not possible, since the iterator doesn't return those
               raise Program_Error;
         end case;

         if Element_Validator /= No_Element then
            return;
         end if;

         --  The current element was in fact optional at this point

         Debug_Output ("Skip optional particle in " & Get_Name (Validator));

         if not Move_To_Next_Particle (Validator, D, Force => True) then
            Element_Validator := No_Element;
            return;
         end if;
      end loop;
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator         : access Sequence_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data)
   is
      pragma Unreferenced (Local_Name);
      D   : constant Sequence_Data_Access := Sequence_Data_Access (Data);
   begin
      --  If the only remaining elements are optional, ignore them

      if Get (D.Current) /= null then
         if D.Num_Occurs_Of_Current >= Get_Min_Occurs (D.Current) then
            loop
               Next (D.Current);
               exit when Get (D.Current) = null
                 or else Get_Min_Occurs (D.Current) /= 0;
            end loop;
         else
            Validation_Error
              ("Element must be repeated at least"
               & Integer'Image (Get_Min_Occurs (D.Current)) & " times");
         end if;
      end if;

      if D.Num_Occurs < Validator.Min_Occurs then
         Validation_Error
           ("Not enough occurrences of sequence, expecting at least"
            & Integer'Image (Validator.Min_Occurs)
            & ", got" & Integer'Image (D.Num_Occurs));
      end if;

      if Get (D.Current) /= null then
         Validation_Error
           ("Unexpected end of sequence " & Get_Name (Validator));
      end if;
   end Validate_End_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access Group_Model_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Ch);
   begin
      if not Empty_Element then
         if Validator.Mixed_Content then
            null;
         else
            Validation_Error
              ("No character data allowed by content model");
         end if;
      end if;
   end Validate_Characters;

   ------------
   -- Append --
   ------------

   procedure Append
     (List       : in out Particle_List;
      Item       : XML_Particle) is
   begin
      pragma Assert (List /= null, "List was never created");

      if Item.Max_Occurs /= Unbounded
        and then Item.Min_Occurs > Item.Max_Occurs
      then
         Validation_Error
           ("minOccurs > maxOccurs when creating particle");
      end if;

      if List.First = null then
         List.First := new XML_Particle'(Item);
         List.First.Next := null;
         List.Last  := List.First;
      else
         List.Last.Next := new XML_Particle'(Item);
         List.Last := List.Last.Next;
      end if;
   end Append;

   ---------------------
   -- Create_Sequence --
   ---------------------

   function Create_Sequence
     (Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return Sequence
   is
      Seq : constant Sequence := new Sequence_Record;
   begin
      if Max_Occurs /= Unbounded and then Min_Occurs > Max_Occurs then
         Validation_Error
           ("minOccurs > maxOccurs when creating sequence");
      end if;

      Seq.Max_Occurs := Max_Occurs;
      Seq.Min_Occurs := Min_Occurs;
      return Seq;
   end Create_Sequence;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Element;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      if Item.Elem.Local_Name = null then
         Raise_Exception
           (Program_Error'Identity,
            "Adding empty element to a sequence");
      end if;

      Append
        (Seq.Particles, XML_Particle'
           (Typ        => Particle_Element,
            Element    => Item,
            Next       => null,
            Min_Occurs => Min_Occurs,
            Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Item       : Sequence) is
   begin
      Append (Seq.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Item.Min_Occurs,
                 Max_Occurs => Item.Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq        : access Sequence_Record;
      Item       : Choice) is
   begin
      Append (Seq.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Item.Min_Occurs,
                 Max_Occurs => Item.Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, XML_Particle'
                (Typ        => Particle_Group,
                 Group      => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      D     : constant Choice_Data_Access := Choice_Data_Access (Data);
      Item  : Particle_Iterator := Start (Validator.Particles);
      It    : XML_Particle_Access;
      Skip_Current : Boolean;
   begin
      if D.Nested /= null then
         Run_Nested
           (Validator, D, Local_Name, Namespace_URI, Grammar,
            Element_Validator);
         if Element_Validator /= No_Element then
            return;
         end if;
      end if;

      --  No more match to be had from this choice
      if D.Parent /= null
        and then (Validator.Max_Occurs /= Unbounded
                  and then D.Num_Occurs >= Validator.Max_Occurs)
      then
         Element_Validator := No_Element;
         return;
      end if;

      Debug_Output ("++ Start choice " & Get_Name (Validator)
                    & " minOccurs=" & Validator.Min_Occurs'Img
                    & " maxOccurs=" & Validator.Max_Occurs'Img);

      --  Check whether the current item is valid

      while Get (Item) /= null loop
         It := Get (Item);
         case It.Typ is
            when Particle_Element =>
               Element_Validator := Check_Substitution_Groups
                 (It.Element, Local_Name, Namespace_URI);
               exit when Element_Validator /= No_Element;

            when Particle_Nested =>
               Debug_Output
                 ("++ Choice Testing nested " & Get_Name (It.Validator));

               Check_Nested
                 (Validator, It.Validator, D, Local_Name, Namespace_URI,
                  Grammar, Element_Validator, Skip_Current);
               exit when Element_Validator /= No_Element;

            when Particle_Any =>
               Validate_Start_Element
                 (It.Any, Local_Name, Namespace_URI,
                  null, Grammar, Element_Validator);
               exit when Element_Validator /= No_Element;

            when Particle_Group | Particle_XML_Type =>
               --  Not possible, since the iterator hides these
               raise Program_Error;
         end case;

         Next (Item);
      end loop;

      if Get (Item) = null then
         if D.Parent /= null
           and then D.Num_Occurs >= Validator.Min_Occurs
         then
            Debug_Output
              ("Terminating nested choice, since no longer matches");

            Element_Validator := No_Element;
            return;
         else
            Validation_Error ("Invalid choice: " & Local_Name);
         end if;
      end if;

      D.Num_Occurs := D.Num_Occurs + 1;
      if Validator.Max_Occurs /= Unbounded
        and then D.Num_Occurs > Validator.Max_Occurs
      then
         Validation_Error
           ("Too many occurrences of choice, expecting at most"
            & Integer'Image (Validator.Max_Occurs));
         Element_Validator := No_Element;
      end if;

      if Element_Validator /= No_Element then
         Check_Qualification (Element_Validator, Namespace_URI);
      end if;
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator         : access Choice_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data)
   is
      pragma Unreferenced (Local_Name);
   begin
      if Choice_Data (Data.all).Num_Occurs < Validator.Min_Occurs then
         Validation_Error
           ("Not enough occurrences of choice, expecting at least"
            & Integer'Image (Validator.Min_Occurs));
      end if;
   end Validate_End_Element;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access Choice_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return new Choice_Data;
   end Create_Validator_Data;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Validator_Data_Record) is
      pragma Unreferenced (Data);
   begin
      null;
   end Free;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access XML_Validator_Record) return Validator_Data
   is
      pragma Unreferenced (Validator);
   begin
      return null;
   end Create_Validator_Data;

   -------------------
   -- Create_Choice --
   -------------------

   function Create_Choice
     (Min_Occurs : Natural := 1;
      Max_Occurs : Integer := 1) return Choice
   is
      C : constant Choice := new Choice_Record;
   begin
      if Max_Occurs /= Unbounded and then Min_Occurs > Max_Occurs then
         Validation_Error
           ("minOccurs > maxOccurs when creating choice");
      end if;
      C.Max_Occurs := Max_Occurs;
      C.Min_Occurs := Min_Occurs;
      return C;
   end Create_Choice;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C          : access Choice_Record;
      Item       : XML_Element;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      if Item.Elem.Local_Name = null then
         Raise_Exception
           (Program_Error'Identity,
            "Adding unnamed element to choice");
      end if;
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Element,
                 Element    => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C : access Choice_Record; Item : XML_Any;
      Min_Occurs : Integer := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Any,
                 Any        => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Seq : access Sequence_Record; Item : XML_Any;
      Min_Occurs : Integer := 1; Max_Occurs : Integer := 1) is
   begin
      Append (Seq.Particles, XML_Particle'
                (Typ        => Particle_Any,
                 Any        => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C : access Choice_Record; Item : Sequence) is
   begin
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Item.Min_Occurs,
                 Max_Occurs => Item.Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C : access Choice_Record; Item : Choice) is
   begin
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Nested,
                 Validator  => Group_Model (Item),
                 Next       => null,
                 Min_Occurs => Item.Min_Occurs,
                 Max_Occurs => Item.Max_Occurs));
   end Add_Particle;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (C : access Choice_Record; Item : XML_Group;
      Min_Occurs : Natural := 1; Max_Occurs : Integer := 1) is
   begin
      Append (C.Particles, XML_Particle'
                (Typ        => Particle_Group,
                 Group      => Item,
                 Next       => null,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs));
   end Add_Particle;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Validator_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Validator_Data_Record'Class, Validator_Data);
   begin
      if Data /= null then
         Free (Data.all);
         Unchecked_Free (Data);
      end if;
   end Free;

   --------------------
   -- Applies_To_Tag --
   --------------------

   procedure Applies_To_Tag
     (Group         : access Group_Model_Record;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Grammar       : in out XML_Grammar;
      Applies       : out Boolean;
      Skip_Current  : out Boolean)
   is
      pragma Unreferenced (Group, Local_Name, Namespace_URI, Grammar);
   begin
      Applies := False;
      Skip_Current := False;
   end Applies_To_Tag;

   --------------------
   -- Applies_To_Tag --
   --------------------

   procedure Applies_To_Tag
     (Group         : access Sequence_Record;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Grammar       : in out XML_Grammar;
      Applies       : out Boolean;
      Skip_Current  : out Boolean)
   is
      Iter : Particle_Iterator := Start (Group.Particles);
      Item : XML_Particle_Access;

   begin
      loop
         Item := Get (Iter);
         exit when Item = null;

         case Item.Typ is
            when Particle_Element =>
               Applies := Check_Substitution_Groups
                 (Item.Element, Local_Name, Namespace_URI) /= No_Element;

            when Particle_Nested =>
               Applies_To_Tag
                 (Item.Validator, Local_Name, Namespace_URI,
                  Grammar, Applies, Skip_Current);

            when Particle_Any =>
               --  ??? Tmp
               declare
                  Element_Validator : XML_Element;
               begin
                  Validate_Start_Element
                    (Item.Any, Local_Name, Namespace_URI, null,
                     Grammar, Element_Validator);
                  Applies := True;
               exception
                  when XML_Validation_Error =>
                     Applies := False;
               end;

            when Particle_Group | Particle_XML_Type =>
               --  Not possible since hidden by the iterator
               raise Program_Error;
         end case;

         if Applies then
            return;
         elsif Item.Min_Occurs > 0 then
            Skip_Current := False;
            Applies := False;
            return;
         end if;
         Next (Iter);
      end loop;

      Skip_Current := True;
      Applies := False;
   end Applies_To_Tag;

   --------------------
   -- Applies_To_Tag --
   --------------------

   procedure Applies_To_Tag
     (Group        : access Choice_Record;
      Local_Name   : Unicode.CES.Byte_Sequence;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Grammar      : in out XML_Grammar;
      Applies      : out Boolean;
      Skip_Current : out Boolean)
   is
      Item : Particle_Iterator := Start (Group.Particles);
      It   : XML_Particle_Access;
   begin
      while Get (Item) /= null loop
         It := Get (Item);
         case It.Typ is
            when Particle_Element =>
               Applies := Check_Substitution_Groups
                 (It.Element, Local_Name, Namespace_URI) /=
                 No_Element;

            when Particle_Nested =>
               Applies_To_Tag
                 (It.Validator, Local_Name, Namespace_URI,
                  Grammar, Applies, Skip_Current);

            when Particle_Any =>
               --  ??? Tmp
               declare
                  Element_Validator : XML_Element;
               begin
                  Validate_Start_Element
                    (It.Any, Local_Name, Namespace_URI, null,
                     Grammar, Element_Validator);
                  Applies := True;
               exception
                  when XML_Validation_Error =>
                     Applies := False;
               end;

            when Particle_Group | Particle_XML_Type =>
               --  Not possible since hidden by the iterator
               raise Program_Error;
         end case;

         if Applies then
            return;
         end if;

         Next (Item);
      end loop;

      Applies := False;
      Skip_Current := Group.Min_Occurs = 0;
   end Applies_To_Tag;

   ----------------------------
   -- Set_Substitution_Group --
   ----------------------------

   procedure Set_Substitution_Group
     (Element : XML_Element; Head : XML_Element)
   is
      Is_Extension, Is_Restriction : Boolean;
   begin
      if Get_Type (Element) /= No_Type then
         Is_Extension := Is_Extension_Of
           (Get_Validator (Get_Type (Element)), Get_Type (Head));
         Is_Restriction := Is_Restriction_Of
           (Get_Validator (Get_Type (Element)), Get_Type (Head));

--           if not Is_Extension and not Is_Restriction then
--              Validation_Error
--                ("substitutionGroup can only be used for restrictions or"
--                 & " extensions of the head element");
--           end if;

         if Head.Elem.Final_Restriction and then Is_Restriction then
            Validation_Error
              ("""" & Head.Elem.Local_Name.all
               & """ is final for restrictions, and cannot be substituted by"
               & """" & Element.Elem.Local_Name.all & """");
         end if;

         if Head.Elem.Final_Extension and then Is_Extension then
            Validation_Error
              ("""" & Head.Elem.Local_Name.all
               & """ is final for extensions, and cannot be substituted by"
               & """" & Element.Elem.Local_Name.all & """");
         end if;
      end if;

      Append (Head.Elem.Substitution_Groups, Element);
   end Set_Substitution_Group;

   -------------------------
   -- Empty_Particle_List --
   -------------------------

   function Empty_Particle_List return Particle_List is
   begin
      return new Particle_List_Record;
   end Empty_Particle_List;

   ------------------
   -- Create_Group --
   ------------------

   function Create_Group
     (Local_Name : Unicode.CES.Byte_Sequence) return XML_Group is
   begin
      return new XML_Group_Record'
        (Local_Name => new String'(Local_Name),
         Particles  => Empty_Particle_List);
   end Create_Group;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name
     (Group : XML_Group) return Unicode.CES.Byte_Sequence is
   begin
      return Group.Local_Name.all;
   end Get_Local_Name;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type
     (Local_Name : Unicode.CES.Byte_Sequence;
      Validator  : access XML_Validator_Record'Class) return XML_Type is
   begin
      return new XML_Type_Record'
        (Local_Name => new String'(Local_Name),
         Validator  => XML_Validator (Validator));
   end Create_Type;

   -------------------
   -- Get_Validator --
   -------------------

   function Get_Validator (Typ : XML_Type) return XML_Validator is
   begin
      if Typ = null then
         return null;
      else
         return Typ.Validator;
      end if;
   end Get_Validator;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Group : in out XML_Group; Particle : access Group_Model_Record'Class) is
   begin
      Append
        (Group.Particles, XML_Particle'
           (Typ        => Particle_Nested,
            Validator  => Group_Model (Particle),
            Next       => null,
            Min_Occurs => 1,
            Max_Occurs => 1));
   end Add_Particle;

   --------------------
   -- Set_Debug_Name --
   --------------------

   procedure Set_Debug_Name
     (Typ : access XML_Validator_Record'Class; Name : String) is
   begin
      Free (Typ.Debug_Name);
      Typ.Debug_Name := new Unicode.CES.Byte_Sequence'(Name);
   end Set_Debug_Name;

   -----------
   -- Start --
   -----------

   function Start (List : Particle_List) return Particle_Iterator is
      Iter : Particle_Iterator;
   begin
      if List.First /= null
        and then List.First.Typ = Particle_Group
      then
         if List.First.Group.Particles.First = null then
            Iter := Particle_Iterator'
              (Current  => List.First,
               In_Group => null);
            Next (Iter);
            return Iter;
         else
            return Particle_Iterator'
              (Current  => List.First,
               In_Group => List.First.Group.Particles.First);
         end if;
      else
         return Particle_Iterator'
           (Current  => List.First,
            In_Group => null);
      end if;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Particle_Iterator) is
   begin
      if Iter.In_Group /= null then
         Iter.In_Group := Iter.In_Group.Next;
         if Iter.In_Group = null then
            Debug_Output ("---> End of group "
                          & Iter.Current.Group.Local_Name.all);
         end if;
      end if;

      if Iter.In_Group = null then
         Iter.Current := Iter.Current.Next;

         if Iter.Current /= null
           and then Iter.Current.Typ = Particle_Group
         then
            Debug_Output
              ("---> in group " & Iter.Current.Group.Local_Name.all);
            Iter.In_Group := Iter.Current.Group.Particles.First;

            if Iter.In_Group = null then
               Next (Iter);
            end if;
         end if;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Particle_Iterator) return XML_Particle_Access is
   begin
      if Iter.In_Group /= null then
         return Iter.In_Group;
      else
         return Iter.Current;
      end if;
   end Get;

   --------------------
   -- Get_Min_Occurs --
   --------------------

   function Get_Min_Occurs (Iter : Particle_Iterator) return Natural is
   begin
      return Iter.Current.Min_Occurs;
   end Get_Min_Occurs;

   --------------------
   -- Get_Max_Occurs --
   --------------------

   function Get_Max_Occurs (Iter : Particle_Iterator) return Integer is
   begin
      return Iter.Current.Max_Occurs;
   end Get_Max_Occurs;

   ------------------
   -- Global_Check --
   ------------------

   procedure Global_Check (Grammar : XML_Grammar_NS) is
      use Elements_Htable, Types_Htable;
      Elem_Iter : Elements_Htable.Iterator := First (Grammar.Elements.all);
      Type_Iter : Types_Htable.Iterator := First (Grammar.Types.all);
      Elem : XML_Element_Access;
      Typ  : XML_Type;

   begin
      while Type_Iter /= Types_Htable.No_Iterator loop
         Typ := Current (Type_Iter);
         if Get_Validator (Typ).all in Debug_Validator_Record'Class
           and then Typ.Local_Name.all /= "@@invalid@@"
         then
            Validation_Error
              ("Type """ & Typ.Local_Name.all & """ was referenced, but"
               & " never declared");
         end if;
         Next (Grammar.Types.all, Type_Iter);
      end loop;

      while Elem_Iter /= Elements_Htable.No_Iterator loop
         Elem := Current (Elem_Iter);

         if Elem.Of_Type = No_Type then
            Validation_Error ("No type defined for element "
                              & Elem.Local_Name.all);
         end if;

         if Get_Validator (Elem.Of_Type).all in
           Debug_Validator_Record'Class
         then
            Validation_Error
              ("Element """ & Elem.Local_Name.all
               & """ was referenced, but"
               & " never declared");
         end if;

         Next (Grammar.Elements.all, Elem_Iter);
      end loop;
   end Global_Check;

   ----------------
   -- Create_All --
   ----------------

   function Create_All return XML_All is
   begin
      return new XML_All_Record;
   end Create_All;

   ----------------
   -- Create_Any --
   ----------------

   function Create_Any
     (Process_Contents : Process_Contents_Type := Process_Strict;
      Namespace        : Unicode.CES.Byte_Sequence;
      Target_NS        : XML_Grammar_NS) return XML_Any
   is
      Result : constant XML_Any := new XML_Any_Record;
   begin
      Result.Process_Contents := Process_Contents;
      Result.Namespace := new Byte_Sequence'(Namespace);
      Result.Target_NS := Target_NS;
      return Result;
   end Create_Any;

   ------------------
   -- Add_Particle --
   ------------------

   procedure Add_Particle
     (Validator : access XML_All_Record; Item : XML_Element;
      Min_Occurs : Zero_Or_One := 1; Max_Occurs : Zero_Or_One := 1) is
   begin
      Append (Validator.Particles, XML_Particle'
                (Typ        => Particle_Element,
                 Element    => Item,
                 Min_Occurs => Min_Occurs,
                 Max_Occurs => Max_Occurs,
                 Next       => null));
   end Add_Particle;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access XML_All_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Grammar);
      Tmp     : Particle_Iterator := Start (Validator.Particles);
      D       : constant All_Data_Access := All_Data_Access (Data);
      Count   : Positive := 1;
   begin
      while Get (Tmp) /= null loop
         Element_Validator := Check_Substitution_Groups
           (Get (Tmp).Element, Local_Name, Namespace_URI);

         if Element_Validator /= No_Element then
            D.All_Elements (Count) := D.All_Elements (Count) + 1;
            if D.All_Elements (Count) > Get_Max_Occurs (Tmp) then
               Validation_Error
                 ("Element """ & Local_Name & """ is repeated too many times,"
                  & " maximum number of occurrences is"
                  & Integer'Image (Get_Max_Occurs (Tmp)));
            end if;

            exit;
         end if;

         Count := Count + 1;
         Next (Tmp);
      end loop;

      Check_Qualification (Element_Validator, Namespace_URI);
   end Validate_Start_Element;

   --------------------------
   -- Validate_End_Element --
   --------------------------

   procedure Validate_End_Element
     (Validator         : access XML_All_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data)
   is
      pragma Unreferenced (Local_Name);
      Tmp   : Particle_Iterator := Start (Validator.Particles);
      D     : constant All_Data_Access := All_Data_Access (Data);
      Count : Positive := 1;
   begin
      while Get (Tmp) /= null loop
         if D.All_Elements (Count) < Get_Min_Occurs (Tmp) then
            Validation_Error
              ("Element """ & Get (Tmp).Element.Elem.Local_Name.all
               & """ must appear at least"
               & Integer'Image (Get_Min_Occurs (Tmp)) & " times");
         end if;

         Count := Count + 1;
         Next (Tmp);
      end loop;
   end Validate_End_Element;

   ---------------------------
   -- Create_Validator_Data --
   ---------------------------

   function Create_Validator_Data
     (Validator : access XML_All_Record) return Validator_Data
   is
      Count : Natural := 0;
      Tmp   : Particle_Iterator := Start (Validator.Particles);
   begin
      while Get (Tmp) /= null loop
         Count := Count + 1;
         Next (Tmp);
      end loop;

      return new All_Data'
        (Group_Model_Data_Record with
         Num_Elements => Count,
         All_Elements => (others => 0));
   end Create_Validator_Data;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access XML_All_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Validator, Ch);
   begin
      if not Empty_Element then
         Validation_Error ("No character data allowed by content model");
      end if;
   end Validate_Characters;

   ----------------------------
   -- Validate_Start_Element --
   ----------------------------

   procedure Validate_Start_Element
     (Validator         : access XML_Any_Record;
      Local_Name        : Unicode.CES.Byte_Sequence;
      Namespace_URI     : Unicode.CES.Byte_Sequence;
      Data              : Validator_Data;
      Grammar           : in out XML_Grammar;
      Element_Validator : out XML_Element)
   is
      pragma Unreferenced (Data);
      Start, Last, Index : Integer;
      C : Unicode_Char;
      Valid : Boolean := False;
   begin
      --  Do not check qualification, there is a special handling for
      --  namespaces

      if Validator.Namespace.all = "##any" then
         null;

      elsif Validator.Namespace.all = "##other" then
         if Namespace_URI = Validator.Target_NS.Namespace_URI.all then
            Validation_Error
              ("Namespace should be different from "
               & Validator.Target_NS.Namespace_URI.all);
         end if;

      else
         Index := Validator.Namespace'First;
         while Index <= Validator.Namespace'Last loop
            while Index <= Validator.Namespace'Last loop
               Start := Index;
               Encoding.Read (Validator.Namespace.all, Index, C);
               exit when not Is_White_Space (C);
            end loop;

            while Index <= Validator.Namespace'Last loop
               Last := Index;
               Encoding.Read (Validator.Namespace.all, Index, C);
               exit when Is_White_Space (C);
            end loop;

            if Index > Validator.Namespace'Last then
               Last := Validator.Namespace'Last + 1;
            end if;

            if Validator.Namespace
              (Start .. Last - 1) = "##targetNamespace"
            then
               Valid := Namespace_URI = Validator.Target_NS.Namespace_URI.all;
            elsif Validator.Namespace (Start .. Last - 1) = "##local" then
               Valid := Namespace_URI = "";
            else
               Valid :=
                 Namespace_URI = Validator.Namespace (Start .. Last - 1);
            end if;

            exit when Valid;

         end loop;

         if not Valid then
            Validation_Error ("Invalid namespace for this element: """
                              & Namespace_URI & """ not in """
                              & Validator.Namespace.all & """");
         end if;
      end if;

      Validate_Start_Element
        (Get_Validator
           (Get_Type (Get_UR_Type_Element (Validator.Process_Contents))),
         Local_Name, Namespace_URI, null, Grammar, Element_Validator);
   end Validate_Start_Element;

   -------------------------
   -- Validate_Characters --
   -------------------------

   procedure Validate_Characters
     (Validator     : access XML_Any_Record;
      Ch            : Unicode.CES.Byte_Sequence;
      Empty_Element : Boolean)
   is
      pragma Unreferenced (Validator, Ch, Empty_Element);
   begin
      Validation_Error ("No character data allowed for this element");
   end Validate_Characters;

   ----------
   -- Free --
   ----------

   procedure Free (Id : in out Id_Ref) is
   begin
      Free (Id.Key);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Id : Id_Ref) return Unicode.CES.Byte_Sequence is
   begin
      return Id.Key.all;
   end Get_Key;

   --------------------------
   -- Create_Any_Attribute --
   --------------------------

   function Create_Any_Attribute
     (Kind : Namespace_Kind; NS : XML_Grammar_NS) return Attribute_Validator is
   begin
      return new Any_Attribute_Validator'
        (Kind => Kind,
         NS   => NS);
   end Create_Any_Attribute;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Validator : Any_Attribute_Validator;
      Atts      : Sax.Attributes.Attributes'Class;
      Index     : Natural) is
   begin
      case Validator.Kind is
         when Namespace_Other =>
            if Get_URI (Atts, Index) = "" then
               Validation_Error ("Invalid in this context");

            elsif Get_URI (Atts, Index) = Validator.NS.Namespace_URI.all then
               Validation_Error ("Invalid namespace in this context");
            end if;
         when Namespace_Any =>
            null;
         when Namespace_List =>
            if Get_URI (Atts, Index) /= Validator.NS.Namespace_URI.all then
               Validation_Error ("Invalid namespace in this context");
            end if;
      end case;
   end Validate_Attribute;

   ----------
   -- Free --
   ----------

   procedure Free (Validator : in out Any_Attribute_Validator) is
      pragma Unreferenced (Validator);
   begin
      null;
   end Free;

   ------------------
   -- Set_Abstract --
   ------------------

   procedure Set_Abstract
     (Element     : XML_Element;
      Is_Abstract : Boolean) is
   begin
      Element.Elem.Is_Abstract := Is_Abstract;
   end Set_Abstract;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Is_Abstract;
   end Is_Abstract;

   ------------------
   -- Set_Nillable --
   ------------------

   procedure Set_Nillable
     (Element  : XML_Element;
      Nillable : Boolean) is
   begin
      Element.Elem.Nillable := Nillable;
   end Set_Nillable;

   -----------------
   -- Is_Nillable --
   -----------------

   function Is_Nillable (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Nillable;
   end Is_Nillable;

   ---------------
   -- Set_Final --
   ---------------

   procedure Set_Final
     (Element : XML_Element;
      On_Restriction : Boolean;
      On_Extension   : Boolean) is
   begin
      Element.Elem.Final_Restriction := On_Restriction;
      Element.Elem.Final_Extension   := On_Extension;
   end Set_Final;

   ---------------
   -- Set_Block --
   ---------------

   procedure Set_Block
     (Element        : XML_Element;
      On_Restriction : Boolean;
      On_Extension   : Boolean) is
   begin
      Element.Elem.Block_Restriction := On_Restriction;
      Element.Elem.Block_Extension   := On_Extension;
   end Set_Block;

   ------------------------------
   -- Get_Block_On_Restriction --
   ------------------------------

   function Get_Block_On_Restriction (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Block_Restriction;
   end Get_Block_On_Restriction;

   ----------------------------
   -- Get_Block_On_Extension --
   ----------------------------

   function Get_Block_On_Extension (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Block_Extension;
   end Get_Block_On_Extension;

   ---------------------
   -- Is_Extension_Of --
   ---------------------

   function Is_Extension_Of
     (Validator : access XML_Validator_Record; Typ : XML_Type) return Boolean
   is
      pragma Unreferenced (Validator, Typ);
   begin
      return False;
   end Is_Extension_Of;

   -----------------------
   -- Is_Restriction_Of --
   -----------------------

   function Is_Restriction_Of
     (Validator : access XML_Validator_Record; Typ : XML_Type) return Boolean
   is
      pragma Unreferenced (Validator, Typ);
   begin
      return False;
   end Is_Restriction_Of;

   -----------------------
   -- Free_Nested_Group --
   -----------------------

   procedure Free_Nested_Group (Data : Group_Model_Data) is
   begin
      Data.Nested := null;
      Free (Data.Nested_Data);
   end Free_Nested_Group;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Validator : access XML_Validator_Record) return Boolean
   is
      pragma Unreferenced (Validator);
   begin
      return False;
   end Is_Simple_Type;

   ------------------------------
   -- Set_Element_Form_Default --
   ------------------------------

   procedure Set_Element_Form_Default
     (Grammar : XML_Grammar_NS; Form_Default : Form_Type) is
   begin
      Grammar.Element_Form_Default := Form_Default;
   end Set_Element_Form_Default;

   ------------------------------
   -- Get_Element_Form_Default --
   ------------------------------

   function Get_Element_Form_Default
     (Grammar : XML_Grammar_NS) return Form_Type is
   begin
      return Grammar.Element_Form_Default;
   end Get_Element_Form_Default;

   ---------------
   -- Is_Global --
   ---------------

   function Is_Global (Element : XML_Element) return Boolean is
   begin
      return Element.Elem.Is_Global;
   end Is_Global;

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (Str : String) is
   begin
      if Debug then
         Put_Line (Str);
      end if;
   end Debug_Output;

   -------------------------
   -- Check_Qualification --
   -------------------------

   procedure Check_Qualification
     (Element : XML_Element; Namespace_URI : Unicode.CES.Byte_Sequence) is
   begin
      if not Is_Global (Element)
        and then Element.Elem.Form = Unqualified
        and then Namespace_URI /= ""
      then
         Validation_Error
           ("Namespace specification not authorized in this context");

      elsif Element.Elem.Form = Qualified
        and then Namespace_URI = ""
      then
         Validation_Error
           ("Namespace specification is required in this context");
      end if;
   end Check_Qualification;

end Schema.Validators;
