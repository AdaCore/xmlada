
--  This package implements the content models as described in the DTDs.
--  They are not strictly part of the SAX 2.0 standard, however they are
--  used to simply the handling in users' applications.

with Unicode.CES;

package Sax.Models is

   type Content_Spec is
     (Character_Data,   --  Characters, but no child node  (#PCDATA)
      Element_Ref,      --  A specific child
      Any_Of,           --  child is one of many choices
      Sequence,         --  a sequence of elements (order is imposed)
      Repeat,           --  A repeated element
      Empty,            --  Element must be empty  (EMPTY)
      Anything          --  Content is not described, and can be anything (ANY)
     );

   type Element_Model;
   type Element_Model_Ptr is access Element_Model;
   type Element_Model_Array is array (Natural range <>) of Element_Model_Ptr;
   type Element_Model_Array_Ptr is access Element_Model_Array;

   type Element_Model (Content : Content_Spec) is record
      case Content is
         when Character_Data | Empty | Anything => null;

         when Element_Ref =>
            Name : Unicode.CES.Byte_Sequence_Access; --  Name of the element

         when Any_Of | Sequence =>
            List : Element_Model_Array_Ptr; --  all the possible choices

         when Repeat =>
            Min : Natural;
            Max : Positive;
            Elem : Element_Model_Ptr;
      end case;
   end record;
   --  Type used to describe the model used for an element, as described in
   --  the DTD (see 3.2.* in XML specifications). For instance, the following
   --  model "(#PCDATA|emph)*" is translated to:
   --     (Content => Repeat,
   --      Min     => 0,
   --      Max     => Positive'Last,
   --      Elem    => (Content => Any_Of,
   --                  Choices => (0 => (Content => Character_Data),
   --                              1 => (Content => Element,
   --                                    Name    => "emp"))))

   procedure Free (Model : in out Element_Model_Ptr);
   --  Free the memory allocated for the model.

   function To_String (Model : Element_Model) return Unicode.CES.Byte_Sequence;
   --  Return the string to put in an XML file to describe Model
   --  Invalid_Content_Model is raised if Model can not be described in a
   --  DTD.

   function Is_Mixed (M : Element_Model_Ptr) return Boolean;
   --  Return True if M represents a Mixed content model (3.2.2 in XML
   --  specifications).

   Invalid_Content_Model : exception;
   --  Raised by To_String, when the model is invalid

   --------------------------
   -- Validating the model --
   --------------------------
   --  The model can be used as a non-deterministic state machine, so that
   --  it can be used for validation

   type Model_State is private;

private

   type Model_Item;
   type Model_List is access Model_Item;
   type Model_Item is record
      State : Element_Model_Ptr;
      Next  : Model_List;
   end record;

   type Model_State is record
      Possible_States : Model_List;
   end record;

end Sax.Models;
