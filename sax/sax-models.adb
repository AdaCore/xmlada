with Unicode;                   use Unicode;
with Unicode.CES;               use Unicode.CES;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;
with Unicode.CES.Basic_8bit;    use Unicode.CES.Basic_8bit;
with Unchecked_Deallocation;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Encodings;                 use Encodings;

package body Sax.Models is

   --------------
   -- Is_Mixed --
   --------------

   function Is_Mixed (M : Element_Model_Ptr) return Boolean is
   begin
      pragma Assert (M /= null);
      return M.Content = Any_Of
        and then M.List (M.List'First).Content = Character_Data;
   end Is_Mixed;

   ---------------
   -- To_String --
   ---------------

   function To_String (Model : Element_Model) return Unicode.CES.Byte_Sequence
   is
      Str : Unbounded_String;
   begin
      case Model.Content is
         when Character_Data =>
            return Encodings.From_Utf32 (To_Utf32 ("#PCDATA"));

         when Empty =>
            return Encodings.From_Utf32 (To_Utf32 ("EMPTY"));

         when Anything =>
            return Encodings.From_Utf32 (To_Utf32 ("ANY"));

         when Element_Ref =>
            return Model.Name.all;

         when Any_Of | Sequence =>
            for J in Model.List'Range loop
               if Model.List (J).Content = Character_Data then
                  if Model.Content = Sequence
                    or else J /= Model.List'First
                  then
                     raise Invalid_Content_Model;
                  end if;
               end if;

               if Model.List (J).Content = Anything
                 or else Model.List (J).Content = Empty
               then
                  raise Invalid_Content_Model;
               end if;

               Append (Str, To_String (Model.List (J).all));
               if J /= Model.List'Last then
                  if Model.Content = Any_Of then
                     Append (Str, Encoding.Encode (Vertical_Line));
                  else
                     Append (Str, Encoding.Encode (Comma));
                  end if;
               end if;
            end loop;
            return Encoding.Encode (Opening_Parenthesis)
              & To_String (Str) & Encoding.Encode (Closing_Parenthesis);

         when Repeat =>
            if Model.Elem.Content = Anything
              or else Model.Elem.Content = Empty
            then
               raise Invalid_Content_Model;
            end if;

            if Model.Min = 0 and then Model.Max = Positive'Last then
               return To_String (Model.Elem.all)
                 & Encoding.Encode (Star);
            elsif Model.Min = 0 and then Model.Max = 1 then
               return To_String (Model.Elem.all)
                 & Encoding.Encode (Question_Mark);
            elsif Model.Min = 1 and then Model.Max = Positive'Last then
               return To_String (Model.Elem.all)
                 & Encoding.Encode (Plus_Sign);
            else
               raise Invalid_Content_Model;
            end if;
      end case;
   end To_String;

   ----------
   -- Free --
   ----------

   procedure Free (Model : in out Element_Model_Ptr) is
      procedure Free is new Unchecked_Deallocation
        (Element_Model_Array, Element_Model_Array_Ptr);
      procedure Internal is new Unchecked_Deallocation
        (Element_Model, Element_Model_Ptr);
   begin
      if Model /= null then
         case Model.Content is
            when Character_Data | Anything | Empty => null;
            when Element_Ref =>
               Free (Model.Name);
            when Any_Of | Sequence =>
               for J in Model.List'Range loop
                  Free (Model.List (J));
               end loop;
               Free (Model.List);
            when Repeat =>
               Free (Model.Elem);
         end case;
         Internal (Model);
      end if;
   end Free;
end Sax.Models;
