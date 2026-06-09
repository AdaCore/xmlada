with GNAT.Command_Line;  use GNAT.Command_Line;
with Ada.Text_IO;        use Ada.Text_IO;
with Debug_Readers;      use Debug_Readers;
with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;

procedure TestSAX is
   Read       : File_Input;
   My_Reader  : Debug_Reader;
   Name_Start : Natural;
   Silent     : Boolean := False;
   Color      : Boolean := True;

   No_External_General_Entities   : Boolean := False;
   No_External_Parameter_Entities : Boolean := False;
   No_Load_External_DTD           : Boolean := False;
   No_Doctype_Decl                : Boolean := False;
   Max_Expansion_Count            : Integer := 0;
   Max_Expansion_Size             : Integer := 0;

   function Normalize_Parameter (P : String) return String;
   --  Return parameter value without a leading '='.

   -------------------------
   -- Normalize_Parameter --
   -------------------------

   function Normalize_Parameter (P : String) return String is
   begin
      if P'Length > 0 and then P (P'First) = '=' then
         return P (P'First + 1 .. P'Last);
      end if;

      return P;
   end Normalize_Parameter;

begin
   loop
      case Getopt
         ("silent nocolor -noextgen -noextpar -noextdtd -nodoctype"
          & " -max-expansion-count: -max-expansion-size:")
      is
         when 's' =>
            Silent := True;

         when 'n' =>
            Color := False;

         when '-' =>
            if Full_Switch = "-noextgen" then
               No_External_General_Entities := True;

            elsif Full_Switch = "-noextpar" then
               No_External_Parameter_Entities := True;

            elsif Full_Switch = "-noextdtd" then
               No_Load_External_DTD := True;

            elsif Full_Switch = "-nodoctype" then
               No_Doctype_Decl := True;

            elsif Full_Switch = "-max-expansion-count" then
               Max_Expansion_Count := Integer'Value
                 (Normalize_Parameter (Parameter));

            elsif Full_Switch = "-max-expansion-size" then
               Max_Expansion_Size := Integer'Value
                 (Normalize_Parameter (Parameter));
            end if;

         when others =>
            exit;
      end case;
   end loop;

   Set_Silent (My_Reader, Silent);
   Set_Color  (My_Reader, Color);

   declare
      S : constant String := Get_Argument;
   begin
      if S'Length > 0 then
         --  Base file name should be used as the public Id
         Name_Start := S'Last;
         while Name_Start >= S'First  and then S (Name_Start) /= '/' loop
            Name_Start := Name_Start - 1;
         end loop;
         Set_Public_Id (Read, S (Name_Start + 1 .. S'Last));
         Open (S, Read);

      else
         Put_Line ("First argument should be xml_file_name");
         raise Invalid_Parameter;
      end if;
   end;

   --  If True, xmlns:* attributes will be reported in Start_Element
   Set_Feature (My_Reader, Namespace_Prefixes_Feature, False);
   Set_Feature (My_Reader, Validation_Feature, False);

   Set_Feature
     (My_Reader,
      External_General_Entities_Feature,
      not No_External_General_Entities);
   Set_Feature
     (My_Reader,
      External_Parameter_Entities_Feature,
      not No_External_Parameter_Entities);
   Set_Feature
     (My_Reader,
      Load_External_DTD_Feature,
      not No_Load_External_DTD);
   Set_Feature
     (My_Reader,
      Disallow_Doctype_Decl_Feature,
      No_Doctype_Decl);

   Set_Property
     (My_Reader, Entity_Expansion_Limit_Property, Max_Expansion_Count);
   Set_Property
     (My_Reader, Total_Entity_Size_Limit_Property, Max_Expansion_Size);

   Parse (My_Reader, Read);
   Close (Read);

exception
   when XML_Fatal_Error =>
      Close (Read);
end TestSAX;
