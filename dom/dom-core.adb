-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2001-2008, AdaCore                   --
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

with Unicode.CES;   use Unicode.CES;
with Sax.Encodings; use Sax.Encodings;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;
with Unicode;       use Unicode;
with Interfaces;    use Interfaces;

package body DOM.Core is
   use Nodes_Htable;

   Node_List_Growth_Factor : Float := Default_Node_List_Growth_Factor;

   function Internalize_Node_Name
     (Doc  : Document; Name : Node_Name_Def) return Shared_Node_Name_Def;
   --  Return a shared version of Name

   ---------------------------------
   -- Set_Node_List_Growth_Factor --
   ---------------------------------

   procedure Set_Node_List_Growth_Factor (Factor : Float) is
   begin
      Node_List_Growth_Factor := Factor;
   end Set_Node_List_Growth_Factor;

   ---------------------
   -- Create_Document --
   ---------------------

   function Create_Document
     (Implementation : DOM_Implementation;
      NameSpace_URI  : DOM_String := "";
      Qualified_Name : DOM_String := "";
      Doc_Type       : Node := null) return Node
   is
      pragma Warnings (Off, NameSpace_URI);
      pragma Warnings (Off, Qualified_Name);
   begin
      pragma Assert
        (Doc_Type = null or else Doc_Type.Node_Type = Document_Type_Node);
      return new Node_Record'
        (Node_Type      => Document_Node,
         Parent         => null,
         Doc_Children   => Null_List,
         Doc_Type       => Doc_Type,
         Parent_Is_Owner => False,
         Implementation => Implementation,
         Ids            => null,
         Node_Names     => new Node_Name_Htable.HTable (127),
         Shared_Strings => new String_Htable.HTable (127));
   end Create_Document;

   -----------------
   -- Has_Feature --
   -----------------

   function Has_Feature
     (Implementation : DOM_Implementation;
      Feature        : DOM_String;
      Version        : String := "2.0") return Boolean
   is
      pragma Warnings (Off, Implementation);
   begin
      return Feature = "XML" and then Version = "2.0";
   end Has_Feature;

   ------------
   -- Append --
   ------------

   procedure Append (List : in out Node_List; N : Node) is
      Old : Node_Array_Access := List.Items;
   begin
      if Old = null or else Old'Last = List.Last then
         List.Items := new Node_Array
           (0 .. List.Last + 1
              + (Integer'Max (0,
                   Integer (Float (List.Last) * Node_List_Growth_Factor))));
         if Old /= null then
            List.Items (0 .. List.Last) := Old.all;
            Free (Old);
         end if;
      end if;
      List.Last := List.Last + 1;
      List.Items (List.Last) := N;
   end Append;

   ------------
   -- Remove --
   ------------

   procedure Remove (List : in out Node_List; N : Node) is
   begin
      if List.Items = null or else List.Last = 0 then
         Free (List.Items);
         List.Last := -1;
      else
         for J in 0 .. List.Last loop
            if List.Items (J) = N then
               List.Items (J .. List.Last - 1) :=
                 List.Items (J + 1 .. List.Last);
               List.Last := List.Last - 1;
               return;
            end if;
         end loop;
      end if;
   end Remove;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Node_List) is
   begin
      Free (List.Items);
   end Free;

   ----------------
   -- Force_Free --
   ----------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Shared_String, Shared_String_Access);

   procedure Force_Free (Str : in out Shared_String_Access) is
   begin
      if Str /= No_String then
         Unicode.CES.Free (Str.Str);
         Unchecked_Free (Str);
         Str := No_String;
      end if;
   end Force_Free;

   ------------------------
   -- Free_Unless_Shared --
   ------------------------

   procedure Free_Unless_Shared (Str : in out Shared_String_Access) is
   begin
      if Shared_Strings then
         Str := No_String;
      else
         Force_Free (Str);
      end if;
   end Free_Unless_Shared;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Str : Shared_String_Access) return DOM_String_Access is
   begin
      return Str.Str;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (Key : DOM_String_Access) return Interfaces.Unsigned_32 is
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

   ---------------
   -- Key_Equal --
   ---------------

   function Key_Equal (Key1, Key2 : DOM_String_Access) return Boolean is
   begin
      return Key1.all = Key2.all;
   end Key_Equal;

   ------------------------
   -- Internalize_String --
   ------------------------

   function Internalize_String
     (Doc  : Document;
      Name : DOM_String) return Shared_String_Access
   is
      Result : Shared_String_Access;
   begin
      if Shared_Strings then
         Result := String_Htable.Get
             (Doc.Shared_Strings.all, Name'Unrestricted_Access);

         if Result = null then
            Result := new Shared_String'
              (Str => new DOM_String'(Name));
            String_Htable.Set (Doc.Shared_Strings.all, Result);
         end if;
      else
         Result := new Shared_String'(Str => new DOM_String'(Name));
      end if;
      return Result;
   end Internalize_String;

   ------------------
   -- Clone_Shared --
   ------------------

   procedure Clone_Shared
     (Dest   : out Shared_String_Access;
      Source : Shared_String_Access) is
   begin
      if Shared_Strings then
         Dest := Source;
      elsif Source = null then
         Dest := null;
      else
         Dest := new Shared_String'(Str => new DOM_String'(Source.Str.all));
      end if;
   end Clone_Shared;

   ---------------------------
   -- Internalize_Node_Name --
   ---------------------------

   function Internalize_Node_Name
     (Doc  : Document; Name : Node_Name_Def) return Shared_Node_Name_Def
   is
      Result : Shared_Node_Name_Def;
   begin
      if Shared_Node_Names then
         Result := Node_Name_Htable.Get (Doc.Node_Names.all, Name);

         if Result = null then
            Result := new Node_Name_Def'(Name);
            Node_Name_Htable.Set (Doc.Node_Names.all, Result);
         end if;
      else
         Result := new Node_Name_Def'(Name);
      end if;
      return Result;
   end Internalize_Node_Name;

   ---------------------
   -- Clone_Node_Name --
   ---------------------

   procedure Clone_Node_Name
     (Dest   : out Shared_Node_Name_Def;
      Source : Shared_Node_Name_Def) is
   begin
      if Shared_Node_Names then
         Dest := Source;
      elsif Source = null then
         Dest := null;
      else
         Dest := new Node_Name_Def;
         Clone (Dest.all, Source.all);
      end if;
   end Clone_Node_Name;

   --------------------
   -- Qualified_Name --
   --------------------

   function Qualified_Name (N : Node_Name_Def) return DOM_String is
   begin
      pragma Assert (N.Local_Name /= null);
      if N.Prefix = null then
         return N.Local_Name.Str.all;
      else
         return N.Prefix.Str.all & Colon_Sequence & N.Local_Name.Str.all;
      end if;
   end Qualified_Name;

   ----------------
   -- Get_Prefix --
   ----------------

   function Get_Prefix (N : Node_Name_Def) return DOM_String is
   begin
      if N.Prefix = null then
         return "";
      else
         return N.Prefix.Str.all;
      end if;
   end Get_Prefix;

   ----------------
   -- Set_Prefix --
   ----------------

   procedure Set_Prefix
     (Doc : Document; N : in out Node_Name_Def; Prefix : DOM_String) is
   begin
      --  ??? We're supposed to check that Prefix is valid, and raise
      --  Invalid_Character_Err otherwise

      Free_Unless_Shared (N.Prefix);
      N.Prefix := Internalize_String (Doc, Prefix);
   end Set_Prefix;

   -----------
   -- Clone --
   -----------

   procedure Clone (Dest : out Node_Name_Def; Source : Node_Name_Def) is
   begin
      Clone_Shared (Dest.Prefix, Source => Source.Prefix);
      Clone_Shared (Dest.Local_Name, Source => Source.Local_Name);
      Clone_Shared (Dest.Namespace, Source => Source.Namespace);
   end Clone;

   ----------------
   -- Force_Free --
   ----------------

   procedure Force_Free (N : in out Node_Name_Def) is
   begin
      if not Shared_Strings then
         Free_Unless_Shared (N.Prefix);
         Free_Unless_Shared (N.Local_Name);
         Free_Unless_Shared (N.Namespace);
      end if;
   end Force_Free;

   -------------------------
   -- From_Qualified_Name --
   -------------------------

   function From_Qualified_Name
     (Doc       : Document;
      Name      : DOM_String;
      Namespace : Shared_String_Access := null) return Shared_Node_Name_Def
   is
      Index     : Natural := Name'First;
      Colon_Pos : Natural;
      C         : Unicode_Char;
      Candidate : Node_Name_Def;
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must convert Tag_Name to uppercase for HTML documents
      --  ??? Test for Namespace_Err

      while Index <= Name'Last loop
         Colon_Pos := Index;
         Encoding.Read (Name, Index, C);
         exit when C = Colon;
      end loop;

      if Shared_Strings then
         if C = Colon then
            Candidate :=
              (Prefix     =>
                 Internalize_String (Doc, Name (Name'First .. Colon_Pos - 1)),
               Local_Name =>
                 Internalize_String (Doc, Name (Index .. Name'Last)),
               Namespace  => Namespace);
         else
            Candidate :=
              (Prefix     => null,
               Local_Name => Internalize_String (Doc, Name),
               Namespace  => Namespace);
         end if;

         return Internalize_Node_Name (Doc, Candidate);

      else
         if C = Colon then
            Candidate :=
              (Prefix     =>
                 Internalize_String (Doc, Name (Name'First .. Colon_Pos - 1)),
               Local_Name =>
                 Internalize_String (Doc, Name (Index .. Name'Last)),
               Namespace  => Namespace);
         else
            Candidate :=
              (Prefix     => null,
               Local_Name => Internalize_String (Doc, Name),
               Namespace  => Namespace);
         end if;

         --  Memory leak if  not Shared_Strings and Shared_Node_Names and
         --  the node already exists
         return Internalize_Node_Name (Doc, Candidate);
      end if;
   end From_Qualified_Name;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name (N : Node_Name_Def) return DOM_String is
   begin
      pragma Assert (N.Local_Name /= null);
      return N.Local_Name.Str.all;
   end Get_Local_Name;

   -----------------------
   -- Get_Namespace_URI --
   -----------------------

   function Get_Namespace_URI (N : Node_Name_Def) return DOM_String is
   begin
      if N.Namespace /= No_String then
         return N.Namespace.Str.all;
      else
         return "";
      end if;
   end Get_Namespace_URI;

   ----------
   -- Self --
   ----------

   function Self (N : Shared_Node_Name_Def) return Node_Name_Def is
   begin
      return N.all;
   end Self;

   -----------
   -- Equal --
   -----------

   function Equal (N1, N2 : Node_Name_Def) return Boolean is
      function Is_Equal (N1, N2 : Shared_String_Access) return Boolean;

      function Is_Equal (N1, N2 : Shared_String_Access) return Boolean is
      begin
         if N1.Str = null then
            if N2.Str /= null then
               return False;
            end if;
         else
            if N2.Str = null
              or else N1.Str.all /= N2.Str.all
            then
               return False;
            end if;
         end if;
         return True;
      end Is_Equal;

   begin
      if Shared_Strings then
         return N1.Prefix = N2.Prefix
           and then N1.Local_Name = N2.Local_Name
           and then N1.Namespace = N2.Namespace;
      else
         return Is_Equal (N1.Prefix, N2.Prefix)
           and then N1.Local_Name.Str.all = N2.Local_Name.Str.all
           and then Is_Equal (N1.Namespace, N2.Namespace);
      end if;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (N : Node_Name_Def) return Interfaces.Unsigned_32 is
      Tmp : Interfaces.Unsigned_32 := 0;
   begin
      if N.Prefix /= No_String then
         Tmp := Hash (N.Prefix.Str);
      end if;

      if N.Namespace /= No_String then
         Tmp := Tmp + Hash (N.Namespace.Str);
      end if;

      return Tmp + Hash (N.Local_Name.Str);
   end Hash;

   ----------------
   -- Force_Free --
   ----------------

   procedure Force_Free (N : in out Shared_Node_Name_Def) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Node_Name_Def, Shared_Node_Name_Def);
   begin
      if N /= null then
         Force_Free (N.all);
         Unchecked_Free (N);
      end if;
   end Force_Free;

   ------------------------
   -- Free_Unless_Shared --
   ------------------------

   procedure Free_Unless_Shared (N : in out Shared_Node_Name_Def) is
   begin
      if Shared_Node_Names then
         N := null;
      else
         Force_Free (N);
      end if;
   end Free_Unless_Shared;

   ----------
   -- Free --
   ----------

   procedure Free (N : in out Node_String) is
   begin
      Free (N.Key);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (N : Node_String) return DOM_String_Access is
   begin
      return N.Key;
   end Get_Key;

   ---------------------
   -- Document_Add_Id --
   ---------------------

   procedure Document_Add_Id
     (Doc  : Document;
      Id   : DOM_String;
      Elem : Element) is
   begin
      if Doc.Ids = null then
         Doc.Ids := new Nodes_Htable.HTable (203);
      end if;

      Set (Doc.Ids.all, (N => Node (Elem), Key => new DOM_String'(Id)));
   end Document_Add_Id;

   ------------------------
   -- Document_Remove_Id --
   ------------------------

   procedure Document_Remove_Id
     (Doc  : Document;
      Id  : DOM_String) is
   begin
      if Doc.Ids /= null then
         Remove (Doc.Ids.all, Id'Unrestricted_Access);
      end if;
   end Document_Remove_Id;

end DOM.Core;
