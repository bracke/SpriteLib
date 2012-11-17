--------------------------------------------------------------------------------
--                                                                            --
-- Copyright (C) 2004, RISC OS Ada Library (RASCAL) developers.               --
--                                                                            --
-- This library is free software; you can redistribute it and/or              --
-- modify it under the terms of the GNU Lesser General Public                 --
-- License as published by the Free Software Foundation; either               --
-- version 2.1 of the License, or (at your option) any later version.         --
--                                                                            --
-- This library is distributed in the hope that it will be useful,            --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU           --
-- Lesser General Public License for more details.                            --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public           --
-- License along with this library; if not, write to the Free Software        --
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA    --
--                                                                            --
--------------------------------------------------------------------------------

-- $Author$
-- $Date$
-- $Revision$

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C;         use Interfaces.C;

with RASCAL.MessageTrans;  use RASCAL.MessageTrans;
with RASCAL.Utility;       use RASCAL.Utility;
with RASCAL.OS;            use RASCAL.OS;

package body RASCAL.Flex is

   --

   function flex_alloc (anchor : flex_ptr; n : int) return int;

   pragma Import (C,flex_alloc);

   function Alloc (Anchor : in Flex_Ptr;
                   Bytes  : in Integer) return Integer is

      C_Bytes : Int;
      C_Return: Int;
   begin
      C_Bytes  := Int(Bytes);
      C_Return := flex_alloc (Anchor,C_Bytes);
      return Integer(C_Return);
   end Alloc;

   --

   procedure flex_free (anchor : flex_ptr);

   pragma Import (C,flex_free);

   procedure Free (Anchor : in Flex_Ptr) is
   begin
      flex_free(Anchor);
   end Free;

   --

   function flex_size (anchor : flex_ptr) return int;

   pragma Import (C,flex_size);

   function Get_Size (Anchor : in Flex_Ptr) return Integer is

      C_Return : Int;
   begin
      C_Return := flex_size (Anchor);
      return Integer(C_Return);
   end Get_Size;

   --

   function flex_extend (anchor : flex_ptr; newsize : int) return int;

   pragma Import (C,flex_extend);

   function Extend (Anchor   : in Flex_Ptr;
                    New_Size : in Integer) return Integer is

      C_New_Size : Int;
      C_Result   : Int;
   begin
      C_New_Size := Int(New_Size);
      C_Result   := flex_extend (Anchor,C_New_Size);
      return Integer(C_Result);
   end Extend;

   --

   function flex_midextend (anchor : flex_ptr; att : int; by : int) return int;

   pragma Import (C,flex_midextend);

   function Mid_Extend (Anchor   : in Flex_Ptr;
                        Location : in Integer;
                        Extent   : in Integer) return Integer is

      C_Location : Int;
      C_Extent   : Int;
      C_Return   : Int;
   begin
      C_Location := Int(Location);
      C_Extent   := Int(Extent);
      C_Return   := flex_midextend (Anchor, C_Location, C_Extent);
      return Integer(C_Return);
   end Mid_Extend;

   --

   function flex_reanchor (to : flex_ptr; from : flex_ptr) return int;

   pragma Import (C,flex_reanchor);

   function Re_Anchor (To   : in Flex_Ptr;
                       From : in Flex_Ptr) return Integer is
   
      C_Return : Int;
   begin
      C_Return := flex_reanchor (To,From);
      return Integer(C_Return);
   end Re_Anchor;

   --

   function flex_set_budge (newstate : int) return int;

   pragma Import (C,flex_set_budge);

   function Set_Budge (New_State : Integer) return Integer is

      C_New_State : Int;
      C_Previous_State : Int;
   begin
      C_New_State := Int(New_State);
      C_Previous_State := flex_set_budge (C_New_State);
      return Integer(C_Previous_State);
   end Set_Budge;

   --

   procedure flex_init(program_name : chars_ptr ; errors_fd : int; dynamic_size : int);
   
   pragma Import (C,flex_init);

   procedure Init (Program_Name : in String;
                   Errors       : in Messages_Handle_Type;
                   Max_DA_Size  : in Integer := 0) is

      C_Program_Name : Chars_Ptr;
      C_Max_DA_Size  : Int;
   begin
      C_Max_DA_Size  := Int(Max_DA_Size);
      C_Program_Name := New_String (Program_Name);
      if Errors = null then
         flex_init (C_Program_Name,0,C_Max_DA_Size);
      else
         flex_init (C_Program_Name,Adr_To_Int(Errors.all'Address),C_Max_DA_Size);
      end if;
      Free (C_Program_Name);
   end Init;

   --

   procedure flex_save_heap_info (filename : chars_ptr);

   pragma Import (C,flex_save_heap_info);

   procedure Save_HeapInfo (Filename : in String) is

      C_Filename : Chars_Ptr;
   begin
      C_Filename := New_String (Filename);
      flex_save_heap_info (C_Filename);
      Free (C_Filename);
   end Save_HeapInfo;

   --

   function flex_compact return int;

   pragma Import (C,flex_compact);

   function Compact return Integer is

      C_Int : Int;
   begin
      C_Int := flex_compact;
      return Integer(C_Int);
   end Compact;

   --

   function flex_set_deferred_compaction (newstate : int) return int;

   pragma Import (C,flex_set_deferred_compaction);

   function Set_Deferred_Compaction (New_State : Integer) return Integer is

      C_New_State : Int;
      C_Previous_State : Int;
   begin
      C_New_State := Int (New_State);
      C_Previous_State := flex_set_deferred_compaction (C_New_State);
      return Integer(C_Previous_State);
   end Set_Deferred_Compaction;

   --

end RASCAL.Flex;
