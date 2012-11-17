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

with RASCAL.Utility;   use RASCAL.Utility;
with RASCAL.Memory;    use RASCAL.Memory;

with Reporter;

package body RASCAL.Heap is

   --

   procedure Set_Name (Name : in String) is
   begin
      if not Initialised then
         if Name'Length > 32 then
            Heap_Name(1..32) := Name(Name'First..Name'First+31);
         else
            Heap_Name(1..Name'Length) := Name;
         end if;
      end if;
   end Set_Name;

   --

   procedure Set_MaxSize (Extent : in Natural :=0) is
   begin
      if not Initialised then
         DA_Max_Size := Extent;
      end if;
   end Set_MaxSize;

   --

   procedure Set_MessagesFile (Messages : in Messages_Handle_Type) is
   begin
      Errors := Messages;
   end Set_MessagesFile;

   --

   function Get_Address (The : in Heap_Block_Type) return Address is
   begin
      return The.Anchor;
   end Get_Address;

   --

   function Get_Size (The : in Heap_Block_Type) return Integer is
   begin
      return Flex.Get_Size (The.Anchor'Address);
   end Get_Size;

   --

   procedure Extend (The       : in out Heap_Block_Type;
                     New_Size  : in Natural) is

      Result : Integer;
   begin
      Result := Flex.Extend (The.Anchor'Address,New_Size);
      if Result = 1 then
         -- 0 = failure, 1 = success.
         The.Extent := New_Size;
      else
         raise Unable_To_Extend_Block;
      end if;
   end Extend;

   --

   procedure Mid_Extend (The      : in out Heap_Block_Type;
                         Location : in Integer;
                         Extent   : in Integer) is

      Result : Integer;
   begin
      Result := Flex.Mid_Extend (The.Anchor'Address,Location,Extent);
      if Result = 1 then
         -- 0 == failure, 1 == success
         The.Extent := Get_Size (The);
      else
         raise Unable_To_Extend_Block;
      end if;
   end Mid_Extend;

   --

   procedure Re_Anchor (The : in out Heap_Block_Type;
                        To  : in out Heap_Block_Type) is
      Result : Integer;
   begin
      Result := Flex.Re_Anchor (To.Anchor'Address,The.Anchor'Address);
      The.Extent := 0;
      To.Extent  := Get_Size (To);
   end Re_Anchor;

   --

   procedure Set_Budge (Budge : in Boolean) is

      Result    : Integer;
      New_State : Integer := 0;
   begin
      if Budge then
         New_State := 1;
      end if;
      Result := Flex.Set_Budge (New_State);
   end Set_Budge;

   --

   procedure Save_HeapInfo (Filename : in String) is

   begin
      Flex.Save_HeapInfo (Filename);
   end Save_HeapInfo;

   --

   function Compact return Boolean is

      Result : Integer;
   begin
      Result := Flex.Compact;
      return Result = 0;
   end Compact;

   --

   procedure Set_Deferred_Compaction (The   : in out Heap_Block_Type;
                                      Defer : in Boolean) is
      Result    : Integer;
      New_State : Integer := 0;
   begin
      if Defer then
         New_State := 1;
      end if;
      Result := Flex.Set_Deferred_Compaction(New_State);
   end Set_Deferred_Compaction;

   --

   procedure Free (The : in out Heap_Block_Type) is
   begin
      if Adr_To_Integer (The.Anchor) /= 0 then         
         Flex.Free (The.Anchor'Address);
      end if;
   end Free;

   --

   procedure Initialize(The : in out Heap_Block_Type) is

      Result : Integer;
   begin
      -- Initialise heap if necessary
      if not Initialised then
         Flex.Init (Heap_Name,Errors,DA_Max_Size);
         Set_Budge(true);
         Initialised := true;
      end if;
      -- Allocate block
      Result := Flex.Alloc (The.Anchor'Address,The.Extent);
      if Result = 0 then
         -- 0 = failure, 1 = success
         raise Unable_To_Allocate_Block;
      end if;
      if Get_Size (The) < The.Extent then
         Extend (The,The.Extent);
      end if;
   end Initialize;

   --

   procedure Adjust(The : in out Heap_Block_Type) is
   begin
      null;
   end Adjust;

   --

   procedure Finalize(The : in out Heap_Block_Type) is
   begin
      if Adr_To_Integer (The.Anchor) /= 0 then
         Flex.Free (The.Anchor'Address);
      end if;
   end Finalize;

   --

end RASCAL.Heap;
