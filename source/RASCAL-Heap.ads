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

-- @brief Thick binding to the Flex shifting heap.
-- $Author$
-- $Date$
-- $Revision$

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with System;                    use System;
with Ada.Finalization;

with RASCAL.OS;                 use RASCAL.OS;
with RASCAL.Flex;

package RASCAL.Heap is

   Unable_To_Extend_Block    : Exception;
   Unable_To_Allocate_Block  : Exception;

   type Heap_Block_Type (Bytes : natural) is private;
   type Heap_Block_Pointer is access all Heap_Block_Type;
   
   --
   -- The name of the dynamic area used for the heap.
   --
   procedure Set_Name (Name : in String);

   --
   -- This sets the max size of the dynamic area used for the heap. If the value is 0 the WimpSlot will be used instead of a dynamic area.
   --
   procedure Set_MaxSize (Extent : in Natural :=0);

   --
   -- This sets the messages file from which error messages will be read. Otherwise default (UK) messages will be used.
   --
   procedure Set_MessagesFile (Messages : in Messages_Handle_Type);

   --
   -- Returns the address of the block.
   --
   function Get_Address (The : in Heap_Block_Type) return Address;

   --
   -- Returns the size of the block.
   --
   function Get_Size (The : in Heap_Block_Type) return Integer;

   --
   -- Extends the block to the new size.
   --
   procedure Extend (The       : in out Heap_Block_Type;
                     New_Size  : in Natural);

   --
   -- Extends or truncates block, at any point.
   --
   procedure Mid_Extend (The      : in out Heap_Block_Type;
                         Location : in Integer;
                         Extent   : in Integer);

   --
   -- Move the anchor of an allocated block.
   --
   procedure Re_Anchor (The : in out Heap_Block_Type;
                        To  : in out Heap_Block_Type);

   --
   -- Set whether to move the flex store when the runtime needs to extend the heap.
   --
   procedure Set_Budge (Budge : in Boolean);

   --
   -- Appends information about the flex heap to the passed file.
   --
   procedure Save_HeapInfo (Filename : in String);

   --
   -- Compacts the flex heap. Returns 'true' if compaction is complete.
   --
   function Compact return Boolean;

   --
   -- Sets whether flex should compact its heap on every flex_free or on flex_alloc and flex_compact, flex_budge and flex_extend.
   --
   procedure Set_Deferred_Compaction (The   : in out Heap_Block_Type;
                                      Defer : in Boolean);

   --
   -- Deallocates the block.
   --
   procedure Free (The : in out Heap_Block_Type);

private

   Initialised : Boolean       := false;
   Heap_Name   : String(1..32) := "RASCAL - Heap" & 19 * ' ';
   DA_Max_Size : Natural       := 0;
   Errors      : Messages_Handle_Type := null;

   -- Types
   type Heap_Block_Type (Bytes : natural) is new Ada.Finalization.Controlled with
   record
   Extent : natural := Bytes;
   Anchor : Flex.Flex_Ptr;
   end record;

   --

   procedure Initialize (The : in out Heap_Block_Type);

   --

   procedure Adjust (The : in out Heap_Block_Type);

   --

   procedure Finalize (The : in out Heap_Block_Type);

   --

end RASCAL.Heap;
