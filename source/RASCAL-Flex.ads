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

-- @brief Thin binding to the Flex shifting heap.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS;     use RASCAL.OS;

with System;        use System;
with Kernel;

package RASCAL.Flex is

   subtype Flex_Ptr is Kernel.void_ptr_ptr;  
      
   --
   -- Allocates 'Bytes' bytes of store, obtained from the Wimp.
   --
   function Alloc (Anchor : in Flex_Ptr;
                   Bytes  : in Integer) return Integer;

   --
   -- Frees the previously allocated store.
   --
   procedure Free (Anchor : in Flex_Ptr);

   --
   -- Informs caller of the number of bytes allocated
   --
   function Get_Size (Anchor : in Flex_Ptr) return Integer;

   --
   -- Extend ot truncate the store area to have a new size.
   --
   function Extend (Anchor   : in Flex_Ptr;
                    New_Size : in Integer) return Integer;
                    
   --
   -- Extend or truncate store, at any point.
   --
   function Mid_Extend (Anchor   : in Flex_Ptr;
                        Location : in Integer;
                        Extent   : in Integer) return Integer;
                        
   --
   -- Move the anchor of an allocated block.
   --
   function Re_Anchor (To   : in Flex_Ptr;
                       From : in Flex_Ptr) return Integer;

   --
   -- Set whether to move the flex store when the runtime needs to extend the heap.
   --
   function Set_Budge (New_State : Integer) return Integer;

   --
   -- Initialise store allocation module.
   --
   procedure Init (Program_Name : in String;
                   Errors       : in Messages_Handle_Type;
                   Max_DA_Size  : in Integer := 0);

   --
   -- Appends information about the flex heap to the passed file.
   --                                                             
   procedure Save_HeapInfo (Filename : in String);

   --
   -- Compacts the flex heap.
   --
   function Compact return Integer;

   --
   -- Sets whether flex should compact its heap on every flex_free or on flex_alloc and flex_compact, flex_budge and flex_extend.
   --
   function Set_Deferred_Compaction (New_State : Integer) return Integer;

private
end RASCAL.Flex;