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

-- @brief Sprite and sprite area handling.
-- $Author$
-- $Date$
-- $Revision$

with Kernel;
with Ada.Finalization;
with System.Unsigned_Types;

with RASCAL.Memory;
with RASCAL.Utility;
with RASCAL.Heap;

package RASCAL.Sprite is

   use Memory,Heap;

   type Sprite_Area_Type is new Ada.Finalization.Controlled with private;
   type Sprite_Area_Ptr is access Sprite_Area_Type;

   type Sprite_List_Type is array (Positive range <>) of Utility.UString;
   type Sprite_List_Pointer is access Sprite_List_Type;

   --
   -- Returns true if there is a priority spritepool.
   --
   function Is_PriorityPool return Boolean;

   --
   -- Returns the number of sprites in the SpriteArea.
   --
   function Count_Sprites (SpriteArea : Sprite_Area_Type) return Natural;

   --
   -- Returns the name of the sprite SpriteNr in the SpriteArea.
   --
   function Get_SpriteName (SpriteArea : in Sprite_Area_Type;
                            SpriteNr   : in Positive) return String;

   --
   -- Returns an array with the names of all sprites in the SpriteArea.
   --
   function Get_SpriteList (SpriteArea : in Sprite_Area_Type) return Sprite_List_Type;
   
   --
   -- Creates a new blank sprite in SpriteArea
   --
   procedure Create_Sprite (SpriteArea : in out Sprite_Area_Type;
                            PixelWidth : in Positive := 640;
                            PixelHeight: in Positive := 480;
                            ModeNumber : in System.Unsigned_Types.Unsigned := 2**5;
                            Name       : in String   := "Untitled";
                            Palette    : in Boolean  := True);

   --
   -- Save the SpriteArea as a spritefile to Path
   --
   procedure Save_SpriteArea (SpriteArea : in Sprite_Area_Type;
                              Path       : in String);
                              
   --
   -- Does the sprite (Name) exist in the SpriteArea ?
   --
   function Is_Sprite (SpriteArea : in Sprite_Area_Type;
                       Name       : in String) return Boolean;

   --
   -- Does the sprite (Name) exist in the Wimp sprite pool ?
   --
   function Is_Sprite (Name : in String) return Boolean;

   --
   -- Plot sprite (Name) in SpriteArea on to the screen
   --with its bottom left hand corner at the graphics cursor.
   --
   procedure Plot_Sprite (SpriteArea : in Sprite_Area_Type;
                          Name       : in String);
                             
   --
   -- Plot sprite (Name) in SpriteArea on to the screen at X_Position, Y_Position.
   --
   procedure Plot_Sprite (SpriteArea : in Sprite_Area_Type;
                          Name       : in String;
                          X_Position : in Integer;
                          Y_Position : in Integer);

   --
   -- Renames sprite in SpriteArea.
   --
   procedure Rename_Sprite (SpriteArea : in Sprite_Area_Type;
                            Old_Name   : in String;
                            New_Name   : in String);

   --
   -- Deletes the named (Name) sprite in the SpriteArea.
   --
   procedure Delete_Sprite (SpriteArea : in Sprite_Area_Type;
                            Name       : in String);

   --
   -- Save a single sprite (SpriteNr) from a SpriteArea as a spritefile (Path).
   --
   procedure Save_Sprite (SpriteArea  : in Sprite_Area_Type;
                          SpriteNr    : in Positive;
                          Path        : in String);

   --
   -- Copy one (spritenr) sprite from one Spritearea (source) to another (target).
   --
   procedure Copy_Sprite (Source   : in Sprite_Area_Type;
                          SpriteNr : in Positive;
                          Target   : in out Sprite_Area_Type);

   --
   -- Adds sprites from spritefile pointed to by 'Path' to existing SpriteArea.
   --
   procedure Add_Sprite(SpriteArea : in out Sprite_Area_Type;
                        Path       : in String);

   --
   -- Switch output to sprite.
   --
   procedure Output_To_Sprite (SpriteArea : in Sprite_Area_Type;
                               SpriteName : in String;
                               Save_Area  : in Integer := 0);

   --
   -- Switch output to the sprite mask.
   --
   procedure Output_To_Mask (SpriteArea : in Sprite_Area_Type;
                             SpriteName : in String;
                             Save_Area  : in Integer := 0);

   --
   -- Resize Sprite area
   --
   procedure Resize_Area (SpriteArea : in out Sprite_Area_Type;
                          Change     : in Integer);

private

   procedure Initialize (The : in out Sprite_Area_Type);

   type Sprite_Area_Type is new Ada.Finalization.Controlled with
   record
   Area_Size   : Positive := 1000;
   Heap        : Heap_Block_Type(1000);
   end record;

end RASCAL.Sprite;
