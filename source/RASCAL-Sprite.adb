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

with Ada.IO_Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with System;
with System.Storage_Elements;

with Interfaces.C;
with Reporter;

with RASCAL.OS;
with RASCAL.FileExternal;
with RASCAL.FileInternal;
with RASCAL.Memory;

package body RASCAL.Sprite is

   use RASCAL.Utility,System.Storage_Elements,Kernel,Interfaces.C;

   OS_SpriteOp      : constant := 16#2E#;
   Wimp_SpriteOp    : constant := 16#400E9#;
   Wimp_ReadSysInfo : constant := 16#400F2#;

   --
   -- Returns true if there is a priority spritepool.
   --
   function Is_PriorityPool return Boolean is
   
      Register   : aliased Kernel.SWI_Regs;
      Error      : Kernel.OSError_Access;
   begin
      Register.R (0) := 19;
      Error := Kernel.SWI (Wimp_ReadSysInfo,register'Access,register'Access);
      return Error = null;
   end Is_PriorityPool;

   --
   -- Loads Sprite into spritearea, without regarding any previous content.
   --
   procedure Load_Sprite (SpriteArea : in out Sprite_Area_Type;
                          Filename   : in string) is

      Exists     : Boolean := FileExternal.Exists (Filename);
      Filename_0 : String  := Filename & ASCII.NUL;
      FileSize   : Positive;
      Register   : aliased Kernel.SWI_Regs;
      Error      : Kernel.OSError_Access;
      New_Size   : Positive;
   begin
      if Exists then
         FileSize := FileExternal.Get_Size(Filename);
         New_Size := SpriteArea.Area_Size + FileSize;
         Heap.Extend (SpriteArea.Heap,New_Size);
         Memory.PutWord (New_Size,Heap.Get_Address(SpriteArea.Heap));
         SpriteArea.Area_Size := New_Size;
         Register.R (0) := 16#A# +256;
         Register.R (1) := Adr_To_Int (Heap.Get_Address(SpriteArea.Heap));
         Register.R (2) := Adr_To_Int (Filename_0'address);

         Error := Kernel.SWI (OS_SpriteOp,register'Access,register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report("Sprite - Load_Sprite: " &
                         Interfaces.C.To_Ada(Error.errmess)));
            OS.Raise_Error(Error);
         end if;
      else
         raise Ada.IO_Exceptions.Name_Error;
      end if;
   end Load_Sprite;

   --
   -- Returns the number of sprites in the SpriteArea.
   --                         7
   function Count_Sprites (SpriteArea : Sprite_Area_Type) return Natural is
   begin
      return Memory.GetWord(Heap.Get_Address(SpriteArea.Heap),4);
   end Count_Sprites;

   --
   -- Returns the name of the sprite SpriteNr in the SpriteArea.
   --
   function Get_SpriteName (SpriteArea : in Sprite_Area_Type;
                            SpriteNr   : in Positive) return String is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.SWI_Regs;
      Buffer     : array(1..13) of Integer;
   begin
      Register.R(0) := 16#D# + 256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address(SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (Buffer'Address);
      Register.R(3) := 13;
      Register.R(4) := Interfaces.C.Int (SpriteNr);
      Error := Kernel.swi (OS_SpriteOp,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report ("Sprite - Get_SpriteName: " &
                      Interfaces.C.To_Ada (Error.errmess)));
         OS.Raise_Error(Error);
         return "";
      end if;
      return Memory.MemoryToString (Buffer'address,0,Integer (Register.R(3)));
   end Get_SpriteName;

   --
   -- Returns an array with the names of all sprites in the SpriteArea.
   --
   function Get_SpriteList (SpriteArea : in Sprite_Area_Type) return Sprite_List_Type is

      Sprites     : Natural := Count_Sprites (SpriteArea);
      Sprite_List : Sprite_List_Type(1..Sprites);
   begin
      for i in Sprite_List'Range loop
         Sprite_List(i) := U (Get_SpriteName (SpriteArea,i));
      end loop;
      return Sprite_List;
   end Get_SpriteList;

   --
   -- Creates a new blank sprite in SpriteArea
   --
   procedure Create_Sprite (SpriteArea : in out Sprite_Area_Type;
                            PixelWidth : in Positive := 640;
                            PixelHeight: in Positive := 480;
                            ModeNumber : in System.Unsigned_Types.Unsigned := 2**5;
                            Name       : in String   := "Untitled";
                            Palette    : in Boolean  := True) is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.SWI_Regs;
      New_Area   : Sprite_Area_Type;
      New_Size   : Positive := PixelWidth * PixelHeight * 4 + New_Area.Area_Size;
      Name_0     : String   := Name & ASCII.NUL;
   begin
      -- Enlarge area
      Resize_Area(New_Area,New_Size);

      -- Create Sprite in new area
      Register.R(0) := 16#F# + 256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address (New_Area.Heap));
      Register.R(2) := Adr_To_Int (Name_0'address);
      Register.R(3) := Interfaces.C.Int (boolean'Pos(Palette));
      Register.R(4) := Interfaces.C.Int (PixelWidth);
      Register.R(5) := Interfaces.C.Int (PixelHeight);
      Register.R(6) := Interfaces.C.Int (ModeNumber);
      Error := Kernel.SWI (OS_SpriteOp,Register'access,Register'access);
      if Error /= null then
         pragma Debug(Reporter.Report ("Sprite - Create_Sprite: " & To_Ada (Error.errmess)));
         OS.Raise_Error(Error);
      end if;

      -- Copy sprite from new area to target area
      Copy_Sprite (New_Area,1,SpriteArea);

   end Create_Sprite;

   --
   -- Save the SpriteArea as a spritefile to Path
   --
   procedure Save_SpriteArea (SpriteArea : in Sprite_Area_Type;
                              Path       : in string) is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.SWI_Regs;
      Path_0     : String := Path & ASCII.NUL;
   begin
      Register.R(0) := 16#C# + 256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address(SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (Path_0'address);

      Error := Kernel.SWI (OS_SpriteOp,Register'access,Register'access);

      if Error /= null then
         Reporter.Report ("Sprite - Save_SpriteArea: " &
                          To_Ada (Error.errmess));
         OS.Raise_Error(Error);
      end if;
   end Save_SpriteArea;

   --
   -- Does the sprite (Name) exist in the SpriteArea ?
   --
   function Is_Sprite (SpriteArea : in Sprite_Area_Type;
                       Name       : in string) return boolean is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.SWI_Regs;
      Name_0     : String  := Name & ASCII.NUL;
   begin
      Register.R(0) := 40 + 256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address(SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (Name_0'address);

      Error := Kernel.SWI (OS_SpriteOp,Register'access,Register'access);
      if Error /= null then
         return false;
      else
         return true;
      end if;
   end Is_Sprite;

   --
   -- Does the sprite (Name) exist in the Wimp sprite pool ?
   --
   function Is_Sprite (Name : in string) return boolean is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.SWI_Regs;
      Name_0     : string := Name & ASCII.NUL;
   begin
      Register.R(0) := 40;
      Register.R(1) := 0;
      Register.R(2) := Adr_To_Int (Name_0'address);

      Error := Kernel.SWI (Wimp_SpriteOp,Register'access,Register'access);

      if Error /= null then
         return false;
      else
         return true;
      end if;
   end Is_Sprite;

   --
   -- Plot sprite (Name) in SpriteArea on to the screen
   --with its bottom left hand corner at the graphics cursor.
   --
   procedure Plot_Sprite (SpriteArea : in Sprite_Area_Type;
                          Name       : in String) is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.swi_regs;
      Name_0     : string := Name & ASCII.NUL;
   begin
      Register.R(0) := 28 + 256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address(SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (Name_0'address);
      Register.R(5) := 0;

      Error := Kernel.SWI (OS_SpriteOp,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report ("Sprite - Plot_Sprite I: " & To_Ada (Error.errmess)));
         OS.Raise_Error(Error);
      end if;
   end Plot_Sprite;

   --
   -- Plot sprite (Name) in SpriteArea on to the screen at X_Position, Y_Position.
   --Do not use if not in the same mode style (colours/resolution) as the sprite.
   --
   procedure Plot_Sprite (SpriteArea : in Sprite_Area_Type;
                          Name       : in string;
                          X_Position : in integer;
                          Y_Position : in integer) is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.swi_regs;
      Name_0     : string := Name & ASCII.NUL;
   begin
      Register.R(0) := 34 + 256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address(SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (Name_0'address);
      Register.R(3) := Interfaces.C.Int (X_Position);
      Register.R(4) := Interfaces.C.Int (Y_Position);
      Register.R(5) := 16#08#;

      Error := Kernel.SWI (OS_SpriteOp,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report ("Sprite - Plot_Sprite II: " & To_Ada (Error.errmess)));
         OS.Raise_Error(Error);
      end if;
   end Plot_Sprite;

   --
   -- Renames sprite in SpriteArea.
   --
   procedure Rename_Sprite (SpriteArea : in Sprite_Area_Type;
                            Old_Name   : in string;
                            New_Name   : in string) is

      New_Name_12: string    := Ada.Strings.Fixed.Head(
                                Ada.Characters.Handling.To_Lower (New_Name),11,ASCII.NUL);
      offset     : integer   := 0;
      sprites    : integer   := Count_Sprites (SpriteArea);
      Terminator : character := ASCII.NUL;
   begin
      if sprites > 0 then
         if New_Name'Length >= 11 then
            Terminator := Ada.Characters.Handling.To_Lower (New_Name (New_Name'First+11));
         end if;

         offset := Memory.GetWord (Heap.Get_Address(SpriteArea.Heap),8);

         for s in 1..sprites loop
            if Get_SpriteName (SpriteArea,s) = Old_Name then
               Memory.StringToMemory (New_Name_12,
                                      Heap.Get_Address(SpriteArea.Heap),offset+4,0,Terminator);
            end if;
            offset := offset + Memory.GetWord (Heap.Get_Address(SpriteArea.Heap),offset);
         end loop;
      end if;
   end Rename_Sprite;

   --
   -- Deletes the named (Name) sprite in the SpriteArea.
   --
   procedure Delete_Sprite (SpriteArea : in Sprite_Area_Type;
                            Name       : in String) is

      Error      : Kernel.OSError_Access;
      Register   : aliased Kernel.SWI_Regs;
      Name_0     : String := Name & ASCII.NUL;
   begin
      Register.R(0) := 16#19# + 256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address(SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (Name_0'Address);
      Error := Kernel.swi (OS_SpriteOp,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report ("Sprite - Delete_Sprite: " &
                      Interfaces.C.To_Ada (Error.errmess)));
         OS.Raise_Error(Error);
      end if;
   end Delete_Sprite;

   --
   -- Save a single sprite (SpriteNr) from a SpriteArea as a spritefile (Path).
   --
   procedure Save_Sprite (SpriteArea  : in Sprite_Area_Type;
                          SpriteNr    : in Positive;
                          Path        : in String) is

      New_Area    : Sprite_Area_Type;
   begin
      Copy_Sprite (SpriteArea,SpriteNr,New_Area);
      Save_SpriteArea (New_Area,Path);
   end Save_Sprite;

   --
   -- Copy one (spritenr) sprite from one Spritearea (source) to another (target).
   --
   procedure Copy_Sprite (Source   : in Sprite_Area_Type;
                          SpriteNr : in Positive;
                          Target   : in out Sprite_Area_Type) is

      Source_Offset : Positive := Memory.GetWord (Heap.Get_Address(Source.Heap),8);
      Target_Offset : Positive := Memory.GetWord (Heap.Get_Address(Target.Heap),12);
      Sprite_Size   : Positive;
      x             : Positive := 1;
      Source_Adr    : Memory.Mem_Adr_Type;
      Target_Adr    : Memory.Mem_Adr_Type;
   begin
      -- Locate sprite in spritearea
      while x /= SpriteNr loop
         x := x + 1;
         Source_Offset := Source_Offset+Memory.GetWord (Heap.Get_Address(Source.Heap),Source_Offset);
      end loop;

      Sprite_Size := Memory.GetWord (Heap.Get_Address(Source.Heap),Source_Offset);

      -- Resize target spritearea
      Heap.Extend (Target.Heap,Target.Area_Size+Sprite_Size);
      Target.Area_Size := Target.Area_Size+Sprite_Size;
      Memory.PutWord(Target.Area_Size+Sprite_Size,Heap.Get_Address(Target.Heap));

      Source_Adr  := To_Address (Integer_Address (Source_Offset+integer (Adr_To_Int (Heap.Get_Address(Source.Heap)))));
      Target_Adr  := To_Address (Integer_Address (Target_Offset+integer (Adr_To_Int (Heap.Get_Address(Target.Heap)))));

      -- Copy sprite to target spritearea
      Memory.MemCopy (Source_Adr,Target_Adr,0,Sprite_Size);

      -- Update header of target spritearea
      Memory.PutWord (Count_Sprites(Target)+1,Heap.Get_Address(Target.Heap),4); -- Number of sprites in area
      Memory.PutWord (Target_Offset+Sprite_Size,Heap.Get_Address(Target.Heap),12);-- Offset to first free word
   end Copy_Sprite;

   --
   -- Adds sprites from spritefile pointed to by Path to existing SpriteArea.
   --
   procedure Add_Sprite(SpriteArea : in out Sprite_Area_Type;
                        Path       : in String) is

      New_Sprite        : Sprite_Area_Type;
      Nr_Of_Sprites     : Positive;
   begin
      if Count_Sprites(SpriteArea) = 0 then
         Load_Sprite(SpriteArea,Path);
      else
         Load_Sprite (New_Sprite,Path);
         Nr_Of_Sprites := Count_Sprites (New_Sprite);

         for x in 1..Nr_Of_Sprites loop
            Copy_Sprite (New_Sprite,x,SpriteArea);
         end loop;
      end if;
   end Add_Sprite;

   --
   -- Switches output to sprite.
   --
   procedure Output_To_Sprite (SpriteArea : in Sprite_Area_Type;
                               SpriteName : in String;
                               Save_Area  : in Integer := 0) is

      Error        : Kernel.OSError_Access;
      Register     : aliased Kernel.SWI_Regs;
      SpriteName_0 : String := SpriteName & ASCII.NUL;
   begin
      Register.R(0) := 16#3c#+256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address (SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (SpriteName_0'address);
      Register.R(3) := Int(Save_Area);
      Error := Kernel.SWI (OS_SpriteOp,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report ("Sprite.Output_To_Sprite: " & To_Ada (Error.errmess)));
         OS.Raise_Error(Error);
      end if;
   end Output_To_Sprite;

   --

   procedure Output_To_Mask (SpriteArea : in Sprite_Area_Type;
                             SpriteName : in String;
                             Save_Area  : in Integer := 0) is

      Error        : Kernel.OSError_Access;
      Register     : aliased Kernel.SWI_Regs;
      SpriteName_0 : String := SpriteName & ASCII.NUL;
   begin
      Register.R(0) := 16#3D#+256;
      Register.R(1) := Adr_To_Int (Heap.Get_Address (SpriteArea.Heap));
      Register.R(2) := Adr_To_Int (SpriteName_0'address);
      Register.R(3) := Int(Save_Area);
      Error := Kernel.SWI (OS_SpriteOp,Register'access,Register'access);

      if Error /= null then
         pragma Debug(Reporter.Report ("Sprite.Output_To_Mask: " & To_Ada (Error.errmess)));
         OS.Raise_Error(Error);
      end if;
   end Output_To_Mask;

   --

   procedure Resize_Area (SpriteArea : in out Sprite_Area_Type;
                          Change     : in Integer) is
      New_Size : Integer;
   begin
      New_Size := SpriteArea.Area_Size + Change;
      Heap.Extend (SpriteArea.Heap,New_Size);
      Memory.PutWord (New_Size,Heap.Get_Address(SpriteArea.Heap));
      SpriteArea.Area_Size := New_Size;
   end Resize_Area;

   --

   procedure Initialize (The : in out Sprite_Area_Type) is
   begin
      Memory.PutWord (20,Heap.Get_Address(The.Heap),0);
      Memory.PutWord (0,Heap.Get_Address(The.Heap),4);
      Memory.PutWord (16#10#,Heap.Get_Address(The.Heap),8);
      Memory.PutWord (16#10#,Heap.Get_Address(The.Heap),12);
      The.Area_Size := 20;
   end Initialize;

   --

end RASCAL.Sprite;
