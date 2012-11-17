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

with RASCAL.Utility;              use RASCAL.Utility;
with RASCAL.Memory;               use RASCAL.Memory;
with RASCAL.ToolboxWindow;        use RASCAL.ToolboxWindow;
with RASCAL.OS;

with Kernel;                      use Kernel;
with Interfaces.C;                use Interfaces.C;
with Ada.Strings.Unbounded;
with Reporter;

package body RASCAL.ToolboxActionButton is

   Toolbox_ObjectMiscOp      : constant Interfaces.C.unsigned := 16#44EC6#;

   ActionButton_SetText      : constant Interfaces.C.unsigned := 16#80#;
   ActionButton_GetText      : constant Interfaces.C.unsigned := 16#81#;
   ActionButton_SetEvent     : constant Interfaces.C.unsigned := 16#82#;
   ActionButton_GetEvent     : constant Interfaces.C.unsigned := 16#83#;
   ActionButton_SetClickShow : constant Interfaces.C.unsigned := 16#84#;
   ActionButton_GetClickShow : constant Interfaces.C.unsigned := 16#85#;

   --

   function Get_Click_Show (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0)

                                        return Object_ID is

      Register             : aliased Kernel.swi_regs;
      Error                : oserror_access;
   begin
      Register.R (0) := int (Unsigned_to_Int (Flags));
      Register.R (1) := int (Window);
      Register.R (2) := int (ActionButton_GetClickShow);
      Register.R (3) := int (Component);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report ("ToolboxActionButton.Get_Click_Show: "
                                       & To_Ada (Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Object_ID (Register.R (0));
   end Get_Click_Show;

   --

   function Get_Event (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0)
                       
                                   return ToolBox_Event_Code_Type is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R (0) := int (Unsigned_to_Int (Flags));
      Register.R (1) := int (Window);
      Register.R (2) := int (ActionButton_GetEvent);
      Register.R (3) := int (Component);

      Error := Kernel.Swi (Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report ("ToolboxActionButton.Get_Event: "
                                      & To_Ada (Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return ToolBox_Event_Code_Type (Register.R (0));
   end Get_Event;

   --

   function Get_Text (Window    : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0)

                                  return String is

      Register    : aliased Kernel.swi_regs;
      Buffer_Size : integer := 0;
      Error       : oserror_access;
   begin
      Register.R (0) := int (Unsigned_to_Int (Flags));
      Register.R (1) := int (Window);
      Register.R (2) := int (ActionButton_GetText);
      Register.R (3) := int (Component);
      Register.R (4) := 0;
      Register.R (5) := 0;
      Error := Kernel.Swi (Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report ("ToolboxActionButton.Get_Text: "
                           & To_Ada (Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;

      Buffer_Size := Integer (Register.R (5));

      declare
         Buffer : String (1 .. Buffer_Size);
      begin
         Register.R (0) := int (Unsigned_to_Int (Flags));
         Register.R (1) := int (Window);
         Register.R (2) := int (ActionButton_GetText);
         Register.R (3) := int (Component);
         Register.R (4) := Adr_To_Int (Buffer'Address);
         Register.R (5) := int (Buffer_Size);
         Error := Kernel.Swi (Toolbox_ObjectMiscOp,Register'Access,Register'Access);

         if Error /= null then
            pragma Debug(Reporter.Report ("ToolboxActionButton.Get_Text: "
                              & To_Ada (Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
         return MemoryToString (Buffer'Address);
      end;
   end Get_Text;

   --

   procedure Set_Click_Show (Window     : in Object_ID;
                             Component  : in Component_ID;
                             Object     : in Object_ID;
                             Show_Flags : in ActionButton_Show_Type;
                             Flags      : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R (0) := int (Unsigned_to_Int (Flags));
      Register.R (1) := int (Window);
      Register.R (2) := int (ActionButton_SetClickShow);
      Register.R (3) := int (Component);
      Register.R (4) := int (Object);
      Register.R (5) := int (ActionButton_Show_Type'Pos (Show_Flags));
       
      Error := Kernel.Swi (Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report ("ToolboxActionButton.Set_Click_Show: "
                           & To_Ada (Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Click_Show;

   --

   procedure Set_Event (Window    : in Object_ID;
                        Component : in Component_ID;
                        Event     : in ToolBox_Event_Code_Type;
                        Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R (0) := int (Unsigned_to_Int (Flags));
      Register.R (1) := int (Window);
      Register.R (2) := int (ActionButton_SetEvent);
      Register.R (3) := int (Component);
      Register.R (4) := int (Event);
       
      Error := Kernel.Swi (Toolbox_ObjectMiscOp,Register'Access,Register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report ("ToolboxActionButton.Set_Click_Show: "
                           & To_Ada (Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
   end Set_Event;

   --

   procedure Set_Text (Window    : in Object_ID;
                       Component : in Component_ID;
                       Text      : in string;
                       Flags     : in System.Unsigned_Types.Unsigned := 0) is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;

      Text_0      : UString := U(Text & ASCII.NUL);
      Buffer_Size : Integer := (Gadget_Get_BufferSize(Window,Component))-1;
   begin
      if Buffer_Size > -1 then
         if Text'Length > Buffer_Size then
            Text_0 := Ada.Strings.Unbounded.Head(Text_0,Buffer_Size);
            Ada.Strings.Unbounded.Append(Text_0,ASCII.NUL);
         end if;

         Register.R (0) := int (Unsigned_to_Int (Flags));
         Register.R (1) := int (Window);
         Register.R (2) := int (ActionButton_SetText);
         Register.R (3) := int (Component);
         Register.R (4) := Adr_To_Int (S(Text_0)'Address);
         Error := Kernel.Swi (Toolbox_ObjectMiscOp,Register'Access,Register'Access);
         
         if Error /= null then
            pragma Debug(Reporter.Report ("ToolboxActionButton.Set_Click_Show: "
                                          & To_Ada (Error.ErrMess)));
            OS.Raise_Error(Error);
         end if;
      end if;
   end Set_Text;

   --
   
end RASCAL.ToolboxActionButton;
