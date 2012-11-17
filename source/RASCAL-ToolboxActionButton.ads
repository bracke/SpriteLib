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

-- @brief ToolboxActionButton types and methods.
-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Toolbox;    use RASCAL.Toolbox;
with RASCAL.OS;         use RASCAL.OS;

with System.Unsigned_Types;   use System.Unsigned_Types;

package RASCAL.ToolboxActionButton is

   ActionButton_Is_Default          : System.Unsigned_Types.Unsigned := 16#1#;
   ActionButton_Is_Cancel           : System.Unsigned_Types.Unsigned := 16#2#;
   ActionButton_Is_Local            : System.Unsigned_Types.Unsigned := 16#4#;
   ActionButton_ClickShowTransient  : System.Unsigned_Types.Unsigned := 16#8#;

   ActionButton_ClickShowCentred    : System.Unsigned_Types.Unsigned := 16#10#;
   ActionButton_ClickShowAtPointer  : System.Unsigned_Types.Unsigned := 16#10#;
   
   ActionButton_Selected_Adjust     : System.Unsigned_Types.Unsigned := 16#1#;
   ActionButton_Selected_Select     : System.Unsigned_Types.Unsigned := 16#4#;
   
   ActionButton_Selected_Default    : System.Unsigned_Types.Unsigned := 16#8#;
   ActionButton_Selected_Cancel     : System.Unsigned_Types.Unsigned := 16#10#;
   ActionButton_Selected_Local      : System.Unsigned_Types.Unsigned := 16#20#;

   type ActionButton_Show_Type is (Persistent, Transient);
   type ActionButton_Click_Type is (Select_Click, Key_Pressed, Adjust_Click);
   type ActionButton_Button_Type is (Default, Cancel, Local);

   type ActionButton_Gadget_Template is
   record
   Max_Text_Length   : integer;
   Event             : System.Unsigned_Types.Unsigned;
   end record;

   --
   -- This toolbox event is raised when the user clicks on an action button, and the client has not specified their own event to be associated with this event.
   --
   type Toolbox_ActionButton_Selected is
   record
   Header         : Toolbox_Event_Header;
   end record;
   pragma Convention (C, Toolbox_ActionButton_Selected);

   type Toolbox_ActionButton_Selected_Pointer is access Toolbox_ActionButton_Selected;

   type ATEL_Toolbox_ActionButton_Selected is abstract new
        Toolbox_EventListener(Toolbox_Event_ActionButton_Selected,-1,-1) with
   record
   Event : Toolbox_ActionButton_Selected_Pointer;
   end record;

   --
   -- Returns the ObjectID of the object shown when the user clicks on the action button.
   --
   function Get_Click_Show (Window    : in Object_ID;
                            Component : in Component_ID;
                            Flags     : in System.Unsigned_Types.Unsigned := 0)

                                        return Object_ID;

   --
   -- Returns the event code which will be executed when the user clicks on the action button.
   --
   function Get_Event (Window    : in Object_ID;
                       Component : in Component_ID;
                       Flags     : in System.Unsigned_Types.Unsigned := 0)

                                   return ToolBox_Event_Code_Type;

   --
   -- Returns the text displayed in the action button.
   --
   function Get_Text (Window    : in Object_ID;
                      Component : in Component_ID;
                      Flags     : in System.Unsigned_Types.Unsigned := 0)

                                  return String;

   --
   -- Allows the client to specify the object to show when the user clicks on the action button.
   --
   procedure Set_Click_Show (Window     : in Object_ID;
                             Component  : in Component_ID;
                             Object     : in Object_ID;
                             Show_Flags : in ActionButton_Show_Type;
                             Flags      : in System.Unsigned_Types.Unsigned := 0);

   --
   -- Sets the event code which will be executed when the action button is pressed.
   --
   procedure Set_Event (Window    : in Object_ID;
                        Component : in Component_ID;
                        Event     : in ToolBox_Event_Code_Type;
                        Flags     : in System.Unsigned_Types.Unsigned := 0);

   --
   --Sets the text to be displayed in the action button.
   --
   procedure Set_Text (Window    : in Object_ID;
                       Component : in Component_ID;
                       Text      : in string;
                       Flags     : in System.Unsigned_Types.Unsigned := 0);

   procedure Handle (The : in ATEL_Toolbox_ActionButton_Selected) is abstract;

end RASCAL.ToolboxActionButton;
