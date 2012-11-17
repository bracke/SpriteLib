with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.ToolboxWindow;       use RASCAL.ToolboxWindow;
with RASCAL.FileInternal;        use RASCAL.FileInternal;

with Main;                       use Main;
with Ada.Exceptions;
with Reporter;

package body Controller_Quit is

   --

   package Utility       renames RASCAL.Utility;
   package WimpTask      renames RASCAL.WimpTask;     
   package ToolboxWindow renames RASCAL.ToolboxWindow;
   package FileInternal  renames RASCAL.FileInternal;

   --
   
   procedure Save_WindowPosition is

      Target : FileHandle_Type(new UString'(U("<Choices$Write>.SpriteLib.Misc")),Write);
   begin
      if ToolboxWindow.Is_Open(main_objectid) then
         ToolboxWindow.Get_WindowPosition (main_objectid,x_pos,y_pos);
      end if;
      FileInternal.Put_String (Target,"XPOS:" & intstr(x_pos));
      FileInternal.Put_String (Target,"YPOS:" & intstr(y_pos));
   exception
      when e: others => Report_Error("POSSAVE",Ada.Exceptions.Exception_Information (e));
   end Save_WindowPosition;

   --

   procedure Handle (The : in TEL_Quit_Quit) is
   begin
      Save_WindowPosition;
      Set_Status(Main_Task,false);
   exception
      when e: others => Report_Error("TQUIT",Ada.Exceptions.Exception_Information (e));
   end Handle;
   
   --

   procedure Handle (The : in MEL_Message_Quit) is
   begin
      Save_WindowPosition;
      Set_Status(Main_Task,false);
   exception
      when e: others => Report_Error("MQUIT",Ada.Exceptions.Exception_Information (e));      
   end Handle;

   --
        
end Controller_Quit;
