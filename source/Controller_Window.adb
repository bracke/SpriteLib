with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Main;                              use Main;
with Ada.Exceptions;
with Reporter;

with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.ToolboxWindow;
with RASCAL.ToolboxActionButton;
with RASCAL.ToolboxWritableField;
with RASCAL.FileExternal;
with RASCAL.ToolboxDisplayField;

package body Controller_Window is

   --

   package Utility              renames RASCAL.Utility;
   package ToolboxWindow        renames RASCAL.ToolboxWindow;       
   package ToolboxActionButton  renames RASCAL.ToolboxActionButton; 
   package ToolboxWritableField renames RASCAL.ToolboxWritableField;
   package FileExternal         renames RASCAL.FileExternal;        
   package ToolboxDisplayField  renames RASCAL.ToolboxDisplayField;

   --

   procedure Handle (The : in TEL_Again_Type) is
   begin
      if Length(LastSprite) /= 0 then
         if FileExternal.Exists(S(LastSprite)) then
            Main.new_SpriteFile(LastSprite);
         else
            Main.Report_Error("REPEATEXIST","");
         end if;
      end if;
   end Handle;

   --

   procedure Handle (The : in TEL_Paused) is
   begin
      Paused := not Paused;
      if Processing then
         if not Paused then
            --ToolboxWindow.Gadget_UnFade(Main_ObjectID,8);
            ToolboxActionButton.Set_Text(Main_ObjectID,8,S(Pause_String));
         else
            --ToolboxWindow.Gadget_UnFade(Main_ObjectID,8);
            ToolboxActionButton.Set_Text(Main_ObjectID,8,S(Continue_String));
         end if;
      end if;
   end Handle;

   --

   procedure Handle (The : in TEL_OpenPath_Type) is

      Path : String := ToolboxWritableField.Get_Value (Main_ObjectID,6);
   begin
      if Path'Length = 0 then
         Call_OS_CLI ("Filer_OpenDir <SpriteLib$Dir>.Library");
         ToolboxWritableField.Set_Value (Main_ObjectID,6,"<SpriteLib$Dir>.Library");
      else
         Call_OS_CLI ("Filer_OpenDir " & Path);
      end if;
   end Handle;

   --

   procedure Handle (The : in MEL_Message_FontChanged) is
   begin
      ToolboxDisplayField.Set_TruncatedValue(Main_ObjectID,16#b#,S(LastSprite));
   exception
      when Exception_Data : others => Report_Error("HANDLE_FONTCHANGED",Ada.Exceptions.Exception_Information (Exception_Data));
   end Handle; 
end Controller_Window;
