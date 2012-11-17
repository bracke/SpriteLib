with RASCAL.OS;                         use RASCAL.OS;
with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.FileInternal;               use RASCAL.FileInternal;
with RASCAL.Toolbox;                    use RASCAL.Toolbox;
with RASCAL.Mode;                       use RASCAL.Mode;
with RASCAL.ToolboxWritableField;
with RASCAL.ToolboxWindow;
with RASCAL.FileExternal;

with Main;                       use Main;
with Interfaces.C;               use Interfaces.C;
with Reporter;
with ADa.Exceptions;

package body Controller_DataLoad is

   --

   package OS                   renames RASCAL.OS;
   package Utility              renames RASCAL.Utility;             
   package FileInternal         renames RASCAL.FileInternal;        
   package Toolbox              renames RASCAL.Toolbox;             
   package Mode                 renames RASCAL.Mode;                
   package ToolboxWritableField renames RASCAL.ToolboxWritableField;
   package ToolboxWindow        renames RASCAL.ToolboxWindow;       
   package FileExternal         renames RASCAL.FileExternal;

   --
   
   procedure Open_Window is
   begin
      if x_pos > Mode.Get_X_Resolution (OSUnits) or
         y_pos > Mode.Get_Y_Resolution (OSUnits) or
                                       x_pos < 0 or y_pos < 0 then

         Toolbox.Show_Object (main_objectid,0,0,Centre);
      else
         Toolbox.Show_Object_At (main_objectid,x_pos,y_pos,0,0);
      end if;
      
   end Open_Window;

   --

   procedure Handle (The : in MEL_Message_DataLoad) is

      Path          : String  := To_Ada(The.Event.all.Full_Path);
      File_Type     : Integer := The.Event.all.File_Type;
   begin
      case File_Type is
      when 16#ff9#            => if not ToolboxWindow.Is_Open(main_objectid) then
                                    Open_Window;
                                 end if;
                                 new_SpriteFile(U(Path));

                                 -- Write backup
                                 if FileExternal.Exists(Choices_Write) then
                                    FileExternal.Close_AllInPath(Choices_Write);
                                 else
                                    FileExternal.Create_Directory(Choices_Write);
                                 end if;   
                                 if FileExternal.Exists(Choices_Write & ".Backup") then
                                    FileExternal.Delete_File (Choices_Write & ".Backup");
                                 end if;
                                 declare
                                    File : FileHandle_Type(new UString'(U(Choices_Write & ".Backup")),Write);
                                 begin
                                    FileInternal.Put_String (File,"VERSION:1");
                                    FileInternal.Put_String (File,"LASTSPRITE:" & Path);
                                    FileInternal.Put_String (File,"TARGETPATH:" & ToolboxWritableField.Get_Value(Main_ObjectID,16#6#));
                                 end;

      when 16#1000#..16#2000# => ToolboxWritableField.Set_Value(Main_ObjectID,6,Path);
                                 if not ToolboxWindow.Is_Open(main_objectid) then
                                    Open_Window;
                                 end if;
      when others             => null;
      end case;
   end Handle;

   --

   procedure Handle (The : in TEL_OpenWindow) is
   begin
      Open_Window;
   exception
      when Exception_Data : others => Report_Error("OPENWINDOW",Ada.Exceptions.Exception_Information (Exception_Data));
   end Handle; 
end Controller_DataLoad;
