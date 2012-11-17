with RASCAL.WimpTask;                   use RASCAL.WimpTask;
with RASCAL.Utility;                    use RASCAL.Utility;
with RASCAL.OS;                         use RASCAL.OS;
with RASCAL.Variable;                   use RASCAL.Variable;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Main is

   app_name       : constant String := "SpriteLib";
   Choices_Write  : constant String := "<Choices$Write>." & app_name;
   Choices_Read   : constant String := "Choices:" & app_name & ".Choices";

   processing     : boolean         := false;

   --
   Main_Task       : ToolBox_Task_Class;
   main_objectid   : Object_ID             := -1;
   main_winid      : Wimp_Handle_Type      := -1;
   Paused          : Boolean               := false;
   Untitled_String : Ada.Strings.Unbounded.Unbounded_String := U("Untitled");
   Ready_String    : Ada.Strings.Unbounded.Unbounded_String := U("Ready for the next file");
   Pause_String    : Ada.Strings.Unbounded.Unbounded_String := U("Pause");
   Continue_String : Ada.Strings.Unbounded.Unbounded_String := U("Continue");
   Substitution    : Ada.Strings.Unbounded.Unbounded_String := U("_____________");

   LastSprite      : Unbounded_String;
   TargetPath      : Unbounded_String;

   x_pos           : Integer := -1;
   y_pos           : Integer := -1;

   --

   procedure Report_Error (Token : in String;
                           Info  : in String);

   --

   procedure Main;

   --

   procedure new_SpriteFile(path : Ada.Strings.Unbounded.Unbounded_String);

   --

   procedure next_SpriteFile;

 end Main;


