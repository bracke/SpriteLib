with RASCAL.Font;                 use RASCAL.Font;
with RASCAL.OS;                   use RASCAL.OS;

package Controller_Window is

   type TEL_Again_Type     is new Toolbox_UserEventListener(16#62#,-1,-1) with null record;
   type TEL_Paused         is new Toolbox_UserEventListener(16#60#,-1,-1) with null record;
   type TEL_OpenPath_Type  is new Toolbox_UserEventListener(16#61#,-1,-1) with null record;

   type MEL_Message_FontChanged is new AMEL_Message_FontChanged with null record;

   --
   -- The user wants reprocess the previous file
   --
   procedure Handle (The : in TEL_Again_Type);

   --
   -- The user has clicked upon the 'Pause' button.
   --
   procedure Handle (The : in TEL_Paused);

   --
   -- The user wants to view the target directory.
   --
   procedure Handle (The : in TEL_OpenPath_Type);

   --
   -- The desktop font has changed - update display.
   --
   procedure Handle (The : in MEL_Message_FontChanged);

   
private
end Controller_Window;
