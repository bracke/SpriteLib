with RASCAL.ToolboxQuit;          use RASCAL.ToolboxQuit;
with RASCAL.TaskManager;          use RASCAL.TaskManager;

package Controller_Quit is
   
   type TEL_Quit_Quit    is new ATEL_Toolbox_Quit_Quit with null record;
   type MEL_Message_Quit is new AMEL_Message_Quit      with null record;

   procedure Handle (The : in TEL_Quit_Quit);
   procedure Handle (The : in MEL_Message_Quit);

end Controller_Quit;
