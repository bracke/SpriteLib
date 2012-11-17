with Controller_Quit;           use Controller_Quit;
with Controller_Internet;       use Controller_Internet;
with Controller_Bugz;           use Controller_Bugz;
with Controller_Help;           use Controller_Help;
with Controller_DataLoad;       use Controller_DataLoad;
with Controller_Window;         use Controller_Window;
with Controller_Error;          use Controller_Error;
with Controller_Choices;        use Controller_Choices;
with Controller_Dummy;          use Controller_Dummy;
with SpriteLib;
with Reporter;
with Ada.Exceptions;

with RASCAL.DragNDrop;          use RASCAL.DragNDrop;
with RASCAL.MessageTrans;       use RASCAL.MessageTrans;
with RASCAL.Error;              use RASCAL.Error;
with RASCAL.FileInternal;       use RASCAL.FileInternal;
with RASCAL.FileExternal;
with RASCAL.Toolbox;            use RASCAL.Toolbox;
with RASCAL.ToolboxWindow;
with RASCAL.ToolboxDisplayField;
with RASCAL.ToolboxWritableField;
with RASCAL.ToolboxProgInfo;

package body Main is

   --

   package MessageTrans         renames RASCAL.MessageTrans;
   package Error                renames RASCAL.Error;              
   package FileInternal         renames RASCAL.FileInternal;       
   package FileExternal         renames RASCAL.FileExternal;       
   package Toolbox              renames RASCAL.Toolbox;            
   package ToolboxWindow        renames RASCAL.ToolboxWindow;      
   package ToolboxDisplayField  renames RASCAL.ToolboxDisplayField;
   package ToolboxWritableField renames RASCAL.ToolboxWritableField;
   package ToolboxProgInfo      renames RASCAL.ToolboxProgInfo;    
   package WimpTask             renames RASCAL.WimpTask;
   package Utility              renames RASCAL.Utility;  
   package OS                   renames RASCAL.OS;       
   package Variable             renames RASCAL.Variable;
   package DragNDrop            renames RASCAL.DragNDrop;
                       
   --

   procedure Read_Choices is

      Misc   : Messages_Handle_Type;
      MCB    : Messages_Handle_Type  := Open_File(Choices_Read);
      E      : Error.Error_Pointer   := Get_Error(Main_Task);
      M      : Error_Message_Pointer := new Error_Message_Type;
      R      : Error_Return_Type;
   begin
      begin
         Substitution := U(MessageTrans.Lookup("SUBSTITUTION",Get_Message_Block(Main_Task)));
      exception
         when others => Substitution := U("_");
      end;
      Close_File(MCB);
      if Length(Substitution) = 0 then
         Substitution := U("_");
      end if;
      if Length(Substitution) /= 13 then
         if Length(Substitution) < 13 then
            Substitution := 13*Element(Substitution,1);
         else
            Substitution := U(Slice(Substitution,1,13));
         end if;
      end if;
   exception
      when Messages_File_Is_Closed => M.all.Token(1..9) := "MSGCLOSED";
                                      R := Show_Message(E,M);
      when e2 : others => Report_Error("CHOICES",Ada.Exceptions.Exception_Information (e2));
   end Read_Choices;

   --

   procedure Report_Error (Token : in String;
                           Info  : in String) is

      E        : Error_Pointer          := Get_Error (Main_Task);
      M        : Error_Message_Pointer  := new Error_Message_Type;
      Result   : Error_Return_Type      := XButton1;
   begin
      M.all.Token(1..Token'Length) := Token;
      M.all.Param1(1..Info'Length) := Info;
      M.all.Category := Warning;
      M.all.Flags    := Error_Flag_OK;
      Result         := Error.Show_Message (E,M);
   end Report_Error;

   --

   procedure new_SpriteFile(path : Unbounded_String) is
   begin
      -- Is procedure Go busy ?
      if Processing then
         DragNDrop.Push_DragObject (Path);
      else
         if FileExternal.Exists(Choices_Read) then
            Read_Choices;
         end if;
         -- Procedure go is NOT busy - go for it!
         SpriteLib.Go(S(Path));
         next_SpriteFile;
      end if;
   end new_SpriteFile;

   --

   procedure next_SpriteFile is

      path   : Unbounded_String;
      Window : Wimp_Handle_Type;
      Icon   : Icon_Handle_Type;
   begin
      if (not Empty) and
         (not Processing) then

         -- Secure information
         DragNDrop.Pop_DragObject(Path,Window,Icon);
         -- Process file
         if FileExternal.Exists(Choices_Read) then
            Read_Choices;
         end if;
         SpriteLib.Go(S(Path));
         -- Next file
         next_SpriteFile;
      end if;
   end next_SpriteFile;

   --

   procedure Main is
      dummy           : Unbounded_String;
      ProgInfo_Window : Object_ID;
      Misc            : Messages_Handle_Type;
   begin      
      -- Messages
      Add_Listener (Main_Task,new MEL_Message_Bugz_Query);
      Add_Listener (Main_Task,new MEL_Message_DataLoad);      -- Icon drag'n'drop
      Add_Listener (Main_Task,new MEL_Message_Quit);          -- React upon quit from taskmanager
      Add_Listener (Main_Task,new MEL_Message_FontChanged);

      -- Toolbox Events
      Add_Listener (Main_Task,new TEL_Quit_Quit);
      Add_Listener (Main_Task,new TEL_ViewManual_Type);
      Add_Listener (Main_Task,new TEL_ViewSection_Type);
      Add_Listener (Main_Task,new TEL_ViewIHelp_Type);
      Add_Listener (Main_Task,new TEL_ViewHomePage_Type);
      Add_Listener (Main_Task,new TEL_SendEmail_Type);
      Add_Listener (Main_Task,new TEL_CreateReport_Type);
      Add_Listener (Main_Task,new TEL_Paused);
      Add_Listener (Main_Task,new TEL_ViewChoices_Type);
      Add_Listener (Main_Task,new TEL_Toolbox_Error);
      Add_Listener (Main_Task,new TEL_OpenPath_Type);
      Add_Listener (Main_Task,new TEL_Again_Type);
      Add_Listener (Main_Task,new TEL_OpenWindow);
      Add_Listener (Main_Task,new TEL_Dummy);
      
      -- Start task
      WimpTask.Set_Resources_Path(Main_Task,"<SpriteLibRes$Dir>");
      WimpTask.Initialise(Main_Task);

      if FileExternal.Exists("Choices:SpriteLib.Misc") then
         Misc := MessageTrans.Open_File("Choices:SpriteLib.Misc");
         begin
            Read_Integer ("XPOS",x_pos,Misc);
            Read_Integer ("YPOS",y_pos,Misc);
         exception
            when others => null;            
         end;
      end if;

      ProgInfo_Window := Toolbox.Create_Object("ProgInfo",From_Template);
      ToolboxProgInfo.Set_Version(ProgInfo_Window,MessageTrans.Lookup("VERS",Get_Message_Block(Main_Task)));

      dummy := U(MessageTrans.Lookup("PAUSE",Get_Message_Block(Main_Task)));
      if Length(dummy) > 0 then
         Pause_String := dummy;
      end if;
      dummy := U(MessageTrans.Lookup("CONTINUE",Get_Message_Block(Main_Task)));
      if Length(dummy) > 0 then
         Continue_String := dummy;
      end if;
      dummy := U(MessageTrans.Lookup("READY",Get_Message_Block(Main_Task)));
      if Length(dummy) > 0 then
         Ready_String := dummy;
      end if;
      dummy := U(MessageTrans.Lookup("UNTITLED",Get_Message_Block(Main_Task)));
      if Length(dummy) > 0 then
         Untitled_String := dummy;
      end if;
      
      main_objectid := Toolbox.Create_Object ("Main");
      ToolboxDisplayField.Set_Value(Main_ObjectID,1,S(Ready_String));

      -- Read backup
      ToolboxWindow.Gadget_Fade(Main_ObjectID,8);
      if FileExternal.Exists("Choices:" & app_name & ".Backup") then

         -- Version 1 backup file
         declare
            MCB : Messages_Handle_Type := Open_File("Choices:" & app_name & ".Backup");
         begin
            begin
               LastSprite := U(MessageTrans.Lookup("LASTSPRITE",MCB));
            exception
               when others => LastSprite := U("");
            end;
            begin
               TargetPath := U(MessageTrans.Lookup("TARGETPATH",MCB));
            exception
               when others => TargetPath := U("");
            end;
            Close_File(MCB);

            ToolboxDisplayField.Set_TruncatedValue(Main_ObjectID,16#b#,S(LastSprite));            
            if Length(TargetPath) = 0 then
               ToolboxWritableField.Set_Value (Main_ObjectID,6,"<SpriteLib$Dir>.Library");
            else
               ToolboxWritableField.Set_Value(Main_ObjectID,16#6#,S(TargetPath));
            end if;

            if Ada.Strings.Unbounded.Length(LastSprite) > 0 then
               ToolboxWindow.Gadget_UnFade(Main_ObjectID,16#c#);
            end if;
         exception
            when others => null;
         end;
      else
         -- Old backup file
         if FileExternal.Exists("Choices:" & app_name & ".LastFile") then
            declare
               File : FileHandle_Type(new UString'(U("Choices:" & app_name & ".LastFile")),Read);
            begin
               if not FileInternal.Is_EOF(File) then
                  ToolboxDisplayField.Set_Value(Main_ObjectID,16#b#,FileInternal.Read_Line (File));
                  ToolboxWindow.Gadget_UnFade(Main_ObjectID,16#c#);
               end if;
            exception
               when others => null;
            end;
         end if;
      end if;

      WimpTask.Poll(Main_Task);

   exception
      when e: others => Report_Error("UNTRAPPED",Ada.Exceptions.Exception_Information (e));
   end Main;

   --

end Main;

