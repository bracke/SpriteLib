with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.UserMessages;
with RASCAL.InteractiveHelp;
with RASCAL.StrongHelp;

with Main;                       use Main;
with Reporter;
with Ada.Exceptions;

package body Controller_Help is

   --

   package Utility         renames RASCAL.Utility;
   package Toolbox         renames RASCAL.Toolbox;         
   package WimpTask        renames RASCAL.WimpTask;        
   package UserMessages    renames RASCAL.UserMessages;    
   package InteractiveHelp renames RASCAL.InteractiveHelp; 
   package StrongHelp      renames RASCAL.StrongHelp;

   --

   procedure Handle (The : TEL_ViewManual_Type) is
   begin
      Call_OS_CLI("Filer_Run <SpriteLib$Dir>.!Help");
   exception
      when e: others => Report_Error("VIEWMANUAL",Ada.Exceptions.Exception_Information (e));
   end Handle;

   --

   procedure Handle (The : TEL_ViewSection_Type) is

      Menu_Entry  : Component_ID := Get_Self_Component(Main_Task);
   begin
      if StrongHelp.Run then
         case Menu_Entry is
         when 16#0# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Intro");
         when 16#1# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Install");
         when 16#2# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Start");
         when 16#3# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Problems");
         when 16#4# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Bugz");
         when 16#5# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Language");
         when 16#6# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib History");
         when 16#7# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Third");
         when 16#8# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib WhatToDo");
         when 16#9# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib Shortcuts");
         when 16#a# => Call_OS_CLI("stronghelp <SpriteLibRes$Dir>.SpriteLib CLIUse");
         when others=> null;
         end case;
      end if;
   exception
      when e: others => Report_Error("VIEWCHAPTER",Ada.Exceptions.Exception_Information (e));      
   end Handle;

   --
   
   procedure Handle (The : in TEL_ViewIHelp_Type) is
   begin
      InteractiveHelp.Run;
   exception
      when e: others => Report_Error("VIEWIHELP",Ada.Exceptions.Exception_Information (e));      
   end Handle;

   --

end Controller_Help;
