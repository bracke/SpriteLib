with RASCAL.Utility;             use RASCAL.Utility;

with Main;                       use Main;
with Ada.Exceptions;

package body Controller_Choices is

   --

   package Utility renames RASCAL.Utility;
   
   --

   procedure Handle (The : in TEL_ViewChoices_Type) is
   begin
      Call_OS_CLI("Filer_Run <SpriteLib$Dir>.!Configure");
   exception
      when ex: others => Report_Error("CONFIGURE",Ada.Exceptions.Exception_Information (ex));
   end Handle;

   --

end Controller_Choices;
