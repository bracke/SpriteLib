with RASCAL.OS;                use RASCAL.OS;

package Controller_Choices is

   type TEL_ViewChoices_Type is new Toolbox_UserEventListener(16#10#,-1,-1) with null record;

   --
   -- The user wants to view the choices.
   --
   procedure Handle (The : in TEL_ViewChoices_Type);

end Controller_Choices;
