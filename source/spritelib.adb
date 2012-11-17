with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Main;                       use Main;
with Ada.Exceptions;
with Reporter;

with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.FileExternal;        use RASCAL.FileExternal;
with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.Sprite;              use RASCAL.Sprite;
with RASCAL.ToolboxNumberRange;  use RASCAL.ToolboxNumberRange;
with RASCAL.OS;                  use RASCAL.OS;
with RASCAL.ToolboxWritableField;
with RASCAL.ToolboxDisplayField;
with RASCAL.ToolboxActionButton;
with RASCAL.ToolboxWindow;

package body SpriteLib is

   --

   package WimpTask             renames RASCAL.WimpTask;
   package FileExternal         renames RASCAL.FileExternal;        
   package Utility              renames RASCAL.Utility;             
   package Sprite               renames RASCAL.Sprite;              
   package ToolboxNumberRange   renames RASCAL.ToolboxNumberRange;  
   package OS                   renames RASCAL.OS;                  
   package ToolboxWritableField renames RASCAL.ToolboxWritableField;
   package ToolboxDisplayField  renames RASCAL.ToolboxDisplayField; 
   package ToolboxActionButton  renames RASCAL.ToolboxActionButton; 
   package ToolboxWindow        renames RASCAL.ToolboxWindow;       
                                                           
   --

   procedure Go(Path : String) is

      Illegal_Letters : constant String := "$&%@\^:.,#*|" & '"';
      Legal_Letters   : constant String := S(Substitution);
      Legalise        : Character_Mapping := To_Mapping(Illegal_Letters,Legal_Letters);
      Target_path     : Unbounded_String := U(ToolboxWritableField.Get_Value (Main_ObjectID,6));
      
      Sprite_Area     : Sprite_Area_Type;
      File_Exists     : Boolean := false;
      Nr_Of_Sprites   : Natural := 0;
      SpriteFile      : Unbounded_String;
      SpriteName      : Unbounded_String;
      Target_Sprite   : Unbounded_String;
   begin
      Processing := true;
      ToolboxDisplayField.Set_Value(Main_ObjectID,1,Path);

      if Ada.Strings.Unbounded.Length(Target_Path) = 0 then
         Target_Path := U("<SpriteLib$Dir>.Library");
      else
         File_Exists := Exists(S(Target_Path));

         if not File_Exists then
            Create_Directory(S(Target_Path));
         end if;
         File_Exists := Exists(S(Target_Path));
         if not File_Exists then
            Target_Path:=U("<SpriteLib$Dir>.Library");
         end if;
      end if;
      ToolboxWritableField.Set_Value(Main_ObjectID,6,S(Target_Path));

      Sprite.Add_Sprite(Sprite_Area,Path);
      Nr_Of_Sprites := Sprite.Count_Sprites(Sprite_Area);

      -- Set slider to zero
      ToolboxNumberRange.Set_Bounds(Main_ObjectID,2,Upper,Nr_Of_Sprites);
      ToolboxNumberRange.Set_Bounds(Main_ObjectID,2,Current_Size,1);
      ToolboxNumberRange.Set_Value(Main_ObjectID,2,0);
      ToolboxNumberRange.Set_Value(Main_ObjectID,7,Nr_Of_Sprites);

      ToolboxWindow.Gadget_UnFade(Main_ObjectID,8);
      ToolboxActionButton.Set_Text(Main_ObjectID,8,S(Pause_String));
      Single_Poll(Main_Task);

      for x in 1..Nr_Of_Sprites
      loop
         SpriteName := U(Sprite.Get_SpriteName(Sprite_Area,x));

         SpriteName := Translate (SpriteName,Legalise);

         ToolboxDisplayField.Set_Value(Main_ObjectID,5,To_String(SpriteName));

         Target_Sprite := U(S(Target_Path) & "." & To_String(SpriteName));
         if FileExternal.Exists (S(Target_Sprite)) then
            declare
               New_Area    : Sprite_Area_Type;
            begin
               Sprite.Copy_Sprite(Sprite_Area,x,New_Area);
               Sprite.Add_Sprite (New_Area,S(Target_Sprite));
               Sprite.Save_SpriteArea(New_Area,S(Target_Sprite));
            exception
               when e : others => Report_Error("ADDSPRITE",
                                               Ada.Exceptions.Exception_Information (e));
            end;
         else
            Sprite.Save_Sprite (Sprite_Area,x,S(Target_Sprite));
         end if;

         ToolboxNumberRange.Set_Value(Main_ObjectID,2,x);

         Single_Poll(Main_Task);
         while Paused and Boolean(Get_Status(Main_Task)) loop
            Single_Poll(Main_Task);
         end loop;
         exit when not Boolean(Get_Status(Main_Task));
      end loop;

      Processing := false;
      ToolboxWindow.Gadget_Fade(Main_ObjectID,8);
      ToolboxActionButton.Set_Text(Main_ObjectID,8,S(Pause_String));
      ToolboxDisplayField.Set_Value(Main_ObjectID,1,S(Ready_String));
      ToolboxDisplayField.Set_Value(Main_ObjectID,5,"");
      ToolboxNumberRange.Set_Value(Main_ObjectID,2,0);
      ToolboxNumberRange.Set_Value(Main_ObjectID,7,0);

      LastSprite := U(Path);
      ToolboxDisplayField.Set_TruncatedValue(Main_ObjectID,16#b#,S(LastSprite));
      
      ToolboxWindow.Gadget_UnFade(Main_ObjectID,16#c#);
   exception
      when e : others => Report_Error("PROCESS",Ada.Exceptions.Exception_Information (e));
   end Go;

   --

   function ScanDir(Path : in String;
                    Name : in String) return String is

      Itemname       : Unbounded_String;
      Itemnr         : Integer:=0;
      Loadadr        : Integer:=0;
      Execadr        : Integer:=0;
      Length         : Integer:=0;
      Attributes     : Integer:=0;
      Itemtype       : Integer:=0;
      Target_Sprite  : Unbounded_String;
   begin
      -- Scan directory
      loop
         FileExternal.Get_Directory_Entry (Path,Itemnr,Itemname,Loadadr,Execadr,Length,Attributes,Itemtype);

         -- Are there any objects left ?
         if Itemnr=-2 then
            -- No - return string - exit
            return To_String(target_Sprite);
         else
            -- is it a directory ?
            if FileExternal.Get_Object_Type(path & "." & To_String(itemname))=dir then
               -- if yes then scan that directory
               target_sprite:=To_Unbounded_String(ScanDir(path & "." & To_String(itemname),name));
            elsif To_String(itemname)=name then
               -- if no then return the full path of the file
               return path & "." & To_String(itemname);
            end if;
         end if;
      end loop;
      return To_String(target_sprite);
   end ScanDir;

   --

end SpriteLib;
