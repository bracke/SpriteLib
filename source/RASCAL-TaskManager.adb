--------------------------------------------------------------------------------
--                                                                            --
-- Copyright (C) 2004, RISC OS Ada Library (RASCAL) developers.               --
--                                                                            --
-- This library is free software; you can redistribute it and/or              --
-- modify it under the terms of the GNU Lesser General Public                 --
-- License as published by the Free Software Foundation; either               --
-- version 2.1 of the License, or (at your option) any later version.         --
--                                                                            --
-- This library is distributed in the hope that it will be useful,            --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU           --
-- Lesser General Public License for more details.                            --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public           --
-- License along with this library; if not, write to the Free Software        --
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA    --
--                                                                            --
--------------------------------------------------------------------------------

-- $Author$
-- $Date$
-- $Revision$

with RASCAL.OS;
with RASCAL.memory;              use RASCAL.memory;
with RASCAL.Utility;             use RASCAL.Utility;

with System.Storage_Elements;    use System.Storage_Elements;
with kernel;                     use kernel;
with Reporter;

package body RASCAL.TaskManager is

   TaskManager_EnumerateTasks           : constant Interfaces.C.unsigned := 16#42681#;
   TaskManager_TaskNameFromHandle       : constant Interfaces.C.unsigned := 16#42680#;
   Wimp_StartTask                       : constant Interfaces.C.unsigned := 16#400DE#;

   --

   function Start_Task (Command : in String) return Integer is
   
      Command_0    : String := Command & ASCII.NUL;
      Error        : oserror_access;
      Register     : aliased Kernel.SWI_Regs;
   begin
      Register.R(0) := Adr_To_Int (Command_0'Address);
      Error := Kernel.SWI (Wimp_StartTask, Register'Access, Register'Access);
      if Error /= null then
         pragma Debug(Reporter.Report("TaskManager.Start_Task: " & Interfaces.C.To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
      end if;
      return Integer (Register.R(0));
   end Start_Task;

   --

   function Enumerate_Task (Buffer : in Task_Type;
                            Index  : in integer := 0) return integer is

      Register : aliased Kernel.swi_regs;
      Error    : oserror_access;
   begin
      Register.R(0) := int(Index);
      Register.R(1) := int(To_Integer(Buffer'Address));
      Register.R(2) := 16;
      Error := Kernel.SWI (TaskManager_EnumerateTasks,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("TaskManager.Enumerate_Task: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return -1;
      else
         return Integer(Register.R(0));
      end if;
   end Enumerate_Task;

   --

   function Get_TaskName (Handle : in integer) return String is

      Register            : aliased Kernel.swi_regs;
      Error               : oserror_access;
   begin
      Register.R(0) := int (Handle);
      Error := Kernel.SWI (TaskManager_TaskNameFromHandle,register'Access,register'Access);

      if Error /= null then
         pragma Debug(Reporter.Report("TaskManager.Get_TaskName: " & To_Ada(Error.ErrMess)));
         OS.Raise_Error(Error);
         return "";
      else
         return MemoryToString (Int_To_Adr (Register.R (0)));
      end if;
   end Get_Taskname;

   --

   function Is_Task (Name : in String) return boolean is

      Buffer  : Task_Type;
      i       : integer   := Enumerate_Task (Buffer);
   begin
      loop
        if Name = MemoryToString (Buffer.Name) then
           return true;
        end if;
        exit when i < 0;

        i := Enumerate_Task (Buffer,i);
      end loop;
      return false;
   end is_Task;

   --

   function Nr_Of_Tasks return Natural is

      Buffer  : Task_Type;
      i       : integer   := Enumerate_Task (Buffer);
      Nr      : Natural   := 0;
   begin
      loop
         exit when i < 0;
         Nr := Nr + 1;
         i  := Enumerate_Task (Buffer,i);
      end loop;
      return Nr;
   end Nr_Of_Tasks;

   --

   function Get_Task_List return Task_List_Type is

      Tasks     : Natural                   := Nr_Of_Tasks;
      Task_List : Task_List_Type (1..Tasks);
      Task_Index: Positive                  := 1;
      i         : Integer                   := Enumerate_Task (Task_List (Task_Index));
   begin
      loop
         exit when i < 0;
         Task_Index := Task_Index + 1;
         i := Enumerate_Task (Task_List (Task_Index),i);
      end loop;
      return Task_List;
   end Get_Task_List;

   --

end RASCAL.TaskManager;
