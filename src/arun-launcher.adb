------------------------------------------------------------------------------
--
--  Copyright (C) 2017 R. Tyler Croy <tyler@linux.com>
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
------------------------------------------------------------------------------

with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Command_Line.Environment;
with Ada.Environment_Variables;

with GNAT.OS_Lib;
with GNAT.String_Split;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

package body Arun.Launcher is

   function Find_Full_Path (Snippet : in String) return String is
      use GNAT.String_Split;

      PATH : constant String := Ada.Environment_Variables.Value ("PATH");
      Separator : constant String := ":";
      Path_Components : Slice_Set;

   begin

      Create (S          => Path_Components,
              From       => PATH,
              Separators => Separator,
              Mode       => Single);


      for Index in 1 .. Slice_Count (Path_Components) loop
         Put_Line ("Looking for an executable in " & Slice (S     => Path_Components,
                          Index => Index));
         declare
            Path_Component : constant String := Slice (S     => Path_Components,
                                                       Index => Index);
            Computed_Location : constant String := Path_Component & "/" & Snippet;
         begin

            if GNAT.OS_Lib.Is_Executable_File (Computed_Location) then
               Put_Line ("Found executable at " & Computed_Location);
               return Computed_Location;
            end if;
         end;

      end loop;


      return "";
   end Find_Full_Path;


   procedure Execute (Executable_Path : in String) is
      use GNAT.OS_Lib;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Ada.Command_Line.Environment;


      function Exec_And_Replace (Filename : in Char_Array;
                                 Arguments : in Chars_Ptr_Array) return Integer
        with Import,
        Convention => C,
        Link_Name => "execv";

      procedure Print_Errno (Message : in String)
        with Import,
        Convention => C,
          Link_Name => "perror";

      Default_Args : Chars_Ptr_Array  (1 .. 2) := (1 => New_String (Executable_Path), 2 => Null_Ptr);
      Status : Integer;
   begin
      Put_Line ("Spawning " & Executable_Path);

      Status := Exec_And_Replace (Filename  => To_C (Item       => Executable_Path,
                                                     Append_Nul => True),
                                  Arguments => Default_Args);

      -- If the Exec_And_Replace function returns then something has gone wrong
      Put_Line ("Spawned " & Executable_Path & " with " & Status'Img);

      if Status /= 0 then
         Print_Errno ("Something went wrong");
      end if;

   end Execute;

end Arun.Launcher;
