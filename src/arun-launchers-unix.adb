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

with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.String_Split;
with Interfaces.C;
with Interfaces.C.Strings;


package body Arun.Launchers.Unix is

   procedure Initialize (L : in out UnixLauncher) is
      use Gnat.String_Split;

      PATH      : constant String := Ada.Environment_Variables.Value ("PATH");
      Separator : constant String := ":";
   begin

      Create (L.Path_Components, PATH, Separator, Single);
      L.Initialized := True;
   end Initialize;


   function Find_Full_Path (L            : in UnixLauncher;
                            Path_Snippet : in String) return String is
      use GNAT.String_Split;
   begin

      if L.Initialized /= True then
         Put_Line ("Uninitialized UnixLauncher!");
         return "";
      end if;

      for Index in 1 .. Slice_Count (L.Path_Components) loop
         Put_Line ("Looking for an executable in " & Slice (S     => L.Path_Components,
                          Index => Index));
         declare
            Path_Component : constant String := Slice (S     => L.Path_Components,
                                                       Index => Index);
            Computed_Location : constant String := Path_Component & "/" & Path_Snippet;
         begin

            if GNAT.OS_Lib.Is_Executable_File (Computed_Location) then
               Put_Line ("Found executable at " & Computed_Location);
               return Computed_Location;
            end if;
         end;

      end loop;

      return "";
   end Find_Full_Path;




   function Exec_And_Replace (Filename : in Interfaces.C.Char_Array;
                              Arguments : in Interfaces.C.Strings.Chars_Ptr_Array) return Integer
     with Import,
     Convention => C,
     Link_Name => "execv";

   procedure Print_Errno (Message : in String)
     with Import,
     Convention => C,
       Link_Name => "perror";

   procedure Execute (L               : in UnixLauncher;
                      Executable_Path : in String;
                      Argv            : in GNAT.String_Split.Slice_Set) is
      use GNAT.String_Split;
      use Interfaces.C;
      use Interfaces.C.Strings;

      Status : Integer;
      Argc : constant Size_T := Size_T (Slice_Count (Argv));
      Arguments : Chars_Ptr_Array (1 .. (Argc + 1)) := (others => Null_Ptr);
   begin
      -- For the first argument, we must replace the executable name with
      -- the full path, e.g. "xeyes" => "/usr/bin/xeyes"
      Arguments (1) := New_String (Executable_Path);
      for Index in 1 .. Argc loop
         Arguments (Index) := New_String (Slice (S     => Argv,
                                                 Index => Slice_Number (Index)));
      end loop;

      Put_Line ("Spawning " & Executable_Path);

      Status := Exec_And_Replace (Filename  => To_C (Item       => Executable_Path,
                                                     Append_Nul => True),
                                  Arguments => Arguments);

      -- If the Exec_And_Replace function returns then something has gone wrong
      if Status /= 0 then
         Print_Errno ("Something went wrong");
      end if;
   end Execute;


   function Discover_Executables (L : in UnixLauncher) return Arun.String_Vectors.Vector is
      use GNAT.Directory_Operations;
      use GNAT.OS_Lib;
      use GNAT.String_Split;
      use Ada.Strings.Unbounded;

      Dir : Dir_Type;
      File_Name : String (1 .. MAX_FILENAME_LENGTH);
      File_Index : Natural := 0;
      Executables : Arun.String_Vectors.Vector;
   begin
      for Index in 1 .. Slice_Count (L.Path_Components) loop
         declare
            Path_Dir : constant String := Slice (L.Path_Components, Index);
         begin
            if Is_Directory (Path_Dir) then
               Open (Dir, Path_Dir);
               loop
                  Read (Dir, File_Name, File_Index);
                  exit when File_Index = 0;
                  declare
                     Name : constant String := File_Name (1 .. File_Index);
                     Full_Path : constant String := Path_Dir & "/" & Name;
                  begin
                     if Is_Executable_File (Full_Path) then
                        Executables.Append (To_Unbounded_String (Name));
                     end if;
                  end;
               end loop;
               Close (Dir);
            end if;
         end;
      end loop;
      return Executables;
   end Discover_Executables;


end Arun.Launchers.Unix;
