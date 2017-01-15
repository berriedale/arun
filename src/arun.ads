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

with GNAT.String_Split;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Arun is
   procedure Main;

   type Launcher_Type is interface;

   type Discovered_Executable_Handler is access function (L : in Launcher_Type'Class;
                                                          Name : in String;
                                                          Full_Path : in String) return Boolean;

   procedure Initialize (L : in Launcher_Type'Class) is abstract;
   -- Launcher_Type-specific initialization routine

   function Find_Full_Path (L            : in Launcher_Type;
                            Path_Snippet : in String) return String is abstract;
   -- Determine the full path of the snippet based on PATH or other environment
   -- variables.
   --
   -- Will return an empty string if a full path was not discoverable.

   procedure Execute (L               : in Launcher_Type;
                      Executable_Path : in String;
                      Argv            : in GNAT.String_Split.Slice_Set) is abstract;
   -- Spawn the Executable in place of the current process

   function Compare_Strings (Left  : in Ada.Strings.Unbounded.Unbounded_String;
                             Right : in Ada.Strings.Unbounded.Unbounded_String) return Boolean;
   package String_Vectors is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                         Element_Type => Ada.Strings.Unbounded.Unbounded_String,
                                                         "="          => Compare_Strings);

   function Discover_Executables (L : in Launcher_Type) return String_Vectors.Vector is abstract;
   -- Discover executables which can be launched by the configured Launcher_Type

end Arun;
