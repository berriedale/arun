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

with Arun;

private with GNAT.String_Split;

package Arun.Launchers.Unix is
   type UnixLauncher is new Arun.Launcher_Type with private;

   procedure Initialize (L : in out UnixLauncher);

private

   type UnixLauncher is new Arun.Launcher_Type with record
      Initialized     : Boolean := False;
      Path_Components : Gnat.String_Split.Slice_Set;
   end record;

end Arun.Launchers.Unix;
