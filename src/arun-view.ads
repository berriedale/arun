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
--  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
--  02110-1301, USA.
------------------------------------------------------------------------------

with Gtkada.Builder; use Gtkada.Builder;
with Arun.Launchers.Unix;

with Gtk.Widget;

package Arun.View is

   type Arun_Builder_Record is new Gtkada_Builder_Record with record
      Launcher : Arun.Launchers.Unix.UnixLauncher;
   end record;
   type Arun_Builder is access all Arun_Builder_Record'Class;

   function From_Object (Builder     : out Arun_Builder_Record'Class;
                         Object_Name : in String) return Gtk.Widget.Gtk_Widget;

end Arun.View;
