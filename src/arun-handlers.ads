---
-- Basic GtkAda handlers for Arun
---
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

with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget;
with Gdk.Event;

with Arun.Launchers.Unix;

with Arun.View; use Arun.View;

package Arun.Handlers is

   procedure Quit (Object : access Gtkada_Builder_Record'Class);

   procedure Search_Changed (Object : access Gtkada_Builder_Record'Class);
   -- Whenever the search entry changes call this handler for autocompletion

   procedure Execute_Command (Object : access Gtkada_Builder_Record'Class);
   -- On "activate" of the search entry call this handler (basically when the user
   -- hits the enter key

   function Search_KeyPress (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                             Event  : in Gdk.Event.Gdk_Event_Key) return Boolean;
   -- On key-presses in the commandEntry field


end Arun.Handlers;
