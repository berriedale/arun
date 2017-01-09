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


with Gtk;             use Gtk;
with Gtk.Box;         use Gtk.Box;

with Gtk.Label;       use Gtk.Label;
with Gtk.Widget;      use Gtk.Widget;

with Glib;            use Glib;
with Glib.Error;      use Glib.Error;
with Gtk.Main;        use Gtk.Main;
with Gtk.Window;      use Gtk.Window;
with Gtkada.Builder;  use Gtkada.Builder;

with Ada.Text_IO;
with Arun.Handlers;

package body Arun is
   procedure Main is
      Builder     : Gtkada_Builder;
      Error       : aliased Glib.Error.GError;
      Return_Code : Guint;

      use Ada.Text_IO;
      use Gtkada.Builder;
   begin

      Gtk.Main.Init;
      Put_Line ("Starting arun");

      Gtk_New (Builder);

      Return_Code := Add_From_Resource (Builder       => Builder,
                                        Resource_Path => "/io/lasagna/arun/arun.glade",
                                        Error         => Error'Access);
      if Error /= null then
         Put_Line ("Error : " & Get_Message (Error));
         Error_Free (Error);
         return;
      end if;

      Register_Handler (Builder      => Builder,
                        Handler_Name => "Main_Quit",
                        Handler      => Arun.Handlers.Quit'Access);

      Register_Handler (Builder      => Builder,
                        Handler_Name => "commandEntry_search_changed_cb",
                        Handler      => Arun.Handlers.Search_Changed'Access);

      Register_Handler (Builder      => Builder,
                        Handler_Name => "commandEntry_activate_cb",
                        Handler      => Arun.Handlers.Execute_Command'Access);

      Do_Connect (Builder);

      declare
         Command_Entry : Gtk_Widget := Gtk_Widget (Get_Object (builder, "commandEntry"));
      begin
         Command_Entry.On_Key_Release_Event (Call  => Arun.Handlers.Search_KeyPress'Access,
                                             After => False);
      end;

      Gtk.Widget.Show_All ( Gtk_Widget (Get_Object (Builder, "commandWindow")));
      Gtk.Main.Main;

      Unref (Builder);
   end Main;

end Arun;
