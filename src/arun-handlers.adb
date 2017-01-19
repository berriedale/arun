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

with Ada.Text_IO;
with GNAT.OS_Lib;
with Gdk.Event;
with Gdk.Types.Keysyms;
with Glib;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Search_Entry;
with Gtkada.Builder; use Gtkada.Builder;

with GNAT.String_Split;

with Arun;
with Arun.Launchers.Unix;
with Arun.View; use Arun.View;

package body Arun.Handlers is

   procedure Quit (Object : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Ada.Text_IO.Put_Line ("Exiting arun");
      Gtk.Main.Main_Quit;
   end Quit;

   procedure Search_Changed (Object : access Gtkada_Builder_Record'Class) is
      use Ada.Text_IO;
      use Gtk.Search_Entry;
      use Gtkada.Builder;

      Widget : Gtk_Search_Entry := Gtk_Search_Entry (Get_Object (Object, "commandEntry"));
   begin
      Put_Line ("Searching for " & Widget.Get_Text);
   end Search_Changed;


   function Slice_Command (Text : in String) return GNAT.String_Split.Slice_Set is
      use GNAT.String_Split;

      Separator : constant String := " ";
      Slices    : Slice_Set;
   begin

      Create (Slices, Text, Separator, Single);

      return Slices;
   end Slice_Command;

   procedure Execute_Command (Object : access Gtkada_Builder_Record'Class) is
      use Ada.Text_IO;
      use Gtk.Search_Entry;
      use Gtkada.Builder;
      use GNAT.String_Split;

      Widget  : Gtk_Search_Entry := Gtk_Search_Entry (Get_Object (Object, "commandEntry"));
      Builder : Arun.View.Arun_Builder_Record renames Arun.View.Arun_Builder_Record (Object.all);
      L       : Arun.Launchers.Unix.UnixLauncher renames Arun.Launchers.Unix.UnixLauncher (Builder.Launcher);


      Slices  : constant Slice_Set := Slice_Command (Widget.Get_Text);
      Command : constant String := Slice (S => Slices, Index => 1);
      Full_Path :  aliased constant String := L.Find_Full_Path (Command);
   begin

      if Command (1) = '/' then
         -- If the command starts with a slash, it's likely already a full path so
         -- just try to execute it!
         L.Execute (Command, Slices);
      end if;

      if Full_Path /= "" then
         Put_Line ("Should Execute: " & Full_Path);
         L.Execute (Full_Path, Slices);
      end if;

      Gtk.Main.Main_Quit;
   end Execute_Command;

   function Search_KeyPress (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                             Event  : in Gdk.Event.Gdk_Event_Key) return Boolean is
      use Ada.Text_IO;
      use Gdk.Types;
      use Gdk.Types.Keysyms;
   begin

      if Event.Keyval = GDK_Tab then
         -- When the Tab key is presented, let's assume the user is finishing
         -- an auto-complete operation jump to the end so
         -- they can add arguments if desired
         declare
            use Glib;
            Search_Entry : Gtk.Search_Entry.Gtk_Search_Entry_Record renames Gtk.Search_Entry.Gtk_Search_Entry_Record (Widget.all);
         begin
            Search_Entry.Set_Position (-1);
         end;
      end if;


      if Event.Keyval = GDK_Escape then
         Put_Line ("Escape! Exiting arun");
         Gtk.Main.Main_Quit;
      end if;

      return True;
   end Search_KeyPress;

end Arun.Handlers;
