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

with Gtk.Widget;      use Gtk.Widget;
with Gtk.GEntry;
with Gtk.Entry_Completion;
with Gtk.Tree_Model;
with Gtk.List_Store;

with Glib;            use Glib;
with Glib.Error;      use Glib.Error;
with Gtk.Main;        use Gtk.Main;
with Gtkada.Builder;  use Gtkada.Builder;
with Glib.Values;

with Ada.Text_IO;
with Arun.Handlers;
with Arun.View;

package body Arun is

   --  Rig up the autocomplete support for the "commandEntry"
   --
   procedure Rig_Autocomplete (Builder : in Arun.View.Arun_Builder);

   procedure Main is
      use Ada.Text_IO;
      use Gtkada.Builder;
      use Arun.View;

      Builder     : Arun_Builder;
      Error       : aliased Glib.Error.GError;
      Return_Code : Guint;

   begin

      Gtk.Main.Init;
      Builder := new Arun_Builder_Record;
      Gtkada.Builder.Initialize (Builder);
      Builder.Launcher.Initialize;

      Return_Code :=
        Add_From_Resource
          (Builder       => Builder,
           Resource_Path => "/io/lasagna/arun/resources/arun.glade",
           Error         => Error'Access);
      if Error /= null then
         Put_Line ("Error : " & Get_Message (Error));
         Error_Free (Error);
         return;
      elsif Return_Code /= 0 then
         Put_Line ("Error " & Return_Code'Image);
         --  In normal operations this returns a "1" error, not yet sure what that
         --  relates to.
         --  return;
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
      Rig_Autocomplete (Builder);

      Gtk.Widget.Show_All (Builder.From_Object ("commandWindow"));
      Gtk.Main.Main;
      Unref (Builder);
   end Main;

   procedure Rig_Autocomplete (Builder : in Arun.View.Arun_Builder) is

      use Gtk.Entry_Completion;
      use Gtk.List_Store;
      use Gtk.Tree_Model;
      use Ada.Strings.Unbounded;

      Completion_Types   : constant GType_Array (1 .. 1) :=
                             (1 => GType_String);
      Items              : constant Gtk_List_Store :=
                             Gtk_List_Store_Newv (Types => Completion_Types);
      Iter               : Gtk_Tree_Iter;
      Command_Entry      : constant Gtk.GEntry.Gtk_Entry :=
                             Gtk.GEntry.Gtk_Entry (Builder.From_Object
                                                     ("commandEntry"));
      Command_Completion : aliased constant Gtk_Entry_Completion :=
                             Gtk_Entry_Completion_New;

      Completion_String  : Glib.Values.GValue;

      Executables        : constant String_Vectors.Vector :=
                             Builder.Launcher.Discover_Executables;
   begin
      for Element of Executables loop
         Items.Append (Iter);
         Glib.Values.Init_Set_String (Completion_String,
                                      To_String (Element));
         Items.Set_Value (Iter, 0, Completion_String);
      end loop;

      Command_Completion.Set_Model (Items.To_Interface);
      Command_Completion.Set_Text_Column (Column => 0);
      Command_Completion.Set_Inline_Completion (True);
      Command_Completion.Set_Inline_Selection (True);
      Command_Entry.Set_Completion (Completion => Command_Completion);
      Command_Entry.On_Key_Release_Event
        (Call  => Arun.Handlers.Search_Keypress'Access,
         After => False);
   end Rig_Autocomplete;

end Arun;
