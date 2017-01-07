---
-- Basic GtkAda handlers for Arun
---

with Ada.Text_IO;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Search_Entry;
with Gtkada.Builder; use Gtkada.Builder;

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


   procedure Execute_Command (Object : access Gtkada_Builder_Record'Class) is
      use Ada.Text_IO;
      use Gtk.Search_Entry;
      use Gtkada.Builder;

      Widget : Gtk_Search_Entry := Gtk_Search_Entry (Get_Object (Object, "commandEntry"));
   begin
      Put_Line ("Should Execute: " & Widget.Get_Text);
      Gtk.Main.Main_Quit;
   end Execute_Command;

end Arun.Handlers;
