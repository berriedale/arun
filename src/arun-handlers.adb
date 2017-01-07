---
-- Basic GtkAda handlers for Arun
---

with Ada.Text_IO;
with Gtk.Main;
with Gtkada.Builder;

package body Arun.Handlers is

   procedure Quit (Object : access Gtkada.Builder.Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Ada.Text_IO.Put_Line ("Exiting arun");
      Gtk.Main.Main_Quit;
   end Quit;
end Arun.Handlers;
