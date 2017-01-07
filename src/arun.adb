with Gtk; use Gtk;
with Gtk.Box;         use Gtk.Box;

with Gtk.Label;       use Gtk.Label;
with Gtk.Widget;      use Gtk.Widget;

with Glib; use Glib;
with Glib.Error;     use Glib.Error;
with Gtk.Main; use Gtk.Main;
with Gtk.Window;      use Gtk.Window;

with Ada.Text_IO;
with Arun.Handlers;

with Gtkada.Builder; use Gtkada.Builder;

package body Arun is
   procedure Main is
      Builder     : Gtkada_Builder;
      Error       : aliased Glib.Error.GError;
      Return_Code : Guint;

      use Ada.Text_IO;
   begin
      --  Initialize GtkAda.
      Gtk.Main.Init;
      Put_Line ("Starting arun");

      --  Create a window with a size of 400x400
      Gtk_New (Builder);


      Return_Code := Add_From_File (Builder  => Builder,
                                    Filename => "arun.glade",
                                    Error    => Error'Access);
      if Error /= null then
         Put_Line ("Error : " & Get_Message (Error));
         Error_Free (Error);
         return;
      end if;

      --     Step 2: add calls to "Register_Handler" to associate your
      --     handlers with your callbacks.
      Register_Handler (Builder      => Builder,
                        Handler_Name => "Main_Quit",
                        Handler      => Arun.Handlers.Quit'Access);
      Register_Handler (Builder      => Builder,
                        Handler_Name => "commandEntry_search_changed_cb",
                        Handler      => Arun.Handlers.Search_Changed'Access);

      -- Step 3: call Do_Connect. Once to connect all registered handlers
      Do_Connect (Builder);

      --  Find our main window, then display it and all of its children.
      Gtk.Widget.Show_All ( Gtk_Widget (Gtkada.Builder.Get_Object (Builder, "commandWindow")));
      Gtk.Main.Main;

      -- Step 4: when the application terminates or all Windows described through
      --         your builder should be closed, call Unref to free memory
      --         associated with the Builder.
      Unref (Builder);
   end Main;

end Arun;
