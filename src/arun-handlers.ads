---
-- Basic GtkAda handlers for Arun
---

with Gtkada.Builder; use Gtkada.Builder;

package Arun.Handlers is

   procedure Quit (Object : access Gtkada_Builder_Record'Class);

   -- Whenever the search entry changes call this handler for autocompletion
   procedure Search_Changed (Object : access Gtkada_Builder_Record'Class);

   -- On "activate" of the search entry call this handler (basically when the user
   -- hits the enter key
   procedure Execute_Command (Object : access Gtkada_Builder_Record'Class);

end Arun.Handlers;
