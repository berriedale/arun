---
-- Basic GtkAda handlers for Arun
---

with Gtkada.Builder; use Gtkada.Builder;

package Arun.Handlers is

   procedure Quit (Object : access Gtkada_Builder_Record'Class);

   procedure Search_Changed (Object : access Gtkada_Builder_Record'Class);

end Arun.Handlers;
