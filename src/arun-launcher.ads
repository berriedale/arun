

package Arun.Launcher is

   function Find_Full_Path (Snippet : in String) return String;
   -- Determine the full path of a Snippet based on the PATH environment variable

   procedure Execute (Executable_Path : in String);
   -- Spawn the process for the Executable_Path
end Arun.Launcher;
