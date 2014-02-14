
--  This subprogram generates the test driver by including all the
--  step definitions provided by the user, as well as the predefined
--  steps, regular epressions and mockups.

with BDD.Codegen;    use BDD.Codegen;
with GNATCOLL.VFS;   use GNATCOLL.VFS;

procedure Main is
   Finder : Steps_Finder;
begin
   Discover_Steps
     (Finder,
      Extension => ".ads",
      Directory => Create_From_Base ("features/step_definitions"));
end Main;
