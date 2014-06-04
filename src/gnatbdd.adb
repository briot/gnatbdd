
--  This subprogram generates the test driver by including all the
--  step definitions provided by the user, as well as the predefined
--  steps, regular expressions and mockups.

with BDD.Codegen;     use BDD.Codegen;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS;    use GNATCOLL.VFS;

procedure Gnatbdd is
   Finder : Steps_Finder;
begin
   GNATCOLL.Traces.Parse_Config_File;
   Discover_Steps
     (Finder,
      Extension => ".ads",
      Directory => Create_From_Base ("features/step_definitions"));
end Gnatbdd;
