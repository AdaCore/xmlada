--  This file should have one include file for each of the modules involved
--  in XML/Ada, so that all modules are compiled when doing
--      gnatmake -Pbuild_all.gpr

pragma Warnings (Off);
with Unicode;
with DOM;
with SAX;
with Input_Sources;
pragma Warnings (On);

procedure Build_All is
begin
   null;
end Build_All;
