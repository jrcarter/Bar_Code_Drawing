-- A program to test drawing bar codes in PBM format
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Command_Line;
with Ada.Text_IO;
with Bar_Code_Drawing.How.PBM;
with Bar_Code_Drawing.What.MSI_Code;

procedure PBMMSI is
   package CMSI renames Bar_Code_Drawing.What.MSI_Code;

   Info : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (638, 100, 2);
begin -- PBMMSI
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line (Item => "Usage: pbmmsi <digits to encode>");
      Ada.Text_IO.Put_Line (Item => "   Outputs an MSI bar code containing the argument to standard output");

      return;
   end if;

   Get_Text : declare
      Text : constant String := Ada.Command_Line.Argument (1);

      Last : Natural := Text'Last;
   begin -- Get_Text
      Truncate : loop
         exit Truncate when Info.Scale * CMSI.Width (Text (1 .. Last) ) <= Info.Width;

         Last := Last - 1;
      end loop Truncate;

      CMSI.Draw (Info => Info, Text => Text (1 .. Last) );
      Ada.Text_IO.Put (Item => Bar_Code_Drawing.How.PBM.Image (Info) );
   end Get_Text;
end PBMMSI;
