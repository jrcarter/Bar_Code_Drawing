-- A program to test drawing bar codes in PBM format
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Command_Line;
with Ada.Text_IO;
with Bar_Code_Drawing.How.PBM;
with Bar_Code_Drawing.What.Code_128;

procedure PBM128 is
   package C128 renames Bar_Code_Drawing.What.Code_128;

   Info : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (1003, 100, 2);
begin -- PBM128
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line (Item => "Usage: pbm128 <text to encode>");
      Ada.Text_IO.Put_Line (Item => "   Outputs a Code-128 bar code containing the argument to standard output");

      return;
   end if;

   Get_Text : declare
      Text : constant String := Ada.Command_Line.Argument (1);

      Last : Natural := Text'Last;
   begin -- Get_Text
      Truncate : loop
         exit Truncate when Info.Scale * C128.Width (Text (1 .. Last) ) <= Info.Width;

         Last := Last - 1;
      end loop Truncate;

      C128.Draw (Info => Info, Text => Text (1 .. Last) );
      Ada.Text_IO.Put (Item => Bar_Code_Drawing.How.PBM.Image (Info) );
   end Get_Text;
end PBM128;
