-- A program to test drawing bar codes in PBM format
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Command_Line;
with Ada.Text_IO;
with Bar_Code_Drawing.How.PBM;
with Bar_Code_Drawing.What.QR_Code;

procedure PBMQR is
   package CQR renames Bar_Code_Drawing.What.QR_Code;

   Info : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (250, 250);
begin -- PBMQR
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line (Item => "Usage: pbmqr <text to encode>");
      Ada.Text_IO.Put_Line (Item => "   Outputs a QR code containing the argument to standard output");

      return;
   end if;

   Get_Text : declare
      Text : constant String := Ada.Command_Line.Argument (1);
   begin -- Get_Text
      Info.Set_Scale (Scale => Info.Width / CQR.Width (Text) );
      CQR.Draw (Info => Info, Text => Text);
      Ada.Text_IO.Put (Item => Bar_Code_Drawing.How.PBM.Image (Info) );
   end Get_Text;
end PBMQR;
