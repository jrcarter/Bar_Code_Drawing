-- A program to test drawing bar codes in SVG format
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Command_Line;
with Ada.Text_IO;
with Bar_Code_Drawing.How.SVG;
with Bar_Code_Drawing.What.QR_Code;

procedure SVGQR is
   -- Empty
begin -- SVGQR
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line (Item => "Usage: pbmqr <text to encode>");
      Ada.Text_IO.Put_Line (Item => "   Outputs a QR code containing the argument to standard output");

      return;
   end if;

   Get_Text : declare
      package CQR renames Bar_Code_Drawing.What.QR_Code;

      Text  : constant String   := Ada.Command_Line.Argument (1);
      Width : constant Natural  := CQR.Width (Text);

      Info : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (Width, Width, 2);
   begin -- Get_Text
      CQR.Draw (Info => Info, Text => Text);
      Ada.Text_IO.Put (Item => Bar_Code_Drawing.How.SVG.Image (Info, 1.0) );
   exception -- Get_Text
   when others =>
      Ada.Text_IO.Put_Line (Item => "Text too long");
   end Get_Text;
end SVGQR;
