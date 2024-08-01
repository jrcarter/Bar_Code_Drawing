-- A program to test drawing bar codes in PNG format
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Command_Line;
with Ada.Text_IO;
with Bar_Code_Drawing.How.Image_IO;
with Bar_Code_Drawing.What.QR_Code;
with Image_IO.Holders;
with Image_IO.Operations;

procedure PNGQR is
   package CQR renames Bar_Code_Drawing.What.QR_Code;

   Text_Arg : Positive := 1;
   ECL      : CQR.Error_Correction_Level := CQR.Medium;
begin -- PNGQR
   if Ada.Command_Line.Argument_Count < 2 then
      Ada.Text_IO.Put_Line (Item => "Usage: pngqr [-L | -M | -Q | -H] <text to encode> <file name>");
      Ada.Text_IO.Put_Line (Item => "   Switch indicates the Error Correction Level to use (default M)");
      Ada.Text_IO.Put_Line (Item => "   Outputs a PNG QR code containing the text to a file with the given name");

      return;
   end if;

   if Ada.Command_Line.Argument_Count > 2 then
      Check_Switch : declare
         Arg : constant String := Ada.Command_Line.Argument (1);
      begin -- Check_Switch
         if Arg'Length = 2 and then (Arg (1) = '-' and Arg (2) in 'l' | 'L' | 'm' | 'M' | 'q' | 'Q' | 'h' | 'H') then
            Text_Arg := 2;

            ECL := (case Arg (2) is
                    when 'l' | 'L' => CQR.Low,
                    when 'm' | 'M' => CQR.Medium,
                    when 'q' | 'Q' => CQR.Quartile,
                    when 'h' | 'H' => CQR.High,
                    when others    => raise Program_Error with "Invalid level");
         end if;
      end Check_Switch;
   end if;

   Get_Text : declare
      Text  : constant String   := Ada.Command_Line.Argument (Text_Arg);
      Name  : constant String   := Ada.Command_Line.Argument (Text_Arg + 1);
      Width : constant Natural  := CQR.Width (Text, ECL);
      Scale : constant Positive := Integer'Max (100 / Width, 2);

      Info  : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (Width, Width, 2);
      Image : Image_IO.Holders.Handle;
   begin -- Get_Text
      CQR.Draw (Info => Info, Text => Text, Level => ECL);
      Bar_Code_Drawing.How.Image_IO.Draw (Info => Info, Image => Image, Scale => Scale);
      Image_IO.Operations.Write_PNG (File_Name => Name, Image => Image.Value, Grayscale => True);
   exception -- Get_Text
   when others =>
      Ada.Text_IO.Put_Line (Item => "Text too long");
   end Get_Text;
end PNGQR;
