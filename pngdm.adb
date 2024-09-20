-- A program for drawing Data Matrix codes in PNG format
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Command_Line;
with Ada.Text_IO;
with Bar_Code_Drawing.How.Image_IO;
with Bar_Code_Drawing.What.Data_Matrix;
with Image_IO.Holders;
with Image_IO.Operations;

procedure PNGDM is
   package CDM renames Bar_Code_Drawing.What.Data_Matrix;

   Text_Arg : Positive := 1;
begin -- PNGDM
   if Ada.Command_Line.Argument_Count < 2 then
      Ada.Text_IO.Put_Line (Item => "Usage: pngdm <text to encode> <file name>");
      Ada.Text_IO.Put_Line (Item => "   Outputs a PNG Data Matrix code containing the text to a file with the given name");

      return;
   end if;

   Get_Text : declare
      Text  : constant String   := Ada.Command_Line.Argument (1);
      Name  : constant String   := Ada.Command_Line.Argument (2);

      Width  : Positive;
      Height : Positive;
   begin -- Get_Text
      CDM.Get_Dimensions (Text => Text, Width => Width, Height => Height);--, Rectangular => Text'Length < 37);

      Draw : declare
         Scale : constant Positive := Integer'Max (150 / Width, 2);

         Info  : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (Width, Height, 2);
         Image : Image_IO.Holders.Handle;
      begin -- Draw
         CDM.Draw (Info => Info, Text => Text);--, Rectangular => Text'Length < 37);
         Bar_Code_Drawing.How.Image_IO.Draw (Info => Info, Image => Image, Scale => Scale);
         Image_IO.Operations.Write_PNG (File_Name => Name, Image => Image.Value, Grayscale => True);
      end Draw;
   end Get_Text;
end PNGDM;
