-- A library for drawing bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Text_IO;

package body Bar_Code_Drawing.How.Text is
   procedure Write (Info : in Drawing_Info; Name : in String) is
      Text : String (1 .. Info.Bitmap'Length (1) ) := (others => ' ');
      File : Ada.Text_IO.File_Type;
   begin -- Write
      Fill_Line: for X in Info.Bitmap'Range (1) loop
         if Info.Bitmap (X, 0) then
            Text (X + 1) := 'M';
         end if;
      end loop Fill_Line;

      Ada.Text_IO.Create (File => File, Name => Name);

      All_Lines : for Y in 1 .. Info.Height loop
         Ada.Text_IO.Put_Line (File => File, Item => Text);
      end loop All_Lines;

      Ada.Text_IO.Close (File => File);
   end Write;
end Bar_Code_Drawing.How.Text;
