-- A library for drawing bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package body Bar_Code_Drawing.How.Ada_GUI is
   procedure Render (Info : in Drawing_Info; ID : in GUI.Widget_ID) is
      -- Empty
   begin -- Render
      Draw_X : for X in Info.Bitmap'Range (1) loop
         Draw_Y : for Y in Info.Bitmap'Range (2) loop
            if Info.Bitmap (X, Y) then
               ID.Set_Pixel (X => X, Y => Info.Height - 1 - Y);
            end if;
         end loop Draw_Y;
      end loop Draw_X;
   end Render;
end Bar_Code_Drawing.How.Ada_GUI;
