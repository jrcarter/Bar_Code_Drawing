-- A library for drawing bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

package body Bar_Code_Drawing.How.PBM is
   function Image (Info : in Drawing_Info) return String is
      use Ada.Strings.Unbounded;

      LF : constant Character := Ada.Characters.Latin_1.LF;

      Result : Unbounded_String := To_Unbounded_String ("P1" & LF & Info.Width'Image & Info.Height'Image & LF);
   begin -- Image
      All_Y : for Y in reverse Info.Bitmap'Range (2) loop
         All_X : for X in Info.Bitmap'Range (1) loop
            Append (Source => Result, New_Item => Boolean'Pos (Info.Bitmap (X, Y) )'Image);
         end loop All_X;

         Append (Source => Result, New_Item => LF);
      end loop All_Y;

      return To_String (Result);
   end Image;
end Bar_Code_Drawing.How.PBM;
