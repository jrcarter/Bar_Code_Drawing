-- A library for drawing bar codes
-- Drawing Data-Matrix codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package Bar_Code_Drawing.What.Data_Matrix is
   Cannot_Encode : exception;

   procedure Get_Dimensions (Text : in String; Width : out Natural; Height : out Natural; Rectangular : in Boolean := False);
   -- Calculates the dimensions of a Data-Matrix code for Text ini Width and Height
   -- If Rectangular and a rectangular code is possible, calculates the dimensions for a rectangular code
   -- Otherwise, calculates the dimensions for a square code
   -- Raises Cannot_Encode if Text doesn't fit in any Data Matrix size

   procedure Draw (Info : in out Drawing_Info; Text : in String; Rectangular : in Boolean := False) with
      Pre => Info.Dimensions = 2;
   -- Draws a Data-Matrix code representing Text
   -- If Rectangular and a rectangular code is possible, draws a rectangular code; otherwise, draws a square code
   -- Raises Cannot_Encode if Text doesn't fit in any Data Matrix size
end Bar_Code_Drawing.What.Data_Matrix;
