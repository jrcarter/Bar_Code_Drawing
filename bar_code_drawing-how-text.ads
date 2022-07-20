-- A library for drawing bar codes
-- Drawing bar codes as a text file (ASCII art)
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package Bar_Code_Drawing.How.Text is
   procedure Write (Info : in Drawing_Info; Name : in String) with Pre => Info.Dimensions = 1;
   -- After calling an approprate What.*.Draw procedure, creates a text file named Named and draws the image in Info into it as
   -- ASCII art
   -- Because Characters are generally not square, only supports 1D barcodes
end Bar_Code_Drawing.How.Text;
