-- A library for drawing bar codes
-- Drawing bar codes as an Image_IO image
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Image_IO.Holders;

package Bar_Code_Drawing.How.Image_IO is
   procedure Draw (Info : in Drawing_Info; Image : out Standard.Image_IO.Holders.Handle; Scale : in Positive := 1);
   -- After calling an approprate What.*.Draw procedure, converts Info into Image
   -- Each module will be Scale pixels wide
end Bar_Code_Drawing.How.Image_IO;
