-- A library for drawing bar codes
-- Drawing UPC-A bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package Bar_Code_Drawing.What.UPCA is
   Width : constant := 95;
   -- All UPC-A codes are 95 modules wide

   procedure Draw (Info : in out Drawing_Info; Text : in String) with
      Pre => Info.Dimensions = 1 and Text'Length = 11 and (for all C of Text => C in Digit);
   -- Draws a UPC-A bar code representing Text with its associated checksum digit
   -- Only draws the bar code; cutouts and text are not included
end Bar_Code_Drawing.What.UPCA;
