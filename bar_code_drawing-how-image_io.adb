-- A library for drawing bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package body Bar_Code_Drawing.How.Image_IO is
   procedure Draw (Info : in Drawing_Info; Image : out Standard.Image_IO.Holders.Handle; Scale : in Positive := 1) is
      package Imio renames Standard.Image_IO;

      procedure Convert (Result : in out Imio.Image_Data);
      -- Performs the conversion/drawing

      procedure Convert (Result : in out Imio.Image_Data)is
         Black : constant Imio.Color_Info := (Red | Green | Blue =>   0);
         White : constant Imio.Color_Info := (Red | Green | Blue => 255);

         R : Natural := 0;
         X : Natural := 0;
      begin -- Convert
         if Info.Dim = 1 then -- 1D code
            One_Row : for C in Info.Bitmap'Range (1) loop
               Apply_Scale : for J in 1 .. Scale loop
                  Result (0, X) := (if Info.Bitmap (C, 0) then Black else White);
                  X := X + 1;
               end loop Apply_Scale;
            end loop One_Row;

            Copy_Rows : for R in 1 .. Result'Last (1) loop
               Columns : for C in Result'Range (2) loop
                  Result (R, C) := Result (0, C);
               end loop Columns;
            end loop Copy_Rows;
         else -- 2D code
            All_Y : for Y in reverse Info.Bitmap'Range (2) loop
               X := 0;

               All_X : for C in Info.Bitmap'Range (1) loop
                  Scale_X : for I in 1 .. Scale loop
                     Result (R, X) := (if Info.Bitmap (C, Y) then Black else White);
                     X := X + 1;
                  end loop Scale_X;
               end loop All_X;

               Scale_Y : for I in 1 .. Scale - 1 loop
                  Copy_X : for C in Result'Range (1) loop
                     Result (R + I, C) := Result (R, C);
                  end loop Copy_X;
               end loop Scale_Y;

               R := R + Scale;
            end loop All_Y;
         end if;
      end Convert;
   begin -- Draw
      Image.Create (Width => Scale * Info.Width, Height => Scale * Info.Height);
      Image.Update (Process => Convert'Access);
   end Draw;
end Bar_Code_Drawing.How.Image_IO;
