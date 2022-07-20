-- A library for drawing bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

-- Parts of this software are modified from Bar_Codes.Impl by Gautier de Montmollin
--   Copyright (c) 2018 Gautier de Montmollin

--   Permission is hereby granted, free of charge, to any person obtaining a copy
--   of this software and associated documentation files (the "Software"), to deal
--   in the Software without restriction, including without limitation the rights
--   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--   copies of the Software, and to permit persons to whom the Software is
--   furnished to do so, subject to the following conditions:

--   The above copyright notice and this permission notice shall be included in
--   all copies or substantial portions of the Software.

--   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--   THE SOFTWARE.

package body Bar_Code_Drawing is
   function New_Info (Width : in Positive; Height : in Positive; Dimensions : in Dimension_Number; Scale : in Positive := 1)
   return Drawing_Info is
      Result : Drawing_Info (Last_X => Width - 1, Last_Y => (if Dimensions = 1 then 0 else Height - 1) );
   begin -- New_Info
      Result.Width  := Width;
      Result.Height := Height;
      Result.Dim    := Dimensions;
      Result.Scale  := Scale;
      Result.Bitmap := (others => (others => False) );

      return Result;
   end New_Info;

   procedure Reset (Info : in out Drawing_Info) is
      -- Empty
   begin -- Reset
      Info.Bitmap := (others => (others => False) );
   end Reset;

   procedure Set_Scale (Info : in out Drawing_Info; Scale : in Positive) is
      -- Empty
   begin -- Set_Scale
      Info.Scale := Scale;
   end Set_Scale;

   procedure Draw_Module (Info : in out Drawing_Info; X : in Natural; Y : in Natural) is
      -- Empty
   begin -- Draw_Module
      All_X : for Xx in Info.Scale * X .. Info.Scale * X + Info.Scale - 1 loop
         All_Y : for Yy in
            (if Info.Dim = 1 then 0 else Info.Scale * Y) .. (if Info.Dim = 1 then 0 else Info.Scale * Y + Info.Scale - 1)
         loop
            Info.Bitmap (Xx, Yy) := True;
         end loop All_Y;
      end loop All_X;
   end Draw_Module;
end Bar_Code_Drawing;
