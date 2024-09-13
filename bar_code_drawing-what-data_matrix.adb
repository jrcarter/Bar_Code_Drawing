-- A library for drawing bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

-- Parts of this software are modified from Bar_Codes.Encode_DM by Gautier de Montmollin, which has the following license:
--   Copyright (c) 2018 .. 2024 Gautier de Montmollin

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

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

-- Bar_Codes.Encode_DM is derived from datamatrix.js, and quotes the following license:
--   https://github.com/datalog/datamatrix-svg
--   under MIT license
--   datamatrix.js has no dependencies
--   Copyright (c) 2020 Constantine

with Ada.Containers.Vectors;

package body Bar_Code_Drawing.What.Data_Matrix is
   -- DM means Data Matrix
   -- RS means Reed-Solomon

   type Byte_Value is mod 256;

   package Byte_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Byte_Value);
   subtype Byte_Vector is Byte_Vectors.Vector;

   function Encoded (Text : in String) return Byte_Vector;
   -- Returns the DM encoding for Text

   procedure Calibrate (Encoded_Length   : in     Natural;
                        Num_Rows         :    out Natural;
                        Num_Cols         :    out Natural;
                        Num_Blocks       :    out Natural;
                        Width            :    out Natural;
                        Height           :    out Natural;
                        Symbol_Bytes     :    out Natural;
                        RS_Words         :    out Natural;
                        Rectangular      : in     Boolean := False);
   -- Calculate useful information about a DM with the given Encoded_Length and Rectangular
   -- Width and Height only include the data area(s)
   -- A large DM is made up of a rectangle of smaller DMs ("regions")
   -- Num_Rows and Num_Cols are the number of rows and columns of such regions
   -- Symbol_Bytes is the maximum number of bytes that can fit in a DM of the returned size
   -- RS_Words is the number of error-correction codewords needed
   -- Symbol_Bytes >= Encoded_Length + RS_Words

   procedure Get_Dimensions (Text : in String; Width : out Natural; Height : out Natural; Rectangular : in Boolean := False) is
      Encoding : constant Byte_Vector := Encoded (Text);

      Num_Rows     : Natural;
      Num_Cols     : Natural;
      Wide         : Natural;
      High         : Natural;
      D1           : Natural;
      D2           : Natural;
      D3           : Natural;
   begin -- Get_Dimensions
      Calibrate (Encoded_Length => Integer (Encoding.Length),
                 Num_Rows       => Num_Rows,
                 Num_Cols       => Num_Cols,
                 Num_Blocks     => D1,
                 Width          => Wide,
                 Height         => High,
                 Symbol_Bytes   => D2,
                 RS_Words       => D3,
                 Rectangular    => Rectangular);
      Width  := Wide + 2 * Num_Cols;
      Height := High + 2 * Num_Rows;
   end Get_Dimensions;

   type U16 is mod 2 ** 16;

   procedure Draw (Info : in out Drawing_Info; Text : in String; Rectangular : in Boolean := False) is
      Encoding      : Byte_Vector := Encoded (Text);
      Height        : Natural;
      Width         : Natural;
      Num_Cols      : Natural;
      Num_Rows      : Natural;
      Fw            : Natural;
      Fh            : Natural;

      procedure Bit (X : in Natural; Y : in Natural) with Inline;
      -- Draws a module at (X, Y)

      procedure Prepare;
      -- Prepares for drawing

      procedure Layout_Perimeter_Finder_Pattern;
      -- Draws the perimeter finder pattern using bit

      procedure Draw_Data;
      -- Draws Encoding using bit

      procedure Bit (X : in Natural; Y : in Natural) is
         -- Empty
      begin -- Bit
         Draw_Module (Info => Info, X => X, Y => Info.Last_Y - Y);
      end Bit;

      procedure Prepare is
         type RS_List is array (0 .. 69) of Byte_Value;
         type Table is array (Natural range <>) of Integer;

         Length       : Natural          := Natural (Encoding.Length);
         RS           : RS_List          := (others => 0); -- Reed / Solomon code
         Rc           : RS_List          := (others => 0);
         Log          : Table (0 .. 255) := (others => 0); -- log / exp table for multiplication
         Exp          : Table (0 .. 254) := (others => 0);
         I            : Natural;
         Exp_I        : Natural;
         Symbol_Bytes : Natural;
         RS_Words     : Natural;
         Num_Blocks   : Natural;
         X            : Byte_Value;
         Rc_Index     : Natural;
      begin -- Prepare
         Calibrate (Encoded_Length => Length,
                    Num_Rows       => Num_Rows,
                    Num_Cols       => Num_Cols,
                    Num_Blocks     => Num_Blocks,
                    Width          => Width,
                    Height         => Height,
                    Symbol_Bytes   => Symbol_Bytes,
                    RS_Words       => RS_Words,
                    Rectangular    => Rectangular);
         Fw := Width / Num_Cols; -- Region size
         Fh := Height / Num_Rows;

         if Length < Symbol_Bytes - RS_Words then -- First padding
            Length := Length + 1;
            Encoding.Append (New_Item => 129);
         end if;

         More_Padding : loop
            exit More_Padding when Length >= Symbol_Bytes - RS_Words;

            Length := Length + 1;
            Encoding.Append (New_Item => Byte_Value ( ( ( (149 * Length) rem 253) + 130) rem 254) );
         end loop More_Padding;

         RS_Words := RS_Words / Num_Blocks; -- Reed Solomon error detection and correction

         Exp_I := 1; -- log / exp table of Galois field

         Tables : for I in Exp'Range loop
            Exp (I) := Exp_I;
            Log (Exp_I) := I;
            Exp_I := Exp_I + Exp_I;

            if Exp_I > 255 then
               Exp_I := Integer (U16 (Exp_I) xor 301); -- "301 = a^8 + a^5 + a^3 + a^2 + 1"
            end if;
         end loop Tables;

         RS (RS_Words) := 0; --  RS generator polynomial

         Fill_RS : for I in 1 .. RS_Words loop -- The loop names through the end of Prepare could probably be better
            RS (RS_Words - I) := 1;

            RS_Rest : for J in RS_Words - I .. RS_Words - 1 loop
               RS (J) := RS (J + 1) xor Byte_Value (Exp ( (Log (Integer (RS (J) ) ) + I) rem 255) );
            end loop RS_Rest;
         end loop Fill_RS;

         --  RS correction data for each block
         Correction_Blocks : for C in 0 .. Num_Blocks - 1 loop
            Rc (0 .. RS_Words) := (others => 0);
            I := C;

            Correction_For_Block : loop
               exit Correction_For_Block when I >= Length;

               X := Rc (0) xor Encoding.Element (I);

               Fill_Rc : for J in 0 .. RS_Words - 1 loop
                  Rc (J) := Rc (J + 1) xor Byte_Value (if X /= 0 then
                                                          Exp ( (Log (Integer (RS (J) ) ) + Log (Integer (X) ) ) rem 255)
                                                       else
                                                          0);
               end loop Fill_Rc;

               I := I + Num_Blocks;
            end loop Correction_For_Block;

            Interleaved_Correction : for I in 0 .. RS_Words - 1 loop -- Interleaved correction data
               Rc_Index := Length + C + I * Num_Blocks;
               Encoding.Append
                  (New_Item => 0, Count => Ada.Containers.Count_Type (Integer'Max (Rc_Index - Encoding.Last_Index, 0) ) );
               Encoding.Replace_Element(Index => Rc_Index, New_Item => Rc (I) );
            end loop Interleaved_Correction;
         end loop Correction_Blocks;
      end Prepare;

      procedure Layout_Perimeter_Finder_Pattern is
         I : Natural := 0;
      begin -- Layout_Perimeter_Finder_Pattern
         Horizontal : loop
            exit Horizontal when I >= Height + 2 * Num_Rows;

            All_X : for J in 0 .. Width + 2 * Num_Cols - 1 loop
               Bit (X => J, Y => I + Fh + 1);

               if J rem 2 = 0 then
                  Bit (X => J, Y => I);
               end if;
            end loop All_X;

            I := I + Fh + 2;
         end loop Horizontal;

         I := 0;

         Vertical : loop
            exit Vertical when I >= Width + 2 * Num_Cols;

            All_Y : for J in 0 .. Height - 1 loop
               Bit (X => I, Y => J + 2 * (J / Fh) + 1);

               if J rem 2 = 1 then
                  Bit (X => I + Fw + 1, Y => J + 2 * (J / Fh) );
               end if;
            end loop All_Y;

            I := I + Fw + 2;
         end loop Vertical;
      end Layout_Perimeter_Finder_Pattern;

      procedure Draw_Data is
         procedure Check_Corners;
         -- Tries to find a layout for the current values of Row and Col
         -- If found, assigns the layout to Layout
         -- Otherwise, sets Draw_It to False

         Step : Integer := 2;
         Col  : Integer := 0;
         Row  : Integer := 4;

         type Offset is record -- Offsets relative to current values of Col and Row
            X, Y : Integer;
         end record;

         type Layout_List is array (0 .. 7) of Offset;

         --  Nominal layout (L-shaped) for displaying a byte:
         Normal : constant Layout_List := ( ( 0,  0),   -- -2,-2 | -1,-2 |
                                            (-1,  0),   -- ------+-------+------
                                            (-2,  0),   -- -2,-1 | -1,-1 |  0,-1
                                            ( 0, -1),   -- ------+-------+------
                                            (-1, -1),   -- -2, 0 | -1, 0 |  0, 0
                                            (-2, -1),
                                            (-1, -2),
                                            (-2, -2) );

         Layout  : Layout_List;
         Draw_It : Boolean;
         El      : Byte_Value;
         X       : Integer;
         Y       : Integer;

         procedure Check_Corners is
            -- Empty
         begin -- Check_Corners
            if Row = Height - 3 and Col = -1 then -- Corner A layout
               Layout := ( (Width,     6 - Height),
                           (Width,     5 - Height),
                           (Width,     4 - Height),
                           (Width,     3 - Height),
                           (Width - 1, 3 - Height),
                           (3,         2),
                           (2,         2),
                           (1,         2) );
            elsif Row = Height + 1 and Col = 1 and Width rem 8 = 0 and Height rem 8 = 6 then -- Corner D layout
               Layout := ( (Width - 2,     -Height),
                           (Width - 3,     -Height),
                           (Width - 4,     -Height),
                           (Width - 2, -1 - Height),
                           (Width - 3, -1 - Height),
                           (Width - 4, -1 - Height),
                           (Width - 2, -2),
                           (-1,        -2) );
            elsif Row = 0 and Col = Width - 2 and Width rem 4 /= 0 then -- Corner B: omit upper left.
               Draw_It := False;
            else
               if Row not in 0 .. Height - 1 or Col not in 0 .. Width - 1 then -- We are outside.
                  Step := -Step;  --  Turn around
                  Row := Row + 2 + Step / 2;
                  Col := Col + 2 - Step / 2;

                  Move_Inside : loop
                     exit Move_Inside when Row in 0 .. Height - 1 and Col in 0 .. Width - 1;

                     Row := Row - Step;
                     Col := Col + Step;
                  end loop Move_Inside;
               end if;

               if Row = Height - 2 and Col = 0 and Width rem 4 /= 0 then -- Corner B layout
                  Layout := ( (Width - 1, 3 - Height),
                              (Width - 1, 2 - Height),
                              (Width - 2, 2 - Height),
                              (Width - 3, 2 - Height),
                              (Width - 4, 2 - Height),
                              (0,         1),
                              (0,         0),
                              (0,        -1) );
               elsif Row = Height - 2 and Col = 0 and Width rem 8 = 4 then -- Corner C layout
                  Layout := ( (Width - 1, 5 - Height),
                              (Width - 1, 4 - Height),
                              (Width - 1, 3 - Height),
                              (Width - 1, 2 - Height),
                              (Width - 2, 2 - Height),
                              (0,         1),
                              (0,         0),
                              (0,        -1) );
               elsif Row = 1 and Col = Width - 1 and Width rem 8 = 0 and Height rem 8 = 6 then -- Omit corner D
                  Draw_It := False;
               else
                  Layout := Normal;
               end if;
            end if;
         end Check_Corners;
      begin
         All_Bytes : for Index in 0 .. Encoding.Last_Index loop
            El := Encoding.Element (Index);

            Find_Layout : loop
               Draw_It := True;
               Check_Corners;

               exit Find_Layout when Draw_It;

               Row := Row - Step; -- Diagonal steps (nothing drawn)
               Col := Col + Step;
            end loop Find_Layout;

            All_Bits : for J in Layout_List'Range loop
               if (El and 1) /= 0 then
                  X := Col + Layout (J).X;
                  Y := Row + Layout (J).Y;

                  -- Wrap around:
                  if X < 0 then
                     X := X + Width;
                     Y := Y + 4 - ( (Width + 4) rem 8);
                  end if;

                  if Y < 0 then
                     X := X + 4 - ( (Height + 4) rem 8);
                     Y := Y + Height;
                  end if;

                  --  Plot at (x, y), plus region gap
                  Bit (X + 2 * (X / Fw) + 1, Y + 2 * (Y / Fh) + 1);
               end if;

               El := El / 2;
            end loop All_Bits;

            Row := Row - Step; -- Diagonal steps (El was drawn)
            Col := Col + Step;
         end loop All_Bytes;

         Unfilled_Corner : for I in reverse 0 .. Width loop
            exit Unfilled_Corner when I rem 4 = 0;

            Bit (I, I);
         end loop Unfilled_Corner;
      end Draw_Data;
   begin -- Draw
      Prepare;
      Layout_Perimeter_Finder_Pattern;
      Draw_Data;
   end Draw;

   function Encoded (Text : in String) return Byte_Vector is
      function ASCII_Encoded (Text : in String) return Byte_Vector;
      -- Returns the ASCII encoding of Text
      -- ASCII encoding is good for values < 128 and for digit pairs

      Max_Base_256_Length : constant := (255 - 37) * 250 + 249;

      function Base_256_Encoded (Text : in String) return Byte_Vector with
         Pre => Text'Length <= Max_Base_256_Length;
      -- Returns the Base-256 encoding of Text
      -- Base-256 encoding is good for data that is not suited to ASCII encoding, but note the limit on length

      function ASCII_Encoded (Text : in String) return Byte_Vector is
         subtype Digit is Byte_Value range 48 .. 57;

         Result : Byte_Vector;
         Index  : Positive := Text'First;
         B1     : Byte_Value;
         B2     : Byte_Value;
      begin -- ASCII_Encoded
         All_Chars : loop
            exit All_Chars when Index > Text'Last;

            B1 := Character'Pos (Text (Index) );
            B2 := (if Index < Text'Last then Character'Pos (Text (Index + 1) ) else 0);

            if B1 in Digit and B2 in Digit then -- Digit pair; Codes 130 .. 229 are used for "00" .. "99"
               Result.Append (New_Item => (10 * (B1 - Digit'First) + B2 - Digit'First + 130) );
               Index := Index + 1;
            elsif B1 > 127 then
               Result.Append (New_Item => 235); -- Code for an upper-half character
               Result.Append (New_Item => B1 - 127); -- Upper-half characters take 2 bytes
            else
               Result.Append (New_Item => B1 + 1);
            end if;

            Index := Index + 1;
         end loop All_Chars;

         return Result;
      end ASCII_Encoded;

      function Base_256_Encoded (Text : in String) return Byte_Vector is
         Result : Byte_Vector;

         use type Ada.Containers.Count_Type;
      begin -- Base_256_Encoded
         Result.Append (New_Item => 231);

         if Text'Length > 250 then --  Length high byte (in 255-state algo)
            Result.Append (New_Item => Byte_Value (Integer'(37 + (Text'Length / 250) rem 256) ) );
         end if;

         Result.Append (New_Item => Byte_Value ( (Text'Length rem 250 + 149 * Integer (Result.Length + 1) rem 255 + 1) rem 256) );
         --  Length low byte (in 255-state algo)

         All_Chars : for C of Text loop
            Result.Append
               (New_Item => Byte_Value ( (Character'Pos (C) + 149 * Integer (Result.Length + 1) rem 255 + 1) rem 256) );
         end loop All_Chars;

         return Result;
      end Base_256_Encoded;

      Candidate  : Byte_Vector;
      Challenger : Byte_Vector;

      use type Ada.Containers.Count_Type;
   begin -- Encoded
      Candidate := ASCII_Encoded (Text);

      if Text'Length <= Max_Base_256_Length then
         Challenger := Base_256_Encoded (Text);

         if Challenger.Length < Candidate.Length then
            return Challenger;
         end if;
      end if;

      return Candidate;
   end Encoded;

   procedure Calibrate (Encoded_Length   : in     Natural;
                        Num_Rows         :    out Natural;
                        Num_Cols         :    out Natural;
                        Num_Blocks       :    out Natural;
                        Width            :    out Natural;
                        Height           :    out Natural;
                        Symbol_Bytes      :    out Natural;
                        RS_Words         :    out Natural;
                        Rectangular      : in     Boolean := False)
   is
      procedure Calibrate_Rectangle (Encoded_Length   : in     Natural;
                                     Num_Rows         :    out Natural;
                                     Num_Cols         :    out Natural;
                                     Num_Blocks       :    out Natural;
                                     Width            :    out Natural;
                                     Height           :    out Natural;
                                     Symbol_Bytes     :    out Natural;
                                     RS_Words         :    out Natural);
      -- Calibration when Rectangular and a rectangular code is possible

      procedure Calibrate_Square (Encoded_Length   : in     Natural;
                                  Num_Rows         :    out Natural;
                                  Num_Cols         :    out Natural;
                                  Num_Blocks       :    out Natural;
                                  Width            :    out Natural;
                                  Height           :    out Natural;
                                  Symbol_Bytes     :    out Natural;
                                  RS_Words         :    out Natural);
      -- Calibration for a square code

      procedure Calibrate_Rectangle (Encoded_Length   : in     Natural;
                                     Num_Rows         :    out Natural;
                                     Num_Cols         :    out Natural;
                                     Num_Blocks       :    out Natural;
                                     Width            :    out Natural;
                                     Height           :    out Natural;
                                     Symbol_Bytes     :    out Natural;
                                     RS_Words         :    out Natural)
      is
         type Positive_List is array (0 .. 5) of Positive;

         Symbol_Width  : constant Positive_List := (16, 28, 24, 32, 32, 44);
         Symbol_Height : constant Positive_List := ( 6,  6, 10, 10, 14, 14);
         RS_Checkword  : constant Positive_List := ( 7, 11, 14, 18, 24, 28);
      begin -- Calibrate_Rectangle
         Num_Rows := 1;
         Num_Blocks := 1;

         Calculate : for J in Symbol_Width'Range loop
            Width  := Symbol_Width  (J);
            Height := Symbol_Height (J);
            Symbol_Bytes := Width * Height / 8;
            RS_Words := RS_Checkword (J);

            exit Calculate when Symbol_Bytes >= Encoded_Length + RS_Words;
         end loop Calculate;

         Num_Cols := (if Width > 25 then 2 else 1);
      end Calibrate_Rectangle;

      procedure Calibrate_Square (Encoded_Length   : in     Natural;
                                  Num_Rows         :    out Natural;
                                  Num_Cols         :    out Natural;
                                  Num_Blocks       :    out Natural;
                                  Width            :    out Natural;
                                  Height           :    out Natural;
                                  Symbol_Bytes     :    out Natural;
                                  RS_Words         :    out Natural)
      is
         type Checkword_List is array (0 .. 23) of Positive;

         RS_Checkword : constant Checkword_List :=
            (5, 7, 10, 12, 14, 18, 20, 24, 28, 36, 42, 48, 56, 68, 84, 112, 144, 192, 224, 272, 336, 408, 496, 620);

         Increment : Positive := 2;
      begin -- Calibrate_Square
         Width  := 6;
         Height := 6;

         All_Checkwords : for J in RS_Checkword'Range loop
            if Width > 11 * Increment then --  Increase increment
               Increment := 4 + Integer (U16 (Increment) and 12);
            end if;

            Width  := Width  + Increment;
            Height := Height + Increment;
            Symbol_Bytes := Width * Height / 8;
            RS_Words := RS_Checkword (J);

            exit All_Checkwords when Symbol_Bytes >= Encoded_Length + RS_Words;

            if J = RS_Checkword'Last then
               raise Cannot_Encode with "Message to be encoded doesn't fit in any Data Matrix size";
            end if;
         end loop All_Checkwords;

         Num_Rows := (if Width > 27 then 2 * (Width / 54) + 2 else 1);
         Num_Cols := Num_Rows;
         Num_Blocks := (if Symbol_Bytes > 255 then 2 * (Symbol_Bytes / 512) + 2 else 1);
      end Calibrate_Square;
   begin -- Calibrate
      if Rectangular and Encoded_Length < 50 then
         Calibrate_Rectangle (Encoded_Length => Encoded_Length,
                              Num_Rows       => Num_Rows,
                              Num_Cols       => Num_Cols,
                              Num_Blocks     => Num_Blocks,
                              Width          => Width,
                              Height         => Height,
                              Symbol_Bytes   => Symbol_Bytes,
                              RS_Words       => RS_Words);
      else
         Calibrate_Square (Encoded_Length => Encoded_Length,
                           Num_Rows       => Num_Rows,
                           Num_Cols       => Num_Cols,
                           Num_Blocks     => Num_Blocks,
                           Width          => Width,
                           Height         => Height,
                           Symbol_Bytes   => Symbol_Bytes,
                           RS_Words       => RS_Words);
      end if;
   end Calibrate;
end Bar_Code_Drawing.What.Data_Matrix;
