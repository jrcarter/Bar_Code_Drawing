-- A library for drawing bar codes
-- Drawing UPC-A bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package body Bar_Code_Drawing.What.UPCA is
   procedure Draw (Info : in out Drawing_Info; Text : in String) is
      subtype Digit_Pattern is String (1 .. 7); -- Each digits takes 7 modules
      type Pattern_Map is array (Digit) of Digit_Pattern;

      Left_Map : constant Pattern_Map :=  ('0' => "0001101",
                                           '1' => "0011001",
                                           '2' => "0010011",
                                           '3' => "0111101",
                                           '4' => "0100011",
                                           '5' => "0110001",
                                           '6' => "0101111",
                                           '7' => "0111011",
                                           '8' => "0110111",
                                           '9' => "0001011");
      Right_Map : constant Pattern_Map := ('0' => "1110010",
                                           '1' => "1100110",
                                           '2' => "1101100",
                                           '3' => "1000010",
                                           '4' => "1011100",
                                           '5' => "1001110",
                                           '6' => "1010000",
                                           '7' => "1000100",
                                           '8' => "1001000",
                                           '9' => "1110100");
      End_Guard    : constant String := "101";
      Middle_Guard : constant String := "01010";

      procedure Draw (Info : in out Drawing_Info; X : in out Natural; Pattern : in String) with
         Pre => (for all C of Pattern => C in '0' .. '1');
      -- Draws the bits of Pattern in Info starting at X
      -- Advances X to the position after Pattern

      function Checksum return Digit;
      -- Returns the checksum digit for Text

      procedure Draw (Info : in out Drawing_Info; X : in out Natural; Pattern : in String) is
         -- Empty
      begin -- Draw
         All_Lines : for C of Pattern loop
            if C = '1' then
               Draw_Module (Info => Info, X => X, Y => 0);
            end if;

            X := X + 1;
         end loop All_Lines;
      end Draw;

      function Checksum return Digit is
         Sum : Natural := 0;
      begin -- Checksum
         Add : for I in Text'Range loop
            Sum := Sum + (if (I - Text'First + 1) rem 2 = 0 then 1 else 3) * (Character'Pos (Text (I) ) - Character'Pos ('0') );
         end loop Add;

         Sum := Sum rem 10;

         if Sum > 0 then
            Sum := 10 - Sum;
         end if;

         return Character'Val (Character'Pos ('0') + Sum);
      end Checksum;

      X : Natural := 0;
   begin -- Draw
      Draw (Info => Info, X => X, Pattern => End_Guard);

      Draw_Left : for I in Text'First .. Text'First + 5 loop
         Draw (Info => Info, X => X, Pattern => Left_Map (Text (I) ) );
      end loop Draw_Left;

      Draw (Info => Info, X => X, Pattern => Middle_Guard);

      Draw_Right : for I in Text'First + 6 .. Text'Last loop
         Draw (Info => Info, X => X, Pattern => Right_Map (Text (I) ) );
      end loop Draw_Right;

      Draw (Info => Info, X => X, Pattern => Right_Map (Checksum) );
      Draw (Info => Info, X => X, Pattern => End_Guard);
   end Draw;
end Bar_Code_Drawing.What.UPCA;
