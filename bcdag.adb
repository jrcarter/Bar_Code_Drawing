-- A program to test drawing bar codes with Ada-GUI
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada.Strings.Unbounded;
with Ada_GUI;
with Bar_Code_Drawing.How.Ada_GUI;
with Bar_Code_Drawing.What.Code_128;
with Bar_Code_Drawing.What.Data_Matrix;
with Bar_Code_Drawing.What.QR_Code;
with Bar_Code_Drawing.What.MSI_Code;
with Bar_Code_Drawing.What.UPCA_EAN13;

procedure BCDAG is
   package Code_128 renames Bar_Code_Drawing.What.Code_128;
   package Code_QR  renames Bar_Code_Drawing.What.QR_Code;
   package Code_DM  renames Bar_Code_Drawing.What.Data_Matrix;
   package Code_MSI renames Bar_Code_Drawing.What.MSI_Code;
   package Code_UPC renames Bar_Code_Drawing.What.UPCA_EAN13;

   H128   : Ada_GUI.Widget_ID;
   C128   : Ada_GUI.Widget_ID;
   ASC    : Ada_GUI.Widget_ID;
   Gen128 : Ada_GUI.Widget_ID;

   HQR   : Ada_GUI.Widget_ID;
   CQR   : Ada_GUI.Widget_ID;
   Text  : Ada_GUI.Widget_ID;
   ECL_T : Ada_GUI.Widget_ID;
   ECL   : Ada_GUI.Widget_ID;
   GenQR : Ada_GUI.Widget_ID;

   HDM   : Ada_GUI.Widget_ID;
   CDM   : Ada_GUI.Widget_ID;
   Word  : Ada_GUI.Widget_ID;
   GenDM : Ada_GUI.Widget_ID;

   HMSI   : Ada_GUI.Widget_ID;
   CMSI   : Ada_GUI.Widget_ID;
   Digit  : Ada_GUI.Widget_ID;
   GenMSI : Ada_GUI.Widget_ID;
   SepMSI : Ada_GUI.Widget_ID;

   HUPC   : Ada_GUI.Widget_ID;
   CUPC   : Ada_GUI.Widget_ID;
   N12    : Ada_GUI.Widget_ID;
   GenUPC : Ada_GUI.Widget_ID;

   I128  : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (1016, 100, 1);
   IQR   : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info ( 250, 250, 2);
   IMSI  : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info ( 630, 100, 1);
   IUPC  : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (2 * Code_UPC.Width, 100, 1);
   Event : Ada_GUI.Next_Result_Info;

   CDM_Size : constant := 250;

   function US (S : in String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

   use type Ada_GUI.Event_Kind_ID;
   use type Ada_GUI.Widget_ID;
begin -- BCDAG
   Ada_GUI.Set_Up (Grid => (1 => (1 => <>, 2 => (Kind => Ada_GUI.Extension) ),
                            2 => (1 => <>, 2 => <>),
                            3 => (1 => <>, 2 => (Kind => Ada_GUI.Extension) ),
                            4 => (1 => <>, 2 => (Kind => Ada_GUI.Extension) ) ),
                   Title => "Bar-Code Drawing Test");
   H128 := Ada_GUI.New_Background_Text (Text => "Code 128: Full ASCII");
   C128 := Ada_GUI.New_Graphic_Area (Width => I128.Width, Height => I128.Height, Break_Before => True);
   ASC  :=
      Ada_GUI.New_Text_Box (Break_Before => True, Label => "ASCII to encode", Width => 45, Placeholder => "ASCII to encode");
   Gen128 := Ada_GUI.New_Button (Text => "Generate", Break_Before => True);

   HQR  := Ada_GUI.New_Background_Text (Row => 2, Column => 1, Text => "QR code: Full Latin 1", Break_Before => True);
   CQR  := Ada_GUI.New_Graphic_Area (Row => 2, Column => 1, Width => IQR.Width, Height => IQR.Height, Break_Before => True);
   Text := Ada_GUI.New_Text_Box
             (Row          => 2,
              Column       => 1,
              Break_Before => True,
              Label        => "Téxt tô éncôdé",
              Width        => 80,
              Placeholder  => "Téxt tô éncôdé");
   ECL_T := Ada_GUI.New_Background_Text (Row => 2, Column => 1, Text => "Error Correction Level:", Break_Before => True);
   ECL   := Ada_GUI.New_Radio_Buttons
               (Row         => 2,
                Column      => 1,
                Label       => (1 => US ("Low"), 2 => US ("Medium"), 3 => US ("Quartile"), 4 => US ("High") ),
                Orientation => Ada_GUI.Horizontal);
   GenQR := Ada_GUI.New_Button (Row => 2, Column => 1, Text => "Generate", Break_Before => True);

   HDM  := Ada_GUI.New_Background_Text (Row => 2, Column => 2, Text => "Data Matrix code: Full Latin 1", Break_Before => True);
   CDM  := Ada_GUI.New_Graphic_Area (Row => 2, Column => 2, Width => CDM_Size, Height => CDM_Size, Break_Before => True);
   Word := Ada_GUI.New_Text_Box
             (Row          => 2,
              Column       => 2,
              Break_Before => True,
              Label        => "Téxt tô éncôdé",
              Width        => 80,
              Placeholder  => "Téxt tô éncôdé");
   GenDM := Ada_GUI.New_Button (Row => 2, Column => 2, Text => "Generate", Break_Before => True);

   HMSI  := Ada_GUI.New_Background_Text (Row => 3, Column => 1, Text => "MSI code: digits only", Break_Before => True);
   CMSI  := Ada_GUI.New_Graphic_Area (Row => 3, Column => 1, Width => IMSI.Width, Height => IMSI.Height, Break_Before => True);
   Digit := Ada_GUI.New_Text_Box
               (Row          => 3,
                Column       => 1,
                Break_Before => True,
                Label        => "Digits to encode",
                Width        => 25,
                Placeholder  => "Digits to encode");
   GenMSI := Ada_GUI.New_Button (Row => 3, Column => 1, Text => "Generate", Break_Before => True);
   SepMSI := Ada_GUI.New_Background_Text (Row => 3, Column => 1, Text => "<b>* * *</b>", Break_Before => True);

   HUPC := Ada_GUI.New_Background_Text (Row => 4, Column => 1, Text => "UPC/EAN code: 11-12 digits only", Break_Before => True);
   CUPC := Ada_GUI.New_Graphic_Area (Row => 4, Column => 1, Width => IUPC.Width, Height => IUPC.Height, Break_Before => True);
   N12  := Ada_GUI.New_Text_Box
              (Row          => 4,
               Column       => 1,
               Break_Before => True,
               Label        => "11-12 digits to encode",
               Width        => 13,
               Placeholder  => "11-12 Digits");
   GenUPC := Ada_GUI.New_Button (Row => 4, Column => 1, Text => "Generate", Break_Before => True);

   All_Events : loop
      Event := Ada_GUI.Next_Event;

      if not Event.Timed_Out then
         exit All_Events when Event.Event.Kind = Ada_GUI.Window_Closed;

         if Event.Event.Kind = Ada_GUI.Left_Click then
            if Event.Event.ID = Gen128 then -- Code 128
               Get_Line : declare
                  Line : constant String := ASC.Text;

                  Last : Natural := Line'Last;
               begin -- Get_Line
                  if (for Some C of Line => C not in Bar_Code_Drawing.What.ASCII) then
                     ASC.Set_Text (Text => "ASCII only!");
                  else
                     Shorten : loop
                        exit Shorten when 2 * Code_128.Width (Line (1 .. Last) ) <= I128.Width;

                        Last := Last - 1;
                     end loop Shorten;

                     if Last > 0 then
                        ASC.Set_Text (Text => Line (1 .. Last) );
                        C128.Draw_Rectangle (From_X     => 0,
                                             From_Y     => 0,
                                             To_X       => I128.Width,
                                             To_Y       => I128.Height,
                                             Line_Color => (None => True),
                                             Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
                        I128.Reset;
                        Code_128.Draw (Info => I128, Text => Line (1 .. Last) );
                        Bar_Code_Drawing.How.Ada_GUI.Render (Info => I128, ID => C128, Scale => 2);
                     end if;
                  end if;
               end Get_Line;
            elsif Event.Event.ID = GenQR then -- QR
               Get_QR : declare
                  Line  : constant String := Text.Text;
                  Level : constant Code_QR.Error_Correction_Level := (case Integer'(ECL.Active) is
                                                                      when 1 => Code_QR.Low,
                                                                      when 2 => Code_QR.Medium,
                                                                      when 3 => Code_QR.Quartile,
                                                                      when 4 => Code_QR.High,
                                                                      when others => raise Program_Error with "Invalid level");
               begin -- Get_QR
                  CQR.Draw_Rectangle (From_X     => 0,
                                      From_Y     => 0,
                                      To_X       => IQR.Width,
                                      To_Y       => IQR.Height,
                                      Line_Color => (None => True),
                                      Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
                  IQR.Reset;
                  Code_QR.Draw (Info => IQR, Text => Line, Level => Level);
                  Bar_Code_Drawing.How.Ada_GUI.Render (Info => IQR, ID => CQR, Scale => IQR.Width / Code_QR.Width (Line, Level) );
               exception -- Get_QR
               when others =>
                  Text.Set_Text (Text => "Text too long");
               end Get_QR;
            elsif Event.Event.ID = GenDM then -- Data Matrix
               Get_DM : declare
                  Line : constant String := Word.Text;

                  Width  : Positive;
                  Height : Positive;
               begin -- Get_DM
                  CDM.Draw_Rectangle (From_X     => 0,
                                      From_Y     => 0,
                                      To_X       => CDM_Size,
                                      To_Y       => CDM_Size,
                                      Line_Color => (None => True),
                                      Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
                  Code_DM.Get_Dimensions (Text => Line, Width => Width, Height => Height);

                  Info : declare
                     IDM : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (Width, Height, 2);
                  begin -- Info
                     Code_DM.Draw (Info => IDM, Text => Line);
                     Bar_Code_Drawing.How.Ada_GUI.Render (Info => IDM, ID => CDM, Scale => CDM_Size / Width);
                  end Info;
               exception -- Get_DM
               when others =>
                  Word.Set_Text (Text => "Text too long");
               end Get_DM;
            elsif Event.Event.ID = GenMSI then -- MSI
               Get_MSI : declare
                  Line : constant String := Digit.Text;

                  Last : Natural := Line'Last;
               begin -- Get_MSI
                  if (for Some C of Line => C not in Bar_Code_Drawing.What.Digit) then
                     Digit.Set_Text (Text => "Digits only!");
                  else
                     Truncate : loop
                        exit Truncate when 2 * Code_MSI.Width (Line (1 .. Last) ) <= IMSI.Width;

                        Last := Last - 1;
                     end loop Truncate;

                     if Last > 0 then
                        Digit.Set_Text (Text => Line (1 .. Last) );
                        CMSI.Draw_Rectangle (From_X     => 0,
                                             From_Y     => 0,
                                             To_X       => IMSI.Width,
                                             To_Y       => IMSI.Height,
                                             Line_Color => (None => True),
                                             Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
                        IMSI.Reset;
                        Code_MSI.Draw (Info => IMSI, Text => Line (1 .. Last) );
                        Bar_Code_Drawing.How.Ada_GUI.Render (Info => IMSI, ID => CMSI, Scale => 2);
                     end if;
                  end if;
               end Get_MSI;
            elsif Event.Event.ID = GenUPC then -- UPC/EAN
               Get_UPC : declare
                  Line : constant String := N12.Text;
               begin -- Get_UPC
                  if Line'Length not in 11 .. 12 or (for Some C of Line => C not in Bar_Code_Drawing.What.Digit) then
                     N12.Set_Text (Text => "11-12 digits!");
                  else
                     CUPC.Draw_Rectangle (From_X     => 0,
                                          From_Y     => 0,
                                          To_X       => IUPC.Width,
                                          To_Y       => IUPC.Height,
                                          Line_Color => (None => True),
                                          Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
                     IUPC.Reset;
                     Code_UPC.Draw (Info => IUPC, Text => Line);
                     Bar_Code_Drawing.How.Ada_GUI.Render (Info => IUPC, ID => CUPC, Scale => 2);
                  end if;
               end Get_UPC;
            else
               null;
            end if;
         end if;
      end if;
   end loop All_Events;

   Ada_GUI.End_GUI;
end BCDAG;
