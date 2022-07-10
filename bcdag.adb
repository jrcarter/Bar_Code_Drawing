-- A program to test drawing bar codes with Ada-GUI
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

with Ada_GUI;
with Bar_Code_Drawing.How.Ada_GUI;
with Bar_Code_Drawing.What.Code_128;
with Bar_Code_Drawing.What.QR_Code;
with Bar_Code_Drawing.What.MSI_Code;

procedure BCDAG is
   package Code_128 renames Bar_Code_Drawing.What.Code_128;
   package Code_QR  renames Bar_Code_Drawing.What.QR_Code;
   package Code_MSI renames Bar_Code_Drawing.What.MSI_Code;

   H1    : Ada_GUI.Widget_ID;
   C128  : Ada_GUI.Widget_ID;
   ASC   : Ada_GUI.Widget_ID;
   Gen1  : Ada_GUI.Widget_ID;
   Sep1  : Ada_GUI.Widget_ID;
   H2    : Ada_GUI.Widget_ID;
   CQR   : Ada_GUI.Widget_ID;
   Text  : Ada_GUI.Widget_ID;
   Gen2  : Ada_GUI.Widget_ID;
   Sep2  : Ada_GUI.Widget_ID;
   H3    : Ada_GUI.Widget_ID;
   CMSI  : Ada_GUI.Widget_ID;
   Digit : Ada_GUI.Widget_ID;
   Gen3  : Ada_GUI.Widget_ID;
   I128  : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (1016, 100, 2);
   IQR   : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (250, 250);
   IMSI  : Bar_Code_Drawing.Drawing_Info := Bar_Code_Drawing.New_Info (630, 100, 2);
   Event : Ada_GUI.Next_Result_Info;

   use type Ada_GUI.Event_Kind_ID;
   use type Ada_GUI.Widget_ID;
begin -- BCDAG
   Ada_GUI.Set_Up (Title => "Bar-Code Drawing Test");
   H1 := Ada_GUI.New_Background_Text (Text => "Code 128: Full ASCII");
   C128 := Ada_GUI.New_Graphic_Area (Width => I128.Width, Height => I128.Height, Break_Before => True);
   ASC := Ada_GUI.New_Text_Box (Break_Before => True, Label => "ASCII to encode", Width => 45, Placeholder => "ASCII to encode");
   Gen1 := Ada_GUI.New_Button (Text => "Generate", Break_Before => True);
   Sep1 := Ada_GUI.New_Background_Text (Text => "<b>* * *</b>", Break_Before => True);
   H2 := Ada_GUI.New_Background_Text (Text => "QR code: Full Latin 1", Break_Before => True);
   CQR := Ada_GUI.New_Graphic_Area (Width => IQR.Width, Height => IQR.Height, Break_Before => True);
   Text := Ada_GUI.New_Text_Box (Break_Before => True, Label => "Téxt tô éncôdé", Width => 90, Placeholder => "Téxt tô éncôdé");
   Gen2 := Ada_GUI.New_Button (Text => "Generate", Break_Before => True);
   Sep2 := Ada_GUI.New_Background_Text (Text => "<b>* * *</b>", Break_Before => True);
   H3 := Ada_GUI.New_Background_Text (Text => "MSI code: digits only", Break_Before => True);
   CMSI := Ada_GUI.New_Graphic_Area (Width => IMSI.Width, Height => IMSI.Height, Break_Before => True);
   Digit :=
      Ada_GUI.New_Text_Box (Break_Before => True, Label => "Digits to encode", Width => 25, Placeholder => "Digits to encode");
   Gen3 := Ada_GUI.New_Button (Text => "Generate", Break_Before => True);

   All_Events : loop
      Event := Ada_GUI.Next_Event;

      if not Event.Timed_Out then
         exit All_Events when Event.Event.Kind = Ada_GUI.Window_Closed;

         if Event.Event.Kind = Ada_GUI.Left_Click then
            if Event.Event.ID = Gen1 then -- Code 128
               Get_Line : declare
                  Line : constant String := ASC.Text;

                  Last : Natural := Line'Last;
               begin -- Get_Line
                  if (for Some C of Line => C not in Bar_Code_Drawing.What.ASCII) then
                     ASC.Set_Text (Text => "ASCII only!");
                  else
                     Shorten : loop
                        exit Shorten when I128.Scale * Code_128.Width (Line (1 .. Last) ) <= I128.Width;

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
                        Bar_Code_Drawing.How.Ada_GUI.Render (Info => I128, ID => C128, Line => True);
                     end if;
                  end if;
               end Get_Line;
            elsif Event.Event.ID = Gen2 then -- QR
               Get_Text : declare
                  Line : constant String := Text.Text;

                  Last : Natural := Line'Last;
               begin -- Get_Text
                  CQR.Draw_Rectangle (From_X     => 0,
                                      From_Y     => 0,
                                      To_X       => IQR.Width,
                                      To_Y       => IQR.Height,
                                      Line_Color => (None => True),
                                      Fill_Color => (None => False, Color => Ada_GUI.To_Color (Ada_GUI.White) ) );
                  IQR.Reset;
                  IQR.Set_Scale (Scale => IQR.Width / Code_QR.Width (Line) );
                  Code_QR.Draw (Info => IQR, Text => Line);
                  Bar_Code_Drawing.How.Ada_GUI.Render (Info => IQR, ID => CQR, Line => False);
               end Get_Text;
            elsif Event.Event.ID = Gen3 then -- MSI
               Get_MSI : declare
                  Line : constant String := Digit.Text;

                  Last : Natural := Line'Last;
               begin -- Get_MSI
                  if (for Some C of Line => C not in Bar_Code_Drawing.What.Digit) then
                     Digit.Set_Text (Text => "Digits only!");
                  else
                     Truncate : loop
                        exit Truncate when IMSI.Scale * Code_MSI.Width (Line (1 .. Last) ) <= IMSI.Width;

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
                        Bar_Code_Drawing.How.Ada_GUI.Render (Info => IMSI, ID => CMSI, Line => True);
                     end if;
                  end if;
               end Get_MSI;
            else
               null;
            end if;
         end if;
      end if;
   end loop All_Events;

   Ada_GUI.End_GUI;
end BCDAG;
