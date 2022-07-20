-- A library for drawing bar codes
--
-- Copyright (C) by PragmAda Software Engineering
--
-- Released under the terms of the 3-Clause BSD License. See https://opensource.org/licenses/BSD-3-Clause

package body Bar_Code_Drawing.How.Ada_GUI is
   procedure Render (Info : in Drawing_Info; ID : in GUI.Widget_ID) is
      X     : Natural := Info.Bitmap'First (1);
      Y     : Natural;
      Start : Natural := Info.Bitmap'First (1);
      Stop  : Natural;
   begin -- Render
      if Info.Dim = 2 then -- 2D code
         Draw_X : loop
            exit Draw_X when X not in Info.Bitmap'Range (1);

            Y := Info.Bitmap'First (2);

            Draw_Y : loop
               exit Draw_Y when Y not in Info.Bitmap'Range (2);

               if Info.Bitmap (X, Y) then
                  ID.Draw_Rectangle (From_X     => X,
                                     From_Y     => Info.Last_Y - Y,
                                     To_X       => X + Info.Scale - 1,
                                     To_Y       => Info.Last_Y - Y - Info.Scale + 1,
                                     Line_Color => (None => True),
                                     Fill_Color => (None => False, Color => GUI.To_Color (GUI.Black) ) );
               end if;

               Y := Y + Info.Scale;
            end loop Draw_Y;

            X := X + Info.Scale;
         end loop Draw_X;
      else -- 1D code
         All_Bars : loop
            exit All_Bars when Start not in Info.Bitmap'Range (1);

            Find_Bar : loop
               exit Find_Bar when Start not in Info.Bitmap'Range (1) or else Info.Bitmap (Start, 0);

               Start := Start + Info.Scale;
            end loop Find_Bar;

            exit All_Bars when Start not in Info.Bitmap'Range (1);

            Stop := Start + Info.Scale - 1;

            Find_Edge : loop
               exit Find_Edge when Stop + 1 not in Info.Bitmap'Range (1) or else not Info.Bitmap (Stop + 1, 0);

               Stop := Stop + Info.Scale;
            end loop Find_Edge;

            ID.Draw_Rectangle (From_X     => Start,
                               From_Y     => 0,
                               To_X       => Stop,
                               To_Y       => Info.Height,
                               Line_Color => (None => True),
                               Fill_Color => (None => False, Color => GUI.To_Color (GUI.Black) ) );
            Start := Stop + 1;
         end loop All_Bars;
      end if;
   end Render;
end Bar_Code_Drawing.How.Ada_GUI;
