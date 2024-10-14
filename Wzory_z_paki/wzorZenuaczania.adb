with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;

procedure demo is

   task type Aaa is
      entry Start;
   end Aaa;

   task type Bbb is
      entry Start;
   end Bbb;

   task type Aabb is
      entry APrint;
      entry BPrint;
   end Aabb;

   A  : Aaa;
   B  : Bbb;
   AB : Aabb;

   task body Aaa is
      subtype Aaa_Time_Range is Integer range 1 .. 2;
      package Random_Aaa is new Ada.Numerics.Discrete_Random (Aaa_Time_Range);
      G           : Random_Aaa.Generator;
      Random_Time : Duration;
      a           : String (1 .. 3);
      b           : Integer;
   begin
      accept Start do
         Random_Aaa.Reset (G);
      end Start;
      Put_Line (ESC & "[93m" & "Aaa start" & ESC & "[0m");
      loop
        Random_Time := Duration (Random_Aaa.Random (G));
        Put_Line (Duration'Image(Random_Time));
        select
            --delay Random_Time;
            AB.APrint;
           then abort
            Put_Line ("teraz nic nie robie, nacisnij przycisk");
            Get_Line (a, b);
        end select;
      end loop;
   end Aaa;

   task body Bbb is
      subtype Bbb_Time_Range is Integer range 3 .. 6;
      package Random_bbb is new Ada.Numerics.Discrete_Random (Bbb_Time_Range);
      G           : Random_bbb.Generator;
      Random_Time : Duration;
   begin
      accept Start do
         Random_bbb.Reset (G);
      end Start;
      Put_Line (ESC & "[93m" & "Bbb start" & ESC & "[0m");
      loop
         Random_Time := Duration (Random_bbb.Random (G));
         delay Random_Time;
         AB.BPrint;
      end loop;
   end Bbb;

   task body Aabb is
      subtype Aabb_Range is Integer range 1 .. 2;
      package Random_aabb is new Ada.Numerics.Discrete_Random (Aabb_Range);
      G : Random_aabb.Generator;   
      dice: Integer;
   begin
      Put_Line (ESC & "[91m" & "Aabb started" & ESC & "[0m");
      Random_aabb.Reset (G);
      loop
         dice := Random_aabb.Random(G);
         Put_Line("Dice: "  & Integer'Image(dice));

         select
            accept APrint do
               Put_Line (ESC & "[95m" & " AAA " & ESC & "[0m");
            end APrint;

         --or
            --accept BPrint do
               --Put_Line (ESC & "[96m" & " BBB " & ESC & "[0m");
            --end BPrint;

            --  else
            --     delay 1.0;
            --     Put_Line (ESC & "[96m" & " NUDZE SIE " & ESC & "[0m");
         end select;

      end loop;
   end Aabb;

begin
   A.Start;
   B.Start;

end demo;
