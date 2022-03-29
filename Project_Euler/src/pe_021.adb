-- Finding the sum of all the amicable numbers under 10000.

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_021 is
   sum, amicable1, amicable2, i : Integer := 0;
begin
   i := 220;
   loop
      for j in 1..i / 2 loop
         if i mod j = 0 then
            amicable1 := amicable1 + j;
         end if;
      end loop;

      for j in 1..amicable1 / 2 loop
         if amicable1 mod j = 0 then
            amicable2 := amicable2 + j;
         end if;
      end loop;

      if amicable2 = i and amicable1 > i then
         sum := sum + amicable1 + i;
      end if;

      exit when i >= 10000;
      i := i + 2;
      amicable1 := 0;
      amicable2 := 0;
   end loop;

   Put_Line(Integer'Image(sum));
end Pe_021;
