-- Finding the sum of the digits in the number 100!

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Pe_020 is
   num : Big_Integer := 100;
   sum : Big_Integer := 0;
   product : Big_Integer := 1;

begin
   for i in 1..To_Integer(num) loop
      product := product * To_Big_Integer(i);
   end loop;
   num := product;

   for i of To_String(num) loop
      if i /= ' ' then
         sum := sum + To_Big_Integer(Character'Pos(i) - 48);
      end if;
   end loop;

   Put_Line(To_String(sum));
end Pe_020;
