-- Ada
-- Finding the sum of the digits of the number 2^1000

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Pe_016 is
   num : Big_Integer := 2 ** 1000;
   sum : Big_Integer := 0;

begin
   for i of To_String(num) loop
      if i /= ' ' then
         sum := sum + To_Big_Integer(Character'Pos(i) - 48);
      end if;
   end loop;

   Put_Line(To_String(sum));
end Pe_016;
