-- Ada
-- Finding the difference between the sum of the squares of the
-- first one hundred natural numbers and the square of the sum

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_006 is

   hundred : array(1..100) of positive;
   sum1, sum2, diff : integer := 0;

begin

   for i in hundred'range loop
      hundred(i) := i;
   end loop;

   SumSquare:
   for x of hundred loop
      sum1 := x ** 2 + sum1;
   end loop SumSquare;

   SquareSum:
   for x of hundred loop
      sum2 := sum2 + x;
   end loop SquareSum;
   sum2 := sum2 ** 2;

   diff := sum2 - sum1;
   Put_Line(integer'image(diff));

end Pe_006;
