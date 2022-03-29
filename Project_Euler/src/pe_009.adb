-- Ada
-- Find the product abc of the Pythagorean
-- triplet for which a + b + c = 1000

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_009 is
   sum, product, a, b, c : Integer := 0;

begin
   product := 1;

   for n in 3..24 loop
      for m in n + 1..25 loop
         a := m ** 2 - n ** 2;
         b := 2 * n * m;
         c := m ** 2 + n ** 2;
         sum := a + b + c;
         if sum = 1000 then
            product := a * b * c;
         end if;
      end loop;
   end loop;

   Put_Line(Integer'Image(product));
end Pe_009;
