-- Ada
-- Finding a starting number under one million
-- that produces the longest Collatz sequence

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_014 is
   i, ans, len : Integer := 0;
   n : Long_Long_Integer := 0;

begin
   for x in 1..999999 loop
      n := Long_Long_Integer(x);
      if n mod 2 /= 0 then
         loop
            if n mod 2 = 0 then
               n := n / 2;
            else
               n := 3 * n + 1;
            end if;
            exit when n = 1;
            i := i + 1;
         end loop;
      end if;
      if len < i then
         len := i;
         ans := x;
      end if;
      i := 0;
   end loop;

   Put_Line(Integer'Image(ans));
end Pe_014;
