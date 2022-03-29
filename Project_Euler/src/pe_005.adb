-- Ada
-- Looking for the smallest positive number that is
-- evenly divisible by all of the numbers from 1 to 20

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_005 is

   div : array(11..20) of positive;
   i : long_long_integer := 20;
   count : integer := 0;

begin

   for i in div'range loop
      div(i) := i;
   end loop;

   Full:
   loop
      for x of div loop
         if i mod long_long_integer(x) = 0 then
            count := count + 1;
         end if;
      end loop;

      if count = div'length then
         exit Full;
      end if;

      i := i + 2;
      count := 0;
   end loop Full;

   Put_Line(long_long_integer'image(i));
end Pe_005;
