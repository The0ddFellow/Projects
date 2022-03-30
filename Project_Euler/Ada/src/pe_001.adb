-- Ada
-- Finding the sum of all the multiples of 3 or 5 below 1000

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure PE_001 is

   sum : integer := 0;
   
begin

   for i in 0..999 loop
      if i mod 3 = 0 or i mod 5 = 0 then
         sum := sum + (1 / 2) * i * (i + 1);
      end if;
   end loop;

   Put(sum);
end PE_001;
