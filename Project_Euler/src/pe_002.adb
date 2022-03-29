-- Ada
-- Finding the sum of the even-valued terms in the
-- Fibonacci sequence below 4 million

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure PE_002 is

   n : integer := 1;
   sum : integer := 2;
   prev : array(1..2) of integer := (1 => 1, 2 => 2);

begin
   loop
      n := prev(1) + prev(2);

      prev(1) := prev(2);
      prev(2) := n;

      if n mod 2 = 0 then
         sum := sum + n;
      end if;

      exit when n >= 4000000;
   end loop;
   Put(sum);
end PE_002;
