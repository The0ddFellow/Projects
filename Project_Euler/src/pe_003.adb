-- Ada
-- Finding the largest prime factor of the number 600851475143

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure PE_003 is

   p, c : integer;
   n : Long_Long_Long_Integer := 600851475143;
   f : array(2..Integer(Sqrt(Float(n)))) of boolean := (others => false);
      
begin
   c := 0;
   for i in f'range loop
      p := i ** 2 + c * i;
      if n mod Long_Long_Long_Integer(i) = 0 then
         Put(i);
         f(i) := true;
         while p <= f'length loop
            f(i) := false;
            c := c + 1;
            p := i ** 2 + c * i;
         end loop;
      end if;
   end loop;

end PE_003;
