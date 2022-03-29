-- Ada
-- Finding the sum of all the primes below two million

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Pe_010 is
   primeList : array(Natural range 2..2000000) of Boolean := (others => True);
   sum : Long_Long_Long_Integer := 0;

begin
   for x in primeList'First..Integer(Sqrt(Float(primeList'Last))) loop
      declare
         j : Integer := x ** 2;
      begin
         if primeList(j) then
            while j < primeList'Last loop
               primeList(j) := False;
               j := j + x;
            end loop;
         end if;
      end;
   end loop;

   for i in primeList'Range loop
      if primeList(i) then
         sum := sum + Long_Long_Long_Integer(i);
      end if;
   end loop;

   Put_Line(Long_Long_Long_Integer'image(sum));
end Pe_010;
