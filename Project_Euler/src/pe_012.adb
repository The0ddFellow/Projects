-- Ada
-- Finding the value of the first triangle
-- number to have over five hundred divisors

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Long_Elementary_Functions; use Ada.Numerics.Long_Elementary_Functions;

procedure Pe_012 is
   c_factors, num : Long_Integer := 0;
   tri, temp_tri : Long_Float := 0.0;

begin
   loop
      num := num + 1;
      c_factors := 0;
      temp_tri := 0.0;

      for i in 1..num loop
         temp_tri := temp_tri + Long_Float(i);
      end loop;

      for i in 1..Long_Integer(Sqrt(temp_tri)) loop
         if Long_Integer(temp_tri) mod i = 0 then
            c_factors := c_factors + 1;
         end if;
      end loop;

      c_factors := c_factors * 2 - 1;
      if temp_tri > tri then
         tri := temp_tri;
      end if;
      exit when c_factors > 500;
   end loop;

   Put_Line(Long_Integer'Image(Long_Integer(tri)));
end Pe_012;
