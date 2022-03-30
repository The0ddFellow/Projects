-- Ada
-- Finding the 10,001st prime number

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_007 is

   package Integer_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Long_Integer);
   use Integer_Vectors;

   primeList : Vector;
   num : Long_Integer := 3;
   count : integer := 0;

begin
   primeList.Append(2);

   loop
      for x in primeList.First_Index..primeList.Last_Index loop
         if num mod primeList(x) /= 0 then
            count := count + 1;

            if count = Integer(primeList.Length) then
               primeList.Append(num);
            end if;
         end if;
      end loop;

      exit when Integer(primeList.Length) >= 10001;
      num := num + 2;
      count := 0;
   end loop;

   Put_Line(Long_Integer'image(primeList.Last_Element));

end Pe_007;
