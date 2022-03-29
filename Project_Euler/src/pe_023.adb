-- Finding the sum of all the positive integers which
-- cannot be written as the sum of two abundant numbers

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_023 is
   largest_sum_of_two : constant Integer := 28123;
   subtype abundant_range is Natural range 12..largest_sum_of_two;
   subtype non_abundant_range is Natural range 1..largest_sum_of_two;
   abundant_nums : array(abundant_range) of Boolean := (others => False);
   non_abundant_nums : array(non_abundant_range) of Boolean := (others => True);
   abundant, properdiv_sum, abundant_sum, non_abundant_sum, len : Integer := 0;

begin
   for i in abundant_range loop
      abundant := i;
      for j in 1..abundant / 2 loop
         if abundant mod j = 0 then
            properdiv_sum := properdiv_sum + j;
         end if;
      end loop;

      if properdiv_sum > abundant then
         abundant_nums(i) := True;
         len := len + 1;
      end if;
      properdiv_sum := 0;
   end loop;

   for i in abundant_nums'Range loop
      if abundant_nums(i) then
         abundant_sum := i;
         for j in i..abundant_nums'Last loop
            if abundant_nums(j) then
               abundant_sum := abundant_sum + j;
               if abundant_sum <= non_abundant_nums'Last then
                  non_abundant_nums(abundant_sum) := False;
               end if;
               abundant_sum := i;
            end if;
         end loop;
      end if;
   end loop;

   for i in non_abundant_nums'Range loop
      if non_abundant_nums(i) then
         non_abundant_sum := non_abundant_sum + i;
      end if;
   end loop;

   Put_Line(Integer'Image(non_abundant_sum));
end Pe_023;
