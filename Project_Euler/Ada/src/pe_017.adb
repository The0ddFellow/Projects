-- Ada
-- Finding how many letters would be used if all the numbers
-- from 1 to 1000 (one thousand) inclusive were written out in words

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_017 is
   ones : constant array(Natural range 1..9) of Integer := (3, 3, 5, 4, 4, 3, 5, 5, 4);
   f_ten : constant array(Natural range 1..9) of Integer := (6, 6, 8, 8, 7, 7, 9, 8, 8);
   tens : constant array(Natural range 1..9) of integer := (3, 6, 6, 5, 5, 5, 7, 6, 6);
   thousand : constant Integer := 8;
   hundred : constant Integer := 7;
   a : constant Integer := 3;
   ans : Integer := 0;

begin
   for i in 1..1000 loop
      declare
         i_str : String(1..4) := (others => '0');
         c : Integer := 1;
      begin
         for n in reverse 2..Integer'Image(i)'Length loop
            i_str(c) := Integer'Image(i)(n);
            c := c + 1;
         end loop;

         if i_str'Length >= 4 and i_str(4) /= '0' then
            for num in ones'Range loop
               if Character'Val(num + 48) = i_str(4) then
                  ans := ans + ones(num) + thousand;
               end if;
            end loop;

            if i_str(3) = '0'
              and (i_str(2) /= '0' or i_str(1) /= '0')
            then
               ans := ans + a;
            end if;
         end if;

         if i_str'Length >= 3
           and i_str(3) /= '0'
         then

            for num in ones'Range loop
               if Character'Val(num + 48) = i_str(3) then
                  ans := ans + ones(num);
               end if;
            end loop;
            ans := ans + hundred;

            if i_str(2) /= '0' or i_str(1) /= '0' then
               ans := ans + a;
            end if;
         end if;

         if i_str'Length >= 2
           and i_str(2) /= '0'
         then

            if i_str(2) = '1' then
               if i_str(1) = '0' then
                  ans := ans + tens(1);

               else
                  for num in f_ten'Range loop
                     if Character'Val(num + 48) = i_str(1) then
                        ans := ans + f_ten(num);
                     end if;
                  end loop;
               end if;

            else
               for num in tens'Range loop
                  if Character'Val(num + 48) = i_str(2) then
                     ans := ans + tens(num);
                  end if;
               end loop;
            end if;
         end if;

         if i_str'Length >= 1
           and i_str(1) /= '0'
           and i_str(2) /= '1'
         then

            for num in ones'Range loop
               if Character'Val(num + 48) = i_str(1) then
                  ans := ans + ones(num);
               end if;
            end loop;
         end if;
      end;
   end loop;

   Put_Line(Integer'Image(ans));
end Pe_017;
