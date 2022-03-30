-- Finding the millionth lexicographic permutation
-- of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_024 is
   subtype Size is Natural range 0..9;
   type int_arr is array(Size) of Integer;
   lexicon : int_arr;
   permutations : Integer := 1;
   small_i, ind, len, count : Integer := 0;
   small : Integer := Integer'Last;

   procedure swap(a, b : Integer; lex : in out int_arr) is
      temp : Integer;
   begin
      temp := lex(a);
      lex(a) := lex(b);
      lex(b) := temp;
   end swap;

begin
   for i in lexicon'Range loop
      lexicon(i) := i;
   end loop;

   Permute:
   loop
      len := lexicon'Last;

      loop
         if len - 1 >= 0 then
            if lexicon(len) > lexicon(len - 1) then
               ind := len - 1;
               exit;
            end if;
         end if;

         len := len - 1;
      end loop;

      for i in ind + 1..lexicon'Last loop
         if small > lexicon(i) and lexicon(ind) < lexicon(i) then
            small := lexicon(i);
            small_i := i;
         end if;
      end loop;

      if small_i /= ind then
         swap(small_i, ind, lexicon);
      end if;

      for i in ind + 1..lexicon'Last loop
         if lexicon'last + count < lexicon'Length then
            swap(i, lexicon'last - count, lexicon);
            count := count + 1;
         end if;
      end loop;

      permutations := permutations + 1;
      exit Permute when permutations = 1000000;
      count := 0;
      small := Integer'Last;
   end loop Permute;

   for i of lexicon loop
      Put(Integer'Image(i));
   end loop;
   New_Line;
   Put("2783915460");
end Pe_024;
