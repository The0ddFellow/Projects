-- Ada
-- Finding the first ten digits of the sum of
-- the following one-hundred 50-digit numbers

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Pe_013 is
   numbers : File_Type;
   subtype Arr_Size is Positive range 1..100;
   subtype Str_Size is Positive range 1..50;
   str_num : String(Str_Size);
   big_num, sum : Big_Integer := 0;
   ans : String(1..10);

begin
   Open(File => numbers, Mode => In_File, Name => "C:\Users\Stephan\Documents\Ada_Projects\Project_Euler\src\PE_13-File.txt");
   for n in Arr_Size loop
      for i in Str_Size loop
         if not End_Of_File(numbers) then
            Get(File => numbers, Item => str_num(i));
         end if;
      end loop;
      big_num := From_String(str_num);
      sum := sum + big_num;
   end loop;
   Close(File => numbers);

   for i in ans'Range loop
      ans(i) := To_String(sum)(i + 1);
   end loop;

   Put_Line(ans);
end Pe_013;
