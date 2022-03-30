-- Ada
-- Finding the greatest product of four adjacent numbers in the same
-- direction (up, down, left, right, or diagonally) in the 20×20 grid

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Pe_011 is
   grid_file : File_Type;
   subtype r is Positive range 1..4;
   subtype Size is Positive range 1..20;
   type Matrix is array(Size, Size) of Integer;
   grid : Matrix;
   ox, oy : Integer := 0;
   product, temp_p : Integer := 1;

begin
   Open(File => grid_file, Mode => In_File, Name => "C:\Users\Stephan\Documents\Ada_Projects\Project_Euler\src\PE_11-File.txt");
   for y in grid'Range loop
      for x in 2..grid'Length loop
         if not End_Of_File(grid_file) then
            Get(File => grid_file, Item => grid(x, y));
            Put(grid(x, y));
         end if;
      end loop;
      New_Line;
   end loop;
   Close(File => grid_file);

   for y in grid'Range loop
      for x in 2..grid'Length loop
         for dir in r loop
            if dir = 1 then
               ox := -1;
               oy := 1;
            end if;
            if dir = 2 then
               ox := 0;
               oy := 1;
            end if;
            if dir = 3 then
               ox := 1;
               oy := 1;
            end if;
            if dir = 4 then
               ox := 1;
               oy := 0;
            end if;

            if x + (ox * r'Last) > 0 and x + (ox * r'Last) <= grid'Length
              and y + (oy * r'Last) > 0 and y + (oy * r'Last) <= grid'Length
            then
               for j in 0..3 loop
                  temp_p := temp_p * Integer(grid(x + (ox * j), y + (oy * j)));
               end loop;
               if temp_p > product then
                  product := temp_p;
               end if;
               temp_p := 1;
            end if;
         end loop;
      end loop;
   end loop;

   Put_Line("Product: " & Integer'Image(product));
end Pe_011;
