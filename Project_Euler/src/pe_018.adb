-- Ada
-- Finding the maximum total from top to bottom of the triangle below

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_018 is
   grid_file : File_Type;
   subtype Size is Natural range 1..15;
   type Matrix is array(Size, Size) of Integer;
   grid : Matrix;
   ans : Integer := 0;

begin
   Open(File => grid_file, Mode => In_File, Name => "C:\Users\Stephan\Documents\Ada_Projects\Project_Euler\src\PE_18-File.txt");
   for y in grid'Range loop
      for x in 1..y loop
         if not End_Of_File(grid_file) then
            Get(File => grid_file, Item => grid(x, y));
         end if;
      end loop;
   end loop;
   Close(File => grid_file);

   for y in reverse grid'First..grid'Length - 1 loop
      for x in grid'First..y loop
         if grid(x, y + 1) > grid(x + 1, y + 1) then
            grid(x, y) := grid(x, y) + grid(x, y + 1);
         else
            grid(x, y) := grid(x, y) + grid(x + 1, y + 1);
         end if;
      end loop;
   end loop;

   Put(grid(1, 1));
end Pe_018;
