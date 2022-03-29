-- Ada
-- Finding how many routes are there through a 20×20 grid
-- starting at the top left corner to the bottom right corner

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_015 is
   subtype Size is Natural range 1..21;
   type Matrix is array(Size, Size) of Long_Long_Integer;
   grid : Matrix := (others => (others => 1));

begin
   for y in 2..grid'Length loop
      for x in 2..grid'Length loop
         grid(x, y) := grid(x - 1, y) + grid(x, y - 1);
      end loop;
   end loop;

   Put_Line(Long_Long_Integer'Image(grid(grid'Length, grid'Length)));
end Pe_015;
