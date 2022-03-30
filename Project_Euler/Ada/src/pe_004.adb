-- Ada
-- Finding the largest palindrome made
-- from the product of two 3-digit numbers

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure PE_004 is

   ans, product : integer := 0;
   palindrome : boolean := false;

   package Integer_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Integer);

   use Integer_Vectors;

   function rev(p : integer) return boolean is
      r_p : integer := 0;
      new_p : integer := p;
      palindrome : boolean := false;
      v_p, rev : vector;

   begin
      v_p.clear;
      rev.clear;
      while new_p > 0 loop
         r_p := new_p mod 10;
         rev.prepend(r_p);
         v_p.append(r_p);
         new_p := new_p / 10;
      end loop;

      if v_p = rev then
         palindrome := true;
      end if;

      return palindrome;
   end rev;

begin
   for i in 100..999 loop
      for j in i..999 loop
         product := i * j;
         palindrome := rev(product);
         if palindrome = true then
            if product > ans then
               ans := product;
            end if;
         end if;
      end loop;
   end loop;

   Put_Line(integer'image(ans));

end PE_004;
