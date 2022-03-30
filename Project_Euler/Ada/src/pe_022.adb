-- Finding the total alphabetical value of all the name scores in the file

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Pe_022 is
   names : File_Type;

   package StringVector is new Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);
   package Sorter is new StringVector.Generic_Sorting;
   use StringVector;
   use Sorter;

   names_vec, name_scores : Vector;
   j : Integer := 1;
   ans : Long_Long_Long_Integer := 0;
   name_all, name_temp : Unbounded_String;
   buffer : Character;

begin
   Open(File => names, Mode => In_File, Name => "C:\Users\Stephan\Documents\Ada_Projects\Project_Euler\src\names.txt");
   while not End_Of_File(names) loop
      Get_Line(File => names, Item => name_all);
   end loop;
   Close(File => names);

   FileToVector:
   loop
      buffer := To_String(name_all)(j);
      if buffer /= ',' then
         loop
            if j + 1 <= To_String(name_all)'Length then
               j := j + 1;
               buffer := To_String(name_all)(j);
               exit when buffer = '"';
               Append(name_temp, buffer);
            end if;
            exit when j = To_String(name_all)'Length;
         end loop;
         names_vec.Append(name_temp);
         name_temp := To_Unbounded_String("");
      end if;
      exit FileToVector when j + 1 > To_String(name_all)'Length;
      j := j + 1;
   end loop FileToVector;

   Sort(Container => names_vec);
   declare
      last : Integer := Integer(names_vec.Last_Index);
      first : Integer := Integer(names_vec.First_Index);
      name_scores : array(0..last) of Integer := (others => 0);
      word_score : Integer := 0;

   begin
      for i in first..last loop
         for j of To_String(names_vec(i)) loop
            word_score := word_score + Character'Pos(j) - 64;
         end loop;
         name_scores(i) := (i + 1) * word_score;
         word_score := 0;
      end loop;

      for i of name_scores loop
         ans := ans + Long_Long_Long_Integer(i);
      end loop;
   end;

   Put_Line(Long_Long_Long_Integer'Image(ans));
end Pe_022;
