with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure TicTacToe is
   n, r, c, turn : integer;
   p, o : character;
   taken : boolean := true;
   win : boolean := false;
   board : array(1..3, 1..3) of character;

   -- Checks if there's a winner or a tie, return win state (1 - player, 2 - opponent, 0 - tie, 4 - no win)
   function win_check(I : integer) return integer is
      t : integer := board'length;
      x_c, o_c, x_r, o_r, x_d1, o_d1, x_d2, o_d2 : integer := 0;
      tie : boolean := true;
      w_c, w_r, d1_c, d2_c : array(1..3) of character;

   begin
      -- Goes through rows
      for i in board'range loop

         -- Goes through columns
         for j in board'range loop

            w_r(j) := board(i, j); -- rows
            w_c(j) := board(j, i); -- columns

            -- Checks if rows or columns have all the same symbol
            if p in w_r(j) then
               x_r := x_r + 1;
            elsif o in w_r(j) then
               o_r := o_r + 1;
            end if;
            if p in w_c(j) then
               x_c := x_c + 1;
            elsif o in w_c(j) then
               o_c := o_c + 1;
            end if;

            -- Checks if there's an open space
            if '-' = board(i,j) then
               tie := false;
            end if;
         end loop;

         d1_c(i) := board(i, i); -- diagonal1
         d2_c(i) := board(i, t); -- diagonal2

         -- Checks if diagonals have all the same symbol
         if p in d1_c(i) then
            x_d1 := x_d1 + 1;
         elsif o in d1_c(i) then
            o_d1 := o_d1 + 1;
         end if;

         if p in d2_c(i) then
            x_d2 := x_d2 + 1;
         elsif o in d2_c(i) then
            o_d2 := o_d2 + 1;
         end if;

         -- Checks if rows or columns have the same symbol
         if x_c = 3 or x_r = 3 or x_d1 = 3 or x_d2 = 3 then
            return 1;

         elsif o_c = 3 or o_r = 3 or o_d1 = 3 or o_d2 = 3 then
            return 2;

         else
            x_r := 0;
            x_c := 0;
            x_d1 := 0;
            x_d2 := 0;
            o_r := 0;
            o_c := 0;
            o_d1 := 0;
            o_d2 := 0;

         end if;
         t := t - 1;
      end loop;
      if tie then
         return 0;
      end if;
      return 4;
   end win_check;

begin
   turn := 1;

   -- Fill up board array
   for i in board'range loop
      for j in board'range loop
         board(i, j) := '-';
      end loop;
   end loop;

   Put_Line("Welcome to Tic Tac Toe. Select a symbol: (1-X 2-O)");

   -- Make sure user puts correct input
   loop
      Get(n);
      if n = 1 then
         p := 'X';
         o := 'O';
      elsif n = 2 then
         p := 'O';
         o := 'X';
      else
         Put_Line("Pick again.");
      end if;
      exit when n = 1 or n = 2;
   end loop;

   Put_Line("You picked " & p);

   -- Display board
   for i in board'range loop
      for j in board'range loop
         Put('|');
         Put(board(i, j));
         if j = 3 then
            Put('|');
            New_Line(1);
         end if;
      end loop;
   end loop;

   Put_Line("You will be prompted to enter 2 numbers. First, pick the row, then pick the column.");

   -- Loop until someone wins
   loop
      -- Make sure spot picked isn't taken
      loop
         -- User picks a row
         loop
            if turn mod 2 /= 0 then
               New_Line(1);
               Put_Line("Your move.");
            else
               New_Line(1);
               Put_Line("Your opponent's move.");
            end if;

            Put_Line("Select row: ");
            Get(r);

            if r = 1 or r = 2 or r = 3 then
               Put_Line("Select column: ");
            else
               Put_Line("Pick again.");
            end if;
            exit when r = 1 or r = 2 or r = 3;
         end loop;

         -- User picks a column
         loop
            Get(c);

            if c = 1 or c = 2 or c = 3 then
               if board(r, c) = '-' then
                  if turn mod 2 /= 0 then
                     board(r, c) := p;
                  else
                     board(r, c) := o;
                  end if;
                  taken := false;
               else
                  Put_Line("Spot taken. Pick again.");
                  taken := true;
               end if;
            else
               Put_Line("Pick again.");
            end if;
            exit when c = 1 or c = 2 or c = 3;
         end loop;
         exit when taken = false;
      end loop;

      -- Put symbol into board array
      for i in board'range loop
         for j in board'range loop
            Put('|');
            Put(board(i, j));
            if j = board'length then
               Put('|');
               New_Line(1);
            end if;
         end loop;
      end loop;

      -- Checks if someone won
      if win_check(1) = 1 then
         Put_Line("You Win!");
         win := true;
      elsif win_check(1) = 2 then
         Put_Line("Your Opponent Wins!");
         win := true;
      elsif win_check(1) = 0 then
         Put_Line("It's a tie.");
         win := true;
      end if;

      turn := turn + 1;

      exit when win = true;
   end loop;
end TicTacToe;
