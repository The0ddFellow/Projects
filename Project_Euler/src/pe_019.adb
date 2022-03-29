-- Finding how many Sunmonth_days fell on the first of the month
-- during the twentieth century (1 Jan 1901 to 31 Dec 2000)

with Ada.Text_IO; use Ada.Text_IO;

procedure Pe_019 is
   month_days : array(Natural range 1..12) of Integer :=
     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   subtype years is Natural range 1901..2000;
   weekday_month, weekday_year, ans : Integer := 0;
   leap : array(years) of Boolean := (others => False);

begin
   weekday_year := 2;

   for i in leap'Range loop
      if i mod 4 = 0 then
         leap(i) := True;
         if i mod 100 = 0 and i mod 400 /= 0 then
            leap(i) := False;
         end if;
      end if;
   end loop;

   for i in years loop
      if leap(i) then
         month_days(2) := 29;
         weekday_year := weekday_year + 1;
      else
         month_days(2) := 28;
         if i - 1 >= leap'First then
            if leap(i - 1) then
               weekday_year := weekday_year + 2;
            else
               weekday_year := weekday_year + 1;
            end if;
         end if;
      end if;

      if weekday_year > 7 then
         weekday_year := weekday_year rem 7;
      end if;
      weekday_month := weekday_year;

      for j in month_days'Range loop
         if j = 1 and weekday_year rem 7 = 0 then
            ans := ans + 1;
         elsif weekday_month mod 7 = 0 then
            ans := ans + 1;
         end if;
         weekday_month := weekday_month + (month_days(j) rem 7);
      end loop;
   end loop;

   Put_Line(Integer'Image(ans));
end Pe_019;
