with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
procedure main is
   ------------- Data and task types ------------
   type Integer_Access is access Integer;
   type String_Access is access String;
   type Array2D is array (Positive range <>, Positive range <>) of Boolean;
   task type Student (Name : String_Access; Is_infected : Boolean);
   type Student_Access is access Student;
   task type Uni is
      entry go_in (x : in Integer; y : in Integer; infect : in out Boolean);
      entry move_to (x : in Integer; y : in Integer; infect : in out Boolean);
      entry go_out;
   end Uni;
   type Uni_Access is access Uni;
   ------------ Global units --------
   delta_x    : array (Positive range 1 .. 4) of Integer := (-1, 0, 1, 0);
   delta_y    : array (Positive range 1 .. 4) of Integer := (0, 1, 0, -1);
   University : Uni_Access;
   package My_random_int is new Ada.Numerics.Discrete_Random (Positive);
   use My_random_int;

   function In_range
     (e : in Integer; low : in Integer; up : in Integer) return Boolean
   is
      ans : Boolean;
   begin
      ans := (e <= up and low <= e);
      return ans;
   end In_range;

   function Can_move
     (x : in Integer; y : in Integer; dir : in Integer) return Boolean
   is
      ans : Boolean;
   begin
      ans :=
        In_range (e => x + delta_x (dir), low => 1, up => 10) and
        In_range (e => y + delta_y (dir), low => 1, up => 10);
      return ans;
   end Can_move;

   procedure Move (x : in out Integer; y : in out Integer; dir : in Integer) is

   begin
      x := x + delta_x (dir);
      y := y + delta_y (dir);
   end Move;

   ---------- Protected objects --------
   protected My_rand is
      function New_int (L : Integer; R : Integer) return Integer;
   private
      G : Generator;
   end My_rand;

   protected body My_rand is
      function New_int (L : Integer; R : Integer) return Integer is
         Radom_range : Integer;
      begin
         Reset (G);
         Radom_range := R - L;
         return (((My_random_int.Random (G) rem Radom_range) + 1) + L);
      end New_int;
   end My_rand;

   protected Printer is
      procedure Print (S : in String);
   end Printer;

   protected body Printer is
      procedure Print (S : in String) is
      begin
         Put_Line (S);
      end Print;
   end Printer;

   protected Building is
      function getRoom (x : Integer; y : Integer) return Boolean;
      procedure setRoom (x : Integer; y : Integer; infected : in Boolean);
   private
      Rooms : Array2D (1 .. 10, 1 .. 10) := (others => (others => False));
   end Building;
   protected body Building is
      function getRoom (x : in Integer; y : in Integer) return Boolean is
         ans : Boolean;
      begin
         ans := Rooms (x, y);
         return ans;
      end getRoom;

      procedure setRoom (x : Integer; y : Integer; infected : in Boolean) is
      begin
         Rooms (x, y) := infected;
      end setRoom;

   end Building;

   -------------- Students constrol -----------------
   task Student_Control is
      entry Create_new;
      entry Init;
      entry Stop;
   end Student_Control;
   task body Student_Control is
      Student_Arr : array (Positive range 1 .. 5) of Student_Access;
      Active      : Boolean := True;
   begin
      while Active loop
         select
            accept Create_new do
               delay 0.01; -- wait for the task Student terminated;
               for i in 1 .. 5 loop
                  if Student_Arr (i)'Terminated then
                     Student_Arr (i) :=
                       new Student (new String'(Integer'Image (i)), False);
                  end if;
               end loop;
            end Create_new;
         or
            accept Init do
               for i in 1 .. 5 loop
                  Student_Arr (i) :=
                    new Student (new String'(Integer'Image (i)), True);
               end loop;
            end Init;
         or
            accept Stop do
               Active := False;
            end Stop;
         end select;
      end loop;
   end Student_Control;
   ------------- Student ---------------
   task body Student is
      Start_time     : Time;
      End_time       : Time;
      Stay_period    : Time_Span := Seconds (My_rand.New_int (1, 4));
      Infected       : Boolean   := Is_infected;
      Next_direction : Integer;
      X              : Integer   := My_rand.New_int (1, 10);
      Y              : Integer   := My_rand.New_int (1, 10);
   begin
      Start_time := Clock;
      End_time   := Start_time + Stay_period;

      University.go_in (x => X, y => Y, infect => Infected);
      Printer.Print ("Student" & Name.all & " start at" & X'Image & Y'Image);

      -- choose direction
      loop
         Next_direction := My_rand.New_int (1, 4);
         if Can_move (x => X, y => Y, dir => Next_direction) then
            Move (x => X, y => Y, dir => Next_direction);
            exit;
         end if;
      end loop;
      delay 0.5;--move after 0.5 seconds
      --  move to new coordinates
      University.move_to (x => X, y => Y, infect => Infected);
      Printer.Print ("Student" & Name.all & " move to" & X'Image & Y'Image);

      delay until End_time;
      University.go_out;
      Printer.Print ("Student" & Name.all & " go out");
      Student_Control.Create_new;
   end Student;

   -------------------- University ----------
   task body Uni is
      Max_limit         : Integer := 5;
      Open_duration     : Integer := 60;
      Infected_cells    : Integer := 0;
      Infected_students : Integer := 0;
      Total_students    : Integer := 0;
      Current_students  : Integer := 0;
      Start_time        : Time;
   begin
      Start_time := Clock;
      while Clock - Start_time < Seconds (Open_duration)
      loop --open for 60 seconds
         select when Current_students <= Max_limit =>
            accept go_in
              (x : in Integer; y : in Integer; infect : in out Boolean)
            do
               Total_students   := Total_students + 1;
               Current_students := Current_students + 1;
               if infect then
                  Infected_students := Infected_students + 1;
                  if not Building.getRoom (x, y) then
                     Building.setRoom (x, y, True);
                     Infected_cells := Infected_cells + 1;
                  end if;
               else
                  if Building.getRoom (x, y) then
                     infect            := True;
                     Infected_students := Infected_students + 1;
                  end if;
               end if;
            end go_in;
         or
            accept go_out do
               delay 1.0; -- check each second
               Current_students := Current_students - 1;
            end go_out;
         or
            accept move_to
              (x : in Integer; y : in Integer; infect : in out Boolean)
            do
               if infect then
                  if not Building.getRoom (x, y) then
                     Building.setRoom (x, y, True);
                     Infected_cells := Infected_cells + 1;
                  end if;
               else
                  if Building.getRoom (x, y) then
                     infect            := True;
                     Infected_students := Infected_students + 1;
                  end if;
               end if;
            end move_to;
         end select;
      end loop;
      -- ask all student go out
      Student_Control.Stop;
      Printer.Print
        (Integer'Image (Infected_cells) & " percent of area is infected");
      Printer.Print
        (Integer'Image (Infected_students) & " Students got infected out of " &
         Integer'Image (Total_students) & " visited the University");
   end Uni;

   ------------------------------------------
begin
   University := new Uni;
   Student_Control.Init;
end main;
