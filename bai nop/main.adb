with Ada.Text_IO; use Ada.Text_IO;    
with Ada.Real_time;use Ada.Real_time;
with Ada.numerics.discrete_random;
procedure main is
    ------------- Data types ------------
    type Integer_Access is access Integer;
    type String_Access is access String;

    ------------ Global units --------
    delta_x: array (Positive range 1..4) of Integer :=(-1,0,1,0);
    delta_y: array (Positive range 1..4) of Integer :=(0,1,0,-1);

    package My_random_int is new ada.numerics.discrete_random (Positive);
    use My_random_int;

    function In_range (e: in Integer; low : in Integer; up: in Integer) return Boolean is
        ans: Boolean;
    begin
        ans:= (e<=up and low<=e);
        return ans;
    end In_range;

    function Can_move (x: in Integer; y: in Integer; dir: in Integer) return Boolean is
        ans: Boolean;
    begin
        ans:= In_range (e => x + delta_x(dir), low => 1, up =>10) and In_range (e => y + delta_y(dir), low => 1, up => 10);
        return ans;
    end Can_move;

    procedure Move (x: in out Integer; y: in out Integer; dir: in Integer) is
        
    begin
        x:=x + delta_x(dir);
        y:=y + delta_y(dir);
    end Move;

    ---------- Protected objects --------
    protected My_rand is
        function New_int (L: Integer ; R:Integer ) return Integer;
        private
         G: Generator;
    end My_rand;

    protected body My_rand is
        function New_int (L: Integer ; R:Integer ) return Integer is
            Radom_range: Integer;
        begin
            Reset (G);
            Radom_range:=R-L;
            return ( (My_random_int.Random (G) rem Radom_range) + L) ;
        end New_int;
    end My_rand;

    ------------- Student ---------------
    task type Student (Name: String_Access; Is_infected: Boolean);

    task body Student is
        Start_time: Time;
        Stay_period: Time_span:=Seconds(My_rand.New_int(1,4));
        Infected: Boolean:= Is_infected;
        Next_direction:Integer;
        X: Integer:=My_rand.New_int(1,10);
        Y: Integer:=My_rand.New_int(1,10);
    begin
        Start_time := Clock;
        while Clock - Start_time <=Stay_period loop
            delay 0.5;
            loop
                 Next_direction:=My_rand.New_int(1,4);
                 if Can_move (x => X, y =>Y, dir => Next_direction) then
                    Move (x => X, y => Y, dir => Next_direction);
                    exit;
                 end if;
            end loop;
            --  move to new coordinates

            -- change status

        end loop;

    end Student;

    -------------------- University ----------

    

    ------------------------------------------
   -- N: Student( new String'("nam"), False);
    u:Integer:=10;
    v: Integer:=10;
begin
    for i in 1..4 loop
    Put_Line (Boolean'Image(Can_move(u,v,i)));
    end loop;
end main;
