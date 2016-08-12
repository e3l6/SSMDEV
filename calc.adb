with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;        use Ada.Float_Text_IO;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;
with GPS;                      use GPS;
with Sunangle;                 use Sunangle;

procedure Calc is
   
   Current_Angle  : Angle_Type;
   My_Position    : GPS.Position_Type;
   My_Time_Offset : Time_Offset;
   Sunrise        : Ada.Calendar.Time;
   Sunset         : Ada.Calendar.Time;
   
begin
   
   My_Position := Get_Position;
   
   Current_Angle := Calculate_Current_Angle (My_Position);
   
   My_Time_Offset := UTC_Time_Offset (My_Position.Current_Time);
   
   Put ("Current time: ");
   Put (Ada.Calendar.Formatting.Image (My_Position.Current_Time, False,
                                       My_Time_Offset));
   New_Line;
   
   Put ("UTC time:     ");
   Put (Ada.Calendar.Formatting.Image (My_Position.Current_Time, False,
                                       0));
   New_Line (2);
   
   Put ("Az: ");
   Put (Current_Angle.Azimuth, 0, 3, 0);
   Put ("  El: ");
   Put (Current_Angle.Elevation, 0, 3, 0);
   New_Line (2);
   
   Sunrise := Calculate_Sunrise (My_Position);
   Sunset  := Calculate_Sunset  (My_Position);
   
   Put ("Sunrise: ");
   Put (Ada.Calendar.Formatting.Image (Sunrise, False, My_Time_Offset));
   New_Line;
   
   Put ("Sunset:  ");
   Put (Ada.Calendar.Formatting.Image (Sunset, False, My_Time_Offset));
   New_Line;
end Calc;
