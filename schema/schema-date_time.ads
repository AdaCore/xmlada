
--  This package provides handling for the various time-related types found
--  in the XML schema standard.
--  This includes durations, dates, times, and combination of those.
--
--  We cannot use the standard Ada types to represent dates, since the range of
--  dates supported by XML is much broader (any year should be manageable),
--  whic isn't provided by Ada.
--
--  These types also handle timezones, which means that sometimes two dates
--  might not be comparable if we do not know the timezone of one of them. The
--  precise semantics of the comparison of dates is defined in the XML Schema
--  standard part 3.

with Ada.Calendar;

package Schema.Date_Time is

   type Duration_T  is private;  --  A duration, no timezone
   type Date_Time_T is private;  --  A date and time, with timezone
   type Date_T      is private;  --  A date, with timezone
   type Time_T      is private;  --  A time, with timezone
   type GDay_T      is private;  --  A day in a month

   function Image (Date : Date_Time_T) return String;
   --  Return the string representation of Date, as defined in the XML
   --  schema standard, that is:
   --      yyyy-mm-ddThh:mm:ss.sss+tz:tz
   --  (year, month, day, hour, minutes, seconds, subseconds and timezone).
   --  The subsecond field's precision is given by the precision of the
   --  Duration type in Ada

   function Image (Duration : Duration_T) return String;
   function Image (Time : Time_T) return String;
   function Image (Date : Date_T) return String;
   function Image (Day  : GDay_T) return String;
   --  Return the string representation of the argument

   function Value (Ch : String) return Duration_T;
   --  Return the duration stored in Ch. It should contain a string of the
   --  type "PyyyyYmmM".

   function Value (Ch : String) return Date_Time_T;
   --  Return the date stored in Ch. It should contain a string with the format
   --      yyyy-mm-ddThh:mm:ss.sss+tz:tz
   --  Any number of digits is supported for the date and the subseconds field

   function Value (Ch : String) return Time_T;
   --  Return the time stored in Ch, which should contain a string with the
   --  format:   hh:mm:ss.sss+tz:tz

   function Value (Ch : String) return GDay_T;
   --  Return the day stored in Ch. This is a string with the format
   --    --dd

   function Value (Ch : String) return Date_T;
   --  Return the date stored in Ch, which should contain a string with the
   --  format:  yyyy-mm-dd+tz:tz

   function "+"
     (Date : Date_Time_T; Duration : Duration_T) return Date_Time_T;
   --  Add duration to Date, according to the algorithm described in appendix
   --  E of the XML Schema standard

   function "<"  (Time1, Time2 : Date_Time_T) return Boolean;
   function "<=" (Time1, Time2 : Date_Time_T) return Boolean;
   function "="  (Time1, Time2 : Date_Time_T) return Boolean;
   function ">"  (Time1, Time2 : Date_Time_T) return Boolean;
   function ">=" (Time1, Time2 : Date_Time_T) return Boolean;
   --  Raises Not_Comparable if the two dates are not comparable according
   --  to the XML Schema standard.

   function "<"  (Duration1, Duration2 : Duration_T) return Boolean;
   function "<=" (Duration1, Duration2 : Duration_T) return Boolean;
   function "="  (Duration1, Duration2 : Duration_T) return Boolean;
   function ">"  (Duration1, Duration2 : Duration_T) return Boolean;
   function ">=" (Duration1, Duration2 : Duration_T) return Boolean;
   --  Raises Not_Comparable if the two dates are not comparable according
   --  to the XML Schema standard.

   function "<"  (Time1, Time2 : Time_T) return Boolean;
   function "<=" (Time1, Time2 : Time_T) return Boolean;
   function "="  (Time1, Time2 : Time_T) return Boolean;
   function ">"  (Time1, Time2 : Time_T) return Boolean;
   function ">=" (Time1, Time2 : Time_T) return Boolean;
   --  Raises Not_Comparable if the two times are not comparable according
   --  to the XML Schema standard.

   function "<"  (Date1, Date2 : Date_T) return Boolean;
   function "<=" (Date1, Date2 : Date_T) return Boolean;
   function "="  (Date1, Date2 : Date_T) return Boolean;
   function ">"  (Date1, Date2 : Date_T) return Boolean;
   function ">=" (Date1, Date2 : Date_T) return Boolean;
   --  Raises Not_Comparable if the two times are not comparable according
   --  to the XML Schema standard.

   function "<"  (Day1, Day2 : GDay_T) return Boolean;
   function "<=" (Day1, Day2 : GDay_T) return Boolean;
   function "="  (Day1, Day2 : GDay_T) return Boolean;
   function ">"  (Day1, Day2 : GDay_T) return Boolean;
   function ">=" (Day1, Day2 : GDay_T) return Boolean;
   --  Raises Not_Comparable if the two times are not comparable according
   --  to the XML Schema standard.

   Not_Comparable : exception;

private

   subtype Day_Range is Duration range -86_400.0 .. 86_400.0;

   type Timezone_T is new Integer;
   No_Timezone : constant Timezone_T := Timezone_T'Last;
   --  A timezone indicator. This is an offset, in minutes, to UTC.

   type Date_NZ_T is record
      Year, Month, Day : Integer;
   end record;
   No_Date_NZ : constant Date_NZ_T := (0, 0, 0);
   --  A non-timezoned date.

   type GDay_T is record
      Day : Integer;
      TZ  : Timezone_T;
   end record;
   No_Gday : constant GDay_T := (0, 0);

   subtype Time_NZ_T is Day_Range;
   No_Time_NZ : constant Time_NZ_T := 0.0;
   --  A non-timezoned time

   type Duration_T is record
      Sign             : Integer;
      Year, Month, Day : Natural;
      Seconds          : Ada.Calendar.Day_Duration;
   end record;
   No_Duration : constant Duration_T := (1, 0, 0, 0, 0.0);
   --  A negative duration is representated by having all fields to a negative
   --  value.

   type Date_T is record
      Date : Date_NZ_T;
      TZ   : Timezone_T;
   end record;
   No_Date_T : constant Date_T := (No_Date_NZ, No_Timezone);
   --  A timezoned date. TZ is the timezone offset in minutes. It is set to
   --  Integer'Last if there is no timezone specified

   type Time_T is record
      Time : Time_NZ_T;
      TZ   : Timezone_T;
   end record;
   No_Time_T : constant Time_T := (No_Time_NZ, No_Timezone);
   --  A timezoned time

   type Date_Time_T is record
      Date     : Date_NZ_T;
      Time     : Time_NZ_T;
      TZ       : Timezone_T;
   end record;
   No_Date_Time : constant Date_Time_T :=
     (No_Date_NZ, No_Time_NZ, No_Timezone);
   --  TZ is the timezone offset, in minutes. TZ is set to Integer'Last if
   --  there is no timezone specified

end Schema.Date_Time;
