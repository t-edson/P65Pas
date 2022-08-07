

  Description: Precision timer/stopwatch component for Lazarus/FPC
  Author: Tom Lisjac <netdxr@gmail.com>
  Contributors:
    * Felipe Monteiro de Carvalho <felipemonteiro.carvalho@gmail.com>
    * Marcel Minderhoud
  License: Modifyed LGPL (The same as Free Pascal RTL and LCL)
  Copyright (C) 2003-2006 by Tom Lisjac, Felipe Monteiro de Carvalho and Marcel Minderhoud
  Latest version can be obtained at: http://wiki.lazarus.freepascal.org/EpikTimer

 Contents:

   1. The EpikTimer.pas component and palette icon
   2. ETPackage.lpk package for installation
   3. ETDemo demonstration app and host system clock evaluator
 
 -----------------------------------------------------------------

 The EpikTimer Component
  
  Documentation:
    See epiktimer.pas for detailed discussion of timebase sources, timing
    accuracy and clock correlation techniques that can provide traceable
    precision during long term measurements.
  
  Installation:
    - In Components/Open Package File, open etpackage.lpk.
    - Compile the component to verify that everything is there.
    - Install and let Lazarus rebuild
    - Component will be in the System Palette (stopwatch-ruler icon)
  
  Usage:
    Drop the component on a form. The component contains a single timer
    instance and parameterless calls to start, stop, elapsed and clear
    will implicitly reference it. If the timer is named ET:
    
    Procedure InstrumentedCall;
    Begin
      ET.Clear; // optional... timer is cleared at creation
      ET.Start;
      ExecuteFirstTimedSection;
      ET.Stop; // the timer is actually paused and can be restarted later
      TimedSection1:=ET.Elapsed; // store the elapsed in a global
      MakeAnUntimedOverheadCall; // not counted in the timer
      ET.Start; //resume the timer... continue accumulating ticks
      CallTimedSection2;
      TimedSection2:=ET.Elapsed; //timer keeps running... we've just sample it.
      CallTimedSection3;
      CallSomethingElse;
      TimedSection3:=ET.Elapsed; //keep counting... tap the elapsed
      CallTimedSection4;
      TimedSection4:=ET.Elapsed; //keep counting... tap the elapsed
      ET.clear // done... timer is stopped and zeroed
    end;
          
    You can also create any number of timers from a single component on
    the form by declaring a TimerData record and passing it as a parameter
    to start, stop, elapsed and clear using the overloaded methods in the
    component. An example would be:
    
    Function TimedExecution:Extended;
      Var DiskAccessTime:TimerData;
    Begin
      ET.Clear(DiskAccessTimer); // Declared timers *must* be cleared before use. 
      ET.Start(DiskAccessTimer);
      ExecuteTheTimedSection;
      Result:=ET.Elapsed(DiskAccessTimer); // the timer keeps running...
      etc...
      
    See etdemo.pas for additional examples of component usage
     
 The ETDemo Application
 
   The ETDemo application does not require EpikTimer to be installed in order
   to compile and operate. I never liked having to install a palette full of
   components only to find out that I didn't like any of them! :)
   
   Installation
   
     Open etdemo.lpi and compile it.
     
   Operation
   
     As the program comes up, it will create and initialize 10 invisible timer
     forms that can be spawned from the main program's Stopwatch group box. A
     progress bar is supposed to reduce the boredom.
     
     Host Hardware Information
     
       This group evaluates the host system and reports if it finds hardware
       support for the Pentium Time Stamp Counter. If so, you'll be able to get
       a snapshot of it's value along with the microsecond ticks from your
       OS clock. The sizes of the hardware and system ticks isn't as important
       as the rates that they change. On a Linux system, the system ticks value
       represent microseconds of Epoch time.
     
     Timebase Calibration
     
       If your system lacks the TSC or a microsecond resolution system clock,
       EpikTimer falls back to using gated measurements for setting the
       internal tick frequencies. Timing is non-deterministic when calling
       the Linux kernel so some averaging and smoothing of the resulting jitter
       is helpful. If EpikTimer is in this mode, long term accuracy isn't
       guaranteed... but short term comparitive measurements can still be made.
       
       Pressing "Calibrate" performs overhead extraction and gates the selected
       timebase against the best timebase gate available on a given host. The 
       results are displayed in the memo boxes.
       
    Timebase Correlation
    
       This is the default mode for measuring the TSC frequency and provides a
       reliable mechanism for synchronizing the TSC ticks to the system clock.
       If the system clock is maintained via NTP and the CorrelateTimebases
       method is called at regular intervals, the TSC stream can display the
       same long term accuracy (at very high resolutions) as the quality of
       the system's synchronizing time source.
       
    Timer/Stopwatch Functions
    
       This section implements a single stopwatch using the component's internal
       timer data record. The Spawn Timers group box will bring up the 10 timers
       that were created and initialized during program startup.
       

      ----------------- End of EpikTimer Release Documentation ------------------
   
