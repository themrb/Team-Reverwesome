pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~boards.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~boards.adb");

with System.Restrictions;

package body ada_main is
   pragma Warnings (Off);

   procedure Do_Finalize;
   pragma Import (C, Do_Finalize, "system__standard_library__adafinal");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   procedure adainit is
      E019 : Boolean; pragma Import (Ada, E019, "system__secondary_stack_E");
      E015 : Boolean; pragma Import (Ada, E015, "system__soft_links_E");
      E011 : Boolean; pragma Import (Ada, E011, "system__exception_table_E");
      E081 : Boolean; pragma Import (Ada, E081, "ada__io_exceptions_E");
      E008 : Boolean; pragma Import (Ada, E008, "ada__strings_E");
      E068 : Boolean; pragma Import (Ada, E068, "ada__tags_E");
      E066 : Boolean; pragma Import (Ada, E066, "ada__streams_E");
      E090 : Boolean; pragma Import (Ada, E090, "interfaces__c_E");
      E065 : Boolean; pragma Import (Ada, E065, "system__finalization_root_E");
      E092 : Boolean; pragma Import (Ada, E092, "system__os_lib_E");
      E053 : Boolean; pragma Import (Ada, E053, "ada__strings__maps_E");
      E076 : Boolean; pragma Import (Ada, E076, "system__finalization_implementation_E");
      E063 : Boolean; pragma Import (Ada, E063, "ada__finalization_E");
      E097 : Boolean; pragma Import (Ada, E097, "ada__finalization__list_controller_E");
      E049 : Boolean; pragma Import (Ada, E049, "ada__strings__unbounded_E");
      E095 : Boolean; pragma Import (Ada, E095, "system__file_control_block_E");
      E088 : Boolean; pragma Import (Ada, E088, "system__file_io_E");
      E083 : Boolean; pragma Import (Ada, E083, "ada__text_io_E");
      E002 : Boolean; pragma Import (Ada, E002, "boards_E");

      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Zero_Cost_Exceptions : Integer;
      pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");
   begin
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, True, True, False, False, False, True, 
           True, True, False, False, True, False, False, True, 
           True, False, True, True, True, True, True, True, 
           False, False, True, False, True, False, False, True, 
           False, False, True, False, False, False, True, False, 
           False, True, False, False, False, False, False, False, 
           False, False, True, True, True, False, False, True, 
           False, True, True, False, True, True, True, False, 
           False, False, False, False, False, False),
         Count => (0, 0, 0, 0, 0, 0, 0),
         Unknown => (False, False, False, False, False, False, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Zero_Cost_Exceptions := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      if not E015 then
         System.Soft_Links'Elab_Body;
         E015 := True;
      end if;
      if not E019 then
         System.Secondary_Stack'Elab_Body;
         E019 := True;
      end if;
      if not E011 then
         System.Exception_Table'Elab_Body;
         E011 := True;
      end if;
      if not E081 then
         Ada.Io_Exceptions'Elab_Spec;
         E081 := True;
      end if;
      if not E008 then
         Ada.Strings'Elab_Spec;
         E008 := True;
      end if;
      if not E068 then
         Ada.Tags'Elab_Spec;
      end if;
      if not E066 then
         Ada.Streams'Elab_Spec;
         E066 := True;
      end if;
      if not E090 then
         Interfaces.C'Elab_Spec;
      end if;
      E090 := True;
      if not E065 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E065 := True;
      if not E092 then
         System.Os_Lib'Elab_Body;
         E092 := True;
      end if;
      if not E053 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      E053 := True;
      if not E076 then
         System.Finalization_Implementation'Elab_Spec;
      end if;
      if not E076 then
         System.Finalization_Implementation'Elab_Body;
         E076 := True;
      end if;
      if not E063 then
         Ada.Finalization'Elab_Spec;
      end if;
      E063 := True;
      if not E097 then
         Ada.Finalization.List_Controller'Elab_Spec;
      end if;
      E097 := True;
      if not E049 then
         Ada.Strings.Unbounded'Elab_Spec;
      end if;
      E049 := True;
      if not E095 then
         System.File_Control_Block'Elab_Spec;
         E095 := True;
      end if;
      if not E088 then
         System.File_Io'Elab_Body;
         E088 := True;
      end if;
      if not E068 then
         Ada.Tags'Elab_Body;
         E068 := True;
      end if;
      if not E083 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if not E083 then
         Ada.Text_Io'Elab_Body;
         E083 := True;
      end if;
      if not E002 then
         Boards'Elab_Spec;
      end if;
      E002 := True;
   end adainit;

   procedure adafinal is
   begin
      Do_Finalize;
   end adafinal;

--  BEGIN Object file/option list
   --   ./boards.o
   --   -L./
   --   -L/usr/lib/gcc/x86_64-linux-gnu/4.4.3/adalib/
   --   -shared
   --   -lgnat-4.4
--  END Object file/option list   

end ada_main;
