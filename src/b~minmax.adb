pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~minmax.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~minmax.adb");

with System.Restrictions;

package body ada_main is
   pragma Warnings (Off);

   procedure Do_Finalize;
   pragma Import (C, Do_Finalize, "system__standard_library__adafinal");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   procedure adainit is
      E018 : Boolean; pragma Import (Ada, E018, "system__secondary_stack_E");
      E014 : Boolean; pragma Import (Ada, E014, "system__soft_links_E");
      E024 : Boolean; pragma Import (Ada, E024, "system__exception_table_E");
      E071 : Boolean; pragma Import (Ada, E071, "ada__io_exceptions_E");
      E085 : Boolean; pragma Import (Ada, E085, "ada__strings_E");
      E049 : Boolean; pragma Import (Ada, E049, "ada__tags_E");
      E047 : Boolean; pragma Import (Ada, E047, "ada__streams_E");
      E073 : Boolean; pragma Import (Ada, E073, "interfaces__c_E");
      E064 : Boolean; pragma Import (Ada, E064, "system__finalization_root_E");
      E075 : Boolean; pragma Import (Ada, E075, "system__os_lib_E");
      E091 : Boolean; pragma Import (Ada, E091, "ada__strings__maps_E");
      E066 : Boolean; pragma Import (Ada, E066, "system__finalization_implementation_E");
      E062 : Boolean; pragma Import (Ada, E062, "ada__finalization_E");
      E080 : Boolean; pragma Import (Ada, E080, "ada__finalization__list_controller_E");
      E087 : Boolean; pragma Import (Ada, E087, "ada__strings__unbounded_E");
      E078 : Boolean; pragma Import (Ada, E078, "system__file_control_block_E");
      E060 : Boolean; pragma Import (Ada, E060, "system__file_io_E");
      E007 : Boolean; pragma Import (Ada, E007, "ada__text_io_E");
      E082 : Boolean; pragma Import (Ada, E082, "boards_E");
      E109 : Boolean; pragma Import (Ada, E109, "gametree_E");
      E002 : Boolean; pragma Import (Ada, E002, "minmax_E");

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

      if not E014 then
         System.Soft_Links'Elab_Body;
         E014 := True;
      end if;
      if not E018 then
         System.Secondary_Stack'Elab_Body;
         E018 := True;
      end if;
      if not E024 then
         System.Exception_Table'Elab_Body;
         E024 := True;
      end if;
      if not E071 then
         Ada.Io_Exceptions'Elab_Spec;
         E071 := True;
      end if;
      if not E085 then
         Ada.Strings'Elab_Spec;
         E085 := True;
      end if;
      if not E049 then
         Ada.Tags'Elab_Spec;
      end if;
      if not E047 then
         Ada.Streams'Elab_Spec;
         E047 := True;
      end if;
      if not E073 then
         Interfaces.C'Elab_Spec;
      end if;
      E073 := True;
      if not E064 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E064 := True;
      if not E075 then
         System.Os_Lib'Elab_Body;
         E075 := True;
      end if;
      if not E091 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      E091 := True;
      if not E066 then
         System.Finalization_Implementation'Elab_Spec;
      end if;
      if not E066 then
         System.Finalization_Implementation'Elab_Body;
         E066 := True;
      end if;
      if not E062 then
         Ada.Finalization'Elab_Spec;
      end if;
      E062 := True;
      if not E080 then
         Ada.Finalization.List_Controller'Elab_Spec;
      end if;
      E080 := True;
      if not E087 then
         Ada.Strings.Unbounded'Elab_Spec;
      end if;
      E087 := True;
      if not E078 then
         System.File_Control_Block'Elab_Spec;
         E078 := True;
      end if;
      if not E060 then
         System.File_Io'Elab_Body;
         E060 := True;
      end if;
      if not E049 then
         Ada.Tags'Elab_Body;
         E049 := True;
      end if;
      if not E007 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if not E007 then
         Ada.Text_Io'Elab_Body;
         E007 := True;
      end if;
      E082 := True;
      E109 := True;
      E002 := True;
   end adainit;

   procedure adafinal is
   begin
      Do_Finalize;
   end adafinal;

--  BEGIN Object file/option list
   --   ./boards.o
   --   ./configure.o
   --   ./gametree.o
   --   ./minmax.o
   --   -L./
   --   -L/usr/lib/gcc/x86_64-linux-gnu/4.4.3/adalib/
   --   -shared
   --   -lgnat-4.4
--  END Object file/option list   

end ada_main;
