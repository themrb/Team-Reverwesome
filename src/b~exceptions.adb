pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~exceptions.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~exceptions.adb");

with System.Restrictions;

package body ada_main is
   pragma Warnings (Off);

   procedure Do_Finalize;
   pragma Import (C, Do_Finalize, "system__standard_library__adafinal");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   procedure adainit is
      E012 : Boolean; pragma Import (Ada, E012, "system__secondary_stack_E");
      E016 : Boolean; pragma Import (Ada, E016, "system__soft_links_E");
      E022 : Boolean; pragma Import (Ada, E022, "system__exception_table_E");
      E123 : Boolean; pragma Import (Ada, E123, "ada__io_exceptions_E");
      E091 : Boolean; pragma Import (Ada, E091, "ada__tags_E");
      E118 : Boolean; pragma Import (Ada, E118, "ada__streams_E");
      E053 : Boolean; pragma Import (Ada, E053, "interfaces__c_E");
      E117 : Boolean; pragma Import (Ada, E117, "system__finalization_root_E");
      E135 : Boolean; pragma Import (Ada, E135, "system__os_lib_E");
      E120 : Boolean; pragma Import (Ada, E120, "system__finalization_implementation_E");
      E115 : Boolean; pragma Import (Ada, E115, "ada__finalization_E");
      E125 : Boolean; pragma Import (Ada, E125, "ada__finalization__list_controller_E");
      E138 : Boolean; pragma Import (Ada, E138, "system__file_control_block_E");
      E133 : Boolean; pragma Import (Ada, E133, "system__file_io_E");
      E067 : Boolean; pragma Import (Ada, E067, "system__task_info_E");
      E099 : Boolean; pragma Import (Ada, E099, "system__tasking__initialization_E");
      E107 : Boolean; pragma Import (Ada, E107, "system__tasking__protected_objects_E");
      E129 : Boolean; pragma Import (Ada, E129, "ada__text_io_E");
      E111 : Boolean; pragma Import (Ada, E111, "system__tasking__protected_objects__entries_E");
      E105 : Boolean; pragma Import (Ada, E105, "system__tasking__queuing_E");
      E002 : Boolean; pragma Import (Ada, E002, "exceptions_E");

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
           False, False, False, False, True, False, False, False, 
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
           False, True, True, True, True, True, True, False, 
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

      if not E016 then
         System.Soft_Links'Elab_Body;
         E016 := True;
      end if;
      if not E012 then
         System.Secondary_Stack'Elab_Body;
         E012 := True;
      end if;
      if not E022 then
         System.Exception_Table'Elab_Body;
         E022 := True;
      end if;
      if not E123 then
         Ada.Io_Exceptions'Elab_Spec;
         E123 := True;
      end if;
      if not E091 then
         Ada.Tags'Elab_Spec;
      end if;
      if not E118 then
         Ada.Streams'Elab_Spec;
         E118 := True;
      end if;
      if not E053 then
         Interfaces.C'Elab_Spec;
      end if;
      E053 := True;
      if not E117 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E117 := True;
      if not E135 then
         System.Os_Lib'Elab_Body;
         E135 := True;
      end if;
      if not E120 then
         System.Finalization_Implementation'Elab_Spec;
      end if;
      if not E120 then
         System.Finalization_Implementation'Elab_Body;
         E120 := True;
      end if;
      if not E115 then
         Ada.Finalization'Elab_Spec;
      end if;
      E115 := True;
      if not E125 then
         Ada.Finalization.List_Controller'Elab_Spec;
      end if;
      E125 := True;
      if not E138 then
         System.File_Control_Block'Elab_Spec;
         E138 := True;
      end if;
      if not E133 then
         System.File_Io'Elab_Body;
         E133 := True;
      end if;
      if not E067 then
         System.Task_Info'Elab_Spec;
      end if;
      E067 := True;
      if not E099 then
         System.Tasking.Initialization'Elab_Body;
         E099 := True;
      end if;
      if not E107 then
         System.Tasking.Protected_Objects'Elab_Body;
         E107 := True;
      end if;
      if not E091 then
         Ada.Tags'Elab_Body;
         E091 := True;
      end if;
      if not E129 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if not E129 then
         Ada.Text_Io'Elab_Body;
         E129 := True;
      end if;
      if not E111 then
         System.Tasking.Protected_Objects.Entries'Elab_Spec;
      end if;
      E111 := True;
      if not E105 then
         System.Tasking.Queuing'Elab_Body;
         E105 := True;
      end if;
      E002 := True;
   end adainit;

   procedure adafinal is
   begin
      Do_Finalize;
   end adafinal;

--  BEGIN Object file/option list
   --   ./exceptions.o
   --   -L./
   --   -L/usr/lib/gcc/x86_64-linux-gnu/4.4.3/adalib/
   --   -shared
   --   -lgnarl-4.4
   --   -lgnat-4.4
   --   -lpthread
--  END Object file/option list   

end ada_main;
