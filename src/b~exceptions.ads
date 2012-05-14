pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#f128b7bb#;
   u00002 : constant Version_32 := 16#f9572f31#;
   u00003 : constant Version_32 := 16#4b33036a#;
   u00004 : constant Version_32 := 16#03570e27#;
   u00005 : constant Version_32 := 16#9c7dd3ea#;
   u00006 : constant Version_32 := 16#e83cd539#;
   u00007 : constant Version_32 := 16#992476b0#;
   u00008 : constant Version_32 := 16#df55302f#;
   u00009 : constant Version_32 := 16#91f9f4f9#;
   u00010 : constant Version_32 := 16#0cac0aa9#;
   u00011 : constant Version_32 := 16#6ad13d41#;
   u00012 : constant Version_32 := 16#496d6bfd#;
   u00013 : constant Version_32 := 16#2ea84b20#;
   u00014 : constant Version_32 := 16#2c7b66c6#;
   u00015 : constant Version_32 := 16#b6c89fbe#;
   u00016 : constant Version_32 := 16#430a8d84#;
   u00017 : constant Version_32 := 16#352d93cc#;
   u00018 : constant Version_32 := 16#eaa55474#;
   u00019 : constant Version_32 := 16#9201cee0#;
   u00020 : constant Version_32 := 16#e3d85f73#;
   u00021 : constant Version_32 := 16#fcfb2939#;
   u00022 : constant Version_32 := 16#d1fec254#;
   u00023 : constant Version_32 := 16#1a63fe0c#;
   u00024 : constant Version_32 := 16#a9f5773f#;
   u00025 : constant Version_32 := 16#5b8b9839#;
   u00026 : constant Version_32 := 16#cfea76ef#;
   u00027 : constant Version_32 := 16#55ef7c79#;
   u00028 : constant Version_32 := 16#32c35ddc#;
   u00029 : constant Version_32 := 16#b871c253#;
   u00030 : constant Version_32 := 16#5b0a42fd#;
   u00031 : constant Version_32 := 16#9e37526b#;
   u00032 : constant Version_32 := 16#ad9b204c#;
   u00033 : constant Version_32 := 16#9c49ee35#;
   u00034 : constant Version_32 := 16#b53be4c7#;
   u00035 : constant Version_32 := 16#1faccaca#;
   u00036 : constant Version_32 := 16#743d3d0d#;
   u00037 : constant Version_32 := 16#7457b7b6#;
   u00038 : constant Version_32 := 16#ccde3404#;
   u00039 : constant Version_32 := 16#f020f847#;
   u00040 : constant Version_32 := 16#2c57c517#;
   u00041 : constant Version_32 := 16#ffc4bcd0#;
   u00042 : constant Version_32 := 16#a69cad5c#;
   u00043 : constant Version_32 := 16#300a43f3#;
   u00044 : constant Version_32 := 16#d4ede0a0#;
   u00045 : constant Version_32 := 16#99c8a881#;
   u00046 : constant Version_32 := 16#620a177c#;
   u00047 : constant Version_32 := 16#e1e7b9d6#;
   u00048 : constant Version_32 := 16#17b86634#;
   u00049 : constant Version_32 := 16#3b408220#;
   u00050 : constant Version_32 := 16#86cee0c6#;
   u00051 : constant Version_32 := 16#2fc058de#;
   u00052 : constant Version_32 := 16#59507545#;
   u00053 : constant Version_32 := 16#e98c0dd7#;
   u00054 : constant Version_32 := 16#72523d91#;
   u00055 : constant Version_32 := 16#d3cec6be#;
   u00056 : constant Version_32 := 16#05f38067#;
   u00057 : constant Version_32 := 16#87fe79e5#;
   u00058 : constant Version_32 := 16#04e247f8#;
   u00059 : constant Version_32 := 16#cb51c4d3#;
   u00060 : constant Version_32 := 16#101b2337#;
   u00061 : constant Version_32 := 16#44ed77f3#;
   u00062 : constant Version_32 := 16#978f0be2#;
   u00063 : constant Version_32 := 16#292b2f7a#;
   u00064 : constant Version_32 := 16#ed51e533#;
   u00065 : constant Version_32 := 16#b5095267#;
   u00066 : constant Version_32 := 16#0e608ba6#;
   u00067 : constant Version_32 := 16#49963224#;
   u00068 : constant Version_32 := 16#2f39c159#;
   u00069 : constant Version_32 := 16#ebb1e29b#;
   u00070 : constant Version_32 := 16#66f42c87#;
   u00071 : constant Version_32 := 16#41c587ba#;
   u00072 : constant Version_32 := 16#0255db5c#;
   u00073 : constant Version_32 := 16#28282313#;
   u00074 : constant Version_32 := 16#218c434b#;
   u00075 : constant Version_32 := 16#dd47d1b3#;
   u00076 : constant Version_32 := 16#fa683506#;
   u00077 : constant Version_32 := 16#c64625c7#;
   u00078 : constant Version_32 := 16#f324aafb#;
   u00079 : constant Version_32 := 16#6344170c#;
   u00080 : constant Version_32 := 16#f91553b1#;
   u00081 : constant Version_32 := 16#b2a28bd9#;
   u00082 : constant Version_32 := 16#f5269f91#;
   u00083 : constant Version_32 := 16#48f132c5#;
   u00084 : constant Version_32 := 16#17c88cd6#;
   u00085 : constant Version_32 := 16#3f0d9cb7#;
   u00086 : constant Version_32 := 16#3e7d115b#;
   u00087 : constant Version_32 := 16#d2063e17#;
   u00088 : constant Version_32 := 16#9d99b292#;
   u00089 : constant Version_32 := 16#a923a4ec#;
   u00090 : constant Version_32 := 16#f6ee8365#;
   u00091 : constant Version_32 := 16#111efa70#;
   u00092 : constant Version_32 := 16#5056e8dd#;
   u00093 : constant Version_32 := 16#e3f7d8cd#;
   u00094 : constant Version_32 := 16#8213b492#;
   u00095 : constant Version_32 := 16#857b4d43#;
   u00096 : constant Version_32 := 16#44c7af1b#;
   u00097 : constant Version_32 := 16#4ecf232b#;
   u00098 : constant Version_32 := 16#9559b60c#;
   u00099 : constant Version_32 := 16#420888d8#;
   u00100 : constant Version_32 := 16#d80cf61a#;
   u00101 : constant Version_32 := 16#1de34b3f#;
   u00102 : constant Version_32 := 16#3ad1331d#;
   u00103 : constant Version_32 := 16#e6129a88#;
   u00104 : constant Version_32 := 16#840768f3#;
   u00105 : constant Version_32 := 16#0c7cb4f1#;
   u00106 : constant Version_32 := 16#66616454#;
   u00107 : constant Version_32 := 16#696fe381#;
   u00108 : constant Version_32 := 16#499c33c1#;
   u00109 : constant Version_32 := 16#5f4bc5bc#;
   u00110 : constant Version_32 := 16#36ac9460#;
   u00111 : constant Version_32 := 16#a6f4004e#;
   u00112 : constant Version_32 := 16#293ff6f7#;
   u00113 : constant Version_32 := 16#d1a8db44#;
   u00114 : constant Version_32 := 16#fcec4850#;
   u00115 : constant Version_32 := 16#16dfe486#;
   u00116 : constant Version_32 := 16#6d0998e1#;
   u00117 : constant Version_32 := 16#ae11f1b2#;
   u00118 : constant Version_32 := 16#a8d17654#;
   u00119 : constant Version_32 := 16#31db65a3#;
   u00120 : constant Version_32 := 16#e362cd34#;
   u00121 : constant Version_32 := 16#2461b049#;
   u00122 : constant Version_32 := 16#0aa29e81#;
   u00123 : constant Version_32 := 16#2274d34a#;
   u00124 : constant Version_32 := 16#923573c8#;
   u00125 : constant Version_32 := 16#183b4446#;
   u00126 : constant Version_32 := 16#a9b1294b#;
   u00127 : constant Version_32 := 16#ffb364e8#;
   u00128 : constant Version_32 := 16#96ac68ca#;
   u00129 : constant Version_32 := 16#17b3b4c9#;
   u00130 : constant Version_32 := 16#62e56d2b#;
   u00131 : constant Version_32 := 16#de0efd54#;
   u00132 : constant Version_32 := 16#6c2c3694#;
   u00133 : constant Version_32 := 16#f7ba4e54#;
   u00134 : constant Version_32 := 16#6a4966d7#;
   u00135 : constant Version_32 := 16#756d8fec#;
   u00136 : constant Version_32 := 16#3f280002#;
   u00137 : constant Version_32 := 16#07c1a032#;
   u00138 : constant Version_32 := 16#18dd447c#;
   u00139 : constant Version_32 := 16#842c78ec#;
   u00140 : constant Version_32 := 16#db027b4b#;

   pragma Export (C, u00001, "exceptionsB");
   pragma Export (C, u00002, "exceptionsS");
   pragma Export (C, u00003, "system__standard_libraryB");
   pragma Export (C, u00004, "system__standard_libraryS");
   pragma Export (C, u00005, "adaS");
   pragma Export (C, u00006, "ada__task_identificationB");
   pragma Export (C, u00007, "ada__task_identificationS");
   pragma Export (C, u00008, "systemS");
   pragma Export (C, u00009, "system__address_imageB");
   pragma Export (C, u00010, "system__address_imageS");
   pragma Export (C, u00011, "system__secondary_stackB");
   pragma Export (C, u00012, "system__secondary_stackS");
   pragma Export (C, u00013, "system__parametersB");
   pragma Export (C, u00014, "system__parametersS");
   pragma Export (C, u00015, "system__soft_linksB");
   pragma Export (C, u00016, "system__soft_linksS");
   pragma Export (C, u00017, "ada__exceptionsB");
   pragma Export (C, u00018, "ada__exceptionsS");
   pragma Export (C, u00019, "ada__exceptions__last_chance_handlerB");
   pragma Export (C, u00020, "ada__exceptions__last_chance_handlerS");
   pragma Export (C, u00021, "system__exception_tableB");
   pragma Export (C, u00022, "system__exception_tableS");
   pragma Export (C, u00023, "system__htableB");
   pragma Export (C, u00024, "system__htableS");
   pragma Export (C, u00025, "system__exceptionsB");
   pragma Export (C, u00026, "system__exceptionsS");
   pragma Export (C, u00027, "system__storage_elementsB");
   pragma Export (C, u00028, "system__storage_elementsS");
   pragma Export (C, u00029, "system__string_opsB");
   pragma Export (C, u00030, "system__string_opsS");
   pragma Export (C, u00031, "system__string_ops_concat_3B");
   pragma Export (C, u00032, "system__string_ops_concat_3S");
   pragma Export (C, u00033, "system__tracebackB");
   pragma Export (C, u00034, "system__tracebackS");
   pragma Export (C, u00035, "system__unsigned_typesS");
   pragma Export (C, u00036, "system__wch_conB");
   pragma Export (C, u00037, "system__wch_conS");
   pragma Export (C, u00038, "system__wch_stwB");
   pragma Export (C, u00039, "system__wch_stwS");
   pragma Export (C, u00040, "system__wch_cnvB");
   pragma Export (C, u00041, "system__wch_cnvS");
   pragma Export (C, u00042, "interfacesS");
   pragma Export (C, u00043, "system__wch_jisB");
   pragma Export (C, u00044, "system__wch_jisS");
   pragma Export (C, u00045, "system__traceback_entriesB");
   pragma Export (C, u00046, "system__traceback_entriesS");
   pragma Export (C, u00047, "system__stack_checkingB");
   pragma Export (C, u00048, "system__stack_checkingS");
   pragma Export (C, u00049, "system__task_primitivesS");
   pragma Export (C, u00050, "system__os_interfaceB");
   pragma Export (C, u00051, "system__os_interfaceS");
   pragma Export (C, u00052, "interfaces__cB");
   pragma Export (C, u00053, "interfaces__cS");
   pragma Export (C, u00054, "system__linuxS");
   pragma Export (C, u00055, "system__task_primitives__operationsB");
   pragma Export (C, u00056, "system__task_primitives__operationsS");
   pragma Export (C, u00057, "system__bit_opsB");
   pragma Export (C, u00058, "system__bit_opsS");
   pragma Export (C, u00059, "system__interrupt_managementB");
   pragma Export (C, u00060, "system__interrupt_managementS");
   pragma Export (C, u00061, "system__os_primitivesB");
   pragma Export (C, u00062, "system__os_primitivesS");
   pragma Export (C, u00063, "system__stack_checking__operationsB");
   pragma Export (C, u00064, "system__stack_checking__operationsS");
   pragma Export (C, u00065, "system__crtlS");
   pragma Export (C, u00066, "system__task_infoB");
   pragma Export (C, u00067, "system__task_infoS");
   pragma Export (C, u00068, "system__taskingB");
   pragma Export (C, u00069, "system__taskingS");
   pragma Export (C, u00070, "system__stack_usageB");
   pragma Export (C, u00071, "system__stack_usageS");
   pragma Export (C, u00072, "system__img_intB");
   pragma Export (C, u00073, "system__img_intS");
   pragma Export (C, u00074, "system__ioB");
   pragma Export (C, u00075, "system__ioS");
   pragma Export (C, u00076, "system__string_ops_concat_5B");
   pragma Export (C, u00077, "system__string_ops_concat_5S");
   pragma Export (C, u00078, "system__string_ops_concat_4B");
   pragma Export (C, u00079, "system__string_ops_concat_4S");
   pragma Export (C, u00080, "system__tasking__debugB");
   pragma Export (C, u00081, "system__tasking__debugS");
   pragma Export (C, u00082, "system__img_enum_newB");
   pragma Export (C, u00083, "system__img_enum_newS");
   pragma Export (C, u00084, "system__img_lliB");
   pragma Export (C, u00085, "system__img_lliS");
   pragma Export (C, u00086, "system__img_lluB");
   pragma Export (C, u00087, "system__img_lluS");
   pragma Export (C, u00088, "system__tasking__utilitiesB");
   pragma Export (C, u00089, "system__tasking__utilitiesS");
   pragma Export (C, u00090, "ada__tagsB");
   pragma Export (C, u00091, "ada__tagsS");
   pragma Export (C, u00092, "system__val_lluB");
   pragma Export (C, u00093, "system__val_lluS");
   pragma Export (C, u00094, "system__val_utilB");
   pragma Export (C, u00095, "system__val_utilS");
   pragma Export (C, u00096, "system__case_utilB");
   pragma Export (C, u00097, "system__case_utilS");
   pragma Export (C, u00098, "system__tasking__initializationB");
   pragma Export (C, u00099, "system__tasking__initializationS");
   pragma Export (C, u00100, "system__soft_links__taskingB");
   pragma Export (C, u00101, "system__soft_links__taskingS");
   pragma Export (C, u00102, "ada__exceptions__is_null_occurrenceB");
   pragma Export (C, u00103, "ada__exceptions__is_null_occurrenceS");
   pragma Export (C, u00104, "system__tasking__queuingB");
   pragma Export (C, u00105, "system__tasking__queuingS");
   pragma Export (C, u00106, "system__tasking__protected_objectsB");
   pragma Export (C, u00107, "system__tasking__protected_objectsS");
   pragma Export (C, u00108, "system__tracesB");
   pragma Export (C, u00109, "system__tracesS");
   pragma Export (C, u00110, "system__tasking__protected_objects__entriesB");
   pragma Export (C, u00111, "system__tasking__protected_objects__entriesS");
   pragma Export (C, u00112, "system__restrictionsB");
   pragma Export (C, u00113, "system__restrictionsS");
   pragma Export (C, u00114, "ada__finalizationB");
   pragma Export (C, u00115, "ada__finalizationS");
   pragma Export (C, u00116, "system__finalization_rootB");
   pragma Export (C, u00117, "system__finalization_rootS");
   pragma Export (C, u00118, "ada__streamsS");
   pragma Export (C, u00119, "system__finalization_implementationB");
   pragma Export (C, u00120, "system__finalization_implementationS");
   pragma Export (C, u00121, "system__stream_attributesB");
   pragma Export (C, u00122, "system__stream_attributesS");
   pragma Export (C, u00123, "ada__io_exceptionsS");
   pragma Export (C, u00124, "ada__finalization__list_controllerB");
   pragma Export (C, u00125, "ada__finalization__list_controllerS");
   pragma Export (C, u00126, "system__traces__taskingB");
   pragma Export (C, u00127, "system__traces__taskingS");
   pragma Export (C, u00128, "ada__text_ioB");
   pragma Export (C, u00129, "ada__text_ioS");
   pragma Export (C, u00130, "interfaces__c_streamsB");
   pragma Export (C, u00131, "interfaces__c_streamsS");
   pragma Export (C, u00132, "system__file_ioB");
   pragma Export (C, u00133, "system__file_ioS");
   pragma Export (C, u00134, "system__os_libB");
   pragma Export (C, u00135, "system__os_libS");
   pragma Export (C, u00136, "system__stringsB");
   pragma Export (C, u00137, "system__stringsS");
   pragma Export (C, u00138, "system__file_control_blockS");
   pragma Export (C, u00139, "system__memoryB");
   pragma Export (C, u00140, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.htable%s
   --  system.htable%b
   --  system.img_enum_new%s
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.linux%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.img_enum_new%b
   --  system.secondary_stack%s
   --  system.address_image%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_checking.operations%s
   --  system.stack_usage%s
   --  system.string_ops%s
   --  system.string_ops%b
   --  system.string_ops_concat_3%s
   --  system.string_ops_concat_3%b
   --  system.string_ops_concat_4%s
   --  system.string_ops_concat_4%b
   --  system.string_ops_concat_5%s
   --  system.string_ops_concat_5%b
   --  system.stack_usage%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  ada.exceptions.last_chance_handler%s
   --  system.soft_links%s
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.stack_checking.operations%b
   --  system.secondary_stack%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.soft_links.tasking%s
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.bit_ops%b
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.finalization_implementation%s
   --  system.finalization_implementation%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  ada.finalization.list_controller%s
   --  ada.finalization.list_controller%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.interrupt_management%b
   --  system.tasking%s
   --  ada.task_identification%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.soft_links.tasking%b
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking.initialization%s
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.utilities%s
   --  ada.task_identification%b
   --  system.traces.tasking%s
   --  system.traces.tasking%b
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.tags%b
   --  ada.exceptions%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%b
   --  exceptions%s
   --  exceptions%b
   --  END ELABORATION ORDER

end ada_main;
