pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#0878760b#;
   u00002 : constant Version_32 := 16#18f48d6b#;
   u00003 : constant Version_32 := 16#4b33036a#;
   u00004 : constant Version_32 := 16#03570e27#;
   u00005 : constant Version_32 := 16#9c7dd3ea#;
   u00006 : constant Version_32 := 16#8488033d#;
   u00007 : constant Version_32 := 16#8535bfda#;
   u00008 : constant Version_32 := 16#352d93cc#;
   u00009 : constant Version_32 := 16#eaa55474#;
   u00010 : constant Version_32 := 16#9201cee0#;
   u00011 : constant Version_32 := 16#e3d85f73#;
   u00012 : constant Version_32 := 16#df55302f#;
   u00013 : constant Version_32 := 16#b6c89fbe#;
   u00014 : constant Version_32 := 16#430a8d84#;
   u00015 : constant Version_32 := 16#2ea84b20#;
   u00016 : constant Version_32 := 16#2c7b66c6#;
   u00017 : constant Version_32 := 16#6ad13d41#;
   u00018 : constant Version_32 := 16#496d6bfd#;
   u00019 : constant Version_32 := 16#55ef7c79#;
   u00020 : constant Version_32 := 16#32c35ddc#;
   u00021 : constant Version_32 := 16#e1e7b9d6#;
   u00022 : constant Version_32 := 16#17b86634#;
   u00023 : constant Version_32 := 16#fcfb2939#;
   u00024 : constant Version_32 := 16#d1fec254#;
   u00025 : constant Version_32 := 16#1a63fe0c#;
   u00026 : constant Version_32 := 16#a9f5773f#;
   u00027 : constant Version_32 := 16#5b8b9839#;
   u00028 : constant Version_32 := 16#cfea76ef#;
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
   u00047 : constant Version_32 := 16#aca0f2c0#;
   u00048 : constant Version_32 := 16#a71c6951#;
   u00049 : constant Version_32 := 16#44ed77f3#;
   u00050 : constant Version_32 := 16#978f0be2#;
   u00051 : constant Version_32 := 16#808e35e2#;
   u00052 : constant Version_32 := 16#f6ee8365#;
   u00053 : constant Version_32 := 16#111efa70#;
   u00054 : constant Version_32 := 16#5056e8dd#;
   u00055 : constant Version_32 := 16#e3f7d8cd#;
   u00056 : constant Version_32 := 16#8213b492#;
   u00057 : constant Version_32 := 16#857b4d43#;
   u00058 : constant Version_32 := 16#44c7af1b#;
   u00059 : constant Version_32 := 16#4ecf232b#;
   u00060 : constant Version_32 := 16#96ac68ca#;
   u00061 : constant Version_32 := 16#17b3b4c9#;
   u00062 : constant Version_32 := 16#a8d17654#;
   u00063 : constant Version_32 := 16#62e56d2b#;
   u00064 : constant Version_32 := 16#de0efd54#;
   u00065 : constant Version_32 := 16#b5095267#;
   u00066 : constant Version_32 := 16#6c2c3694#;
   u00067 : constant Version_32 := 16#f7ba4e54#;
   u00068 : constant Version_32 := 16#fcec4850#;
   u00069 : constant Version_32 := 16#16dfe486#;
   u00070 : constant Version_32 := 16#6d0998e1#;
   u00071 : constant Version_32 := 16#ae11f1b2#;
   u00072 : constant Version_32 := 16#31db65a3#;
   u00073 : constant Version_32 := 16#e362cd34#;
   u00074 : constant Version_32 := 16#293ff6f7#;
   u00075 : constant Version_32 := 16#d1a8db44#;
   u00076 : constant Version_32 := 16#2461b049#;
   u00077 : constant Version_32 := 16#0aa29e81#;
   u00078 : constant Version_32 := 16#2274d34a#;
   u00079 : constant Version_32 := 16#59507545#;
   u00080 : constant Version_32 := 16#e98c0dd7#;
   u00081 : constant Version_32 := 16#6a4966d7#;
   u00082 : constant Version_32 := 16#756d8fec#;
   u00083 : constant Version_32 := 16#3f280002#;
   u00084 : constant Version_32 := 16#07c1a032#;
   u00085 : constant Version_32 := 16#18dd447c#;
   u00086 : constant Version_32 := 16#923573c8#;
   u00087 : constant Version_32 := 16#183b4446#;
   u00088 : constant Version_32 := 16#22729470#;
   u00089 : constant Version_32 := 16#1777b77c#;
   u00090 : constant Version_32 := 16#cc1134cf#;
   u00091 : constant Version_32 := 16#b9828a2f#;
   u00092 : constant Version_32 := 16#1bc9f0e1#;
   u00093 : constant Version_32 := 16#dc80ce83#;
   u00094 : constant Version_32 := 16#bb19d785#;
   u00095 : constant Version_32 := 16#70f768a2#;
   u00096 : constant Version_32 := 16#f0ddc3f6#;
   u00097 : constant Version_32 := 16#eccbd1ca#;
   u00098 : constant Version_32 := 16#3c09e836#;
   u00099 : constant Version_32 := 16#87fe79e5#;
   u00100 : constant Version_32 := 16#04e247f8#;
   u00101 : constant Version_32 := 16#3ab3e7b1#;
   u00102 : constant Version_32 := 16#54ed61ee#;
   u00103 : constant Version_32 := 16#55412952#;
   u00104 : constant Version_32 := 16#6c544d08#;
   u00105 : constant Version_32 := 16#330db175#;
   u00106 : constant Version_32 := 16#1c4f7604#;
   u00107 : constant Version_32 := 16#f5269f91#;
   u00108 : constant Version_32 := 16#48f132c5#;
   u00109 : constant Version_32 := 16#0255db5c#;
   u00110 : constant Version_32 := 16#28282313#;
   u00111 : constant Version_32 := 16#fa683506#;
   u00112 : constant Version_32 := 16#c64625c7#;
   u00113 : constant Version_32 := 16#f324aafb#;
   u00114 : constant Version_32 := 16#6344170c#;
   u00115 : constant Version_32 := 16#b33c2d41#;
   u00116 : constant Version_32 := 16#3d487bf5#;
   u00117 : constant Version_32 := 16#2d0437d0#;
   u00118 : constant Version_32 := 16#e74826b3#;
   u00119 : constant Version_32 := 16#294c3b74#;
   u00120 : constant Version_32 := 16#0d6739fd#;
   u00121 : constant Version_32 := 16#647de85b#;
   u00122 : constant Version_32 := 16#fe9387f4#;
   u00123 : constant Version_32 := 16#842c78ec#;
   u00124 : constant Version_32 := 16#db027b4b#;

   pragma Export (C, u00001, "minmaxB");
   pragma Export (C, u00002, "minmaxS");
   pragma Export (C, u00003, "system__standard_libraryB");
   pragma Export (C, u00004, "system__standard_libraryS");
   pragma Export (C, u00005, "adaS");
   pragma Export (C, u00006, "ada__calendarB");
   pragma Export (C, u00007, "ada__calendarS");
   pragma Export (C, u00008, "ada__exceptionsB");
   pragma Export (C, u00009, "ada__exceptionsS");
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerB");
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerS");
   pragma Export (C, u00012, "systemS");
   pragma Export (C, u00013, "system__soft_linksB");
   pragma Export (C, u00014, "system__soft_linksS");
   pragma Export (C, u00015, "system__parametersB");
   pragma Export (C, u00016, "system__parametersS");
   pragma Export (C, u00017, "system__secondary_stackB");
   pragma Export (C, u00018, "system__secondary_stackS");
   pragma Export (C, u00019, "system__storage_elementsB");
   pragma Export (C, u00020, "system__storage_elementsS");
   pragma Export (C, u00021, "system__stack_checkingB");
   pragma Export (C, u00022, "system__stack_checkingS");
   pragma Export (C, u00023, "system__exception_tableB");
   pragma Export (C, u00024, "system__exception_tableS");
   pragma Export (C, u00025, "system__htableB");
   pragma Export (C, u00026, "system__htableS");
   pragma Export (C, u00027, "system__exceptionsB");
   pragma Export (C, u00028, "system__exceptionsS");
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
   pragma Export (C, u00047, "system__arith_64B");
   pragma Export (C, u00048, "system__arith_64S");
   pragma Export (C, u00049, "system__os_primitivesB");
   pragma Export (C, u00050, "system__os_primitivesS");
   pragma Export (C, u00051, "ada__numericsS");
   pragma Export (C, u00052, "ada__tagsB");
   pragma Export (C, u00053, "ada__tagsS");
   pragma Export (C, u00054, "system__val_lluB");
   pragma Export (C, u00055, "system__val_lluS");
   pragma Export (C, u00056, "system__val_utilB");
   pragma Export (C, u00057, "system__val_utilS");
   pragma Export (C, u00058, "system__case_utilB");
   pragma Export (C, u00059, "system__case_utilS");
   pragma Export (C, u00060, "ada__text_ioB");
   pragma Export (C, u00061, "ada__text_ioS");
   pragma Export (C, u00062, "ada__streamsS");
   pragma Export (C, u00063, "interfaces__c_streamsB");
   pragma Export (C, u00064, "interfaces__c_streamsS");
   pragma Export (C, u00065, "system__crtlS");
   pragma Export (C, u00066, "system__file_ioB");
   pragma Export (C, u00067, "system__file_ioS");
   pragma Export (C, u00068, "ada__finalizationB");
   pragma Export (C, u00069, "ada__finalizationS");
   pragma Export (C, u00070, "system__finalization_rootB");
   pragma Export (C, u00071, "system__finalization_rootS");
   pragma Export (C, u00072, "system__finalization_implementationB");
   pragma Export (C, u00073, "system__finalization_implementationS");
   pragma Export (C, u00074, "system__restrictionsB");
   pragma Export (C, u00075, "system__restrictionsS");
   pragma Export (C, u00076, "system__stream_attributesB");
   pragma Export (C, u00077, "system__stream_attributesS");
   pragma Export (C, u00078, "ada__io_exceptionsS");
   pragma Export (C, u00079, "interfaces__cB");
   pragma Export (C, u00080, "interfaces__cS");
   pragma Export (C, u00081, "system__os_libB");
   pragma Export (C, u00082, "system__os_libS");
   pragma Export (C, u00083, "system__stringsB");
   pragma Export (C, u00084, "system__stringsS");
   pragma Export (C, u00085, "system__file_control_blockS");
   pragma Export (C, u00086, "ada__finalization__list_controllerB");
   pragma Export (C, u00087, "ada__finalization__list_controllerS");
   pragma Export (C, u00088, "boardsB");
   pragma Export (C, u00089, "boardsS");
   pragma Export (C, u00090, "ada__charactersS");
   pragma Export (C, u00091, "ada__characters__latin_1S");
   pragma Export (C, u00092, "ada__stringsS");
   pragma Export (C, u00093, "ada__strings__unboundedB");
   pragma Export (C, u00094, "ada__strings__unboundedS");
   pragma Export (C, u00095, "ada__strings__fixedB");
   pragma Export (C, u00096, "ada__strings__fixedS");
   pragma Export (C, u00097, "ada__strings__mapsB");
   pragma Export (C, u00098, "ada__strings__mapsS");
   pragma Export (C, u00099, "system__bit_opsB");
   pragma Export (C, u00100, "system__bit_opsS");
   pragma Export (C, u00101, "ada__strings__searchB");
   pragma Export (C, u00102, "ada__strings__searchS");
   pragma Export (C, u00103, "system__compare_array_unsigned_8B");
   pragma Export (C, u00104, "system__compare_array_unsigned_8S");
   pragma Export (C, u00105, "system__address_operationsB");
   pragma Export (C, u00106, "system__address_operationsS");
   pragma Export (C, u00107, "system__img_enum_newB");
   pragma Export (C, u00108, "system__img_enum_newS");
   pragma Export (C, u00109, "system__img_intB");
   pragma Export (C, u00110, "system__img_intS");
   pragma Export (C, u00111, "system__string_ops_concat_5B");
   pragma Export (C, u00112, "system__string_ops_concat_5S");
   pragma Export (C, u00113, "system__string_ops_concat_4B");
   pragma Export (C, u00114, "system__string_ops_concat_4S");
   pragma Export (C, u00115, "gametreeB");
   pragma Export (C, u00116, "gametreeS");
   pragma Export (C, u00117, "configureS");
   pragma Export (C, u00118, "ada__containersS");
   pragma Export (C, u00119, "system__val_intB");
   pragma Export (C, u00120, "system__val_intS");
   pragma Export (C, u00121, "system__val_unsB");
   pragma Export (C, u00122, "system__val_unsS");
   pragma Export (C, u00123, "system__memoryB");
   pragma Export (C, u00124, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  ada.containers%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.arith_64%s
   --  system.bit_ops%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.compare_array_unsigned_8%s
   --  system.htable%s
   --  system.htable%b
   --  system.img_enum_new%s
   --  system.img_int%s
   --  system.img_int%b
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
   --  system.compare_array_unsigned_8%b
   --  system.secondary_stack%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_ops%s
   --  system.string_ops%b
   --  system.string_ops_concat_3%s
   --  system.string_ops_concat_3%b
   --  system.string_ops_concat_4%s
   --  system.string_ops_concat_4%b
   --  system.string_ops_concat_5%s
   --  system.string_ops_concat_5%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.arith_64%b
   --  ada.exceptions.last_chance_handler%s
   --  system.soft_links%s
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.secondary_stack%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.strings%s
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
   --  system.unsigned_types%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.fixed%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.finalization_implementation%s
   --  system.finalization_implementation%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  ada.finalization.list_controller%s
   --  ada.finalization.list_controller%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.val_int%s
   --  system.val_llu%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_llu%b
   --  system.val_int%b
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
   --  boards%s
   --  boards%b
   --  configure%s
   --  gametree%s
   --  gametree%b
   --  minmax%s
   --  minmax%b
   --  END ELABORATION ORDER

end ada_main;
