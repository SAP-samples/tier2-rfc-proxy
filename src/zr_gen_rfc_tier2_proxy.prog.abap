REPORT zr_gen_rfc_tier2_proxy.

TYPES: BEGIN OF ty_fugr,
         area TYPE tlibg-area,
       END OF ty_fugr.
TABLES sscrfields.
DATA function_type TYPE tftit.
DATA function_modules TYPE cl_aco_metadata_provider=>t_functions .

SELECT-OPTIONS s_func FOR  function_type-funcname  NO INTERVALS .

PARAMETERS package TYPE tadir-devclass.

PARAMETERS : yes_intf RADIOBUTTON GROUP rad1 DEFAULT 'X',
             no_intf  RADIOBUTTON GROUP rad1.

PARAMETERS : wrapclas TYPE sxco_class_name DEFAULT 'ZCL_WRAP_TEST'.
PARAMETERS : wrapfact TYPE sxco_class_name DEFAULT 'ZCL_FACT_TEST'.
PARAMETERS : wrapintf TYPE sxco_class_name DEFAULT 'ZIF_WRAP_TEST'.

LOOP AT s_func INTO DATA(function_module_sel_option).
  "transaction aco_proxy only supports remote enabled function modules
  SELECT SINGLE * FROM tfdir INTO @DATA(function_module_info) WHERE funcname = @function_module_sel_option-low.
  IF function_module_info-fmode IS NOT INITIAL.
    APPEND to_upper( function_module_sel_option-low ) TO function_modules.
  ELSE.
    WRITE : / |function module { function_module_sel_option-low } has been skipped, because it is not remote-enabled. |.
  ENDIF.
ENDLOOP.

TRY.
    DATA(tier2_rfc_proxy_generator) = NEW zcl_gen_rfc_tier2_proxy(
      i_package_name                 = package
      i_transport_request            = ''
      i_generate_intf_and_fact_class = yes_intf
      i_wrapper_class_name           = wrapclas
      i_wrapper_interface_name       = wrapintf
      i_wrapper_factory_class_name   = wrapfact
      i_function_modules             = function_modules
    ).

  CATCH cx_abap_invalid_value INTO DATA(invalid_parameter).
    WRITE : / 'Exception occured:'.
    WRITE : / invalid_parameter->get_text(  ).
    EXIT.
ENDTRY.

DATA(exception_text) = tier2_rfc_proxy_generator->generate_wrapper_objects( ).

IF exception_text IS NOT INITIAL.
  WRITE : 'Exception occured generating objects:'.
  WRITE : exception_text.
  EXIT.
ENDIF.

DATA objects TYPE STANDARD TABLE OF dwinactiv .

IF yes_intf = abap_true.
  objects =  VALUE #(
                       ( object = 'INTF' obj_name = wrapintf uname = sy-uname )
                       ( object = 'CLAS' obj_name = wrapclas uname = sy-uname )
                       ( object = 'CLAS' obj_name = wrapfact uname = sy-uname )
                         ).
ELSE.
  objects =  VALUE #(
                      ( object = 'CLAS' obj_name = wrapclas uname = sy-uname )
                         ).
ENDIF.

CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
  TABLES
    objects                = objects
  EXCEPTIONS
    excecution_error       = 1
    cancelled              = 2
    insert_into_corr_error = 3
    OTHERS                 = 4.

IF sy-subrc <> 0.
  WRITE : / |error occured when activating classes. SY-SUBRC = { sy-subrc } |.
  EXIT.
ELSE.
  WRITE : / |Generation finished:|.
  IF yes_intf = abap_true.
    WRITE : / |{ wrapclas }, { wrapfact } and { wrapintf }|.
  ELSE.
    WRITE : / |{ wrapclas }|.
  ENDIF.
ENDIF.
