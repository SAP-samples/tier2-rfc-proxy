CLASS zcl_gen_rfc_tier2_proxy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA function_modules TYPE cl_aco_metadata_provider=>t_functions READ-ONLY.
    DATA function_module TYPE cl_aco_metadata_provider=>t_function READ-ONLY.

    DATA wrapper_class_name TYPE sxco_class_name  READ-ONLY.
    DATA wrapper_interface_name TYPE sxco_class_name  READ-ONLY.
    DATA wrapper_factory_class_name TYPE sxco_class_name  READ-ONLY.

    DATA package_name TYPE sxco_package READ-ONLY.

    DATA namespace TYPE string READ-ONLY.

    DATA transport_request TYPE sxco_transport READ-ONLY.

    DATA wrapper_interface_code TYPE rswsourcet READ-ONLY.
    DATA wrapper_class_code TYPE rswsourcet READ-ONLY.
    DATA wrapper_factory_class_code TYPE rswsourcet READ-ONLY.

    DATA methods_code_definition TYPE rswsourcet READ-ONLY .
    DATA methods_code_implementation TYPE rswsourcet READ-ONLY.

    INTERFACES if_oo_adt_classrun .


    METHODS constructor
      IMPORTING
                i_package_name                 TYPE sxco_package
                i_transport_request            TYPE sxco_transport OPTIONAL
                i_generate_intf_and_fact_class TYPE abap_bool DEFAULT abap_true
                i_wrapper_class_name           TYPE sxco_class_name
                i_wrapper_interface_name       TYPE sxco_class_name  OPTIONAL
                i_wrapper_factory_class_name   TYPE sxco_class_name  OPTIONAL
                i_function_modules             TYPE cl_aco_metadata_provider=>t_functions
                i_overwrite_objects            TYPE abap_bool DEFAULT abap_true
      RAISING   cx_abap_invalid_value.


    METHODS read_aco_proxy_cls_src_code
*      IMPORTING remove_class_method_statements  TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(aco_proxy_class_src_code) TYPE rswsourcet.

    METHODS get_wrapper_class_code
      IMPORTING I_aco_proxy_class_src_code  TYPE rswsourcet
      RETURNING VALUE(r_wrapper_class_code) TYPE rswsourcet..

    METHODS get_wrapper_interface_code
      IMPORTING I_aco_proxy_class_src_code      TYPE rswsourcet
      RETURNING VALUE(r_wrapper_interface_code) TYPE rswsourcet.

    METHODS get_private_methods_code
      IMPORTING I_aco_proxy_class_src_code    TYPE rswsourcet
      EXPORTING
                r_methods_definition_code     TYPE rswsourcet
                r_methods_implementation_code TYPE rswsourcet.

    METHODS get_wrapper_factory_class_code
      RETURNING VALUE(r_wrapper__factory_class_code) TYPE rswsourcet..

    METHODS generate_wrapper_objects
      IMPORTING i_demo_mode             TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(r_exception_text) TYPE string.

    METHODS generate_wrapper_interface.

    METHODS generate_factory_class.

    METHODS update_wrapper_objects_code
      IMPORTING
        i_object_type TYPE trobjtype
        i_object_name TYPE trobj_name
        i_source_code TYPE rswsourcet.


    METHODS generate_aco_proxy_class
      IMPORTING
                i_function_modules  TYPE cl_aco_metadata_provider=>t_functions
                I_proxy_class_name  TYPE sxco_class_name
                i_package_name      TYPE sxco_package
                i_transport_request TYPE sxco_transport
      RETURNING VALUE(success)      TYPE abap_bool
      RAISING   cx_aco_exception.


    METHODS get_namespace
      IMPORTING i_package_name     TYPE string
      RETURNING VALUE(r_namespace) TYPE string.


    METHODS get_unique_object_name
      IMPORTING i_short_object_name  TYPE sxco_class_name
*                i_namespace         TYPE string
                i_object_type        TYPE trobjtype
      RETURNING VALUE(r_object_name) TYPE sxco_class_name.


    METHODS release_class_and_interface
      RAISING cx_abap_api_state.

    METHODS create_transport RETURNING VALUE(r_transport) TYPE sxco_transport.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA generate_intf_and_fact_class TYPE abap_bool.
    DATA overwrite_objects TYPE abap_bool.


ENDCLASS.



CLASS zcl_gen_rfc_tier2_proxy IMPLEMENTATION.

  METHOD constructor.

    IF NOT xco_abap_repository=>object->devc->for(  i_package_name  )->exists( ).
      RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | package { i_package_name } does not exist |.
    ENDIF.

    package_name  = i_package_name .
    namespace = get_namespace( CONV #( package_name ) ).

    IF i_transport_request IS INITIAL.
      "create transport checks if the selected package records changes
      transport_request = create_transport( ) .
    ELSE.
      transport_request = i_transport_request .
    ENDIF.

    generate_intf_and_fact_class = i_generate_intf_and_fact_class.

    overwrite_objects = i_overwrite_objects.

    IF i_overwrite_objects = abap_false.
      IF i_generate_intf_and_fact_class = abap_true.
        IF xco_abap_repository=>object->clas->for(  i_wrapper_class_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Class { i_wrapper_class_name } does already exist |.
        ELSEIF xco_abap_repository=>object->intf->for(  i_wrapper_interface_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Interface { i_wrapper_interface_name } does already exist |.
        ELSEIF xco_abap_repository=>object->clas->for(  i_wrapper_factory_class_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Class { i_wrapper_factory_class_name } does already exist |.
        ENDIF.
      ELSE.
        IF xco_abap_repository=>object->clas->for(  i_wrapper_class_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Class { i_wrapper_class_name } does already exist |.
        ENDIF.
      ENDIF.
    ENDIF.

    wrapper_class_name = i_wrapper_class_name     .
    wrapper_interface_name = i_wrapper_interface_name   .
    wrapper_factory_class_name = i_wrapper_factory_class_name  .

    LOOP AT i_function_modules INTO DATA(function_module).

      SELECT SINGLE * FROM tfdir INTO @DATA(function_module_info) WHERE funcname = @function_module-functionname.

      IF function_module_info IS INITIAL.
        RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Function module { function_module-functionname } does not exist.|.
      ENDIF.

      IF function_module_info-fmode IS NOT INITIAL.
        APPEND function_module TO function_modules.
      ELSE.
        RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Function module { function_module-functionname } is not remote enabled.|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD generate_wrapper_objects.
    TRY.
        generate_aco_proxy_class(
          i_function_modules  = function_modules
          i_proxy_class_name  = wrapper_class_name
          i_package_name      = package_name
          i_transport_request = transport_request
        ).
      CATCH cx_aco_exception INTO DATA(aco_exception).
        r_exception_text = |ACO error: { aco_exception->get_text(  ) }|.
        RETURN.
    ENDTRY.

    read_aco_proxy_cls_src_code(
*      EXPORTING
*        remove_class_method_statements = abap_true
      RECEIVING
        aco_proxy_class_src_code       = DATA(aco_proxy_class_code)
    ).

    IF aco_proxy_class_code IS INITIAL.
      r_exception_text = |No source code found: { wrapper_class_name }|.
      RETURN.
    ENDIF.


    IF generate_intf_and_fact_class = abap_true.

      get_wrapper_interface_code(
        EXPORTING
          i_aco_proxy_class_src_code = aco_proxy_class_code
        RECEIVING
          r_wrapper_interface_code   = wrapper_interface_code
      ).

      get_private_methods_code(
        EXPORTING
          i_aco_proxy_class_src_code = aco_proxy_class_code
        IMPORTING
          r_methods_definition_code  = methods_code_definition
          r_methods_implementation_code = methods_code_implementation
      ).

      get_wrapper_factory_class_code(
        RECEIVING
          r_wrapper__factory_class_code = wrapper_factory_class_code
      ).

    ENDIF.

    get_wrapper_class_code(
      EXPORTING
        i_aco_proxy_class_src_code = aco_proxy_class_code
      RECEIVING
        r_wrapper_class_code       = wrapper_class_code
    ).

    IF i_demo_mode = abap_false.

      TRY.

          IF generate_intf_and_fact_class = abap_true.

            IF xco_abap_repository=>object->intf->for(  wrapper_interface_name  )->exists( ) = abap_False.
              generate_wrapper_interface( ).
            ELSE.
              ASSERT overwrite_objects = abap_true.
            ENDIF.

            IF xco_abap_repository=>object->clas->for(  wrapper_factory_class_name  )->exists( ) = abap_False.
              generate_factory_class( ).
            ELSE.
              ASSERT overwrite_objects = abap_true.
            ENDIF.

            update_wrapper_objects_code(
              i_object_type = 'INTF'
              i_object_name = CONV #( wrapper_interface_name )
              i_source_code = wrapper_interface_code
            ).

          ENDIF.

          update_wrapper_objects_code(
            i_object_type = 'CLAS'
            i_object_name = CONV #( wrapper_class_name )
            i_source_code = wrapper_class_code
          ).

          IF generate_intf_and_fact_class = abap_true.

            update_wrapper_objects_code(
              i_object_type = 'CLAS'
              i_object_name = CONV #( wrapper_factory_class_name )
              i_source_code = wrapper_factory_class_code
            ).

          ENDIF.

        CATCH cx_oo_class_scan_error  INTO DATA(update_wrapper_code_exc).
          r_exception_text = |cl_oo_factory error: { update_wrapper_code_exc->get_text(  ) }|.
          RETURN.
      ENDTRY.

      TRY.
          release_class_and_interface(  ).
        CATCH  cx_abap_api_state   INTO DATA(api_state_exception).
          r_exception_text = |api_state error: { api_state_exception->get_text(  ) }|.
          RETURN.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    package_name     = 'TEST_AF_GENERATED_OBJECTS_001'.
    transport_request = ''.

    DATA number TYPE i VALUE 106.

    DATA(project_name) = 'af_wrapper_t'.

    function_module-functionname = to_upper( 'bapi_epm_product_get_detail' ).
    APPEND function_module TO function_modules.
    function_module-functionname = to_upper( 'bapi_epm_product_get_list' ).
    APPEND function_module TO function_modules.

    wrapper_class_name = to_upper( namespace && 'cl_wrap_'  && project_name ).
    wrapper_interface_name = to_upper( namespace && 'if_wrap_'  && project_name ).
    wrapper_factory_class_name = to_upper( namespace && 'cl_fact_'  && project_name ) .


    wrapper_class_name = get_unique_object_name(
                           i_short_object_name = wrapper_class_name
                           i_object_type       = 'CLAS'
                         ).

    wrapper_interface_name = get_unique_object_name(
                               i_short_object_name = wrapper_interface_name
                               i_object_type       = 'INTF'
                             )                    .
    wrapper_factory_class_name = get_unique_object_name(
                     i_short_object_name = wrapper_factory_class_name
                     i_object_type       = 'CLAS'
                   ).

    out->write( wrapper_class_name ).
    out->write( wrapper_interface_name ).
    out->write( wrapper_factory_class_name ).

    out->write( 'finished' ).

  ENDMETHOD.

  METHOD generate_aco_proxy_class.

    cl_aco_static_proxy=>create_static_proxy_by_rfc(
               EXPORTING
                 function_names         = i_function_modules
                 proxy_name             = i_proxy_class_name
                 destination_name       = 'NONE'
                 devclass               = i_package_name
                 trkorr                 = i_transport_request
                 classic_exceptions     = abap_false
                 bapi_exceptions        = abap_false
                 generate_inactive      = abap_false
                 destination_by_constructor = abap_false
                 do_not_create_released_type = abap_true
             ).
    success = abap_true.

  ENDMETHOD.

  METHOD read_aco_proxy_cls_src_code.

    FIELD-SYMBOLS <source_code_line> TYPE string.

    SELECT SINGLE  * FROM i_abapobjectdirectoryentry INTO @DATA(obj_entry)
                               WHERE abapobject = @wrapper_class_name
                               .

    DATA(package) = obj_entry-abappackage.

    IF package IS INITIAL.
      EXIT.
    ENDIF.

    "read source code of generated proxy class
    DATA(ref_proxy_class_name) = cl_oo_factory=>create_instance( )->create_clif_source( to_upper( wrapper_class_name ) ).
    ref_proxy_class_name->get_source( IMPORTING source = aco_proxy_class_src_code ).

  ENDMETHOD.

  METHOD get_wrapper_factory_class_code.


    APPEND |CLASS { wrapper_factory_class_name } DEFINITION | TO r_wrapper__factory_class_code.
    APPEND |PUBLIC | TO r_wrapper__factory_class_code.
    APPEND |FINAL | TO r_wrapper__factory_class_code.
    APPEND |CREATE PRIVATE . | TO r_wrapper__factory_class_code.

    APPEND |   PUBLIC SECTION. | TO r_wrapper__factory_class_code.


    APPEND |     CLASS-METHODS create_instance | TO r_wrapper__factory_class_code.
    APPEND |       RETURNING VALUE(result) TYPE REF TO { wrapper_interface_name }.    |            TO r_wrapper__factory_class_code.
    APPEND |   PROTECTED SECTION. |   TO r_wrapper__factory_class_code.
    APPEND |   PRIVATE SECTION. |  TO r_wrapper__factory_class_code.
    APPEND |     METHODS constructor. |  TO r_wrapper__factory_class_code.
    APPEND |ENDCLASS. |  TO r_wrapper__factory_class_code.

    APPEND | CLASS { wrapper_factory_class_name } IMPLEMENTATION.  | TO r_wrapper__factory_class_code.

    APPEND |  METHOD create_instance. | TO r_wrapper__factory_class_code.

    APPEND |   result = NEW { wrapper_class_name }(  ). | TO r_wrapper__factory_class_code.
    APPEND |  ENDMETHOD.| TO r_wrapper__factory_class_code.

    APPEND |  METHOD constructor. | TO r_wrapper__factory_class_code.
    APPEND |  ENDMETHOD. | TO r_wrapper__factory_class_code.

    APPEND | ENDCLASS. | TO r_wrapper__factory_class_code.


  ENDMETHOD.

  METHOD get_wrapper_interface_code.

    DATA add_code TYPE abap_bool.
    DATA source_code_line LIKE LINE OF i_aco_proxy_class_src_code.

    APPEND |INTERFACE { wrapper_interface_name }| TO r_wrapper_interface_code.
    APPEND | PUBLIC . | TO r_wrapper_interface_code.

    LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

      DATA(result_last_statement)  = find( val = source_code_line sub  = |PROTECTED SECTION.| case = abap_false ).
      DATA(result_first_statement) = find( val = source_code_line sub  = |INTERFACES if_aco_proxy| case = abap_false ).
      DATA(result_class_methods)   = find( val = source_code_line sub  = |CLASS-METHODS| case = abap_false ).
      DATA(result_if_aco_proxy)   = find( val = source_code_line sub  = |if_aco_proxy| case = abap_false ).

      IF result_class_methods <> -1.
        source_code_line = replace( val = source_code_line
                                    sub = |CLASS-METHODS|
                                    with = |METHODS| ).
      ENDIF.


      IF result_first_statement <> -1.
        add_code = abap_true.
      ENDIF.

      IF result_last_statement <> -1.
        APPEND |ENDINTERFACE.| TO r_wrapper_interface_code.
        EXIT.
      ENDIF.

      "skip if_aco_proxy statement
      IF add_code = abap_true AND result_if_aco_proxy = -1.
        APPEND source_code_line TO r_wrapper_interface_code.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_wrapper_class_code.

    DATA add_code TYPE abap_bool.
    DATA source_code_line LIKE LINE OF i_aco_proxy_class_src_code.

    IF generate_intf_and_fact_class = abap_false.

      LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

        " add a final statement before the CREATE PUBLIC statement.
        DATA(result_create_public_statement) = find( val = source_code_line sub  = |CREATE PUBLIC| case = abap_false ).
        DATA(result_if_aco_proxy)   = find( val = source_code_line sub  = |if_aco_proxy| case = abap_false ).

        IF result_create_public_statement <> -1.
          APPEND 'FINAL' TO r_wrapper_class_code.
          source_code_line =   replace( val = source_code_line
                                        sub =  |CREATE PUBLIC|
                                        with = |CREATE PRIVATE| ).
        ENDIF.

        "remove DESTINATION _dest_ statements
        DATA(result_destination_statement) = find( val = source_code_line sub  = |DESTINATION _dest_| case = abap_false ).
*        source_code_line = to_upper( source_code_line ).
        IF result_destination_statement <> -1.
          source_code_line =   replace( val = source_code_line
                                                 sub = |DESTINATION _dest_|
                                                 with = |DESTINATION space| ).
        ENDIF.
        IF result_if_aco_proxy = -1.
          APPEND source_code_line TO r_wrapper_class_code.
        ENDIF.
      ENDLOOP.

      RETURN.

    ENDIF.

    CLEAR r_wrapper_class_code.

    APPEND |CLASS { wrapper_class_name } DEFINITION| TO r_wrapper_class_code.
    APPEND |PUBLIC  | TO r_wrapper_class_code.
    APPEND |FINAL  | TO r_wrapper_class_code.
    APPEND |CREATE PUBLIC . | TO r_wrapper_class_code.
    APPEND |PUBLIC SECTION. | TO r_wrapper_class_code.
    APPEND |INTERFACES { wrapper_interface_name }.| TO r_wrapper_class_code.
    APPEND |PROTECTED SECTION. | TO r_wrapper_class_code.
    APPEND |PRIVATE SECTION. | TO r_wrapper_class_code.

    "add private methods code

    LOOP AT methods_code_definition INTO DATA(method_code_line).
      APPEND method_code_line TO r_wrapper_class_code.
    ENDLOOP.
    APPEND |.| TO r_wrapper_class_code.
    APPEND |ENDCLASS.| TO r_wrapper_class_code.
    APPEND | | TO r_wrapper_class_code.

    LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

      DATA(result_method_statement) = find( val = source_code_line sub  = |METHOD| case = abap_false ).
      DATA(result_first_statement) = find( val = source_code_line sub  = |CLASS { wrapper_class_name } IMPLEMENTATION.| case = abap_false ).

      "remove DESTINATION _dest_
      result_destination_statement = find( val = source_code_line sub  = |DESTINATION _dest_| case = abap_false ).

      IF result_first_statement <> -1.
        add_code = abap_true.
      ENDIF.

      "add interface name to method name
*      IF result_method_statement <> -1 .
*        source_code_line = to_upper( source_code_line ).
*        source_code_line =   replace( val = source_code_line
*                                      sub = |METHOD |
*                                      with = |METHOD { wrapper_interface_name }~| ).
*      ENDIF.

      IF result_destination_statement <> -1.
        source_code_line =   replace( val = source_code_line
                                               sub = |DESTINATION _dest_|
                                               with = |DESTINATION space| ).
      ENDIF.

      IF add_code = abap_true.
        APPEND source_code_line TO r_wrapper_class_code.
        IF result_first_statement <> -1.
          LOOP AT methods_code_implementation INTO DATA(methods_code_impl_line).
            APPEND methods_code_impl_line TO r_wrapper_class_code.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD update_wrapper_objects_code.

    DATA(ref) = cl_oo_factory=>create_instance( )->create_clif_source( to_upper(  i_object_name  ) ).

    TRY.
        ref->lock( ).
        ref->set_source( source = i_source_code ).
        ref->save( ).
        ref->unlock( ).
      CATCH  cx_oo_access_permission cx_oo_class_scan_error INTO DATA(access_permission_exc).
*    WRITE : / |error occured: { access_permission_exc->get_text(  ) }|.
*        EXIT.
    ENDTRY.

*    DATA objects TYPE STANDARD TABLE OF dwinactiv .
*
*    objects =  VALUE #( ( object =  i_object_type  obj_name = i_object_name uname = sy-uname ) ).
*
*    CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
*      TABLES
*        objects                = objects
*      EXCEPTIONS
*        excecution_error       = 1
*        cancelled              = 2
*        insert_into_corr_error = 3
*        OTHERS                 = 4.

    IF sy-subrc <> 0.
*  WRITE : / |error occured when activating class { cls_name }. SY-SUBRC = { sy-subrc } |.
*      EXIT.
    ENDIF.
  ENDMETHOD.



  METHOD generate_wrapper_interface.

    DATA  lo_put_operation TYPE REF TO if_xco_gen_intf_o_put  .

    IF transport_request IS INITIAL.
      lo_put_operation = xco_generation=>environment->local->for-intf->create_put_operation( ).
    ELSE.
      lo_put_operation = xco_generation=>environment->transported( transport_request )->for-intf->create_put_operation(  ).
    ENDIF.

    DATA(lo_form_specification) = lo_put_operation->add_object( wrapper_interface_name
      )->set_package( package_name
      )->create_form_specification( ).

    lo_form_specification->set_short_description( 'Sample interface' ) ##NO_TEXT.


    lo_put_operation->execute(  ).

  ENDMETHOD.

  METHOD generate_factory_class.

    DATA  lo_put_operation TYPE REF TO if_xco_gen_clas_o_put  .

    IF transport_request IS INITIAL.
      lo_put_operation = xco_generation=>environment->local->for-clas->create_put_operation( ).
    ELSE.
      lo_put_operation = xco_generation=>environment->transported( transport_request )->for-clas->create_put_operation(  ).
    ENDIF.

    DATA(lo_form_specification) = lo_put_operation->add_object( wrapper_factory_class_name
      )->set_package( package_name
      )->create_form_specification( ).

    lo_form_specification->set_short_description( 'Factory class' ) ##NO_TEXT.


    lo_put_operation->execute(  ).
  ENDMETHOD.

  METHOD get_namespace.

    IF i_package_name = '$TMP'.
      r_namespace = 'Z'.
      EXIT.
    ENDIF.

    FIND ALL OCCURRENCES OF '/'
       IN i_package_name
       IGNORING CASE             " case insensitive
       RESULTS DATA(result). " TYPE match_result_tab

    IF lines( result ) = 2.
      CHECK result[ 1 ]-offset = 0.
      r_namespace = substring( val = i_package_name  len = result[ 2 ]-offset + 1 ).
      EXIT.
    ENDIF.

    DATA(first_character_package) = substring( val = i_package_name  off = 0 len = 1 ).
    DATA(package_name_length) = strlen( i_package_name ).

    IF first_character_package = 'Y'.
      r_namespace = 'Y'.
      EXIT.
    ENDIF.

    IF first_character_package =  'Z'.
      r_namespace = 'Z'.
      EXIT.
    ENDIF.

    IF package_name_length > strlen( 'TEST_' ).
      IF substring( val = i_package_name  len = strlen( 'TEST_' )   ) = 'TEST_' .
        r_namespace = 'Z'.
        EXIT.
      ENDIF.
    ENDIF.

    r_namespace = ''.


  ENDMETHOD.

  METHOD get_unique_object_name.

    DATA is_valid_repo_object_name TYPE abap_bool VALUE abap_false.
    DATA unique_number TYPE i VALUE 0.
    DATA unique_hex_number TYPE xstring .
    DATA unique_hex_number_string TYPE c LENGTH 2.

    "Generate a short class name that provides us the option to add two characters to suggest a unique name
*    DATA short_class_name TYPE c LENGTH 28.

*    short_class_name = i_namespace && 'CL_WRAP_' && i_fugr_name.

    WHILE is_valid_repo_object_name = abap_false AND unique_number < 255 .

      unique_hex_number = CONV xstring( unique_number ).

      IF unique_hex_number = 00.
        unique_hex_number_string = ''.
      ELSE.
        unique_hex_number_string = unique_hex_number.
      ENDIF.

      r_object_name = i_short_object_name && unique_hex_number_string.

      IF  i_object_type   = 'CLAS'.
        IF NOT xco_abap_repository=>object->clas->for( r_object_name )->exists( ).
          is_valid_repo_object_name = abap_true.
        ELSE.
          unique_number += 1.
        ENDIF.
      ELSEIF i_object_type   = 'INTF'.
        IF NOT xco_abap_repository=>object->intf->for( r_object_name )->exists( ).
          is_valid_repo_object_name = abap_true.
        ELSE.
          unique_number += 1.
        ENDIF.
      ELSE.
        ASSERT 1 = 0.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.




  METHOD release_class_and_interface.

    IF generate_intf_and_fact_class = abap_true.

      DATA(api_state_wrapper_fact_class) = cl_abap_api_state=>create_instance(
                api_key = VALUE #(
                object_type     = 'CLAS'
                object_name     = to_upper( wrapper_factory_class_name )
                ) ).

      api_state_wrapper_fact_class->release(
        EXPORTING
          release_contract         = 'C1'
          use_in_cloud_development = abap_true
          use_in_key_user_apps     = abap_false
          request                  = transport_request
      ).


      DATA(api_state_wrapper_interface) = cl_abap_api_state=>create_instance(
                api_key = VALUE #(
                object_type     = 'INTF'
                object_name     = to_upper( wrapper_interface_name )
                ) ).

      api_state_wrapper_interface->release(
        EXPORTING
          release_contract         = 'C1'
          use_in_cloud_development = abap_true
          use_in_key_user_apps     = abap_false
          request                  = transport_request
      ).

    ELSE.
      DATA(api_state_wrapper_class) = cl_abap_api_state=>create_instance(
                api_key = VALUE #(
                object_type     = 'CLAS'
                object_name     = to_upper( wrapper_class_name )
                ) ).

      api_state_wrapper_class->release(
        EXPORTING
          release_contract         = 'C1'
          use_in_cloud_development = abap_true
          use_in_key_user_apps     = abap_false
          request                  = transport_request
      ).
    ENDIF.

  ENDMETHOD.

  METHOD create_transport.
    DATA(xco_package) = xco_abap_repository=>object->devc->for(  package_name  ).
    DATA(record_object_changes) = xco_package->read( )-property-record_object_changes.
    IF record_object_changes = abap_true.
      DATA(lo_transport_target) = xco_package->read( )-property-transport_layer->get_transport_target( ).
      DATA(new_transport_object) = xco_cp_cts=>transports->workbench( lo_transport_target->value  )->create_request( |Wrapper class: { wrapper_class_name } | ).
      r_transport = new_transport_object->value.
    ELSE.
      r_transport = ''.
    ENDIF.
  ENDMETHOD.

  METHOD get_private_methods_code.
    DATA add_code TYPE abap_bool.
    DATA result_in_interface TYPE abap_bool.

    DATA source_code_line LIKE LINE OF i_aco_proxy_class_src_code.

*    APPEND |INTERFACE { wrapper_interface_name }| TO r_methods_definition_code.
*    APPEND | PUBLIC . | TO r_wrapper_interface_code.

    LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

      DATA(result_last_statement)  = find( val = source_code_line sub  = |.| case = abap_false ).
      DATA(result_first_statement) = find( val = source_code_line sub  = |CLASS-METHODS| case = abap_false ).
      DATA(result_type_statement)   = find( val = source_code_line sub  = |TYPE | case = abap_false ).
      DATA(result_endclass_statement) = find( val = source_code_line sub  = |ENDCLASS.| case = abap_false ).

      DATA(result_class_methods)   = find( val = source_code_line sub  = |CLASS-METHODS| case = abap_false ).

      DATA(result_destination_statement) = find( val = source_code_line sub  = |_dest_| case = abap_false ).

      DATA(result_none_statement) = find( val = source_code_line sub  = |'NONE'| case = abap_false ).

      " !prheader               TYPE ZIF_WRAP_TEST_4714~bapimereqheader OPTIONAL

      DATA(result_exclamation_mark) = find( val = source_code_line sub  = |!| case = abap_false ).


      IF result_class_methods <> -1.
        source_code_line = replace( val = source_code_line
                                    sub = |CLASS-METHODS|
                                    with = |METHODS| ).
        DATA(source_code_line_impl) = replace( val = source_code_line
                                    sub = |METHODS |
                                    with = |METHOD { wrapper_interface_name }~| ).
        DATA(method_name) = replace( val = source_code_line
                                    sub = |METHODS |
                                    with = || ).
        CONDENSE method_name NO-GAPS.
        source_code_line_impl =  source_code_line_impl &&  '.'.
*        APPEND |.| TO r_methods_implementation_code.
        APPEND source_code_line_impl TO r_methods_implementation_code.
        APPEND |  "add call to private method { method_name }| TO r_methods_implementation_code.
        APPEND |  "e.g. me->{ method_name }( ... ) | TO r_methods_implementation_code.
        APPEND |RAISE EXCEPTION TYPE cx_method_not_implemented.| TO r_methods_implementation_code.
        APPEND |ENDMETHOD.|     TO r_methods_implementation_code.
      ENDIF.

      IF result_endclass_statement <> -1.
        EXIT.
      ENDIF.
      DATA string1 TYPE string.
      DATA string2 TYPE string.
      DATA pos TYPE i.
      CLEAR pos.
      IF result_exclamation_mark <> -1.

        SPLIT source_code_line AT '!' INTO string1 string2.
        SPLIT string2 AT space INTO TABLE DATA(source_code_line_tab).

        LOOP AT source_code_line_tab INTO DATA(single_statement).
          pos += 1.
          IF single_statement = 'TYPE'.
            DATA(pos_type) = pos.
          ENDIF.
        ENDLOOP.

        DATA(type_of) = source_code_line_tab[ pos_type + 1 ].

        "REPLACE '!' IN type_of WITH ''.

        LOOP AT wrapper_interface_code INTO DATA(interface_code_line).
          CLEAR result_in_interface.
          DATA(result_is_in_interface) = find( val = interface_code_line sub  = type_of case = abap_false ).
          DATA(first_methods_statement) = find( val = interface_code_line sub  = 'METHODS' case = abap_false ).
          "only search in types statements
          IF first_methods_statement <> -1.
            EXIT.
          ENDIF.
          IF result_is_in_interface <> -1.
            result_in_interface = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF result_first_statement <> -1.
        add_code = abap_true.
      ENDIF.

      IF add_code = abap_true AND result_last_statement <> -1.
        add_code = abap_false.
      ENDIF.

      IF result_type_statement <> -1 AND
         result_in_interface = abap_true AND
         result_destination_statement = -1
         .

        "certain function modules such as SATC_CI_GET_RESULT use
        "built in types such as 'I'
        "searching for 'I' by name is not reliable

        CASE to_upper( type_of ).
            "built in numeric types
          WHEN 'I' OR 'B' OR 'S' OR 'INT8' OR 'P' OR 'F'.
            "built in character type
          WHEN 'C' OR 'N'  OR 'STRING'.
            "built in date type
          WHEN 'D' OR 'T'.
            "not a built in type
          WHEN OTHERS.
            source_code_line = replace( val = source_code_line
                                        sub = |TYPE |
                                        with = |TYPE { wrapper_interface_name }~| ).
        ENDCASE.
      ENDIF.

      IF result_destination_statement <> -1.
        source_code_line =   replace( val = source_code_line
                                               sub = |DESTINATION _dest_|
                                               with = |DESTINATION space| ).
      ENDIF.

      IF result_none_statement <> -1.
        source_code_line =   replace( val = source_code_line
                                               sub = |DESTINATION 'NONE'|
                                               with = |DESTINATION space| ).
      ENDIF.

      IF add_code = abap_true.

        IF result_class_methods <> -1.
          APPEND '.' TO r_methods_definition_code.
        ENDIF.
        APPEND source_code_line TO r_methods_definition_code.
      ENDIF.



    ENDLOOP.
    APPEND '.' TO r_methods_definition_code.
  ENDMETHOD.

ENDCLASS.
