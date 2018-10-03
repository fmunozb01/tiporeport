*&---------------------------------------------------------------------*
*&  Include  y_report_example_f01
*&---------------------------------------------------------------------*

CLASS lcl_data IMPLEMENTATION.

  METHOD set_data.
	"Selecciona la información de la base de datos y la coloca en
	"el atributo MO_DATA
	SELECT *
	FROM mara
	WHERE matnr IN @s_matnr
	INTO CORRESPONDING FIELDS OF TABLE @me->mt_data.

	"Si no encontró datos
	IF me->mt_data IS INITIAL.
  	  rv_ok = abap_false.
  	  "No se encontraron registros para la selección indicada.
  	  MESSAGE i130(fr) INTO DATA(lv_mtext).
  	  lcl_report=>add_message( ).
	ELSE.
  	  "Si encontró datos

  	  "Ordena los datos
  	  "SORT me->mt_data.

  	  "Filtra información adicional
  	  "DELETE ADJACENT DUPLICATES FROM me->gt_data.

  	  "Obtiene referencia de los datos
  	  me->mo_data = REF #( me->mt_data ).
  	  "GET REFERENCE OF me->gt_data INTO me->go_data.

  	  rv_ok = abap_true.
	ENDIF.
  ENDMETHOD.

  METHOD get_data.
	"Obtiene la información que fue almacenada en el atributo MO_DATA
	ro_data = me->mo_data.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.

  METHOD load_of_program.
	"Inicializa datos globales. actuaría como el CLASS-CONSTRUCTOR de la clase
	"Crea la instancia a la clase LCL_DATA
	CREATE OBJECT mo_data.

	"Crea la instancia a la clase LCL_ALV
	CREATE OBJECT mo_view.
  ENDMETHOD.

  METHOD at_selection_screen_output.
	"Ejecuta el PBO (Process Before Output) del programa
  ENDMETHOD.

  METHOD initialization.
	"Para inicializar los parámetros u opciones de selección durante la primera llamada del programa
  ENDMETHOD.

  METHOD at_selection_screen.
	"Ejecuta el PAI (Process After Input) del programa
  ENDMETHOD.

  METHOD at_selection_screen_on.
	"Validar parámetros u opciones de selección ingresados por el usuario
  ENDMETHOD.

  METHOD at_selection_screen_on_val_req.
	"Para ayudas de búsqueda (F4)
  ENDMETHOD.

  METHOD at_selection_screen_on_end_of.
	"Para validar los datos seleccionados en ayudas de búsqueda
  ENDMETHOD.

  METHOD at_selection_screen_on_block.
	"Para validar los datos seleccionados en un bloque
  ENDMETHOD.

  METHOD at_selection_screen_on_hlp_req.
	"Para ayudas (F1)
  ENDMETHOD.

  METHOD at_selection_screen_on_rad_but.
	"Para ejecutar acciones en botones de radio
  ENDMETHOD.

  METHOD at_selection_screen_on_exit.
	"Cuando se ejecutan las funciones Back, Exit, o Cancel por el usuario
  ENDMETHOD.

  METHOD start_of_selection.
	"Evento principal, es como el método MAIN
	"Llama la pantalla con los filtros
	IF mo_view->call_screen_100( ) = abap_true.
  	  "Si encuentra información en la base de datos
  	  IF mo_data->set_data( ) = abap_false.
    	    "Si no se encontró información para la selección indicada.

    	    "Muestra mensajes si los hay
    	    show_messages( ).

    	    "Llama nuevamente el evento principal
    	    start_of_selection( ).
  	  ELSE.
    	    "Si encuentra información los muestra en el ALV

    	    "Pasa los datos a la vista
    	    mo_view->set_data( CHANGING co_data = mo_data ).

    	    "La vista imprime los datos
    	    mo_view->print_alv( ).

    	    "Llama nuevamente el evento principal
    	    start_of_selection( ).
  	  ENDIF.
	ELSE.
  	  "Si no desea continuar en el programa
  	  RETURN.
	ENDIF.

  ENDMETHOD.

  METHOD add_message.
	"Adiciona mensaje al atributo MT_MESSAGE
	APPEND INITIAL LINE TO lcl_report=>mt_message ASSIGNING FIELD-SYMBOL(<ls_message>).
	<ls_message>-msgid = sy-msgid.
	<ls_message>-msgty = sy-msgty.
	<ls_message>-msgno = sy-msgno.
	<ls_message>-msgv1 = sy-msgv1.
	<ls_message>-msgv2 = sy-msgv2.
	<ls_message>-msgv3 = sy-msgv3.
	<ls_message>-msgv4 = sy-msgv4.

	"Si encuentra un mensaje de error
	IF sy-msgty = 'E'.
  	  "Indica que el programa tiene error
  	  mv_error = abap_true.
	ENDIF.
  ENDMETHOD.

  METHOD show_messages.
	"Muestra los mensajes que se encuentra en el atributo MT_MESSAGE
	DATA:
      	lt_message_tab TYPE esp1_message_tab_type.

	"Si encuentra mensajes
	IF lcl_report=>mt_message IS NOT INITIAL.
  	  "Muestra los mensajes
  	  lt_message_tab = lcl_report=>mt_message.

  	  "Muestra los mensajes
  	  CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
    	  TABLES
      	    i_message_tab = lt_message_tab.   " Additional Messages
	ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_view IMPLEMENTATION.

  METHOD set_header.
	super->set_header( ).
  ENDMETHOD.

  METHOD call_screen_100.
	"Llama la pantalla con los filtros de selección del programa
	CALL SELECTION-SCREEN 100.
	IF sy-subrc = 0.
  	  rv_ok = abap_true.
	ELSE.
  	  rv_ok = abap_false.
	ENDIF.
  ENDMETHOD.

  METHOD set_default_pfstatus.
	"Configura el estatus por defecto del ALV
	DATA:
      	lo_functions TYPE REF TO cl_salv_functions_list. "Generic and User-Defined Functions in List-Type Tables

	lo_functions = me->mo_alv_table->get_functions( ).
	"lo_functions->set_default( abap_true ).
	lo_functions->set_all( abap_true ).
  ENDMETHOD.

  METHOD on_before_salv_function.
	DATA(lv_message) = |Evento ejecutado antes del llamado de la función { e_salv_function }|.
	MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.

  METHOD on_after_refresh.
	"Evento ejecutado cuando se refresca el ALV, se puede usar para poner editable todo el ALV
	DATA:
  	lo_grid   TYPE REF TO cl_gui_alv_grid,
  	ls_layout TYPE lvc_s_layo,
  	lt_fcat   TYPE lvc_t_fcat.

	FIELD-SYMBOLS:
	<ls_fcat> LIKE LINE OF lt_fcat.

	CONSTANTS:
  	lc_aenam TYPE lvc_fname VALUE 'AENAM'. "Nombre columna AENAM

	"Si el ALV se va a mostrar como editable
	IF pa_edit = abap_true.
  	  TRY .

      	    "Obtiene la instancia a la grilla
      	    lo_grid = lcl_salv_model_list=>get_grid( me->mo_alv_table ).

      	    "Si la grilla es la misma
      	    IF lo_grid = sender.
        	"Desregistra el manejo del evento refresh
        	SET HANDLER me->on_after_refresh
        	FOR ALL INSTANCES
        	ACTIVATION space.

        	"Vuelve todo el ALV editable
        	ls_layout-edit = abap_true.

        	"Modifica el layout dejándolo editable
        	lo_grid->set_frontend_layout( ls_layout ).
        	lo_grid->set_ready_for_input( 1 ).
      	    ENDIF.

    	  CATCH cx_salv_error.
  	  ENDTRY.
	ELSE.
  	  "Aparece editable una sola columna
  	  TRY .
      	    "Desregistra el manejo del evento refresh
      	    SET HANDLER on_after_refresh
        	FOR ALL INSTANCES
        	ACTIVATION space.

      	    "Trae el catálogo del ALV
      	    sender->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).
      	    LOOP AT lt_fcat ASSIGNING <ls_fcat>.
        	CASE <ls_fcat>-fieldname.
          	  WHEN lc_aenam.
            	  "Vuelve editable la columna AENAM
            	  <ls_fcat>-edit = abap_true.
        	ENDCASE.
      	    ENDLOOP.
      	    sender->set_frontend_fieldcatalog( lt_fcat ).
      	    sender->set_ready_for_input( 1 ).
    	  CATCH cx_salv_error.
  	  ENDTRY.
	ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_salv_model_list IMPLEMENTATION.

  METHOD get_grid.
	"Obtiene la instacia a la grilla
	DATA:
  	lo_grid_adap TYPE REF TO cl_salv_grid_adapter,
  	lo_fs_adap   TYPE REF TO cl_salv_fullscreen_adapter,
  	lo_root  	TYPE REF TO cx_root.

	IF io_salv_model->model NE if_salv_c_model=>table.
  	  RAISE EXCEPTION TYPE cx_salv_msg
    	    EXPORTING
      	      msgid = '00'
      	      msgno = '001'
      	      msgty = 'E'
      	      msgv1 = |incorrect salv type|.
	ENDIF.

	TRY .
    	  "Obtiene la instancia al adaptador
    	  lo_grid_adap ?= io_salv_model->r_controller->r_adapter.
  	CATCH cx_root INTO lo_root.
    	"could be fullscreen adapter
    	TRY .
        "Obtiene la instancia al adaptador
        lo_fs_adap ?= io_salv_model->r_controller->r_adapter.
      	  CATCH cx_root INTO lo_root.
          RAISE EXCEPTION TYPE cx_salv_msg
            EXPORTING
              previous = lo_root
              msgid	= '00'
              msgno	= '001'
              msgty	= 'E'
            	 msgv1	= |check previous exception|.
    	  ENDTRY.
	ENDTRY.

	"Obtiene la instancia a la grilla
	IF lo_grid_adap IS NOT INITIAL.
  	  eo_grid = lo_grid_adap->get_grid( ).
	ELSEIF lo_fs_adap IS NOT INITIAL.
  	  eo_grid = lo_fs_adap->get_grid( ).
	ELSE.
  	RAISE EXCEPTION TYPE cx_salv_msg
    	  EXPORTING
      	    msgid = '00'
      	    msgno = '001'
      	    msgty = 'W'
      	    msgv1 = |Adapter is not bound yet|.
	ENDIF.
  ENDMETHOD.

ENDCLASS.


