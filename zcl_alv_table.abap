"! <p>Clase para manejo del ALV simple que abstrae la funcionalidad de la clase CL_SALV_TABLE, usa la interface ZFI_ALV como prototipo</p>
CLASS zcl_alv_table DEFINITION
  PUBLIC
  CREATE PUBLIC .
*----------------------------------------------------------------------*
* DESCRIPTION:
*   Clase para manejo del ALV simple que abstrae la funcionalidad de la
*   clase CL_SALV_TABLE, usa la interface ZFI_ALV como prototipo
*
* AUTHOR: Fernando Muñoz Beltrán - fmunozb@gmail.com
* DATE  : 28/09/2018
*
*----------------------------------------------------------------------*
* CHANGE HISTORY
* Date           Changed by   Defect/RFC     Description
*----------------------------------------------------------------------*
* <dd/mm/aaaa>   <uname>      <XXXX>         xxxxxxxxxxxxxxxxxxxxxxxxxx
*----------------------------------------------------------------------*

  PUBLIC SECTION.
    INTERFACES:
      zif_alv.

    ALIASES:
    set_data FOR zif_alv~set_data,
    print_alv FOR zif_alv~print_alv.

  PROTECTED SECTION.
    DATA:
      "! Instancia de ZIF_DATA con referencia a los datos obtenidos de la base de datos
      mo_data      TYPE REF TO zif_data,
      "! Instancia a la clase CL_SALV_TABLE de manejo básico para ALV sencillo
      mo_alv_table TYPE REF TO cl_salv_table.

    METHODS:
      "! Configura las columnas del ALV
      set_columns,
      "! Configura el menú de estatus propio del cliente
      set_custom_pfstatus,
      "! Configura el menú de estatus por defecto
      set_default_pfstatus,
      "! Configura los eventos del ALV
      set_events,
      "! Configura la selección de registros del ALV
      set_selections,
      "! Configura una disposición propia para el ALV
      set_layout,
      "! Configurar el estilo Zebra del ALV
      set_display_settings,
      "! Configuración para adicionar enlace a un registro en una columna del ALV
      set_hotspot,
      "! Configuración para adicionar opción de selección a un registro en una columna del ALV
      set_check_hotspot,
      "! Configuración de la cabecera del ALV
      set_header,
      "! Configuración del pie de página del ALV
      set_footer,
      "! Configura colores en el ALV
      set_colors,
      "! Configura estilos en un fila y columna del ALV
      set_cell_type,
      "! Configura agrupaciones, agregaciones en el ALV
      set_aggregations,
      "! Configura filtros en el ALV
      set_filters,
      "! Configura ordenamientos en el ALV
      set_orders,
      "! Gestión de eventos adicionales en el ALV
      "! @parameter e_salv_function |
      on_added_function
      FOR EVENT if_salv_events_functions~added_function
                    OF cl_salv_events_table
        IMPORTING e_salv_function,
      "! Evento ejecutado al dar clic en el enlace de un registro de una columna del ALV
      "! @parameter row | Fila de la tabla
      "! @parameter column | Columna de la tabla
      on_link_click
      FOR EVENT link_click
            OF cl_salv_events_table
        IMPORTING
            row
            column,
      "! Evento ejecutado por cualquier función en el ALV
      "! @parameter e_salv_function |
      on_user_command
      FOR EVENT added_function
                    OF cl_salv_events
        IMPORTING e_salv_function,
      "! Evento ejecutado antes del llamado de una función del ALV
      "! @parameter e_salv_function |
      on_before_salv_function
      FOR EVENT before_salv_function
                    OF cl_salv_events
        IMPORTING e_salv_function,
      "! Evento ejecutado después del llamado de una función del ALV
      "! @parameter e_salv_function |
      on_after_salv_function
      FOR EVENT after_salv_function
                    OF cl_salv_events
        IMPORTING e_salv_function,
      "! Evento ejecutado al llegar al final del ALV
      "! @parameter r_end_of_page |
      "! @parameter page |
      "! @parameter e_salv_function |
      on_end_of_page FOR EVENT end_of_page
            OF cl_salv_events
        IMPORTING
            r_end_of_page
            page,
      "! Evento ejecutado al comienzo del ALV
      "! @parameter r_top_of_page |
      "! @parameter page |
      on_top_of_page FOR EVENT top_of_page
            OF cl_salv_events
        IMPORTING
            r_top_of_page
            page,
      "! Evento ejecutado cuando se refresca el ALV, se puede usar para poner editable todo el ALV
      "! @parameter sender |
      on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid
        IMPORTING
            sender,
      "! Evento que mira si hubo cambios en el ALV
      "! @parameter er_data_changed |
      "! @parameter e_onf4 |
      "! @parameter e_onf4_after |
      "! @parameter e_onf4_before |
      "! @parameter e_ucomm |
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
            er_data_changed
            e_onf4
            e_onf4_after
            e_onf4_before
            e_ucomm
        .

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_alv_table IMPLEMENTATION.

  METHOD on_added_function.
    "Configura las funciones ejecutadas en el ALV
    CONSTANTS:
      lc_data_save TYPE string VALUE '&DATA_SAVE', "Función Guardar
      lc_applog    TYPE string VALUE 'APPLOG'. "Función APPLOG

    CASE e_salv_function.
      WHEN lc_data_save.
        "Ejecuta el almacenamiento de los datos del ALV
        DATA(lv_message) = |Los datos se han almacenado correctamente|.
        MESSAGE lv_message TYPE 'I'. "No hay datos válidos para cargar.
        me->mo_alv_table->refresh( ).
      WHEN lc_applog.
        "Muestra el log del programa
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "on_added_function

  METHOD on_link_click.
    "Evento ejecutado al dar clic en el enlace de un registro de una columna
    FIELD-SYMBOLS:
      <lt_data> TYPE ANY TABLE.

    CONSTANTS:
      lc_matnr TYPE lvc_fname VALUE 'MATNR', "Nombre columna MATNR.
      lc_xchpf TYPE lvc_fname VALUE 'XCHPF'. "Nombre columna XCHPF

    "Mira la columna que fue seleccionada
    CASE column.
      WHEN lc_matnr.
        "Si la columna es MATNR asigna los datos del ALV
        ASSIGN me->mo_data->mo_data->* TO <lt_data>.
        IF <lt_data> IS ASSIGNED.
          "Lee la fila deseada de la tabla
          LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
            IF sy-tabix = row.
              EXIT.
            ENDIF.
          ENDLOOP.
          "Asigna el valor al que se le dió clic
          ASSIGN COMPONENT lc_matnr OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_field>).
          IF <lv_field> IS ASSIGNED.
            DATA(lv_message) = |Se ha seleccionado el material { <lv_field> }|.
            MESSAGE lv_message TYPE 'I'.
          ENDIF.
        ENDIF.
      WHEN lc_xchpf.
        "Si la columna es MATNR asigna los datos del ALV
        ASSIGN me->mo_data->mo_data->* TO <lt_data>.
        IF <lt_data> IS ASSIGNED.
          "Lee la fila deseada de la tabla
          LOOP AT <lt_data> ASSIGNING <ls_data>.
            IF sy-tabix = row.
              EXIT.
            ENDIF.
          ENDLOOP.
          "Asigna el valor al que se le dió clic
          ASSIGN COMPONENT lc_xchpf OF STRUCTURE <ls_data> TO <lv_field>.
          IF <lv_field> IS ASSIGNED.
            lv_message = |Se dió clic en la fila { row }|.
          ENDIF.
          IF <lv_field> IS ASSIGNED.
            IF <lv_field> IS INITIAL.
              <lv_field> = abap_true.
            ELSE.
              CLEAR <lv_field>.
            ENDIF.
            me->mo_alv_table->refresh( ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD print_alv.
    "Imprime el ALV
    FIELD-SYMBOLS:
    <lt_data> TYPE ANY TABLE.

    DATA:
      lo_root TYPE REF TO cx_root, "Abstract Superclass for All Global Exceptions
      ls_mes  TYPE string. "Mensaje

    ASSIGN me->mo_data->mo_data->* TO <lt_data>.
    TRY.
        cl_salv_table=>factory(
*          EXPORTING
*            list_display = 'X'
          IMPORTING
            r_salv_table = mo_alv_table
          CHANGING
            t_table      = <lt_data>).

        "Configurar columnas
        me->set_columns( ).

        "Coloca la barra de estatus por defecto
        me->set_default_pfstatus( ).

        "Coloca barra de estatus custom
        "me->set_custom_pfstatus( ).

        "Coloca la cabecera del ALV
        me->set_header( ).

        "Coloca el pie de página
        me->set_footer( ).

        "Coloca el estilo Zebra
        "me->set_display_settings( ).

        "Coloca colores
        me->set_colors( ).

        "Coloca estilos a una celda
        me->set_cell_type( ).

        "Coloca agrupaciones, agregaciones
        "me->set_aggregations( ).

        "Coloca filtros
        "me->set_filters( ).

        "Coloca ordenamientos
        "me->set_orders( ).

        "Coloca los eventos
        me->set_events( ).

        "Coloca los enlaces
        me->set_hotspot( ).

        "Coloca enlace con botón de chequeo
        set_check_hotspot( ).

        "Colocar campo para seleccionar filas
        me->set_selections( ).

        "Coloca la gestión de Layout
        me->set_layout( ).

        "Coloca funciones adicionales
        me->on_added_function( ).

        "Muestra el ALV
        me->mo_alv_table->display( ).

      CATCH cx_salv_msg INTO lo_root.
        ls_mes = lo_root->get_text( ).
        MESSAGE ls_mes TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD set_colors.
    "Configura colores en el ALV
    TYPE-POOLS:
    col.

    FIELD-SYMBOLS:
    <lt_data> TYPE ANY TABLE.

    DATA:
      lo_cols_tab TYPE REF TO cl_salv_columns_table,
      lo_col_tab  TYPE REF TO cl_salv_column_table,
      lt_s_color  TYPE lvc_t_scol,
      ls_color    TYPE lvc_s_colo,
      ls_s_color  TYPE lvc_s_scol.    " Colors structure

    CONSTANTS:
      lc_t_color TYPE lvc_fname VALUE 'T_COLOR', "Nombre columna T_COLOR
      lc_matnr   TYPE lvc_fname VALUE 'MATNR', "Nombre columna MATNR
      lc_aenam   TYPE lvc_fname VALUE 'AENAM', "Nombre columna AENAM
      lc_laeda   TYPE lvc_fname VALUE 'LAEDA'. "Nombre columna LAEDA


    "Obtiene las columnas de la tabla
    lo_cols_tab = me->mo_alv_table->get_columns( ).

    "Configura color en la columna
    TRY.
        lo_col_tab ?= lo_cols_tab->get_column( lc_matnr ).
        ls_color-col = col_total.
        lo_col_tab->set_color( ls_color ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "Aplica color a una fila y columna específica
    ASSIGN me->mo_data->mo_data->* TO <lt_data>.
    IF <lt_data> IS ASSIGNED.
      "Lee la fila deseada de la tabla
      LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
        CASE sy-tabix.
          WHEN 3.
            "Le pone color solamente a la columna AENAM en la fila 3
            ls_s_color-fname     = lc_aenam.
            ls_s_color-color-col = col_negative.
            ls_s_color-color-int = 0.
            ls_s_color-color-inv = 0.
            APPEND ls_s_color TO lt_s_color.
            CLEAR  ls_s_color.
          WHEN 5.
            "Le pone color verde a la fila 5
            ls_s_color-color-col = col_positive.
            ls_s_color-color-int = 0.
            ls_s_color-color-inv = 0.
            APPEND ls_s_color TO lt_s_color.
            CLEAR  ls_s_color.
        ENDCASE.

        "Asigna la tabla de colores al campo T_COLOR
        ASSIGN COMPONENT lc_t_color OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_field>).
        <lv_field> = lt_s_color.
        CLEAR: lt_s_color.
      ENDLOOP.
    ENDIF.

    "Referencia la tabla de colores
    TRY.
        lo_cols_tab->set_color_column( lc_t_color ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.

  METHOD set_columns.
    "Configura las columnas del ALV
    DATA:
      lo_cols   TYPE REF TO cl_salv_columns,      "All Column Objects
      lo_column TYPE REF TO cl_salv_column.       "Individual Column Object

    CONSTANTS:
               lc_ersda TYPE lvc_fname VALUE 'ERSDA'. "Nombre columna ERSDA

    TRY.
        "Obtiene todas las columnas
        lo_cols = me->mo_alv_table->get_columns( ).
        "Optimiza el tamaño de las columnas
        lo_cols->set_optimize( abap_true ).

        "Obtiene una columna
        lo_column = lo_cols->get_column( lc_ersda ).
        "Oculta la columna
        lo_column->set_visible( abap_false ).

        "Pone textos a la columna
*        lo_column = lo_cols->get_column( lc_stats ).
*        lo_column->set_long_text( |Estado del proceso| ).
*        lo_column->set_medium_text( |Estado proceso| ).
*        lo_column->set_short_text( |Es.proceso| ).
*
*        lo_column = lo_cols->get_column( lc_deses ).
*        lo_column->set_long_text( |Descripción del proceso| ).
*        lo_column->set_medium_text( |Desc. proceso| ).
*        lo_column->set_short_text( |Desc.proc.| ).

      CATCH cx_salv_not_found ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "set_hide_columns

  METHOD set_custom_pfstatus.
    "Configura el menú de estatus propio del cliente
    CONSTANTS:
               lc_pfstatus      TYPE sypfkey VALUE 'EJECUTAR'.    "Status GUI actual

    "Configura el menú de estatus deseado
    me->mo_alv_table->set_screen_status(
       pfstatus      =  lc_pfstatus
       report        =  sy-repid
       set_functions = me->mo_alv_table->c_functions_all ).
  ENDMETHOD.                    "set_custom_pfstatus

  METHOD set_data ##NEEDED.
    "Coloca la referencia de los datos en el atributo MO_DATA
    me->mo_data ?= co_data.
  ENDMETHOD.                    "get_data

  METHOD set_default_pfstatus.
    "Configura el menú de estatus por defecto
    DATA:
          lo_functions TYPE REF TO cl_salv_functions_list. "Generic and User-Defined Functions in List-Type Tables

    "Obtiene objeto funciones del ALV
    lo_functions = me->mo_alv_table->get_functions( ).

    "Muestra todas las funciones del ALV
    lo_functions->set_all( abap_true ).

    "Muestra las funciones mínimas del ALV
    "lo_functions->set_default( abap_true ).

  ENDMETHOD.                    "set_default_pfstatus

  METHOD set_display_settings.
    "Configurar el estilo Zebra del ALV
    DATA:
          lo_display TYPE REF TO cl_salv_display_settings.

    "Obtiene el objeto diplay settings
    lo_display = me->mo_alv_table->get_display_settings( ).

    "Configura el estilo Zebra
    lo_display->set_striped_pattern( abap_true ).

    "Pone título al ALV
    lo_display->set_list_header( |ALV Test for Display Settings| ).
  ENDMETHOD.

  METHOD set_events.
    "Configura los eventos del ALV
    DATA:
          lo_events TYPE REF TO cl_salv_events_table. "Events in Simple, Two-Dimensional Tables

    "Obtiene los eventos del ALV
    lo_events = me->mo_alv_table->get_event( ).

    "Configura el manejador con los eventos deseados
    SET HANDLER me->on_added_function FOR lo_events.
    SET HANDLER me->on_link_click FOR lo_events.
    SET HANDLER me->on_user_command FOR lo_events.
    SET HANDLER me->on_before_salv_function FOR lo_events.
    SET HANDLER me->on_after_salv_function FOR lo_events.
    SET HANDLER me->on_end_of_page FOR lo_events.
    SET HANDLER me->on_top_of_page FOR lo_events.

    SET HANDLER me->on_after_refresh FOR ALL INSTANCES ACTIVATION abap_true.
    SET HANDLER me->on_data_changed FOR ALL INSTANCES ACTIVATION abap_true.
  ENDMETHOD.

  METHOD set_footer.
    "Configuración del pie de página del ALV
    DATA:
      lo_footer  TYPE REF TO cl_salv_form_layout_grid,
      lo_f_label TYPE REF TO cl_salv_form_label,
      lo_f_flow  TYPE REF TO cl_salv_form_layout_flow.

    "Objeto pie de página
    CREATE OBJECT lo_footer.

    "Información en negrilla
    lo_f_label = lo_footer->create_label( row = 1 column = 1 ).
    lo_f_label->set_text( |En el pie de página| ).

    "Información en formato tabular
    lo_f_flow = lo_footer->create_flow( row = 2  column = 1 ).
    lo_f_flow->create_text( text = |Texto descriptivo 1| ).

    lo_f_flow = lo_footer->create_flow( row = 3  column = 1 ).
    lo_f_flow->create_text( text = |Columna 1| ).

    lo_f_flow = lo_footer->create_flow( row = 3  column = 2 ).
    lo_f_flow->create_text( text = |Columna 2| ).

    "Configura el pie de página cuando se muestra el ALV en lista.
    me->mo_alv_table->set_end_of_list( lo_footer ).

    "Configura el pie de página cuando se muestra el ALV en impresión.
    me->mo_alv_table->set_end_of_list_print( lo_footer ).
  ENDMETHOD.

  METHOD set_header.
    "Configuración de la cabecera del ALV
    DATA:
      lo_header  TYPE REF TO cl_salv_form_layout_grid,
      lo_h_label TYPE REF TO cl_salv_form_label,
      lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.

    "Objeto cabecera
    CREATE OBJECT lo_header.

    "Información en negrilla
    lo_h_label = lo_header->create_label( row = 1 column = 1 ).
    lo_h_label->set_text( |Información generada| ).

    "Información en formato tabular
    lo_h_flow = lo_header->create_flow( row = 2  column = 1 ).
    lo_h_flow->create_text( text = |Fecha: | ).
    lo_h_flow = lo_header->create_flow( row = 2  column = 2 ).
    lo_h_flow->create_text( text = sy-datum ).

    lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
    lo_h_flow->create_text( text = |Hora| ).
    lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
    lo_h_flow->create_text( text = sy-uzeit ).

    lo_h_flow = lo_header->create_flow( row = 4  column = 1 ).
    lo_h_flow->create_text( text = |Usuario| ).
    lo_h_flow = lo_header->create_flow( row = 4  column = 2 ).
    lo_h_flow->create_text( text = sy-uname ).

    "Configura la cabecera cuando se muestra el ALV en lista.
    me->mo_alv_table->set_top_of_list( lo_header ).

    "Configura la cabecera cuando se muestra el ALV en impresión.
    me->mo_alv_table->set_top_of_list_print( lo_header ).
  ENDMETHOD.

  METHOD set_hotspot.
    "Configura el hotspot para los registros del ALV
    DATA:
      lo_cols_tab TYPE REF TO cl_salv_columns_table, "Columns in Simple, Two-Dimensional Tables
      lo_col_tab  TYPE REF TO cl_salv_column_table. "Column Description of Simple, Two-Dimensional Tables

    CONSTANTS:
               lc_matnr TYPE lvc_fname VALUE 'MATNR'. "Nombre columna MATNR

    "Obtiene las columnas de la tabla
    lo_cols_tab = me->mo_alv_table->get_columns( ).

    "Obtiene la columna deseada
    TRY.
        lo_col_tab ?= lo_cols_tab->get_column( lc_matnr ).

        "Le coloca el enlace a la columna
        lo_col_tab->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found ##NO_HANDLER.
      CATCH cx_salv_data_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.                    "set_hotspot

  METHOD set_layout.
    "Configura una disposición propia para el ALV
    DATA:
      lo_layout TYPE REF TO cl_salv_layout, "Settings for Layout
      ls_key    TYPE salv_s_layout_key.

    CONSTANTS:
          lc_default TYPE slis_vari VALUE 'DEFAULT'.

    "Obtiene el objeto layout
    lo_layout = me->mo_alv_table->get_layout( ).

    "Configura el indicador único para el layout del ALV
    ls_key-report = sy-repid.
    lo_layout->set_key( ls_key ).

    "Quita las restricciones para almacenar layout.
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    "Configura layout por defecto
    lo_layout->set_initial_layout( lc_default ).
  ENDMETHOD.                    "set_layout

  METHOD set_selections.
    "Configura la selección de registros del ALV
    DATA:
          lo_selections TYPE REF TO cl_salv_selections. "Selections in List-Type Output Tables

    "Obtiene las selecciones del ALV
    lo_selections = me->mo_alv_table->get_selections( ).

    "Configura el tipo de selección
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  ENDMETHOD.                    "set_selections

  METHOD set_aggregations.
    "Configura agrupaciones, agregaciones en el ALV

    DATA:
          lo_aggrs TYPE REF TO cl_salv_aggregations.

    CONSTANTS:
               lc_maxc TYPE lvc_fname VALUE 'MAXC'. "Nombre columna MAXC

    "Obtiene las agregaciones del ALV
    lo_aggrs = me->mo_alv_table->get_aggregations( ).

    "Adiciona total a la columna MAXC
    TRY.
        lo_aggrs->add_aggregation( columnname  = lc_maxc aggregation = if_salv_c_aggregation=>total ).
      CATCH cx_salv_data_error ##NO_HANDLER.
      CATCH cx_salv_not_found ##NO_HANDLER.
      CATCH cx_salv_existing ##NO_HANDLER.
    ENDTRY.

    "Coloca el total en la línea superior
    lo_aggrs->set_aggregation_before_items( ).
  ENDMETHOD.

  METHOD set_filters.
    "Configura filtros en el ALV
    DATA:
          lo_filters TYPE REF TO cl_salv_filters.

    CONSTANTS:
               lc_ernam TYPE lvc_fname VALUE 'ERNAM'. "Nombre columna ERNAM

    "Obtiene los filtros del ALV
    lo_filters = me->mo_alv_table->get_filters( ).

    "Le pone filtro a ERNAM
    TRY.
        lo_filters->add_filter( columnname = lc_ernam sign = 'I' option = 'EQ' low = 'JMONTEPA'  ).
      CATCH cx_salv_not_found ##NO_HANDLER.
      CATCH cx_salv_data_error ##NO_HANDLER.
      CATCH cx_salv_existing ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD set_orders.
    "Configura ordenamientos en el ALV
    DATA:
          lo_sort TYPE REF TO cl_salv_sorts.

    CONSTANTS:
               lc_matnr TYPE lvc_fname VALUE 'MATNR'. "Nombre columna MATNR

    "Obtiene los ordenamientos del ALV
    lo_sort = me->mo_alv_table->get_sorts( ).

    "Configura el ordenamiento de MATNR con Subtotal
    TRY.
        lo_sort->add_sort( columnname = lc_matnr subtotal   = if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found ##NO_HANDLER.
      CATCH cx_salv_data_error ##NO_HANDLER.
      CATCH cx_salv_existing ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD set_check_hotspot.
    "Configuración para adicionar opción de selección a un registro en una columna del ALV
    DATA:
      lo_cols_tab TYPE REF TO cl_salv_columns_table, "Columns in Simple, Two-Dimensional Tables
      lo_col_tab  TYPE REF TO cl_salv_column_table. "Column Description of Simple, Two-Dimensional Tables

    CONSTANTS:
               lc_xchpf TYPE lvc_fname VALUE 'XCHPF'. "Nombre columna XCHPF

    "Obtiene las columnas de la tabla
    lo_cols_tab = me->mo_alv_table->get_columns( ).

    "Obtiene la columna deseada
    TRY.
        lo_col_tab ?= lo_cols_tab->get_column( lc_xchpf ).

        "Le coloca la opción de selección a la columna
        lo_col_tab->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot  ).

      CATCH cx_salv_not_found ##NO_HANDLER.
      CATCH cx_salv_data_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD on_user_command.
    "Evento ejecutado por cualquier función en el ALV
*    DATA(lv_message) = |Evento ejecutado por cualquier función en el ALV { e_salv_function }|.
*    MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.

  METHOD on_before_salv_function.
    "Evento ejecutado antes del llamado de una función del ALV
*    DATA(lv_message) = |Evento ejecutado antes del llamado de una función del ALV { e_salv_function }|.
*    MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.

  METHOD on_after_salv_function.
    "Evento ejecutado después del llamado de una función del ALV
*    DATA(lv_message) = |Evento ejecutado después del llamado de una función del ALV { e_salv_function }|.
*    MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.

  METHOD on_end_of_page.
    "Evento ejecutado al llegar al final del ALV
*    DATA(lv_message) = |Evento ejecutado al llegar al final del ALV { page }|.
*    MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.

  METHOD on_top_of_page.
    "Evento ejecutado al comienzo del ALV
*    DATA(lv_message) = |Evento ejecutado al comienzo del ALV { page }|.
*    MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.

  METHOD on_after_refresh.
    "Evento ejecutado cuando se refresca el ALV, se puede usar para poner editable todo el ALV
*    DATA(lv_message) = |Evento ejecutado cuando se refresca el ALV, se puede usar para poner editable todo el ALV|.
*    MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.


  METHOD on_data_changed.
    "Evento que mira si hubo cambios en el ALV
*    DATA(lv_message) = |Evento que mira si hubo cambios en el ALV { e_onf4_before } { e_onf4_after }|.
*    MESSAGE lv_message TYPE 'I'.
  ENDMETHOD.

  METHOD set_cell_type.
    "Configura estilos en un fila y columna del ALV
    TYPE-POOLS:
    col.

    FIELD-SYMBOLS:
    <lt_data> TYPE ANY TABLE.

    DATA:
      lo_cols_tab TYPE REF TO cl_salv_columns_table,
      lt_celltype TYPE salv_t_int4_column,
      ls_celltype TYPE LINE OF salv_t_int4_column. " Colors structure

    CONSTANTS:
      lc_t_celltype TYPE lvc_fname VALUE 'T_CELLTYPE', "Nombre columna T_CELLTYPE
      lc_ernam      TYPE lvc_fname VALUE 'ERNAM', "Nombre columna ERNAM
      lc_laeda      TYPE lvc_fname VALUE 'LAEDA'. "Nombre columna LAEDA


    "Obtiene las columnas de la tabla
    lo_cols_tab = me->mo_alv_table->get_columns( ).

    "Aplica estilo a fila y columna específica
    ASSIGN me->mo_data->mo_data->* TO <lt_data>.
    IF <lt_data> IS ASSIGNED.
      "Lee la fila deseada de la tabla
      LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
        CASE sy-tabix.
          WHEN 1.
            "Le pone texto a la columna AENAM en la fila 2
            ls_celltype-columnname = space. "lc_laeda.
            ls_celltype-value = if_salv_c_cell_type=>hotspot.
            APPEND ls_celltype TO lt_celltype.
            CLEAR  ls_celltype.
          WHEN 2.
            "Le pone enlace a la fila 6 columna ERNAM
            ls_celltype-columnname = lc_ernam.
            ls_celltype-value = if_salv_c_cell_type=>button.
            APPEND ls_celltype TO lt_celltype.
            CLEAR  ls_celltype.
        ENDCASE.

        "Asigna la tabla de colores al campo T_COLOR
        ASSIGN COMPONENT lc_t_celltype OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_field>).
        <lv_field> = lt_celltype.
        CLEAR: lt_celltype.
      ENDLOOP.
    ENDIF.
    "Referencia la tabla de colores
    TRY.
        lo_cols_tab->set_cell_type_column( lc_t_celltype ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

