*&---------------------------------------------------------------------*
*&  Include  y_report_example_top
*&---------------------------------------------------------------------*
REPORT y_report_example.

"! <p>Clase local que sirve de interface para extraer los datos necesarios de la base de datos,
"! esta clase se encarga de usar los filtros de entrada del programa para realizar la consulta
"! de la información en la base de datos, también se encargaría de aplicar la lógica del
"! negocio necesaria para la gestión de la información obtenida.</p>
CLASS lcl_data DEFINITION FINAL.

  PUBLIC SECTION.
	INTERFACES:
  	zif_data.

	ALIASES:
	get_data FOR zif_data~get_data,
	set_data FOR zif_data~set_data.

  PROTECTED SECTION.
	TYPES BEGIN OF: ty_data.
        	INCLUDE TYPE mara.
	TYPES t_color   TYPE lvc_t_scol.
	TYPES t_celltype TYPE salv_t_int4_column.
	TYPES END OF ty_data.

	ALIASES:
	mo_data FOR zif_data~mo_data.

	DATA:
  	"! Tabla interna con la información obtenida de la base de datos
  	mt_data TYPE STANDARD TABLE OF ty_data.

ENDCLASS.

"! <p>Clase local que sirve para volver editable el ALV</p>
CLASS lcl_salv_model_list DEFINITION INHERITING FROM cl_salv_model_base FINAL.

  PUBLIC SECTION.
	CLASS-METHODS:
  	"! Obtiene la instacia a la grilla
  	"! @parameter io_salv_model | Instancia al modelo ALV
  	"! @parameter eo_grid | Instancia a la grilla
  	"! @raising cx_salv_error |
  	get_grid
    	  IMPORTING
      	    io_salv_model  TYPE REF TO cl_salv_model
    	  RETURNING
      	    VALUE(eo_grid) TYPE REF TO cl_gui_alv_grid
    	  RAISING
      	    cx_salv_error.

ENDCLASS.

"! <p>Clase para la gestión de la vista del programa</p>
CLASS lcl_view DEFINITION FINAL INHERITING FROM zcl_alv_table.

  PUBLIC SECTION.
	METHODS:
  	  "! Llama la pantalla con los filtros de selección del programa
  	  "! @parameter rv_ok | Retorna verdadero si la pantalla fue ejecutada (F8)
  	  call_screen_100
    	  RETURNING VALUE(rv_ok) TYPE boolean.

  PROTECTED SECTION.
	METHODS:
  	set_default_pfstatus REDEFINITION,
  	set_header REDEFINITION,
  	on_before_salv_function REDEFINITION,
  	on_after_refresh REDEFINITION.

ENDCLASS.

"! <p>Clase controladora para programas de tipo reporte</p>
CLASS lcl_report DEFINITION.

  PUBLIC SECTION.
	INTERFACES zif_report.
	ALIASES: 
        add_message FOR zif_report~add_message,
        show_messages FOR zif_report~show_messages,
        at_selection_screen FOR zif_report~at_selection_screen,
        at_selection_screen_on FOR zif_report~at_selection_screen_on,
        at_selection_screen_on_block FOR zif_report~at_selection_screen_on_block,
        at_selection_screen_on_end_of FOR zif_report~at_selection_screen_on_end_of,
        at_selection_screen_on_exit FOR zif_report~at_selection_screen_on_exit,
        at_selection_screen_on_hlp_req FOR zif_report~at_selection_screen_on_hlp_req,
        at_selection_screen_on_rad_but FOR zif_report~at_selection_screen_on_rad_but,
        at_selection_screen_on_val_req FOR zif_report~at_selection_screen_on_val_req,
        at_selection_screen_output FOR zif_report~at_selection_screen_output,
        initialization FOR zif_report~initialization,
        load_of_program FOR zif_report~load_of_program,
        start_of_selection FOR zif_report~start_of_selection,
        mt_message FOR zif_report~mt_message,
        mv_error FOR zif_report~mv_error.

	TYPES: BEGIN OF ty_sel_opt,
         	matnr TYPE mara-matnr,
       	END OF ty_sel_opt.

	CLASS-DATA:
  	  "! Estructura para las opciones de selección
  	  ms_sel_opt TYPE ty_sel_opt.

  PROTECTED SECTION.
	CLASS-DATA:
  	  "! Objeto para la gestión de los datos
  	  mo_data TYPE REF TO lcl_data,
  	  "! Objeto para la gestión de la vista
  	  mo_view TYPE REF TO lcl_view.

ENDCLASS.

"Pantalla con los filtros de selección
SELECTION-SCREEN BEGIN OF SCREEN 100 TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_matnr FOR lcl_report=>ms_sel_opt-matnr.
PARAMETERS: pa_edit AS CHECKBOX DEFAULT abap_false.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 100.

