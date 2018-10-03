"! <p>Esta interface es el prototipo para las clases locales LCL_REPORT que se crearían en los programas ABAP de tipo reporte</p>
INTERFACE zif_report
*----------------------------------------------------------------------*
* DESCRIPTION:
*   Esta interface es el prototipo para las clases locales LCL_REPORT
*   que se crearían en los programas ABAP de tipo reporte
*
* AUTHOR: Fernando Muñoz Beltrán - fmunozb@gmail.com
* DATE  : 28/09/2018
*
*----------------------------------------------------------------------*
* CHANGE HISTORY
* Date       	Changed by   Defect/RFC 	Description
*----------------------------------------------------------------------*
* <dd/mm/aaaa>   <uname>  	<XXXX>     	xxxxxxxxxxxxxxxxxxxxxxxxxx
*----------------------------------------------------------------------*

  PUBLIC .
  CLASS-DATA:
    "! Tabla interna para almacenar los mensajes generados en el programa
    mt_message TYPE TABLE OF esp1_message_wa_type,
    "! Indica si ocurrió un error en la ejecución del programa
    mv_error   TYPE boolean.

  CLASS-METHODS:
    "! Inicializa datos globales. actuaría como el CLASS-CONSTRUCTOR de la clase
    load_of_program,
    "! Ejecuta el PBO (Process Before Output) del programa
    at_selection_screen_output,
    "! Para inicializar los parámetros u opciones de selección durante la primera llamada del programa
    initialization,
    "! Ejecuta el PAI (Process After Input) del programa
    at_selection_screen,
    "! Validar parámetros u opciones de selección ingresados por el usuario
    "! @parameter iv_name_parameter | Nombre del parámetro o de la opción de selección
    at_selection_screen_on IMPORTING iv_name_parameter TYPE string,
    "! Para ayudas de búsqueda (F4)
    "! @parameter iv_name_parameter | Nombre del parámetro o de la opción de selección
    at_selection_screen_on_val_req IMPORTING iv_name_parameter TYPE string,
    "! Para validar los datos seleccionados en ayudas de búsqueda
    "! @parameter iv_name_parameter | Nombre del parámetro o de la opción de selección
    at_selection_screen_on_end_of IMPORTING iv_name_parameter TYPE string,
    "! Para validar los datos seleccionados en un bloque
    "! @parameter iv_name_parameter | Nombre del parámetro o de la opción de selección
    at_selection_screen_on_block IMPORTING iv_name_parameter TYPE string,
    "! Para ayudas (F1)
    "! @parameter iv_name_parameter | Nombre del parámetro o de la opción de selección
    at_selection_screen_on_hlp_req IMPORTING iv_name_parameter TYPE string,
    "! Para ejecutar acciones en botones de radio
    "! @parameter iv_name_parameter | Nombre del parámetro o de la opción de selección
    at_selection_screen_on_rad_but IMPORTING iv_name_parameter TYPE string,
    "! Cuando se ejecutan las funciones Back, Exit, o Cancel por el usuario
    at_selection_screen_on_exit,
    "! Evento principal, es como el método MAIN
    start_of_selection,
    "! Adiciona mensaje al atributo MT_MESSAGE
    add_message,
    "! Muestra los mensajes que se encuentra en el atributo MT_MESSAGE
    show_messages.

ENDINTERFACE.

