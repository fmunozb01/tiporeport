"! <p>Esta interface es el prototipo para las clases globales relacionadas a los tipos de ALV,
"! para este caso se hizo como prototipo para la clase ZCL_ALV_TABLE, la cual sería heredada
"! en los programas de tipo reporte por la clase local LCL_VIEW y se usaría para mostrar un
"! reporte en ALV (ABAP List Viewer)</p>
INTERFACE zif_alv
*----------------------------------------------------------------------*
* DESCRIPTION:
*   Esta interface es el prototipo para las clases globales relacionadas a los tipos de ALV,
*   para este caso se hizo como prototipo para la clase ZCL_ALV_TABLE, la cual sería heredada
*   en los programas de tipo reporte por la clase local LCL_VIEW y se usaría para mostrar un
*   reporte en ALV (ABAP List Viewer)
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
  METHODS:
    "! Coloca la referencia ya sea de la clase o del objeto de datos de tipo tabla
    "! que contiene los datos, ubicándolos en el ámbito del ALV
    "! @parameter co_data | Referencia de la clase que maneja o del objeto de datos de tipo tabla
    set_data
      CHANGING co_data TYPE any,
    "! Imprime el ALV
    print_alv.
ENDINTERFACE.

