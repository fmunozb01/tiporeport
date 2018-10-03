"! <p>Esta interface es el prototipo para las clases locales LCL_DATA que se
"! crearían en los programas ABAP que hagan acceso a la base de datos</p>
INTERFACE zif_data
*----------------------------------------------------------------------*
* DESCRIPTION:
*   Esta interface es el prototipo para las clases locales LCL_DATA que se
*   crearían en los programas ABAP que hagan acceso a la base de dato
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
  DATA:
    "! Referencia a los datos obtenidos de la base de datos y de la lógica
    "! de extracción de la información pertinente
    mo_data  TYPE REF TO data.

  METHODS:
    "! Selecciona la información de la base de datos y la coloca en
    "! el atributo <strong>MO_DATA</strong>
    "! @parameter rp_ok | Retorna verdadero si obtuvo información
    set_data
      RETURNING VALUE(rv_ok) TYPE abap_bool,
    "! Obtiene la información que fue almacenada en el atributo <strong>MO_DATA</strong>
    "! @parameter ro_data | Referencia a los datos
    get_data
      RETURNING VALUE(ro_data) TYPE REF TO data.

ENDINTERFACE.

