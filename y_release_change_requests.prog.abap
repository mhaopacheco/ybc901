*&---------------------------------------------------------------------*
*& Report Y_RELEASE_CHANGE_REQUESTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT y_release_change_requests.

TABLES: e070.

" Data's ************************************************************
DATA: "gt_e070 TYPE TABLE OF e070,
      gt_return   TYPE TABLE OF bapiret2,
      return      TYPE bapiret2,
      gd_status   TYPE e070-trstatus .

" Field Symbol's ****************************************************
  FIELD-SYMBOLS: <gt_output>    TYPE ANY TABLE .


" Definition " ******************************************************



" *******************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bq001 WITH FRAME TITLE TEXT-001. .

  SELECTION-SCREEN SKIP.

  SELECT-OPTIONS: s_autor   FOR e070-as4user NO INTERVALS,
                  so_num    FOR e070-trkorr .

  SELECTION-SCREEN SKIP.

  PARAMETERS: p_modif   TYPE c RADIOBUTTON GROUP g1 ,
              p_releas  TYPE c RADIOBUTTON GROUP g1.

  SELECTION-SCREEN SKIP.

  PARAMETERS p_test TYPE C AS CHECKBOX  .

SELECTION-SCREEN END OF BLOCK bq001.

**********************************************************************
INITIALIZATION.
  s_autor-low = sy-uname.
  APPEND s_autor.

**********************************************************************


**********************************************************************
START-OF-SELECTION.

  "Calc gd_status
  IF p_modif = abap_true .
    gd_status = 'D'.
  ELSE.
    gd_status = 'R' .
  ENDIF.

*  DATA: gt_e070[] TYPE .

  SELECT e~trkorr, trstatus, as4text INTO TABLE @DATA(gt_e070)
    FROM e070 AS e LEFT JOIN e07t AS t
      ON e~trkorr = t~trkorr
     AND t~langu = @sy-langu
    WHERE e~trkorr IN @so_num
      AND e~trstatus EQ @gd_status "EQ 'D' " not yet released
      AND e~strkorr EQ @space
      AND as4user IN @s_autor
       . " only request not task

  IF sy-subrc <> 0.
    MESSAGE 'No se encontraron Datos' TYPE 'E' .
  ENDIF.

  IF p_test IS NOT INITIAL AND gd_status = 'D' . " Va A liberar
    PERFORM release_requests .
    ASSIGN gt_return TO <gt_output>.
  ELSE  .
    ASSIGN gt_e070 TO <gt_output>.
  ENDIF.

  PERFORM show_alv .

*&---------------------------------------------------------------------*
*&      Form  RELEASE_REQUESTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM release_requests .

  FIELD-SYMBOLS: <e070> LIKE LINE OF gt_e070.

  LOOP AT gt_e070 ASSIGNING <e070>.

    CALL FUNCTION 'BAPI_CTREQUEST_RELEASE'
      EXPORTING
        requestid = <e070>-trkorr
        complete  = 'X' " Release request including tasks
      IMPORTING
        return    = return.

    return-message_v1 = <e070>-trkorr .
    return-message_v2 = <e070>-as4text .

    IF NOT return-type IS INITIAL.
      APPEND return TO gt_return.
    ELSE .
      return-type = 'S' .
      return-message = 'Orden Liberada Correctamente' .
      APPEND return TO gt_return.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_alv .

" *******************************************************************
  "Imprime ALV
  DATA: salv_tab_ctrl  TYPE REF TO cl_salv_table,
        salv_exception TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
            IMPORTING
              r_salv_table = salv_tab_ctrl
            CHANGING
              t_table      = <gt_output>
          ).

      salv_tab_ctrl->get_display_settings( )->set_striped_pattern( abap_true ).
      salv_tab_ctrl->get_columns( )->set_optimize( abap_true ).
      salv_tab_ctrl->get_functions( )->set_all( abap_true ).

      salv_tab_ctrl->display( ).

    CATCH cx_salv_msg INTO salv_exception.
  ENDTRY.

ENDFORM.
