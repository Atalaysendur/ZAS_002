*&---------------------------------------------------------------------*
*& Include          ZAS_P_TESLIMAT_I_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF gty_tblecntrl,
         teslimat_id TYPE zas_t_teslimat-teslimat_id,
         kalem       TYPE zas_t_teslimat-kalem,
         malzeme_no  TYPE zas_t_teslimat-malzeme_no,
         miktar      TYPE zas_t_teslimat-miktar,
         birim       TYPE zas_t_teslimat-birim,
         tutar       TYPE zas_t_teslimat-tutar,
         tutar_birim TYPE zas_t_teslimat-tutar_birim,
       END OF gty_tblecntrl.


CLASS lcl_main DEFINITION DEFERRED.
DATA: go_main TYPE REF TO lcl_main.

DATA: gs_teslimat TYPE zas_t_teslimat,
      gs_tslmt2   TYPE zas_t_teslimat,
      gs_stok     TYPE zas_t_stok.


DATA: gt_tblecntrl TYPE TABLE OF gty_tblecntrl,
      gs_tblecntrl TYPE gty_tblecntrl,
      gv_mark TYPE char01.


CONTROLS tb_id TYPE TABSTRIP.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'ZASTBLE_CNTRL' ITSELF
CONTROLS: zastble_cntrl TYPE TABLEVIEW USING SCREEN 0103.

*&SPWIZARD: LINES OF TABLECONTROL 'ZASTBLE_CNTRL'
DATA:     g_zastble_cntrl_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.


  SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
    PARAMETERS: p_rad1 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND radio .
    PARAMETERS: p_rad2 RADIOBUTTON GROUP 1.
  SELECTION-SCREEN END OF BLOCK blk1.
