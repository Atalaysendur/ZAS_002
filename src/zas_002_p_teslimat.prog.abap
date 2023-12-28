*&---------------------------------------------------------------------*
*& Report ZAS_P_TESLIMAT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zas_002_p_teslimat.

INCLUDE ZAS_002_I_TESLIMAT_TOP.
*INCLUDE zas_p_teslimat_i_top.
INCLUDE ZAS_002_I_TESLIMAT_CLS.
*INCLUDE zas_p_teslimat_i_class.
INCLUDE ZAS_002_I_TESLIMAT_MDL.
*INCLUDE ZAS_P_TESLIMAT_I_module.

INITIALIZATION.
  go_main = lcl_main=>create_instance( ).


START-OF-SELECTION.
  go_main->start_of_selection( ).
