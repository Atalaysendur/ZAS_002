*&---------------------------------------------------------------------*
*& Include          ZAS_P_TESLIMAT_I_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_main DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-METHODS: create_instance RETURNING VALUE(ro_main) TYPE REF TO lcl_main.
    METHODS: kaydet,
      start_of_selection,
      search_help,
      detail,
      delete,
      goster.
  PRIVATE SECTION.
    CLASS-DATA: mo_main TYPE REF TO lcl_main.


ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD create_instance.
    IF mo_main IS INITIAL.
      mo_main = NEW #( ).
    ENDIF.
    ro_main = mo_main.

  ENDMETHOD.

  METHOD start_of_selection.
    IF p_rad1 EQ 'X'.
      CALL SCREEN 0100.
    ELSEIF p_rad2 EQ 'X'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZAS_T_STOK'.
    ENDIF.

  ENDMETHOD.

  METHOD search_help.

    DATA: lt_return_tab TYPE TABLE OF ddshretval,
          lt_mapping    TYPE TABLE OF dselc,
          ls_mapping    TYPE dselc.



    SELECT teslimat_id,
           kalem,
           malzeme_no,
           miktar,
           birim,
           tutar,
           tutar_birim
       FROM zas_t_teslimat INTO TABLE @DATA(lt_itab).

    lt_mapping = VALUE #(
                          ( fldname = 'F0001' dyfldname = 'GS_TSLMT2-TESLIMAT_ID' )
                          ( fldname = 'F0002' dyfldname = 'GS_TSLMT2-KALEM'       )
                          ( fldname = 'F0003' dyfldname = 'GS_TSLMT2-MALZEME_NO'  )
                          ( fldname = 'F0004' dyfldname = 'GS_TSLMT2-MIKTAR'      )
                          ( fldname = 'F0005' dyfldname = 'GS_TSLMT2-BIRIM'       )
                          ( fldname = 'F0006' dyfldname = 'GS_TSLMT2-TUTAR'       )
                          ( fldname = 'F0007' dyfldname = 'GS_TSLMT2-TUTAR_BIRIM' )   ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TESLIMAT_ID'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'GS_TSLMT2-TESLIMAT_ID'
        value_org       = 'S'
      TABLES
        value_tab       = lt_itab
        return_tab      = lt_return_tab
        dynpfld_mapping = lt_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.

  METHOD kaydet.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_teslimat-teslimat_id
      IMPORTING
        output = gs_teslimat-teslimat_id.


    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = gs_teslimat-malzeme_no
      IMPORTING
        output = gs_teslimat-malzeme_no.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input    = gs_teslimat-birim
        language = sy-langu
      IMPORTING
        output   = gs_teslimat-birim.


    SELECT COUNT(*)
    FROM zas_t_teslimat AS ztslmt
    WHERE ztslmt~teslimat_id EQ gs_teslimat-teslimat_id AND
          ztslmt~kalem EQ gs_teslimat-kalem.

    IF sy-subrc EQ 0.
      MESSAGE e000(ZAS_0002) DISPLAY LIKE 'I'.
    ENDIF.


    SELECT SINGLE zstok~malzeme_no,
                  zstok~miktar,
                  zstok~birim
     FROM zas_t_stok AS zstok
      LEFT JOIN zas_t_teslimat AS ztslmt ON zstok~malzeme_no EQ ztslmt~malzeme_no
      WHERE      zstok~malzeme_no EQ @gs_teslimat-malzeme_no AND
                 zstok~miktar >= @gs_teslimat-miktar
      INTO CORRESPONDING FIELDS OF @gs_stok.

    IF sy-subrc EQ 0.

      INSERT zas_t_teslimat FROM gs_teslimat.
      gs_stok-miktar =  gs_stok-miktar - gs_teslimat-miktar.

      MODIFY zas_t_stok FROM gs_stok.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input    = gs_teslimat-birim
          language = sy-langu
        IMPORTING
          output   = gs_teslimat-birim.

      MESSAGE i001(ZAS_0002).
    ELSE.
      MESSAGE i002(ZAS_0002).

    ENDIF.

  ENDMETHOD.

  METHOD detail.
    SELECT zt~teslimat_id,
           zt~kalem,
           zt~malzeme_no,
           zt~miktar,
           zt~birim,
           zt~tutar,
           zt~tutar_birim
       FROM zas_t_teslimat AS zt INTO TABLE @gt_tblecntrl.
    IF sy-subrc EQ 0.
      SORT gt_tblecntrl BY teslimat_id ASCENDING kalem ASCENDING.
    ENDIF.
  ENDMETHOD.

  METHOD delete.
    DELETE FROM zas_t_teslimat WHERE teslimat_id EQ gs_tblecntrl-teslimat_id AND
                                     kalem       EQ gs_tblecntrl-kalem.
    IF sy-subrc EQ 0.

      CLEAR: gv_mark.
      DELETE gt_tblecntrl WHERE  teslimat_id EQ gs_tblecntrl-teslimat_id AND
                                 kalem       EQ gs_tblecntrl-kalem.
      MESSAGE s004(ZAS_0002).
    ENDIF.
  ENDMETHOD.

  METHOD goster.

    IF gs_tslmt2-teslimat_id IS NOT INITIAL AND gs_tslmt2-kalem IS NOT INITIAL.

*      gs_tslmt2-teslimat_id = | { gs_tslmt2-teslimat_id ALPHA = IN } |.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_tslmt2-teslimat_id
          IMPORTING
            output = gs_tslmt2-teslimat_id.

      SELECT SINGLE *
      FROM zas_t_teslimat
      WHERE teslimat_id = @gs_tslmt2-teslimat_id AND kalem = @gs_tslmt2-kalem
      INTO CORRESPONDING FIELDS OF @gs_tslmt2.
      IF sy-subrc NE 0.
        MESSAGE e003(ZAS_0002).
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input    = gs_tslmt2-birim
          language = sy-langu
        IMPORTING
          output   = gs_tslmt2-birim.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
