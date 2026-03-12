# Code Change Document – CTD Dashboard DMS Attachment Function Modules

**Module:** SCM — CTD (Converted to Dedicated)
**Change Type:** New Development — Function Module Creation
**Date:** 12-Mar-2026
**Prepared By:** Gaurav Prabhu
**Reference FS:** FS - CTD Dashboard DMS Attachment FMs.txt
**SAP Target:** ECC 6.0 / NetWeaver 7.31

---

## Summary of New Objects

| # | Object Type | Object Name | Description |
|---|---|---|---|
| 1 | Function Module | `Z_CTD_DASH_DMS_CREATE` | Upload document to DMS against Vendor+Trip+Counter |
| 2 | Function Module | `Z_CTD_DASH_GET_ATTACH_COUNT` | Bulk attachment count per Leg for dashboard page load |
| 3 | Function Module | `Z_CTD_DASH_GET_DMS_LIST` | Fetch full attachment list for a specific Leg |
| 4 | Function Module | `Z_CTD_DASH_ATTACH_LIST` | Core attachment list builder (internal FM) |
| 5 | Function Module | `Z_CTD_DASH_DOWNLOAD_DOC` | Download a DMS document as XString |
| 6 | Function Module | `Z_CTD_DASH_DMS_DELETE` | Delete a DMS document |
| 7 | Function Group | `ZCTD_DASHBOARD_DMS` | Function Group to house all new FMs |
| 8 | Structure (SE11) | `ZCTD_DASH_ATTACH_COUNT_STR` | Line type: Counter + Attach Count + Has_Attach flag |
| 9 | Table Type (SE11) | `ZCTD_DASH_ATTACH_COUNT_TT` | Table type of `ZCTD_DASH_ATTACH_COUNT_STR` |

---

## ABAP Coding Standards Applied

The following standards from the ABAP Rules are applied throughout:

- **NW 7.31 Compatibility:** No inline declarations (`DATA(...)`), no `VALUE()`, no `NEW`, no string templates
- **Naming:** `lv_` variables, `lt_` tables, `lw_` work areas, `lc_` constants, `lty_` local types
- **Database:** No `SELECT *`, fields specified explicitly; no SELECT in loops; `FOR ALL ENTRIES` guarded by `IS NOT INITIAL` check in ABAP (not in WHERE)
- **Loops:** `LOOP AT ... ASSIGNING` used where table rows are modified; `MODIFY ... INDEX sy-tabix` used when `INTO` is required
- **BAPI Pattern:** Commit on success with `WAIT = 'X'`; Rollback on error; return messages via `ET_RETURN`
- **Comments:** Double-quote style; meaningful comments only; no obvious narration
- **AI Code Markers:** All code blocks marked with `" BEGIN: Cursor Generated Code` / `" END: Cursor Generated Code`
- **FM Header:** Standard header with Purpose, Author, Date, Change History

---

## SE11 Objects to Create First

Before creating the FMs, create the following Dictionary objects in **SE11**:

### Structure: `ZCTD_DASH_ATTACH_COUNT_STR`

| Field | Type | Description |
|---|---|---|
| `COUNTER` | `ZCOUNTER` | Leg Counter |
| `ATTACH_COUNT` | `INT4` | Number of documents attached |
| `HAS_ATTACH` | `CHAR1` | 'X' if attachments exist |

### Table Type: `ZCTD_DASH_ATTACH_COUNT_TT`
- Line Type: `ZCTD_DASH_ATTACH_COUNT_STR`
- Category: Internal Table
- Initial Access: Standard Table

---

## FM 1 — Z_CTD_DASH_DMS_CREATE

**Purpose:** Upload a document to SAP DMS against a specific Leg (Vendor + Trip + Counter).
**Called From:** Screen 1 (Transporter Trip Confirmation) and Screen 3 (Transporter CTD Confirmation) — on Upload action.

### Function Module Interface

```
IMPORTING:
  IM_LIFNR    TYPE LIFNR         (Vendor / Transporter Code)
  IM_TRIP     TYPE ZTRIP_NO      (Trip Number)
  IM_COUNTER  TYPE ZCOUNTER      (Leg Counter)
  IM_TRUCKNO  TYPE YTRUCK_NO     (Truck Number - informational)
  IM_XSTRING  TYPE XSTRING       (File content as binary XString)
  IM_FILENAME TYPE J_8A3E0011    (File name including extension)
  IM_MIMETYPE TYPE W3CONTTYPE    (MIME type e.g. application/pdf)

EXPORTING:
  ET_RETURN   TYPE BAPIRET2_T    (Return messages - Success / Error)
```

### ABAP Code

```abap
FUNCTION z_ctd_dash_dms_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_LIFNR) TYPE  LIFNR
*"     VALUE(IM_TRIP) TYPE  ZTRIP_NO
*"     VALUE(IM_COUNTER) TYPE  ZCOUNTER
*"     VALUE(IM_TRUCKNO) TYPE  YTRUCK_NO
*"     VALUE(IM_XSTRING) TYPE  XSTRING
*"     VALUE(IM_FILENAME) TYPE  J_8A3E0011
*"     VALUE(IM_MIMETYPE) TYPE  W3CONTTYPE
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module : Z_CTD_DASH_DMS_CREATE
*&---------------------------------------------------------------------*
*& Purpose         : CTD Dashboard - Upload document to DMS against
*&                   Vendor + Trip + Counter (Leg) combination
*& Author          : Gaurav Prabhu
*& Creation Date   : 12.03.2026
*& Release         : 1.0
*& Change History  :
*& Date       User           Description
*& 12.03.2026 Gaurav Prabhu  Initial Creation
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  TYPES: BEGIN OF lty_raw_line,
           line TYPE orblk,
         END OF lty_raw_line.

  DATA: lw_zdmsapp              TYPE zdmsapp,
        lw_doc                  TYPE bapi_doc_draw2,
        lw_obj_id               TYPE string,
        lw_drat                 TYPE bapi_doc_drat,
        lt_drat                 TYPE TABLE OF bapi_doc_drat,
        lw_doctype              TYPE bapi_doc_draw2-documenttype,
        lw_docnumber            TYPE bapi_doc_draw2-documentnumber,
        lw_docpart              TYPE bapi_doc_draw2-documenttype,
        lw_docversion           TYPE bapi_doc_draw2-documentversion,
        lt_drad                 TYPE TABLE OF bapi_doc_drad,
        lt_classallocations     TYPE TABLE OF bapi_class_allocation,
        lt_characteristicvalues TYPE TABLE OF bapi_characteristic_values,
        lw_return               TYPE bapiret2,
        lw_draw                 TYPE draw,
        lw_api_control          TYPE cvapi_api_control,
        lw_size                 TYPE i,
        lt_bindata              TYPE STANDARD TABLE OF lty_raw_line,
        lw_bindata              TYPE lty_raw_line,
        lt_drao                 TYPE TABLE OF drao,
        lw_drao                 TYPE drao,
        lw_filename             TYPE string,
        lw_strippedname         TYPE draw-filep,
        lt_files2               TYPE cvapi_tbl_doc_files,
        lw_files2               TYPE cvapi_doc_file,
        lw_message              TYPE messages,
        lw_lifnr_stripped       TYPE lifnr,
        lw_offset1              TYPE i.

  CONSTANTS: lc_s               TYPE bapi_mtype            VALUE 'S',
             lc_content_provide TYPE mcdok-content_provide VALUE 'TBL',
             lc_dokst           TYPE dokst                 VALUE 'CC',
             lc_appcode         TYPE ydmsappl              VALUE 'ZVI',
             lc_tcode           TYPE tcode                 VALUE 'CV01N'.

  " Read DMS Application Configuration for appcode ZVI
  SELECT SINGLE applcode documenttype documentversion
                documentpart statusextern storagecategory
    FROM zdmsapp
    INTO lw_zdmsapp
    WHERE mandt    = sy-mandt
      AND applcode = lc_appcode.
  IF sy-subrc <> 0.
    CLEAR lw_zdmsapp.
  ENDIF.

  " Strip leading zeros from LIFNR for consistent Object ID construction
  lw_lifnr_stripped = im_lifnr.
  SHIFT lw_lifnr_stripped LEFT DELETING LEADING '0'.

  " Build DMS Object ID: CTD/<LIFNR_stripped>/<TRIP>/<COUNTER>
  CONCATENATE 'CTD' lw_lifnr_stripped im_trip im_counter
              INTO lw_obj_id SEPARATED BY '/'.
  TRANSLATE lw_obj_id TO UPPER CASE.

  " Set document data from ZDMSAPP configuration
  lw_doc-documenttype    = lw_zdmsapp-documenttype.
  lw_doc-documentversion = lw_zdmsapp-documentversion.
  lw_doc-documentpart    = lw_zdmsapp-documentpart.
  lw_doc-statusextern    = lw_zdmsapp-statusextern.
  lw_doc-laboratory      = '  '.

  " Set document description = Object ID (stored in DRAT-DKTXT_UC)
  lw_drat-language    = sy-langu.
  lw_drat-description = lw_obj_id.
  APPEND lw_drat TO lt_drat.
  CLEAR lw_drat.

  MOVE im_filename TO lw_filename.

  " Create DMS Document Header
  CALL FUNCTION 'BAPI_DOCUMENT_CREATE2'
    EXPORTING
      documentdata         = lw_doc
    IMPORTING
      documenttype         = lw_doctype
      documentnumber       = lw_docnumber
      documentpart         = lw_docpart
      documentversion      = lw_docversion
      return               = lw_return
    TABLES
      characteristicvalues = lt_characteristicvalues
      classallocations     = lt_classallocations
      documentdescriptions = lt_drat
      objectlinks          = lt_drad.

  IF lw_return-type CA 'EA'.
    APPEND lw_return TO et_return.
    CLEAR lw_return.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

  lw_draw-dokar = lw_zdmsapp-documenttype.
  lw_draw-dokvr = lw_zdmsapp-documentversion.
  lw_draw-doktl = lw_zdmsapp-documentpart.
  lw_draw-dwnam = sy-uname.
  lw_draw-dokst = lc_dokst.
  lw_api_control-tcode = lc_tcode.

  " Convert XString file content to binary table for DMS content server
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = im_xstring
    IMPORTING
      output_length = lw_size
    TABLES
      binary_tab    = lt_bindata.

  LOOP AT lt_bindata INTO lw_bindata.
    CLEAR lw_drao.
    lw_drao-orblk = lw_bindata-line.
    lw_drao-orln  = lw_size.
    lw_drao-dokar = lw_draw-dokar.
    lw_drao-doknr = lw_docnumber.
    lw_drao-dokvr = lw_draw-dokvr.
    lw_drao-doktl = lw_draw-doktl.
    lw_drao-appnr = '1'.
    APPEND lw_drao TO lt_drao.
  ENDLOOP.

  " Strip path prefix from filename - retain only filename with extension
  FIND ALL OCCURRENCES OF '\' IN lw_filename MATCH OFFSET lw_offset1.
  IF lw_offset1 <> 0.
    lw_offset1      = lw_offset1 + 1.
    lw_strippedname = lw_filename+lw_offset1.
  ELSE.
    lw_strippedname = lw_filename.
  ENDIF.

  " Derive SAP DMS application handler from file extension
  CALL FUNCTION 'CV120_DOC_GET_APPL'
    EXPORTING
      pf_file   = lw_strippedname
    IMPORTING
      pfx_dappl = lw_files2-dappl.

  lw_draw-filep = lw_strippedname.
  lw_draw-dappl = lw_files2-dappl.

  lw_files2-appnr       = '1'.
  lw_files2-filename    = lw_strippedname.
  lw_files2-updateflag  = 'I'.
  lw_files2-langu       = sy-langu.
  lw_files2-storage_cat = lw_zdmsapp-storagecategory.
  lw_files2-description = lw_strippedname.
  APPEND lw_files2 TO lt_files2.
  CLEAR lw_files2.

  " Check-in file binary content to DMS content server
  CALL FUNCTION 'CVAPI_DOC_CHECKIN'
    EXPORTING
      pf_dokar           = lw_draw-dokar
      pf_doknr           = lw_docnumber
      pf_dokvr           = lw_draw-dokvr
      pf_doktl           = lw_draw-doktl
      ps_api_control     = lw_api_control
      pf_content_provide = lc_content_provide
    IMPORTING
      psx_message        = lw_message
    TABLES
      pt_files_x         = lt_files2
      pt_content         = lt_drao.

  IF lw_message-msg_type NA 'EA' AND lw_docnumber IS NOT INITIAL.
    lw_return-type    = lc_s.
    lw_return-message = 'Document Uploaded Successfully'(001).
    APPEND lw_return TO et_return.
    CLEAR lw_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    lw_return-id         = lw_message-msg_id.
    lw_return-number     = lw_message-msg_no.
    lw_return-message    = lw_message-msg_txt.
    lw_return-type       = lw_message-msg_type.
    lw_return-message_v1 = lw_message-msg_v1.
    lw_return-message_v2 = lw_message-msg_v2.
    lw_return-message_v3 = lw_message-msg_v3.
    lw_return-message_v4 = lw_message-msg_v4.
    APPEND lw_return TO et_return.
    CLEAR lw_return.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  " END: Cursor Generated Code

ENDFUNCTION.
```

---

## FM 2 — Z_CTD_DASH_GET_ATTACH_COUNT

**Purpose:** Fetch attachment count for ALL Legs of a Trip in a single DB hit. Used on dashboard page load to display count badges against each Leg row.
**Called From:** All 4 Screens — on Page Load and after Upload/Delete actions on Transporter screens.

### Function Module Interface

```
IMPORTING:
  IM_LIFNR    TYPE LIFNR        (Vendor / Transporter Code)
  IM_TRIP_NO  TYPE ZTRIP_NO     (Trip Number)

EXPORTING:
  ET_ATTACH_COUNT  TYPE ZCTD_DASH_ATTACH_COUNT_TT   (Count per Leg)
  ET_RETURN        TYPE BAPIRET2_T                   (Return messages)
```

### ABAP Code

```abap
FUNCTION z_ctd_dash_get_attach_count.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_LIFNR) TYPE  LIFNR
*"     VALUE(IM_TRIP_NO) TYPE  ZTRIP_NO
*"  EXPORTING
*"     VALUE(ET_ATTACH_COUNT) TYPE  ZCTD_DASH_ATTACH_COUNT_TT
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module : Z_CTD_DASH_GET_ATTACH_COUNT
*&---------------------------------------------------------------------*
*& Purpose         : CTD Dashboard - Fetch attachment count for ALL
*&                   Legs of a Trip in a single DB query for page load
*&                   performance. Returns count per Counter (Leg).
*& Author          : Gaurav Prabhu
*& Creation Date   : 12.03.2026
*& Release         : 1.0
*& Change History  :
*& Date       User           Description
*& 12.03.2026 Gaurav Prabhu  Initial Creation
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  TYPES: BEGIN OF lty_drat_count,
           dktxt_uc TYPE drat-dktxt_uc,
           cnt      TYPE i,
         END OF lty_drat_count.

  DATA: lt_drat_count     TYPE TABLE OF lty_drat_count,
        lw_drat_count     TYPE lty_drat_count,
        lw_out            TYPE zctd_dash_attach_count_str,
        lv_pattern        TYPE string,
        lv_lifnr_stripped TYPE lifnr,
        lv_ctd            TYPE string,
        lv_lifnr_parsed   TYPE string,
        lv_trip_parsed    TYPE string,
        lv_counter        TYPE string.

  " Strip leading zeros - must match Object ID format used during upload
  lv_lifnr_stripped = im_lifnr.
  SHIFT lv_lifnr_stripped LEFT DELETING LEADING '0'.

  " Build LIKE pattern: CTD/<LIFNR>/<TRIP>/% — matches all legs of the trip
  CONCATENATE 'CTD/' lv_lifnr_stripped '/' im_trip_no '/%'
              INTO lv_pattern.

  " Single DB query with GROUP BY - one row per Leg that has attachments
  SELECT dktxt_uc COUNT( * ) AS cnt
    FROM drat
    INTO TABLE lt_drat_count
    WHERE langu    = sy-langu
      AND dktxt_uc LIKE lv_pattern
    GROUP BY dktxt_uc.

  IF sy-subrc <> 0.
    " No attachments found for this trip - return empty table (not an error)
    RETURN.
  ENDIF.

  " Parse Counter from each Object ID and populate output table
  LOOP AT lt_drat_count INTO lw_drat_count.
    CLEAR lw_out.
    " Split 'CTD/<LIFNR>/<TRIP>/<COUNTER>' to extract the Counter segment
    SPLIT lw_drat_count-dktxt_uc AT '/' INTO lv_ctd
                                              lv_lifnr_parsed
                                              lv_trip_parsed
                                              lv_counter.
    lw_out-counter      = lv_counter.
    lw_out-attach_count = lw_drat_count-cnt.
    IF lw_drat_count-cnt > 0.
      lw_out-has_attach = 'X'.
    ENDIF.
    APPEND lw_out TO et_attach_count.
    CLEAR lw_out.
  ENDLOOP.

  " END: Cursor Generated Code

ENDFUNCTION.
```

---

## FM 3 — Z_CTD_DASH_GET_DMS_LIST

**Purpose:** Retrieve the full list of attachments for a specific Leg. Validates the Leg exists in CTD item master, builds the DMS Object ID, and delegates retrieval to `Z_CTD_DASH_ATTACH_LIST`.
**Called From:** All 4 Screens — on Attachment Icon click.

### Function Module Interface

```
IMPORTING:
  IM_DMS_LIST  TYPE ZLOG_CTD_DMS_ST   (Structure: LIFNR, TRUCK_NO, TRIP_NO, COUNTER)

EXPORTING:
  ET_DMS_LIST  TYPE ZFIORI_CTD_ATTACH_DISPLAY_TT   (List of attachment details)
  ET_RETURN    TYPE BAPIRET2_T                      (Return messages)
```

### ABAP Code

```abap
FUNCTION z_ctd_dash_get_dms_list.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_DMS_LIST) TYPE  ZLOG_CTD_DMS_ST OPTIONAL
*"  EXPORTING
*"     VALUE(ET_DMS_LIST) TYPE  ZFIORI_CTD_ATTACH_DISPLAY_TT
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module : Z_CTD_DASH_GET_DMS_LIST
*&---------------------------------------------------------------------*
*& Purpose         : CTD Dashboard - Fetch full attachment list for a
*&                   specific Leg (Vendor + Trip + Counter). Validates
*&                   Leg in CTD item master before querying DMS.
*& Author          : Gaurav Prabhu
*& Creation Date   : 12.03.2026
*& Release         : 1.0
*& Change History  :
*& Date       User           Description
*& 12.03.2026 Gaurav Prabhu  Initial Creation
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  DATA: lw_ctd_itm_dtls  TYPE zsce_ctd_itm,
        lw_vendor         TYPE lifnr,
        lw_objid          TYPE objky,
        lt_doc            TYPE zfiori_ctd_attach_display_tt,
        lt_return         TYPE bapiret2_t.

  CHECK im_dms_list IS NOT INITIAL.

  " Validate the Leg exists in CTD Item Master
  SELECT SINGLE lifnr truck_no trip_no counter
    FROM zsce_ctd_itm
    INTO lw_ctd_itm_dtls
    WHERE mandt    = sy-mandt
      AND lifnr    = im_dms_list-lifnr
      AND truck_no = im_dms_list-truck_no
      AND trip_no  = im_dms_list-trip_no
      AND counter  = im_dms_list-counter.

  IF sy-subrc <> 0.
    " Leg not found - return empty list silently
    RETURN.
  ENDIF.

  " Strip leading zeros from LIFNR for Object ID construction
  lw_vendor = im_dms_list-lifnr.
  SHIFT lw_vendor LEFT DELETING LEADING '0'.

  " Build DMS Object ID: CTD/<LIFNR_stripped>/<TRIP>/<COUNTER>
  CONCATENATE 'CTD' lw_vendor
                    im_dms_list-trip_no
                    im_dms_list-counter
              INTO lw_objid SEPARATED BY '/'.
  TRANSLATE lw_objid TO UPPER CASE.

  " Fetch attachment list from DMS using the Object ID
  CALL FUNCTION 'Z_CTD_DASH_ATTACH_LIST'
    EXPORTING
      i_qmnum     = lw_objid
    IMPORTING
      et_document = lt_doc
      et_return   = lt_return.

  IF lt_doc IS NOT INITIAL.
    et_dms_list = lt_doc.
  ENDIF.
  IF lt_return IS NOT INITIAL.
    et_return = lt_return.
  ENDIF.

  " END: Cursor Generated Code

ENDFUNCTION.
```

---

## FM 4 — Z_CTD_DASH_ATTACH_LIST

**Purpose:** Core internal FM. Given a DMS Object ID, searches DRAT for all matching documents, retrieves file binary from content server, converts to XString, derives MIME type, and returns the complete attachment list. Called only by `Z_CTD_DASH_GET_DMS_LIST`.
**Called From:** `Z_CTD_DASH_GET_DMS_LIST` (internal — not called directly from any screen).

### Function Module Interface

```
IMPORTING:
  I_QMNUM   TYPE OBJKY   (DMS Object ID — document description to search by)
  I_OBJECT  TYPE DOKOB   (Document Object Type — optional)

EXPORTING:
  ET_DOCUMENT  TYPE ZFIORI_CTD_ATTACH_DISPLAY_TT   (Attachment details list)
  ET_RETURN    TYPE BAPIRET2_T                      (Return messages)
```

### ABAP Code

```abap
FUNCTION z_ctd_dash_attach_list.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_QMNUM) TYPE  OBJKY OPTIONAL
*"     VALUE(I_OBJECT) TYPE  DOKOB OPTIONAL
*"  EXPORTING
*"     VALUE(ET_DOCUMENT) TYPE  ZFIORI_CTD_ATTACH_DISPLAY_TT
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module : Z_CTD_DASH_ATTACH_LIST
*&---------------------------------------------------------------------*
*& Purpose         : CTD Dashboard - Core internal FM. Searches DRAT
*&                   for DMS documents matching the Object ID, retrieves
*&                   file content from content server, converts to
*&                   XString and returns full attachment details list.
*&                   Called internally by Z_CTD_DASH_GET_DMS_LIST only.
*& Author          : Gaurav Prabhu
*& Creation Date   : 12.03.2026
*& Release         : 1.0
*& Change History  :
*& Date       User           Description
*& 12.03.2026 Gaurav Prabhu  Initial Creation
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  TYPES: BEGIN OF lty_drad,
           dokar TYPE dokar,
           doknr TYPE doknr,
           dokvr TYPE dokvr,
           doktl TYPE doktl_d,
           dokob TYPE dokob,
           obzae TYPE obzae,
           objky TYPE objky,
         END OF lty_drad.

  DATA: lt_drat      TYPE TABLE OF drat,
        lw_drat      TYPE drat,
        lw_drad      TYPE lty_drad,
        lw_doc_data  TYPE bapi_doc_draw2,
        lt_docfile   TYPE TABLE OF bapi_doc_files2,
        lw_docfile   TYPE bapi_doc_files2,
        lw_return    TYPE bapiret2,
        lw_document  TYPE zfiori_ctd_attach_display_str,
        lt_acinf     TYPE TABLE OF scms_acinf,
        lw_acinf     TYPE scms_acinf,
        lt_bindata   TYPE TABLE OF sdokcntbin,
        lw_xstring   TYPE xstring,
        lw_doknr     TYPE doknr,
        lw_att_ext   TYPE char4.

  CHECK i_qmnum IS NOT INITIAL.

  " Search DRAT for all DMS documents matching the Object ID
  SELECT doknr dktxt_uc langu
    FROM drat
    INTO TABLE lt_drat
    WHERE langu    = sy-langu
      AND dktxt_uc = i_qmnum.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  LOOP AT lt_drat INTO lw_drat.
    CLEAR: lt_docfile, lw_doc_data, lw_return, lw_document.

    lw_drad-dokar = 'ZVI'.
    lw_drad-doknr = lw_drat-doknr.
    lw_drad-doktl = '000'.
    lw_drad-dokvr = '000'.

    " Fetch document file metadata
    CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
      EXPORTING
        documenttype    = lw_drad-dokar
        documentnumber  = lw_drad-doknr
        documentpart    = lw_drad-doktl
        documentversion = lw_drad-dokvr
        getdocfiles     = 'X'
      IMPORTING
        documentdata    = lw_doc_data
        return          = lw_return
      TABLES
        documentfiles   = lt_docfile.

    IF lw_return-type = 'E'.
      CONTINUE.
    ENDIF.

    READ TABLE lt_docfile INTO lw_docfile INDEX 1.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    CLEAR: lt_acinf, lt_bindata.
    lw_document-attachmetnum = lw_docfile-originaltype.

    " Read binary content from DMS content server
    CALL FUNCTION 'SCMS_DOC_READ'
      EXPORTING
        mandt                 = sy-mandt
        stor_cat              = lw_docfile-storagecategory
        doc_id                = lw_docfile-file_id
        signature             = 'X'
      TABLES
        access_info           = lt_acinf
        content_bin           = lt_bindata
      EXCEPTIONS
        bad_storage_type      = 1
        bad_request           = 2
        unauthorized          = 3
        comp_not_found        = 4
        not_found             = 5
        forbidden             = 6
        conflict              = 7
        internal_server_error = 8
        error_http            = 9
        error_signature       = 10
        error_config          = 11
        error_format          = 12
        error_parameter       = 13
        error                 = 14
        OTHERS                = 15.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    READ TABLE lt_acinf INTO lw_acinf INDEX 1.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    " Convert binary content to XString
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lw_acinf-comp_size
        first_line   = lw_acinf-first_line
        last_line    = lw_acinf-last_line
      IMPORTING
        buffer       = lw_xstring
      TABLES
        binary_tab   = lt_bindata
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    lw_document-filename = lw_acinf-comp_id.
    TRANSLATE lw_document-filename TO UPPER CASE.

    " Derive MIME type from file extension
    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
      EXPORTING
        filename  = lw_document-filename
        uppercase = 'X'
      IMPORTING
        extension = lw_att_ext.

    TRANSLATE lw_att_ext TO UPPER CASE.

    CASE lw_att_ext.
      WHEN 'PDF'.
        lw_document-mimetype = 'application/pdf'.
      WHEN 'XLS'.
        lw_document-mimetype = 'application/vnd.ms-excel'.
      WHEN 'XLSX'.
        lw_document-mimetype = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
      WHEN 'DOC'.
        lw_document-mimetype = 'application/msword'.
      WHEN 'DOCX'.
        lw_document-mimetype = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'.
      WHEN 'PPT'.
        lw_document-mimetype = 'application/vnd.ms-powerpoint'.
      WHEN 'PPTX'.
        lw_document-mimetype = 'application/vnd.openxmlformats-officedocument.presentationml.presentation'.
      WHEN 'JPG'.
        lw_document-mimetype = 'image/jpeg'.
      WHEN 'PNG'.
        lw_document-mimetype = 'image/png'.
      WHEN 'TXT'.
        lw_document-mimetype = 'text/plain'.
    ENDCASE.

    CLEAR: lw_acinf, lw_xstring.

    " Retrieve final validated XString via document number
    lw_doknr = lw_drat-doknr.
    CALL FUNCTION 'Z_SCE_CTD_GET_XSTRING_FRM_DOKN'
      EXPORTING
        i_docnumber = lw_doknr
      IMPORTING
        ex_xstring  = lw_xstring.

    lw_document-doknr  = lw_doknr.
    lw_document-string = lw_xstring.
    APPEND lw_document TO et_document.
    CLEAR lw_document.

  ENDLOOP.

  " Remove duplicates - same file uploaded multiple times
  SORT et_document BY filename.
  DELETE ADJACENT DUPLICATES FROM et_document COMPARING filename.

  " END: Cursor Generated Code

ENDFUNCTION.
```

---

## FM 5 — Z_CTD_DASH_DOWNLOAD_DOC

**Purpose:** Download a specific DMS document. Accepts a DMS document number and returns the file content as XString. The OData/UI layer streams this to the browser as a file download.
**Called From:** All 4 Screens — on Download button click against a specific document.

### Function Module Interface

```
IMPORTING:
  IM_DOKNR   TYPE DOKNR     (DMS Document Number to download)

EXPORTING:
  E_XSTRING  TYPE XSTRING   (File content as XString for download)
```

### ABAP Code

```abap
FUNCTION z_ctd_dash_download_doc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_DOKNR) TYPE  DOKNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_XSTRING) TYPE  XSTRING
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module : Z_CTD_DASH_DOWNLOAD_DOC
*&---------------------------------------------------------------------*
*& Purpose         : CTD Dashboard - Download a DMS document. Given a
*&                   DMS document number returns the file content as
*&                   XString for streaming to browser via OData layer.
*& Author          : Gaurav Prabhu
*& Creation Date   : 12.03.2026
*& Release         : 1.0
*& Change History  :
*& Date       User           Description
*& 12.03.2026 Gaurav Prabhu  Initial Creation
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  CHECK im_doknr IS NOT INITIAL.

  " Retrieve file content as XString from DMS content server
  CALL FUNCTION 'Z_SCE_CTD_GET_XSTRING_FRM_DOKN'
    EXPORTING
      i_docnumber = im_doknr
    IMPORTING
      ex_xstring  = e_xstring.

  " END: Cursor Generated Code

ENDFUNCTION.
```

---

## FM 6 — Z_CTD_DASH_DMS_DELETE

**Purpose:** Delete a specific document from SAP DMS. Reads the document header, calls the standard DMS Delete BAPI, commits on success or rolls back on failure. Available only to Transporters (Screen 1 and Screen 3).
**Called From:** Screen 1 (Transporter Trip Confirmation) and Screen 3 (Transporter CTD Confirmation) — on Delete icon click.

### Function Module Interface

```
IMPORTING:
  IM_DOKNR   TYPE DOKNR      (DMS Document Number to delete)

EXPORTING:
  ET_RETURN  TYPE BAPIRET2_T  (Return messages — Success / Error)
```

### ABAP Code

```abap
FUNCTION z_ctd_dash_dms_delete.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_DOKNR) TYPE  DOKNR
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module : Z_CTD_DASH_DMS_DELETE
*&---------------------------------------------------------------------*
*& Purpose         : CTD Dashboard - Delete a specific DMS document.
*&                   Available only to Transporters. Reads document
*&                   header from DRAW, calls BAPI_DOCUMENT_DELETE,
*&                   commits on success or rolls back on failure.
*& Author          : Gaurav Prabhu
*& Creation Date   : 12.03.2026
*& Release         : 1.0
*& Change History  :
*& Date       User           Description
*& 12.03.2026 Gaurav Prabhu  Initial Creation
*&---------------------------------------------------------------------*

  " BEGIN: Cursor Generated Code

  TYPES: BEGIN OF lty_draw,
           dokar TYPE draw-dokar,
           doknr TYPE draw-doknr,
           dokvr TYPE draw-dokvr,
           doktl TYPE draw-doktl,
         END OF lty_draw.

  DATA: lw_draw    TYPE lty_draw,
        lw_return  TYPE bapiret2,
        lv_doknr   TYPE char25.

  CONSTANTS: lc_s TYPE bapi_mtype VALUE 'S',
             lc_e TYPE bapi_mtype VALUE 'E'.

  " Validate input
  IF im_doknr IS INITIAL.
    lw_return-type    = lc_e.
    lw_return-message = 'Document number is required for deletion'(001).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

  " Apply ALPHA conversion to get internal zero-padded document number
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = im_doknr
    IMPORTING
      output = lv_doknr.

  " Read document header from DMS
  SELECT dokar doknr dokvr doktl
    FROM draw
    INTO lw_draw
    UP TO 1 ROWS
    WHERE mandt = sy-mandt
      AND doknr = lv_doknr.
  ENDSELECT.

  IF sy-subrc <> 0.
    lw_return-type    = lc_e.
    lw_return-message = 'Document not found in DMS'(002).
    APPEND lw_return TO et_return.
    RETURN.
  ENDIF.

  " Delete the DMS document using standard BAPI
  CALL FUNCTION 'BAPI_DOCUMENT_DELETE'
    EXPORTING
      documenttype    = lw_draw-dokar
      documentnumber  = lw_draw-doknr
      documentpart    = lw_draw-doktl
      documentversion = lw_draw-dokvr
    IMPORTING
      return          = lw_return.

  IF lw_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND lw_return TO et_return.
    CLEAR lw_return.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    lw_return-type    = lc_s.
    lw_return-message = 'Document Deleted Successfully'(003).
    APPEND lw_return TO et_return.
    CLEAR lw_return.
  ENDIF.

  " END: Cursor Generated Code

ENDFUNCTION.
```

---

## Development Checklist for the SAP Developer

### SE11 — Dictionary Objects (Do First)
- [ ] Create Structure `ZCTD_DASH_ATTACH_COUNT_STR` (fields: COUNTER, ATTACH_COUNT, HAS_ATTACH)
- [ ] Create Table Type `ZCTD_DASH_ATTACH_COUNT_TT` (line type: `ZCTD_DASH_ATTACH_COUNT_STR`)
- [ ] Activate both objects

### SE80 — Function Group
- [ ] Create Function Group `ZCTD_DASHBOARD_DMS`
- [ ] Assign to relevant package and Transport Request (TR)

### SE37 — Function Modules (create in this order)
- [ ] Create FM `Z_CTD_DASH_ATTACH_LIST` (FM 4 — no dependency on other new FMs)
- [ ] Create FM `Z_CTD_DASH_GET_DMS_LIST` (FM 3 — calls FM 4)
- [ ] Create FM `Z_CTD_DASH_DMS_CREATE` (FM 1 — standalone)
- [ ] Create FM `Z_CTD_DASH_GET_ATTACH_COUNT` (FM 2 — standalone)
- [ ] Create FM `Z_CTD_DASH_DOWNLOAD_DOC` (FM 5 — standalone)
- [ ] Create FM `Z_CTD_DASH_DMS_DELETE` (FM 6 — standalone)
- [ ] Assign all FMs to Function Group `ZCTD_DASHBOARD_DMS`
- [ ] Add all FMs to the same Transport Request

### Text Symbols (SE37 → Goto → Text Elements → Text Symbols)
For each FM that uses text symbols `(001)`, `(002)`, `(003)`, create the corresponding text:

| FM | Symbol | Text |
|---|---|---|
| `Z_CTD_DASH_DMS_CREATE` | 001 | `Document Uploaded Successfully` |
| `Z_CTD_DASH_DMS_DELETE` | 001 | `Document number is required for deletion` |
| `Z_CTD_DASH_DMS_DELETE` | 002 | `Document not found in DMS` |
| `Z_CTD_DASH_DMS_DELETE` | 003 | `Document Deleted Successfully` |

### Code Inspector / Syntax Check
- [ ] Run Syntax Check (Ctrl+F2) on all FMs — zero errors
- [ ] Run Code Inspector on all FMs — resolve all errors and warnings
- [ ] Activate all FMs

### Unit Testing (SE37 — F8 Test)
- [ ] Test `Z_CTD_DASH_DMS_CREATE` with valid LIFNR, TRIP, COUNTER and a sample XString
- [ ] Verify document created in `CV03N` with description `CTD/<LIFNR>/<TRIP>/<COUNTER>`
- [ ] Test `Z_CTD_DASH_GET_ATTACH_COUNT` — verify count returned per leg
- [ ] Test `Z_CTD_DASH_GET_DMS_LIST` — verify attachment list returned correctly
- [ ] Test `Z_CTD_DASH_DOWNLOAD_DOC` — verify XString returned for a known DOKNR
- [ ] Test `Z_CTD_DASH_DMS_DELETE` — verify document removed from `CV03N`
- [ ] Test `Z_CTD_DASH_DMS_DELETE` with invalid DOKNR — verify error message returned

---

## Key Design Decisions for Developer Reference

| Decision | Detail |
|---|---|
| **LIFNR Leading Zeros** | All FMs strip leading zeros from LIFNR before building Object ID — ensures Upload and Fetch always use the same key format |
| **DMS Object ID Format** | `CTD/<LIFNR_no_zeros>/<TRIP_NO>/<COUNTER>` — uppercase — stored as DRAT-DKTXT_UC |
| **DMS App Code** | `ZVI` — configured in custom table `ZDMSAPP` |
| **Document Type** | Derived from `ZDMSAPP` config, not hardcoded |
| **Content Provider** | `TBL` (table-based binary) used for CVAPI_DOC_CHECKIN |
| **Attachment Count Query** | Single `SELECT ... GROUP BY` on DRAT with LIKE pattern — avoids N+1 queries per leg |
| **Reused FM** | `Z_SCE_CTD_GET_XSTRING_FRM_DOKN` — existing utility, no changes needed |
| **Delete** | Uses `BAPI_DOCUMENT_DELETE` — standard SAP BAPI, not direct DRAW table update |
| **NW 7.31** | No inline declarations, no VALUE(), no NEW operators throughout |
