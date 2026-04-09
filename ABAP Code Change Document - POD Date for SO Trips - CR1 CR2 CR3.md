# ABAP Code Change Document
## POD Date Enablement for SO Trips (CR1, CR2, CR3)

---

| Attribute | Detail |
|---|---|
| Document Type | ABAP Code Change Document |
| System | SAP ECC / NetWeaver 7.31 |
| CD Number | To be assigned |
| Transport Request | To be assigned |
| Date | 08.04.2026 |
| Technical Reference | `ABAP Rules - 02-04-2026` (NW 7.31 standards) |

---

## 1. Purpose

This document captures technical ABAP changes required to implement POD-based destination date handling for SO movement trips across:
- Trip item fetch (display + UI block flag),
- Trip confirmation (DB persistence),
- Trip enrichment logic,
- Shipment load initialization logic.

---

## 2. Objects in Scope

### 2.1 New Object
- Function Module: `Z_SCM_CTD_GET_PODDATE`

### 2.2 Existing Objects to be Changed
- `Z_SCM_CTD_GETTRIPITM`
- `Z_SCM_CTD_TRIPCONFIRM`
- `Z_SCM_CTD_ENRICHTRIPDETAILS`
- `Z_SCE_TRIP_STLMNT`

### 2.3 Configuration Object
- Table: `ZWSO2APIDTL`

---

## 3. ABAP Standards Applied

The following standards must be applied in implementation:
- NW 7.31 compatible syntax only
- No inline declarations / no string templates
- Explicit `DATA` declarations and explicit `SELECT` field list
- `SY-SUBRC` checks after `SELECT/READ/UPDATE/CALL FUNCTION`
- `CONCATENATE` for message construction
- `TRY...CATCH` where applicable for JSON/HTTP handling
- No DB updates in read-only helper FM
- Existing LUW and commit structure to be preserved

---

## 4. CR1 Technical Changes

## CR1.1 - New FM `Z_SCM_CTD_GET_PODDATE`

### 4.1.1 Functional Interface
- Importing:
  - `IM_SHNUMBER` (Shipment Number)
- Exporting:
  - `EV_POD_DATE` (Type `ZDEST_DATE`)
  - `EV_SUBRC` (Type `SY-SUBRC`)

### 4.1.2 Technical Flow
1. Validate shipment input.
2. Read API URL and token API name from `ZWSO2APIDTL` (`platform = 'SCM'`, active entries).
3. Build GET URL by appending shipment number as `search_Text`.
4. Create HTTP client (`CL_HTTP_CLIENT=>CREATE_BY_URL`).
5. Set headers (`Content-Type`, `Authorization`, request method = `GET`).
6. Retrieve token via existing token FM (`Z_SCM_GET_TOKEN`) using configured API name.
7. Send and receive HTTP request.
8. Deserialize JSON response and extract `pod_date`.
9. Convert POD date from API format to SAP date (`YYYYMMDD`).
10. Return date/subrc.

### 4.1.3 Error Handling
- On communication/token/JSON errors, return `EV_SUBRC <> 0`.
- No exception raise to caller for business flow break.
- FM remains read-only (no DB update, no commit, no rollback).

---

## CR1.2 - Changes in `Z_SCM_CTD_GETTRIPITM`

### 4.2.1 Interface Change
- Add export parameter:
  - `EV_CONFIRM_BLOCKED TYPE FLAG`

### 4.2.2 Processing Change
After item select and sort:
1. Identify SO loaded legs (`MVT_TYPE = 'SO'`, `LEG_TYPE = 'L'`) with blank `DEST_DATE`.
2. Build unique shipment list.
3. Call `Z_SCM_CTD_GET_PODDATE` per unique shipment.
4. On successful POD fetch, update in-memory output table:
   - SO leg `DEST_DATE`, `DEST_EXIT_DATE`
   - Immediate next empty leg `SOURCE_DATE` (`counter + 1`)
5. If any SO loaded leg still has blank `DEST_DATE`, set:
   - `EV_CONFIRM_BLOCKED = 'X'`
   - Append warning in `ET_RETURN` with message:
     - `Please complete Proof of Delivery before confirming this trip`

### 4.2.3 Important Constraint
- No DB update in this FM.
- Output enrichment is in-memory only.

---

## CR1.3 - Changes in `Z_SCM_CTD_TRIPCONFIRM`

### 4.3.1 Placement
- Add new internal processing block after existing Step 1 loop and before Step 3 commit path (as per latest FM structure, before final commit sequence).

### 4.3.2 Processing Logic
For TTC context:
1. Select SO loaded legs in current trip where `DEST_DATE = '00000000'`.
2. Build unique shipment list.
3. Call `Z_SCM_CTD_GET_PODDATE` per shipment.
4. For successful POD date:
   - Update SO loaded leg:
     - `DEST_DATE = POD_DATE`
     - `DEST_EXIT_DATE = POD_DATE`
   - Update immediate next empty leg:
     - `SOURCE_DATE = POD_DATE`
5. For POD fetch failure or update failure:
   - Append warning message to `ET_RETURN`
   - Continue processing (non-blocking for current agreed approach)

### 4.3.3 Message Texts
- Keep existing success/error framework.
- Add new text symbols for POD warning/update failure as required in this FM.

### 4.3.4 LUW
- Do not add separate commit.
- Updates remain part of existing FM commit (`COMMIT WORK`) to keep LUW consistency.

---

## 5. CR2 Technical Changes

## Object: `Z_SCM_CTD_ENRICHTRIPDETAILS`

### 5.1 Destination Date Derivation for SO
- In block where `DEST_DATE IS INITIAL`:
  - For `MVT_TYPE = 'SO'`, call `Z_SCM_CTD_GET_PODDATE`.
  - If POD available, set `DEST_DATE`.
  - If POD unavailable, allow `DEST_DATE` to remain initial (no forced SY-DATUM fallback).

### 5.2 Exclusion Logic Change
- Existing trip exclusion on missing source/destination date must be adjusted:
  - Remove strict exclusion due to blank `DEST_DATE` for SO cases.
  - Retain required source date validations.

### 5.3 Downstream Dependency Note
- Existing cascade logic (`DEST_DATE -> DEST_EXIT_DATE -> next empty leg SOURCE_DATE`) remains valid.
- SO pending POD state is now a valid intermediate state.

---

## 6. CR3 Technical Changes

## Object: `Z_SCE_TRIP_STLMNT`

### 6.1 Change
- Comment/remove hardcoded SO assignment:
  - `lw_trip_itm-dest_date = sy-datum`

### 6.2 Reason
- This assignment prevents CR2 logic from invoking POD fetch (because destination date is no longer initial).

### 6.3 Impact
- For SO, destination date remains initial until enrichment/POD logic sets it.
- PO/STO logic remains unchanged.

---

## 7. Configuration Changes (`ZWSO2APIDTL`)

Two active entries required per environment:
- POD data-fetch API (GET URL base)
- POD token API (for `Z_SCM_GET_TOKEN`)

Fields to maintain:
- `MANDT`, `PLATFORM = 'SCM'`, `APINAME`, `URL`, `ACTIVE = 'X'`, credential/token related fields as per existing pattern.

DEV URL known; QA/PROD URLs pending.

---

## 8. Test Cases (Technical)

1. SO with POD available:
   - GETTRIPITM enriches in-memory dates
   - TripConfirm persists system dates
2. SO with POD unavailable:
   - GETTRIPITM sets `EV_CONFIRM_BLOCKED = X`
   - UI blocks confirm with message
3. TripEnrich run with SO pending POD:
   - Trip not excluded solely due to SO destination date blank
4. Shipment load run:
   - SO destination date not auto-defaulted to `SY-DATUM`
5. Regression:
   - PO/STO behavior unchanged
   - RTCA/RCCA checks continue to run correctly

---

## 9. Deployment / Sequencing

Recommended sequence:
1. Deploy helper FM `Z_SCM_CTD_GET_PODDATE`
2. Deploy `GETTRIPITM` and `TRIPCONFIRM` changes (CR1)
3. Deploy `ENRICHTRIPDETAILS` changes (CR2)
4. Deploy `Z_SCE_TRIP_STLMNT` change (CR3)
5. Maintain `ZWSO2APIDTL` entries in target system
6. Execute integration test and UI validation

---

## 10. Clarifications Pending

- CD number and TR number
- QA/PROD POD endpoint hostnames
- Final API name constants for data-fetch and token entries in `ZWSO2APIDTL`

---

## 11. Change Summary

- CR1 adds POD API integration and date persistence for SO loaded legs and dependent empty leg source date.
- CR2 allows SO destination date to stay blank when POD is pending and removes exclusion side effect.
- CR3 removes conflicting legacy hardcoded date assignment in shipment load logic.
- UI linkage is driven by `EV_CONFIRM_BLOCKED` from `Z_SCM_CTD_GETTRIPITM` with message:
  - `Please complete Proof of Delivery before confirming this trip`

