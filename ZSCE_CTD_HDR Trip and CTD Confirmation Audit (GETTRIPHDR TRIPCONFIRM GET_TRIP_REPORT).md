# Code Change Document  
## RIL Trip & CTD confirmation audit fields (RTC / RTCA, RCC / RCCA) — `ZSCE_CTD_HDR`, trip FMs, `Z_SCM_CTD_GETTRIPHDR`, CTD Report FM

---

| Attribute | Detail |
|-----------|--------|
| **Document** | Code Change Document — RTC/RTCA and RCC/RCCA header audit (separate from generic `ZUSERDTLS` / `MODIFIED_*`) |
| **CD Number** | *(assign)* |
| **Transport** | *(assign)* |
| **Target** | SAP ECC 6.0 / NetWeaver 7.31 (`abap_731`) per `ABAP Rules - 02-04-2026` |
| **Reference code** | `27.04.2026/FM - Z_SCM_CTD_GETTRIPHDR.txt`, `27.04.2026/FM - Z_SCM_CTD_TRIPCONFIRM.txt`, `27.04.2026/FM - Z_SCM_CTD_AUTO_TRIPCONFIRM.txt`; CTD Report: `CTD Process Report/Code Change Document - ZCTDENRI CTD Report Enhancement.md` (FM **`Z_SCM_CTD_GET_TRIP_REPORT`**) |
| **Reference DDIC** | `27.04.2026/Table ZSCE_CTD_HDR.png` (SE11) |
| **ABAP standards** | Folder `ABAP Rules - 02-04-2026` (this workspace) |
| **Functional author** | Gaurav Prabhu |
| **Technical author** | *(assign)* |
| **Date** | 27.04.2026 |

---

## Summary (scope)

| Layer | Object | Change |
|--------|--------|--------|
| **Table** | **`ZSCE_CTD_HDR`** | Append **8 fields** (two milestones, same shape each): **`RTC_MOD_*`** — RIL Trip Confirmation (manual **RTC** + auto **RTCA**); **`RCC_MOD_*`** — RIL CTD Confirmation (manual **RCC** + auto **RCCA**). All separate from **`ZUSERDTLS`** / **`MODIFIED_*`**. |
| **Structure** | **`ZSCM_TRIPHDR_ST`** / **`TT`** | Add all **8** components for **`Z_SCM_CTD_GETTRIPHDR`** → **`ET_TRIP_HDR`**. |
| **FM — persist** | **`Z_SCM_CTD_TRIPCONFIRM`** | **`RTC_MOD_*`:** set **only** for **`IM_CALL_CONTEXT = 'RTC'`**. **`RCC_MOD_*`:** set **only** for **`IM_CALL_CONTEXT = 'RCC'`**. No audit updates to these columns for **TTC** / **TCC**; do not cross-populate (e.g. never write **`RCC_MOD_*`** on **RTC**). |
| **FM — persist** | **`Z_SCM_CTD_AUTO_TRIPCONFIRM`** | **`RTCA`:** set **`RTC_MOD_*`** ( **`lv_auto_user`** + **`V_USERNAME`** + date/time). **`RCCA`:** set **`RCC_MOD_*`** with the same **`lv_auto_user`** / name / date/time pattern — **do not** change **`RTC_MOD_*`** on RCCA. |
| **FM — read (Screen 4)** | **`Z_SCM_CTD_GETTRIPHDR`** | Extend **`SELECT`** / internal type / **`ZSCM_TRIPHDR_ST`** so **all 8** fields are returned. **Screen 4 (RIL CTD Confirmation)** uses **`IM_CALL_CONTEXT = 'RCC'`** — show **trip** audit (**`RTC_MOD_*`**) and **CTD** audit (**`RCC_MOD_*`**) as per UX. |
| **FM — read (CTD Report)** | **`Z_SCM_CTD_GET_TRIP_REPORT`** | Extend header **`SELECT`** / move into line type **`ZSCM_CTD_REP_ALV_ST`** so **all 8** fields are available on **`ET_TRIP_DETAILS`** (or agreed export table) for ALV / portal (see **`CTD Process Report/Code Change Document - CTD Trip Report New Fields (HDR ITM).md`** for pattern). |

**Bottom line:** **Trip Confirm** writes **RTC** or **RTCA** → **`RTC_MOD_*`**; **CTD Confirm** writes **RCC** or **RCCA** → **`RCC_MOD_*`**. **Screen 4** reads both via **GETTRIPHDR**; **CTD Report** reads both via **`Z_SCM_CTD_GET_TRIP_REPORT`**.

---

## Table of contents

1. [Business requirement](#1-business-requirement)  
2. [Compliance with ABAP Rules - 02-04-2026](#2-compliance-with-abap-rules---02-04-2026)  
3. [Dictionary — `ZSCE_CTD_HDR`](#3-dictionary--zsce_ctd_hdr)  
4. [Dictionary — `ZSCM_TRIPHDR_ST` / `ZSCM_TRIPHDR_TT`](#4-dictionary--zscm_triphdr_st--zscm_triphdr_tt)  
5. [Change request — `Z_SCM_CTD_GETTRIPHDR`](#5-change-request--z_scm_ctd_gettriphdr)  
6. [Change request — `Z_SCM_CTD_TRIPCONFIRM`](#6-change-request--z_scm_ctd_tripconfirm)  
7. [Change request — `Z_SCM_CTD_AUTO_TRIPCONFIRM` (RTCA and RCCA)](#7-change-request--z_scm_ctd_auto_tripconfirm-rtca-and-rcca)  
8. [Regression scan — other `UPDATE zsce_ctd_hdr`](#8-regression-scan--other-update-zsce_ctd_hdr)  
9. [UI — Screen 4 (RIL CTD Confirmation)](#9-ui--screen-4-ril-ctd-confirmation)  
10. [CTD Report — `Z_SCM_CTD_GET_TRIP_REPORT`](#10-ctd-report--z_scm_ctd_get_trip_report)  
11. [Test cases](#11-test-cases)  
12. [Transport and quality gates](#12-transport-and-quality-gates)  
13. [Implementation checklist](#13-implementation-checklist)  

---

## 1. Business requirement

### 1.1 Problem

`ZSCE_CTD_HDR` includes **`ZUSERDTLS`** (`MODIFIED_BY`, `MODIFIED_DATE`, `MODIFIED_TIME`). Later steps (rule engine, other confirmations, enrichments) overwrite those fields, so they **no longer record** who performed **RIL Trip Confirmation (RTC / RTCA)** or **RIL CTD Confirmation (RCC / RCCA)** or when.

### 1.2 Objective — RIL Trip Confirmation (`RTC_MOD_*`)

Add **four dedicated columns** written **only** when RIL Trip Confirmation is persisted (manual **RTC** or auto **RTCA**):

| # | Meaning | Manual RTC (Screen 2) | Auto RTCA (background) |
|---|---------|------------------------|-------------------------|
| 1 | User ID | `sy-uname` (dialog user on RIL Trip Confirmation screen) | **`lv_auto_user`** from `Z_SCM_CTD_AUTO_TRIPCONFIRM`: read **`ZLOG_EXEC_VAR`** where `name = 'ZCTD_RILOPS_AUTO_USER'` and `active = abap_true`, field **`REMARKS`** (`UP TO 1 ROWS` / `ENDSELECT`); if not found or initial, **fallback `sy-uname`** (same logic as existing Step 0 in that FM, lines ~97–109 of `27.04.2026/FM - Z_SCM_CTD_AUTO_TRIPCONFIRM.txt`). Persist this value in **`RTC_MOD_BY`**. |
| 2 | Display name | `SELECT SINGLE name_text FROM v_username` where `bname =` dialog user (may be initial) | **Same lookup** using **`bname = lv_auto_user`** so the auto technical user’s `NAME_TEXT` is stored when present. |
| 3 | Date | `sy-datum` when manual RTC header `UPDATE` succeeds | **`sy-datum`** in **`Z_SCM_CTD_AUTO_TRIPCONFIRM`** when Step 2 header `UPDATE` succeeds (`im_call_context = 'RTCA'`). |
| 4 | Time | `sy-uzeit` when manual RTC header `UPDATE` succeeds | **`sy-uzeit`** in the same FM / same Step 2 for RTCA. |

**Important:** Manual **RTC** and **RTCA** both populate the **same** four columns (`RTC_MOD_*`).

### 1.3 Objective — RIL CTD Confirmation (`RCC_MOD_*`)

Add **four further columns** (same technical pattern as `RTC_MOD_*`) written **only** when RIL CTD Confirmation is persisted:

| Field group | Manual (`Z_SCM_CTD_TRIPCONFIRM`) | Auto (`Z_SCM_CTD_AUTO_TRIPCONFIRM`) |
|-------------|-----------------------------------|--------------------------------------|
| User ID | `sy-uname` when **`IM_CALL_CONTEXT = 'RCC'`** | **`lv_auto_user`** when **`IM_CALL_CONTEXT = 'RCCA'`** (same `ZLOG_EXEC_VAR` / `ZCTD_RILOPS_AUTO_USER` read as RTCA) |
| Name | `V_USERNAME` by `BNAME` | `V_USERNAME` by `bname = lv_auto_user` |
| Date / time | `sy-datum` / `sy-uzeit` on successful header update | Same in auto FM Step 2 for RCCA |

Manual **RCC** and **RCCA** both populate **`RCC_MOD_*`** only. **`RTC_MOD_*`** must remain unchanged on **RCC** / **RCCA** paths.

### 1.4 Scope boundary

- **`RTC_MOD_*`:** update **only** on **RTC** (manual) and **RTCA** (auto). **Never** on TTC, TCC, RCC, RCCA, or unrelated jobs.  
- **`RCC_MOD_*`:** update **only** on **RCC** (manual) and **RCCA** (auto). **Never** on TTC, TCC, RTC, RTCA, or unrelated jobs.  
- Generic **`MODIFIED_*`** may continue to be updated as today on all paths.

### 1.5 Cross-milestone rules

- **`RTCA`** writes **`RTC_MOD_*`** only; **`RCCA`** writes **`RCC_MOD_*`** only (same auto user source, different columns).  
- Downstream **Screen 4** and **CTD Report** consume **both** field groups from the header row where populated.

---

## 2. Compliance with ABAP Rules - 02-04-2026

Standards are taken from **`ABAP Rules - 02-04-2026`** in this repository. Applicable rule files and how this change must follow them:

| Rule file | Topic | Application to this CR |
|-----------|--------|-------------------------|
| `00-main.mdc` | Target NW 7.31, quality, security/performance awareness | All new ABAP targets **strict 7.31** syntax where new lines are added. |
| `01-compatibility.mdc` | No inline `DATA()`, no `\|...\|`, no `@` host variables in Open SQL, no `VALUE`/`NEW`/`CORRESPONDING` in new code | **New** RTC resolution and any **new** variables use explicit `DATA`, classic `CONCATENATE`, classic Open SQL (`INTO` / `INTO TABLE` without `@`). |
| `02-naming.mdc` | `lv_`, `lt_`, `lw_`, `lty_`, parameters `iv_`/`et_` | New locals for RTC: e.g. `lv_rtc_user`, `lv_rtc_name`, `lv_rtc_date`, `lv_rtc_time`; types `lty_...` if extra work areas are needed. |
| `03-database.mdc` | No `SELECT *`; internal type **matches SELECT field list** and order; no `SELECT` inside `LOOP`; check `sy-subrc` | Extend existing `SELECT` field lists explicitly; extend `lty_trip_hdr` to match; resolve `V_USERNAME` **once per LUW path** (not inside leg loop — see §6.3). |
| `11-interfaces-api.mdc` | Stable FM interfaces | Prefer extending **`ZSCM_TRIPHDR_ST`** over changing FM signatures. |
| `12-documentation.mdc` | Headers / meaningful comments for non-obvious logic | Short comment blocks at RTC/RCC audit `UPDATE` and name lookups: *Milestone fields — not updated by unrelated contexts.* |
| `13-sy-subrc.mdc` | Check `sy-subrc` after `SELECT`, `READ TABLE`, `CALL FUNCTION` | After `SELECT SINGLE` from `v_username`; after header `UPDATE` (already partially done). |
| `14-transport.mdc` | Related objects in one transport, description, no `$TMP` | DDIC + trip FMs + auto FM + **`ZSCM_CTD_REP_ALV_ST`** + **`Z_SCM_CTD_GET_TRIP_REPORT`** (+ report program if ALV extended) in one logical TR; peer review. |
| `20-code-generation-checklist.mdc` | SORT + BINARY SEARCH, no N+1, FOR ALL ENTRIES guards, etc. | If bulk name lookup is ever needed, use `FOR ALL ENTRIES` with `IS NOT INITIAL` guard on source table; for this CR, single user per trip suffices. |

### 2.1 Note on existing code

`Z_SCM_CTD_TRIPCONFIRM` (27.04.2026 extract) already contains syntax patterns that exceed strict 7.31 rules (e.g. string templates, newer JSON calls). **This CR must not trigger a full FM rewrite.**  

**Rule for developers:** all **newly added** statements for **RTC and RCC** audit must comply with **`01-compatibility.mdc`** and **`03-database.mdc`**. Pre-existing lines are out of scope unless a separate modernization CR is opened.

### 2.2 `CLIENT SPECIFIED`

Existing accesses use `CLIENT SPECIFIED` on custom tables. **Keep the same pattern** on all new `SELECT` / `UPDATE` for `zsce_ctd_hdr` and `v_username` (if used with client key per your system definition).

---

## 3. Dictionary — `ZSCE_CTD_HDR`

Per SE11 snapshot, append **after** existing fields (e.g. after `RCCA_FLAG`), **outside** include `ZUSERDTLS`.

| Field | Data element (proposed) | Type | Len | Short description |
|-------|-------------------------|------|-----|-------------------|
| `RTC_MOD_BY` | `SYUNAME` | CHAR | 12 | RTC: user ID (BNAME) |
| `RTC_MOD_NAME` | `ZRTC_MOD_NAME` *(create, CHAR 80)* or equivalent aligned to `V_USERNAME-NAME_TEXT` | CHAR | 80 | RTC: user display name |
| `RTC_MOD_DATE` | `SYDATUM` | DATS | 8 | RTC: modification date |
| `RTC_MOD_TIME` | `SYSTTIMLO` / `TIMS` | TIMS | 6 | RTC: modification time |
| `RCC_MOD_BY` | `SYUNAME` | CHAR | 12 | RCC: user ID (BNAME) — CTD confirmation |
| `RCC_MOD_NAME` | `ZRCC_MOD_NAME` *(create, CHAR 80)* or reuse same DE as `RTC_MOD_NAME` if suitable | CHAR | 80 | RCC: user display name |
| `RCC_MOD_DATE` | `SYDATUM` | DATS | 8 | RCC: modification date |
| `RCC_MOD_TIME` | `SYSTTIMLO` / `TIMS` | TIMS | 6 | RCC: modification time |

Activate table; fix short dumps from strict `SELECT *` into structures if any.

---

## 4. Dictionary — `ZSCM_TRIPHDR_ST` / `ZSCM_TRIPHDR_TT`

`Z_SCM_CTD_GETTRIPHDR` exports `ET_TRIP_HDR` type `ZSCM_TRIPHDR_TT` and uses `MOVE-CORRESPONDING` from an internal type into `ZSCM_TRIPHDR_ST`.

Add all **eight** components **`RTC_MOD_*`** and **`RCC_MOD_*`** with **identical** technical names and types as on `ZSCE_CTD_HDR` so `MOVE-CORRESPONDING` fills `ET_TRIP_HDR` without manual mapping.

---

## 5. Change request — `Z_SCM_CTD_GETTRIPHDR`

**Source:** `27.04.2026/FM - Z_SCM_CTD_GETTRIPHDR.txt`

**UI driver:** **Screen 4 — RIL CTD Confirmation** uses this FM with **`IM_CALL_CONTEXT = 'RCC'`** (trip status **`09`** in current FM). Return **all eight** audit fields: **`RTC_MOD_*`** (prior RIL Trip Confirmation — **RTC** / **RTCA**) and **`RCC_MOD_*`** (RIL CTD Confirmation — **RCC** / **RCCA** once completed).

| Step | Action |
|------|--------|
| 5.1 | Extend local type `lty_trip_hdr` with the **eight** fields (same order as in `SELECT`). |
| 5.2 | Extend the `SELECT` from `zsce_ctd_hdr` (lines ~118–126) to include the **eight** fields for **all** contexts that read the header. **Do not** use `SELECT *` (`03-database.mdc`). |
| 5.3 | Output loop: after `ZSCM_TRIPHDR_ST` extension, existing `MOVE-CORRESPONDING lw_trip_hdr TO lw_trip_hdr_op` maps the new fields. |

**Optional (not default):** live refresh of name from `V_USERNAME` at read time — would add DB reads per trip list row; only if BA requires. Default is **persisted** values from the table only.

---

## 6. Change request — `Z_SCM_CTD_TRIPCONFIRM`

**Source:** `27.04.2026/FM - Z_SCM_CTD_TRIPCONFIRM.txt`

### 6.1 Current behaviour

Inside `LOOP AT it_leg_update`, **STEP 2** (~515–523) executes `UPDATE zsce_ctd_hdr` for **every** `im_call_context`, setting `trip_status` and generic `modified_*`.

### 6.2 When to set `RTC_MOD_*` vs `RCC_MOD_*`

| `im_call_context` | `RTC_MOD_*` | `RCC_MOD_*` |
|--------------------|-------------|-------------|
| `RTC` | **Yes** — on successful header update (Confirm **`07`** / Reject **`06`** per BA). | **No** — omit from `UPDATE`. |
| `RCC` | **No** — omit from `UPDATE`. | **Yes** — on successful header update (Confirm **`11`** / Rework **`10`** / Reject paths per existing FM). |
| `TTC`, `TCC` | **No** | **No** |

### 6.3 Name lookup placement (performance)

**Do not** call `SELECT SINGLE` from `v_username` inside `LOOP AT it_leg_update` for the same trip/user — avoid N identical lookups (`20-code-generation-checklist.mdc` / `03-database.mdc`).

**Recommended pattern:**

- When `im_call_context = 'RTC'` (after STEP 0): set `lv_rtc_user = sy-uname`; **once** resolve `lv_rtc_name` from `v_username` by `bname = lv_rtc_user`.
- When `im_call_context = 'RCC'` (after STEP 0): set `lv_rcc_user = sy-uname`; **once** resolve `lv_rcc_name` from `v_username` by `bname = lv_rcc_user`.
- In **STEP 2**, extend the `UPDATE` so only the relevant quartet is populated for the active context (see §6.4).

Use **classic Open SQL** and **predeclared** variables per `01-compatibility.mdc`.

### 6.4 Update statement design

- **`RTC_MOD_*`:** populate **only** when `im_call_context = 'RTC'`. **Never** clear or overwrite on `RCC`, `TTC`, `TCC`, etc.  
- **`RCC_MOD_*`:** populate **only** when `im_call_context = 'RCC'`. **Never** clear or overwrite on `RTC`, `TTC`, `TCC`, etc.

Implement via **separate `UPDATE` variants** or a **single `UPDATE` with mutually exclusive `SET` fragments** per context — whichever matches existing FM style with least risk.

---

## 7. Change request — `Z_SCM_CTD_AUTO_TRIPCONFIRM` (RTCA and RCCA)

**Source:** `27.04.2026/FM - Z_SCM_CTD_AUTO_TRIPCONFIRM.txt`

**Caller:** `Z_SCM_CTD_TRIPCONFIRM` Step 4 (post-commit) calls this FM with `im_call_context = 'RTCA'` or `'RCCA'`. This FM runs its **own** `COMMIT WORK` / `ROLLBACK WORK` (second LUW in the chain).

### 7.1 Auto user ID (shared — already implemented)

| Step | Location in reference FM | Behaviour |
|------|---------------------------|-----------|
| Read auto user | ~lines 97–109 | `SELECT remarks FROM zlog_exec_var … WHERE name = 'ZCTD_RILOPS_AUTO_USER' AND active = abap_true` into `lv_auto_user_raw`; move to `lv_auto_user`; **else** `lv_auto_user = sy-uname`. |
| Header update | ~lines 272–278 | `UPDATE zsce_ctd_hdr` sets `trip_status`, `rtca_flag` / `rcca_flag`, **`modified_by = lv_auto_user`**, `modified_date`, `modified_time`. |

### 7.2 RTCA — extend header `UPDATE` with `RTC_MOD_*`

**New:** After `lv_auto_user` is final, **once** resolve display name:

- `SELECT SINGLE name_text FROM v_username INTO lv_rtc_auto_name WHERE mandt = sy-mandt AND bname = lv_auto_user.` (keys per SE11.)

In the header `UPDATE` executed for **`im_call_context = 'RTCA'`**, add:

- `rtc_mod_by`, `rtc_mod_name`, `rtc_mod_date`, `rtc_mod_time` (same values as in §7.1 auto user + `sy-datum` / `sy-uzeit`).

**Do not** set **`RCC_MOD_*`** on RTCA.

### 7.3 RCCA — extend header `UPDATE` with `RCC_MOD_*`

For **`im_call_context = 'RCCA'`**, reuse the **same** `lv_auto_user` and the **same** `v_username` result (or a second `SELECT SINGLE` if code structure is simpler — still **once per FM call**, not per leg).

In the header `UPDATE` for RCCA, add:

- `rcc_mod_by`, `rcc_mod_name`, `rcc_mod_date`, `rcc_mod_time`

**Do not** set or clear **`RTC_MOD_*`** on RCCA (trip-confirmation audit remains from prior RTC / RTCA).

### 7.4 Consistency with `modified_*`

Header `modified_by` continues to equal **`lv_auto_user`** for both RTCA and RCCA as today. **`RTC_MOD_*`** / **`RCC_MOD_*`** add **milestone-specific** audit aligned with those events.

### 7.5 Transport

Include **`Z_SCM_CTD_AUTO_TRIPCONFIRM`** in the **same transport** as `ZSCE_CTD_HDR`, `Z_SCM_CTD_TRIPCONFIRM`, `Z_SCM_CTD_GETTRIPHDR`, **`ZSCM_CTD_REP_ALV_ST`**, and **`Z_SCM_CTD_GET_TRIP_REPORT`**.

---

## 8. Regression scan — other `UPDATE zsce_ctd_hdr`

Programs such as **`ZCTD_RULEENG_EXEC`** (and any other updater) must **not** blank or overwrite **`rtc_mod_*`** or **`rcc_mod_*`** unless explicitly designed to do so.

---

## 9. UI — Screen 4 (RIL CTD Confirmation, `RCC`)

- Call **`Z_SCM_CTD_GETTRIPHDR`** with **`IM_CALL_CONTEXT = 'RCC'`** (per existing FM design for this screen).  
- Bind **all eight** `ET_TRIP_HDR` audit fields.  
- **Trip confirmation block:** **`RTC_MOD_NAME`** if not initial; **else** **`RTC_MOD_BY`**; show date/time.  
- **CTD confirmation block:** **`RCC_MOD_NAME`** if not initial; **else** **`RCC_MOD_BY`**; show date/time (may be initial until RCC/RCCA has run).

---

## 10. CTD Report — `Z_SCM_CTD_GET_TRIP_REPORT`

**References:** `CTD Process Report/Code Change Document - ZCTDENRI CTD Report Enhancement.md`, `CTD Process Report/Code Change Document - CTD Trip Report New Fields (HDR ITM).md`

| Step | Action |
|------|--------|
| 10.1 | Append **`RTC_MOD_*`** and **`RCC_MOD_*`** to **`ZSCM_CTD_REP_ALV_ST`** (and document new ALV column order / labels with Functional). |
| 10.2 | In **`Z_SCM_CTD_GET_TRIP_REPORT`**, extend the read from **`ZSCE_CTD_HDR`** (or join path already used) to **select** the eight fields and **move** them into each report line (item-level report: repeat header values on each line for that trip — same pattern as `CREATED_DATE` / flags in existing HDR-on-report design). |
| 10.3 | Ensure **portal** and **SAP** caller types both receive extended `ET_TRIP_DETAILS` (or agreed export parameter) without breaking consumers; coordinate column append order with UI/ALV. |

Read-only: **no** `UPDATE` to CTD tables from this FM.

---

## 11. Test cases

| # | Scenario | Expected |
|---|----------|----------|
| 1 | Manual RTC Confirm | `trip_status = 07`; `rtc_mod_*` set; `modified_*` also updated as today. |
| 2 | Manual RTC Reject | If in scope: `06` + `rtc_mod_*` set. |
| 3 | RTCA (auto trip confirm) | After TTC + auto path: `rtc_mod_by` = **`lv_auto_user`** from `ZLOG_EXEC_VAR` / fallback; `rtc_mod_date` / `rtc_mod_time` match auto FM Step 2; `rtca_flag = 'X'` as today; **`modified_by`** on header still `lv_auto_user`. |
| 3b | RTCA with `ZCTD_RILOPS_AUTO_USER` blank | `lv_auto_user = sy-uname` per FM fallback; `rtc_mod_by` stores that runtime user. |
| 3c | RCCA only | **`rcc_mod_*`** set with **`lv_auto_user`**; **`rtc_mod_*`** unchanged; `rcca_flag` / `modified_*` as today. |
| 4 | Manual RCC | **`rcc_mod_*`** set with dialog user; **`rtc_mod_*`** unchanged. |
| 5 | TTC only | **`rtc_mod_*`** and **`rcc_mod_*`** unchanged from prior values. |
| 6 | GETTRIPHDR (`RCC`) | Row includes **eight** fields for Screen 4. |
| 7 | `Z_SCM_CTD_GET_TRIP_REPORT` | Report line includes **eight** fields; values match header for trip. |
| 8 | Empty `NAME_TEXT` | Name fields initial; UI/report shows respective user ID. |
| 9 | Code Inspector | No new critical findings attributable to this change. |

---

## 12. Transport and quality gates

Per **`14-transport.mdc`** and **`20-code-generation-checklist.mdc`**:

1. Data elements (if new) + append **`ZSCE_CTD_HDR`** (**eight** fields).  
2. Extend **`ZSCM_TRIPHDR_ST`** / **`TT`**.  
3. Extend **`ZSCM_CTD_REP_ALV_ST`** / **`TT`** (CTD Report line type).  
4. `Z_SCM_CTD_GETTRIPHDR`.  
5. `Z_SCM_CTD_TRIPCONFIRM`.  
6. `Z_SCM_CTD_AUTO_TRIPCONFIRM`.  
7. **`Z_SCM_CTD_GET_TRIP_REPORT`** (+ ALV program `ZSCM_CTD_ENRICHTRIPDETAILS` CTD Report branch if columns added there).  
8. Recompile dependents (reports, screens, WD/Fiori).  
9. **SCI / SLIN** clean for touched objects; transport description lists CD/ticket and objects.

---

## 13. Implementation checklist

- [ ] SE11: **eight** fields on `ZSCE_CTD_HDR`; activate.  
- [ ] SE11: `ZSCM_TRIPHDR_ST` (+ TT) extended; activate.  
- [ ] SE11: `ZSCM_CTD_REP_ALV_ST` extended; activate.  
- [ ] `Z_SCM_CTD_GETTRIPHDR`: `lty_trip_hdr` + `SELECT` (**8**) + syntax check.  
- [ ] `Z_SCM_CTD_TRIPCONFIRM`: **`RTC_MOD_*`** for **`RTC`** only; **`RCC_MOD_*`** for **`RCC`** only; name lookups **once** per context before leg loop.  
- [ ] `Z_SCM_CTD_AUTO_TRIPCONFIRM`: **`RTC_MOD_*`** for **`RTCA`**; **`RCC_MOD_*`** for **`RCCA`**; **`RTC_MOD_*`** untouched on RCCA; **`v_username`** once per call where possible.  
- [ ] `Z_SCM_CTD_GET_TRIP_REPORT`: select/move **eight** fields into report line type.  
- [ ] Scan codebase for `UPDATE zsce_ctd_hdr` / `MODIFY zsce_ctd_hdr`.  
- [ ] UI Screen 4: bind **eight** fields + fallback display for both blocks.  
- [ ] CTD Report ALV: new columns for **eight** fields (or agreed subset per UX).  
- [ ] Unit / integration test per §11.  

---

## Appendix A — Example: classic 7.31-style name lookup (illustrative)

*Illustrative only — adjust `v_username` key fields per SE11 on target system.*

```abap
DATA: lv_rtc_user TYPE syuname,
      lv_rtc_name TYPE zrtc_mod_name.  " or char80 if DE not yet active

CLEAR: lv_rtc_user, lv_rtc_name.
lv_rtc_user = sy-uname.

SELECT SINGLE name_text
  FROM v_username
  INTO lv_rtc_name
 WHERE mandt = sy-mandt
   AND bname = lv_rtc_user.

" sy-subrc / initial lv_rtc_name are both acceptable; persist as-is
```

## Appendix A2 — Auto RTCA (same lookup, `lv_auto_user` already populated)

*After existing `ZLOG_EXEC_VAR` block that fills `lv_auto_user` in `Z_SCM_CTD_AUTO_TRIPCONFIRM`:*

```abap
DATA: lv_rtc_auto_name TYPE zrtc_mod_name.

CLEAR lv_rtc_auto_name.

SELECT SINGLE name_text
  FROM v_username
  INTO lv_rtc_auto_name
 WHERE mandt = sy-mandt
   AND bname = lv_auto_user.

" Step 2 UPDATE zsce_ctd_hdr: for RTCA only, SET rtc_mod_by     = lv_auto_user
"                                   rtc_mod_name   = lv_rtc_auto_name
"                                   rtc_mod_date   = sy-datum
"                                   rtc_mod_time   = sy-uzeit
"
" For RCCA (same lv_auto_user, same lv_rtc_auto_name variable or lv_rcc_auto_name):
"   SET rcc_mod_by = lv_auto_user, rcc_mod_name = lv_rcc_auto_name,
"       rcc_mod_date = sy-datum, rcc_mod_time = sy-uzeit
"   (do not alter rtc_mod_*)
```

---

## Appendix A3 — Manual RCC (mirror of Appendix A)

Use **`lv_rcc_user = sy-uname`** and the same **`v_username`** pattern; persist into **`RCC_MOD_*`** on successful **`IM_CALL_CONTEXT = 'RCC'`** header update only.

---

## Appendix B — Reference to standard documents

| Path | Purpose |
|------|---------|
| `ABAP Rules - 02-04-2026/README.md` | Index of rule set |
| `ABAP Rules - 02-04-2026/00-main.mdc` | Core principles |
| `ABAP Rules - 02-04-2026/01-compatibility.mdc` | NW 7.31 syntax |
| `ABAP Rules - 02-04-2026/02-naming.mdc` | Prefixes |
| `ABAP Rules - 02-04-2026/03-database.mdc` | Open SQL performance |
| `ABAP Rules - 02-04-2026/11-interfaces-api.mdc` | API stability |
| `ABAP Rules - 02-04-2026/12-documentation.mdc` | Comments |
| `ABAP Rules - 02-04-2026/13-sy-subrc.mdc` | Return code checks |
| `ABAP Rules - 02-04-2026/14-transport.mdc` | Transport hygiene |
| `ABAP Rules - 02-04-2026/20-code-generation-checklist.mdc` | Pre/post generation QA |

---

*End of document.*
