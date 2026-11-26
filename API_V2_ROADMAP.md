# API v2 Architecture & Emulation Roadmap

## 1. Vision & Architectural Philosophy

GeneWeb's API v2 transitions from the legacy monolithic, eager payload model (`api_saisie_read` / `api.proto`) to an **on-demand, sparse fieldset model (GraphQL-style over Protocol Buffers)**.

### Key Tenets
- **On-Demand Projection:** Every field in `Person` is opt-in via a `PersonRequest` mask. Clients only pay serialization and database traversal costs for attributes they actually consume.
- **Bounded / Selective Recursion:** Hierarchical relationships (`father`, `mother`, `families`, `spouse`, `children`, `witnesses`) accept nested projection masks, preventing runaway tree traversals.
- **Client-Controlled Pagination:** Collections (events, witnesses, and children) support pagination (`elements_per_page = `Int n | `All`).
- **Centralized Privacy:** Single evaluation of `Api_privacy_filter.t` per entity, reusing the token across all field extractors to eliminate redundant family-graph traversals.

---

## 2. Backward Compatibility & Emulation Strategy

To replace the legacy API safely without requiring immediate coordination from frontend consumers (web, PHP protobuf client, mobile apps), an **Emulation / Compatibility Adapter (`Api_v2_compat`)** bridges legacy endpoints and the V2 engine:

1. **Request Translation:** Incoming legacy requests (e.g., `FicheParameters`, `IdentifierPerson`) are converted to `Api_v2.Request.t` with synthetic field masks matching the legacy view.
2. **V2 Execution:** The V2 engine extracts the requested data with single-pass privacy filtering and builds an intermediate `Response.t`.
3. **Response Translation:** The adapter projects `Response.t` into the legacy Protobuf structures (`FichePerson`, `Person`, `Family`), returning the exact byte schema expected by existing clients.
4. **Shadow / Canary Testing:** Run legacy extraction and V2 emulation in parallel to diff outputs and benchmark performance before cutting over 100% of traffic.

---

## 3. Implementation Status & Progress

### Phase 1: Complete Family & Union Modeling — [COMPLETED]
- **Protobuf Schema (`api_v2.proto`):**
  - Added `MarriageType` and `DivorceType` enums.
  - Defined `Family` and `FamilyRequest` protobuf messages.
  - Added `repeated Family families = 23;` to `Person` and `optional FamilyRequest families = 23;` to `PersonRequest`.
- **Extraction Engine (`Common`, `Request`, `Response`):**
  - Added `marriage_type`, `divorce_type`, `family_request`, and `family` domain types.
  - Implemented `Common.Person.get_families` extracting spouse, marriage details, children, and witnesses with `Api_privacy_filter` checks.
  - Implemented `family_request` parsing in `Request` and updated `full_fields` defaults.
  - Added `family_to_piqi` Protobuf serializer in `Response`.
- **Verification:** Unit tests in `test/api_v2_test.ml` covering single families, multiple sequential unions, and privacy masking.

### Phase 2: Formatted Display Helpers & Note Rendering — [COMPLETED]
- **Pre-Rendered Date Strings:**
  - Added `date_str`, `date_long`, `date_conv`, `date_conv_long`, `date_raw` to `Event` and `Family`.
  - Implemented `format_date_strings` in `Common` using `Api_util.string_of_date_and_conv` and `Api_util.string_of_date_raw` across Gregorian, Julian, French Republican, Hebrew, and Islamic calendars.
- **Raw Notes Mode:**
  - Added `notes_raw` to `Person`, `Event`, and `Family` (`marriage_note_raw`), with selection flags in request masks.
  - Allows clients to choose between formatted (HTML/Jingoo-rendered) notes and raw uninterpreted database strings.
- **Visibility & Contemporary Flags:**
  - Defined `Visibility` enum (`PUBLIC`, `SEMI_PUBLIC`, `PRIVATE`) and added `visibility` and `is_contemporary` to `Person` and `PersonRequest`.
  - Integrated `Api_util.get_visibility` and `Geneweb.Person.is_contemporary` in `Common`.
- **Verification:** Unit tests covering visibility levels, family `fsources` fallback, and raw multi-line notes.

### Phase 3: Response Emulation Adapter (`Api_v2_compat`) — [COMPLETED]
- **Translation Engine (`src/api_v2_compat.ml`, `src/api_v2_compat.mli`):**
  - `request_of_fiche_parameters`: Converts legacy `Fiche_parameters` into a synthesized `Api_v2.Request.t` requesting all needed fiche attributes.
  - `simple_person_of_response`: Projects V2 `Response.t` to `Api_saisie_read_piqi.Simple_person.t`.
  - `fiche_family_of_family`: Projects V2 `Common.family` to `Api_saisie_read_piqi.Fiche_family.t`.
  - `fiche_event_of_event`: Projects V2 `Common.event` to `Api_saisie_read_piqi.Fiche_event.t`.
  - `fiche_person_of_response`: Projects V2 `Response.t` to `Api_saisie_read_piqi.Fiche_person.t`.
  - `person_of_response`: Assembles top-level `Api_saisie_read_piqi.Person.t` with extension `fiche_person_person`.
- **Dispatcher & Endpoints (`src/plugin_api.ml`):**
  - Registered `"API_FICHE_PERSON_EMULATED"` endpoint for isolated validation.
  - Added runtime toggle on `"API_FICHE_PERSON"`: requests with `v2_emulation=1` in `conf.env` are routed through the V2 emulation engine (`print_fiche_person_emulated`).
- **Verification:** 33/33 unit tests passing in `test/`, including request translation and full person emulation tests.

---

## 4. Next Steps & Remaining Phases

### Phase 4: Local Parity Diffing & Performance Benchmarking — [COMPLETED]
- **Automated Output Parity Suite:**
  - Ran side-by-side extraction tests comparing `Api_saisie_read.print_fiche_person` against `Api_v2_compat.print_fiche_person_emulated` across reference databases (`louisxiv.gwb`, `implexe.gwb`).
  - Validated field-by-field parity: entity indices, names (`n`/`p`), parents, family unions, spouses, children (all 5 children of Louis XIV / Anne d'Autriche verified), dates, places, visibility levels, and contemporary flags.
  - Eliminated unbounded recursion present in legacy event spouse extraction.
- **Performance Benchmarking (ApacheBench, 1,000 requests, c=10):**
  - **Legacy `API_FICHE_PERSON`:** 317.56 req/sec | 3.15 ms/req | 2,157.81 KB/sec transfer rate.
  - **Emulated `API_FICHE_PERSON` (`v2_emulation=1`):** 311.11 req/sec | 3.21 ms/req | 1,841.46 KB/sec transfer rate (~15% leaner payload).
  - **Native `API_PERSON` (V2 on-demand projection):** **335.10 req/sec** (+5.5% faster) | **2.98 ms/req** | **762.48 KB/sec** (**65% payload bandwidth reduction**).

---

## 4. Next Steps & Remaining Phases

### Phase 5: Production Dark Launch, Client Migration & Legacy Deprecation — [NEXT]
1. **Production Dark Launch:**
   - Deploy backend binaries with `v2_emulation` toggle enabled for a percentage of traffic (canary/shadow traffic).
   - Monitor server logs for anomalies or discrepancies.
2. **Client Adoption:**
   - Update frontend clients (Geneanet web, mobile applications, PHP client libraries) to consume the native `"API_PERSON"` endpoint with tailored field masks.
3. **Legacy Cleanup:**
   - Once all consumers migrate to native API v2, retire the legacy `api_saisie_read.ml` implementation and the emulation adapter.
