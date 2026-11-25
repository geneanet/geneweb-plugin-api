# API v2 Architecture & Emulation Roadmap

## 1. Vision & Architectural Philosophy

GeneWeb's API v2 transitions from the legacy monolithic, eager payload model (`api_saisie_read` / `api.proto`) to an **on-demand, sparse fieldset model (GraphQL-style over Protocol Buffers)**.

### Key Tenets
- **On-Demand Projection:** Every field in `Person` is opt-in via a `PersonRequest` mask. Clients only pay serialization and database traversal costs for attributes they actually consume.
- **Bounded / Selective Recursion:** Hierarchical relationships (`father`, `mother`, `families`, `spouse`, `children`, `witnesses`) accept nested projection masks, preventing runaway tree traversals.
- **Client-Controlled Pagination:** Collections (events, witnesses, and children) support pagination (`elements_per_page = `Int n | `All`).
- **Centralized Privacy:** Single evaluation of `Api_privacy_filter.t` per entity, reusing the token across all field extractors to eliminate redundant family-graph traversals.

## 2. Backward Compatibility & Emulation Strategy

To replace the legacy API safely without requiring immediate coordination from frontend consumers (web, PHP protobuf client, mobile apps), an **Emulation / Compatibility Adapter (`Api_v2_compat`)** will bridge legacy endpoints and the V2 engine:

1. **Request Translation:** Incoming legacy requests (e.g., `FicheParameters`, `IdentifierPerson`) are converted to `Api_v2.Request.t` with synthetic field masks matching the legacy view.
2. **V2 Execution:** The V2 engine extracts the requested data with single-pass privacy filtering and builds an intermediate `Response.t`.
3. **Response Translation:** The adapter projects `Response.t` into the legacy Protobuf structures (`FichePerson`, `Person`, `Family`), returning the exact byte schema expected by existing clients.
4. **Shadow / Canary Testing:** Run legacy extraction and V2 emulation in parallel to diff outputs and benchmark performance before cutting over 100% of traffic.

## 3. Implementation Phases

### Phase 1: Complete Family & Union Modeling (In Progress)
- Schema additions in `api_v2.proto`:
  * `Family` message: index, spouse (nested `Person`), marriage date/place/type/notes/sources, divorce date/type, children (nested `Person`), witnesses.
  * `FamilyRequest` mask: projection masks for spouse, marriage fields, children, witnesses.
  * Add `repeated Family families` and `optional FamilyRequest families` to `Person` and `PersonRequest`.
- Engine additions in `Common`, `Request`, and `Response`:
  * Extraction of person unions via `Gwdb.get_family`, resolving spouse and children with nested field masks.
  * Unit test suite covering unions, marriage events, multiple children, and privacy masking.

### Phase 2: Formatted Display Helpers & Note Rendering
- Pre-rendered date strings (`date_str`, `date_long`, `date_conv`) alongside structured `Date` objects.
- Presentation modes for notes: raw database string vs. formatted (HTML/Jingoo rendered).

### Phase 3: Response Emulation Adapter (`Api_v2_compat`)
- Translation module:
  * `request_of_fiche_parameters : Api_saisie_read_piqi.Fiche_parameters -> Request.t`
  * `fiche_person_of_response : Response.t -> Api_saisie_read_piqi.Fiche_person.t`
  * `person_of_response : Response.t -> Api_saisie_read_piqi.Person.t`
- Dual-run parity test suite comparing `Api_saisie_read.print_fiche_person` vs. `Api_v2_compat.emulate_fiche_person` on `Mock_db` fixtures.

### Phase 4: Production Dark Launch & Client Migration
- Enable V2 backend for legacy endpoints via configuration toggle.
- Transition frontend consumers to native `API_PERSON` endpoints.
- Deprecate legacy endpoints once traffic shifts.
