# Cad-Setup-v4: Layered / Horizontal Architecture

A clean, traditional, enterprise-grade architecture for AutoCAD AutoLISP/DCL production drafting systems. Every file has a single technical responsibility, zero duplicate helper code, pure data isolation, and strict unidirectional execution flow.

---

## 1. System Architecture & Layer Hierarchy

The system is organized into **five horizontal layers** that strictly follow a unidirectional dependency model:

```mermaid
graph TD
    subgraph Execution Order [Sequential Loading Order Low to High]
        L0["Core (Level 0)<br/>Environment, Paths, Undo-Manager"]
        L1["Helpers (Level 1)<br/>ActiveX, Selection, Geometry, Layers"]
        L2["Database (Level 2)<br/>Pure Data: Layers, SysVars, Presets"]
        L3["Commands (Level 3)<br/>Workflow Keys, Blocks, Utilities, Aliases"]
        L4["UI (Level 4)<br/>Pure DCL Dialogs & Event Controllers"]
    end

    L0 --> L1
    L1 --> L2
    L2 --> L3
    L3 --> L4

    classDef core fill:#2d3748,stroke:#4a5568,stroke-width:2px,color:#fff;
    classDef helper fill:#1a365d,stroke:#2b6cb0,stroke-width:2px,color:#fff;
    classDef db fill:#22543d,stroke:#38a169,stroke-width:2px,color:#fff;
    classDef cmd fill:#744210,stroke:#d69e2e,stroke-width:2px,color:#fff;
    classDef ui fill:#44337a,stroke:#805ad5,stroke-width:2px,color:#fff;

    class L0 core;
    class L1 helper;
    class L2 db;
    class L3 cmd;
    class L4 ui;
```

---

## 2. Startup & Execution Flow

When AutoCAD loads `Autorun.lsp` (via `APPLOAD` or the Startup Suite), the following sequential lifecycle executes:

```mermaid
sequenceDiagram
    autonumber
    participant ACAD as AutoCAD Engine
    participant Auto as Autorun.lsp
    participant Core as Core/
    participant Help as Helpers/
    participant Db as Database/
    participant Cmd as Commands/
    participant UI as UI/

    ACAD->>Auto: Load Autorun.lsp
    Auto->>Core: 1. Load 00_Init.lsp & 01_Undo-Manager.lsp
    Auto->>Help: 2. Load Help_ActiveX, Help_Selection, Help_Geometry, Help_Layers
    Auto->>Db: 3. Load Db_Layers, Db_SysVars, Db_Presets (Pure Data)
    Auto->>Cmd: 4. Load 00_SysVars ... 99_Aliases (Register c: commands)
    Auto->>UI: 5. Load CAD-SETTINGS-CTRL.lsp
    Auto->>Auto: CadSetup:Initialize Routine
    Auto->>Help: Load Standard Linetypes (acadiso.lin)
    Auto->>Db: Read *CadSetup-Layers-Data*
    Auto->>Help: EnsureLayer for each of 33+ Production Layers
    Auto->>ACAD: Setup Typography (ARCH-TEXT) & Dimstyles (ARCH-TICK)
    Auto->>ACAD: Set Defaults (CLAYER=R-LINE-VISB, DIMLAYER, HPLAYER)
    Auto-->>ACAD: Initialization Complete [✓]
```

---

## 3. Directory Structure & Technical Responsibilities

```
d:\Cad-Setup-v4\
├── Autorun.lsp                 ; Master orchestrator & APPLOAD entry point
├── ARCHITECTURE.md             ; Architectural system diagrams & folder rules
├── DEVELOPER-GUIDE.md          ; Practical recipes & coding conventions
├── Core/
│   ├── 00_Init.lsp             ; Path resolution, logging, environment checks
│   └── 01_Undo-Manager.lsp     ; Re-entrant Undo-mark wrappers (UndoStart / UndoEnd)
├── Database/
│   ├── Db_Layers.lsp           ; Master layer dictionary & properties (colors, lineweights)
│   ├── Db_SysVars.lsp          ; Recommended system variables & drawing states
│   └── Db_Presets.lsp          ; Workspace presets (11 industry drafting profiles)
├── Helpers/
│   ├── Help_ActiveX.lsp        ; Safe COM/VLA wrappers, safe getvar/setvar, clamp
│   ├── Help_Geometry.lsp       ; Bounding boxes in UCS, curve lengths, midpoints, annotative scale
│   ├── Help_Graphics.lsp       ; 2D primitives (DrawBox, DrawLine, AddText), hatch & draw order
│   ├── Help_Layers.lsp         ; EnsureLayer, SetCurrentLayerSafe, LoadLinetype
│   └── Help_Selection.lsp      ; ssget filters, predicates (is-annotation, is-revcloud)
├── Commands/
│   ├── 00_System-Variables.lsp ; Applies sysvars from Database/Db_SysVars.lsp
│   ├── 01_Workflow-Keys.lsp    ; Fast drafting key shortcuts (1, 2, 3, 4)
│   ├── 02_Layers.lsp           ; Layer management commands (RL, DCL, L0, BLL)
│   ├── 03_Blocks.lsp           ; Block creation & transforms (CB, OB, RB, RBH, RBV)
│   ├── 04_Layout-Views.lsp     ; Viewport and presentation tools (LB, A3SERIES)
│   ├── 05_Selection-Filters.lsp; Fast selection isolators (SR, SL, SB, SH, SA, SW)
│   ├── 06_Utilities.lsp        ; Geometry utilities & repair (TC, CIP, BBOX, FL0, PUA, WF)
│   └── 99_Aliases.lsp          ; AutoCAD command alias mapping & quick macros
└── UI/
    ├── CAD-SETTINGS.dcl        ; Pure static DCL dialog interface definition
    └── CAD-SETTINGS-CTRL.lsp   ; Dialog event handlers, preview renderer & presets
```

---

## 4. Architectural Rules & Boundaries ("The Golden Rules")

To maintain high code quality and avoid architectural drift as the project expands, all contributors must adhere to five strict rules:

### Rule 1: Strict Unidirectional Dependency Flow
- Lower levels **must never** call or depend on higher levels.
- `Core/` cannot reference `Helpers/`, `Database/`, `Commands/`, or `UI/`.
- `Helpers/` can only reference `Core/`.
- `Database/` contains pure data and can only reference basic LISP constructs.
- `Commands/` and `UI/` can reference `Core/`, `Helpers/`, and `Database/`.
- `Commands/` files **must never** depend on each other.

### Rule 2: Database Layer Is 100% Pure Data
- Files in `Database/` (`Db_Layers.lsp`, `Db_SysVars.lsp`, `Db_Presets.lsp`) **never execute imperative AutoCAD commands**.
- No `(command ...)`, no `(setvar ...)`, and no DOM manipulation in `Database/`.
- They define associative lists, constants, and pure getter accessors (`CadSetup:GetAllLayers`, `CadSetup:GetRecommendedSysVars`, `CadSetup:GetPresetData`).

### Rule 3: Zero Duplicate Helper Code
- Math functions, bounding box transformations, curve calculations, selection set loops, and layer checks belong in `Helpers/`.
- If two commands need similar geometric logic, the logic must be extracted into `Helpers/Help_Geometry.lsp` or `Helpers/Help_Selection.lsp`.

### Rule 4: Re-Entrant Undo & Error Safety
- Never call raw `(vla-startundomark doc)` or `(vla-endundomark doc)` directly in command files.
- Always use `(CadSetup:UndoStart)` at the beginning of a command and `(CadSetup:UndoEnd)` at the conclusion.
- In every `(defun *error* (msg) ...)`, call `(CadSetup:UndoReset)` to guarantee that an aborted command never leaves an unclosed undo mark in the drawing.

### Rule 5: Pure UI Separation
- `UI/CAD-SETTINGS.dcl` contains **only DCL syntax**. It never writes temporary files at runtime.
- `UI/CAD-SETTINGS-CTRL.lsp` loads the static DCL file, binds event handlers (`action_tile`), renders the image tile, and sets system variables.
