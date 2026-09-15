# Cad-Setup-v4

> **Enterprise-Grade AutoLISP & DCL Productivity Suite for AutoCAD**  
> High-velocity drafting ergonomics, database-enforced layer standardization, in-place block manipulation, and re-entrant undo safety.

[![AutoCAD](https://img.shields.io/badge/AutoCAD-2020%20--%202026-blue.svg)](#requirements)
[![Language](https://img.shields.io/badge/Language-AutoLISP%20%7C%20Visual%20LISP%20%7C%20DCL-brightgreen.svg)](#architecture)
[![Architecture](https://img.shields.io/badge/Architecture-5--Layer%20Horizontal-orange.svg)](ARCHITECTURE.md)
[![Standard](https://img.shields.io/badge/Standards-36%20Production%20Layers%20(ISO%2013567%20Adapted)-purple.svg)](#layer-standards)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](#license)

---

## Table of Contents

- [Overview](#overview)
- [Why Cad-Setup-v4?](#why-cad-setup-v4)
- [Quick Start & Installation](#quick-start--installation)
- [Core Features](#core-features)
  - [1. Numbered Workflow Keys (1–8, -)](#1-numbered-workflow-keys-18--)
  - [2. In-Place Block Surgery (CB, RB, RBH, RBV)](#2-in-place-block-surgery-cb-rb-rbh-rbv)
  - [3. Automated Model Space Layer Legend (BLL)](#3-automated-model-space-layer-legend-bll)
  - [4. Viewport Duplicate & Rectangular Clip (VR)](#4-viewport-duplicate--rectangular-clip-vr)
  - [5. Layout Presentation Theme Switcher (TS)](#5-layout-presentation-theme-switcher-ts)
  - [6. Parametric Structural Grid Engine (GL / Key 3)](#6-parametric-structural-grid-engine-gl--key-3)
  - [7. Instant Entity Selection Filters (S* Suite)](#7-instant-entity-selection-filters-s-suite)
  - [8. Rapid Geometry Utilities & Repair](#8-rapid-geometry-utilities--repair)
  - [9. Real-Time Vector CAD-SETTINGS Dashboard](#9-real-time-vector-cad-settings-dashboard)
  - [10. Re-Entrant Undo Engine & Error Shield](#10-re-entrant-undo-engine--error-shield)
- [Command Reference Matrix](#command-reference-matrix)
- [Repository Structure](#repository-structure)
- [Configuration & Customization](#configuration--customization)
- [Troubleshooting & Environment Fixes](#troubleshooting--environment-fixes)
- [Documentation & Links](#documentation--links)
- [License](#license)

---

## Overview

**Cad-Setup-v4** is a production drafting system built for architects, interior designers, joinery detailers, and CAD managers. Unlike ad-hoc scripts or unmaintained LISP snippets, Cad-Setup-v4 is structured as an enterprise **horizontal architecture**:

* **Core**: Strict re-entrant undo transactions and error safety wrappers.
* **Helpers**: Modular COM/ActiveX wrappers, UCS geometry calculation, and selection utilities.
* **Database**: Pure data repositories defining 36 production layers, system variables, block components, and 11 industry drafting presets.
* **Commands**: Ergonomic, single-key and atomic command implementations.
* **UI**: Clean, static DCL dialogs paired with event controllers and real-time vector previews.

---

## Why Cad-Setup-v4?

| Traditional AutoCAD Friction | Cad-Setup-v4 Engineering Solution |
|---|---|
| **Layer Distraction**: Drafters spend up to 25% of time manually changing layers and fixing geometry drawn on wrong layers. | **Layer Memory**: Single-key shortcuts (`1`–`8`) switch to the designated standard layer and **automatically restore your previous layer** upon completion or `Esc`. |
| **Awkward Block Transformations**: Rotating or mirroring a block requires picking base points and mirror axes, disrupting alignment. | **Zero-Axis Hinge Transforms**: `RB`, `RBH`, and `RBV` rotate or mirror blocks **in place around their true insertion point** with zero sub-prompts. |
| **Broken Undo Stacks**: Aborted commands leave open undo marks, corrupt counters, and trapped active layers. | **Re-Entrant Undo Manager**: `CadSetup:UndoStart` and `CadSetup:UndoReset` ensure every command or cancel cleans up atomically. |
| **Outdated Standards**: Layer standards are hidden in static manuals; drawings accumulate rogue layers with random colors. | **Database Enforcement**: Zero blind layer creation. `BLL` automatically draws an up-to-date visual layer schedule directly in model space. |

---

## Quick Start & Installation

### 1. Requirements
* AutoCAD 2020 through 2026 (or AutoCAD-based verticals: Architecture, MEP, Civil 3D).
* Full AutoLISP & Visual LISP support (AutoCAD LT prior to 2024 does not support LISP).

### 2. Setup via APPLOAD (Recommended)

1. Clone or download this repository to a permanent location (e.g., `D:\HASEEB\Cad-Setup-v4`).
2. Open AutoCAD and enter command: `APPLOAD`
3. In the **Startup Suite** (briefcase icon), click **Contents...**.
4. Click **Add...** and select [`Autorun.lsp`](file:///d:/HASEEB/Cad-Setup-v4/Autorun.lsp) from the root folder.
5. Add the folder to AutoCAD Trusted Locations:
   * Run `OPTIONS` -> **Files** tab -> **Trusted Locations** -> **Add...** -> browse to your `Cad-Setup-v4` directory.
6. Restart AutoCAD or type `(load "D:/HASEEB/Cad-Setup-v4/Autorun.lsp")`.

Upon startup, the system automatically initializes all 36 production layers, linetypes (`acadiso.lin`), typography styles (`ARCH-TEXT`, `ARCH-TITLE`), and dimension styles (`ARCH-TICK`, `ARCH-ARROW`).

---

## Core Features

### 1. Numbered Workflow Keys (1–8, -)
Single-digit drafting shortcuts mapped to top-row keys for rapid execution. Each key safely sets the destination standard layer, executes the drafting tool, and **restores the active working layer**:

* `[1]` / `HL` : **Help Line** — Draws on `01-HELP-LINE` with live snap point nodes.
* `[2]` / `VP` : **Viewport Boundary** — Draws on `02-VIEW-PORT` and stamps dynamic ISO A3 scale metadata MText.
* `[3]` / `GL` : **Grid Line Maker** — Launches the parametric structural grid generator.
* `[4]` / `LL` : **Line Layer** — DCL selector for `R-LINE-*` layers + polyline drawing.
* `[5]` / `ML` : **Material Layer** — DCL selector for `R-MTL-*` layers + rectangle drawing.
* `[6]` / `AD` : **Annotation Suite** — DCL layer switcher for dimensions, text, leaders, and tags.
* `[7]` / `HD` : **Hardware Suite** — DCL layer switcher for fittings and ironmongery.
* `[8]` / `BL` : **Joinery Blocks** — Visual DCL palette for standard joinery blocks.
* `[-]` / `R-` : **R-Layer Selector** — Unified Dynamic Layer Selector (UDLS).

---

### 2. In-Place Block Surgery (`CB`, `RB`, `RBH`, `RBV`)
* **`CB` / `G` (Create Block)**: Converts selected objects into a block instantly. Assigns an automatic timestamp name (`BLK_YYYYMMDD_HHMMSS`) with collision protection and computes the collective **bottom-left bounding box in current UCS** as the base point.
* **`RB` (Rotate Block)**: Rotates selected blocks **+90° around their insertion point** in place.
* **`RBH` (Flip Horizontal)**: Mirrors selected blocks horizontally around their insertion point without needing a mirror line.
* **`RBV` (Flip Vertical)**: Mirrors selected blocks vertically around their insertion point.

---

### 3. Automated Model Space Layer Legend (`BLL`)
Type `BLL` (or `LAYER-LEGEND`) and pick a point in Model Space. The routine reads [Db_Layers.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_Layers.lsp) and builds a live, formatted graphic schedule table containing:
* Color swatches
* Plotted lineweights and ACI color numbers
* Plottable status & transparency percentages
* Official layer descriptions and classifications

---

### 4. Viewport Duplicate & Rectangular Clip (`VR`)
Type `VR` (or `VPCLIPRECTANGLE`) in Paper Space:
1. Select an existing viewport.
2. Drag a rectangle over the desired detail area.
3. Cad-Setup-v4 clones the viewport, applies the rectangular clipping boundary, relocates the boundary to `02-VIEW-PORT`, and sets standard non-plotting properties.

---

### 5. Layout Presentation Theme Switcher (`TS`)
Type `TS` to toggle any Paper Space layout between:
* **Dark Drafting Mode**: Charcoal background (`RGB 33,40,48`), paper shadow disabled, plot styles off for drafting speed.
* **Presentation White Paper Mode**: Clean white sheet (`RGB 255,255,255`), true paper margins visible, and plot styles active (`ShowPlotStyles = True`) to verify true line weights.

---

### 6. Parametric Structural Grid Engine (`GL` / Key `3`)
Type `GL` to open the structural grid dialog:
* Enter bay spacings using natural multiplier syntax (e.g. `6000, 3*4500, 6000`).
* Generates centered grid lines on `03-GRID-LINE`.
* Automatically places alphanumeric bubble callouts (`A, B, C...` and `1, 2, 3...`).
* Automatically draws continuous dimension strings.

---

### 7. Instant Entity Selection Filters (`S*` Suite)
Isolate or exclude entity categories in one click without touching layer freeze/thaw:

| Target Entity | Isolate Selection | Exclude from Selection |
|---|:---:|:---:|
| **Dimensions** | `SR` | `SRI` |
| **Leaders & Multileaders** | `SL` | `SLI` |
| **Block References** | `SB` | `SBI` |
| **Hatches** | `SH` | `SHI` |
| **All Annotations** | `SA` | `SAI` |
| **Wipeouts** | `SW` | `SWI` |

---

### 8. Rapid Geometry Utilities & Repair
* **`FF`**: Fillet with Radius 0 (clean sharp corner without changing system `FILLETRAD`).
* **`CC`**: Chamfer with 0x0 distance.
* **`JJ`**: Batch convert and join lines/arcs into closed 2D polylines.
* **`TC`**: Calculate cumulative total curve length for lines, polylines, arcs, splines, and circles.
* **`CIP`**: Duplicate objects in place, bring to front, and keep new copies selected.
* **`BBOX`**: Draw automatic bounding box rectangle around selected geometry in current UCS.
* **`FL0`**: Safely flatten 3D geometry to `Z = 0.0`.
* **`PUA` / `QA`**: Deep purge (All, RegApps, Zero-length geometry, Empty text) + drawing database audit.

---

### 9. Real-Time Vector CAD-SETTINGS Dashboard
Type `CAD-SETTINGS` (or `CADSETTINGS`):
* An interactive DCL control panel featuring a **live vector preview canvas**.
* Watch the crosshair, aperture box, pickbox, and grid dots update in real time as you adjust sliders.
* Apply one of **11 industry drafting presets** from [Db_Presets.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_Presets.lsp) (Architectural Millimeters, Interior Design, High-DPI 4K Display, Civil Engineering, etc.).

---

### 10. Re-Entrant Undo Engine & Error Shield
Located in [Core/01_Undo-Manager.lsp](file:///d:/HASEEB/Cad-Setup-v4/Core/01_Undo-Manager.lsp):
* Enforces atomic transactions via `(CadSetup:UndoStart)` and `(CadSetup:UndoEnd)`.
* Employs an internal depth counter (`*CadSetup-UndoDepth*`) to handle nested commands safely.
* In every `*error*` handler, `(CadSetup:UndoReset)` unwinds open undo groups and restores original system variables, preventing broken drawings when drafters press `Esc`.

---

## Command Reference Matrix

### Rapid Drafting & Workflow Keys
| Key / Command | Alias | Destination Layer | Description |
|:---:|:---:|:---|:---|
| `1` | `HL` | `01-HELP-LINE` | Help line with live snap nodes; restores previous layer |
| `2` | `VP` | `02-VIEW-PORT` | Viewport box with auto A3 scale metadata stamp |
| `3` | `GL` | `03-GRID-LINE` | Parametric structural grid generator with bay parser |
| `4` | `LL` | `R-LINE-*` | Line layer DCL selector + polyline tool |
| `5` | `ML` | `R-MTL-*` | Material layer DCL selector + rectangle tool |
| `6` | `AD` | `R-ANNO-*` | Annotation & dimension layer switcher |
| `7` | `HD` | `R-HARD-*` | Hardware & fittings layer switcher |
| `8` | `BL` | `R-BLCK-*` | Visual standard joinery block palette |
| `-` | `R-` | Dynamic | Unified Dynamic Layer Selector (UDLS) |

### Layer & Standards Management
| Command | Alias | Description |
|:---:|:---:|:---|
| `LOAD-LAYERS` | `RL`, `RELOAD-LAYERS` | Synchronizes 36 standardized production layers from database |
| `LOAD-DEFAULT-CURRENT-LAYERS` | `DCL` | Safely applies default drawing, dim, and hatch layers |
| `L0` | `L0` | Instantly resets current layer to standard `"0"` |
| `BUILD-LAYER-LEGEND` | `BLL`, `LAYER-LEGEND`| Generates graphic layer schedule with color swatches in model space |

### Blocks & Transformations
| Command | Alias | Description |
|:---:|:---:|:---|
| `CB` | `G` | Auto-block with timestamp name & UCS bottom-left base point |
| `OB` | `OB` | Quick block creator with origin `(0,0,0)` base point |
| `RB` | `RB` | Rotate block +90° in place around insertion point |
| `RBH` | `RBH`, `RH` | Mirror block horizontally in place around insertion point |
| `RBV` | `RBV`, `RV` | Mirror block vertically in place around insertion point |

### Layout & Sheet Tools
| Command | Alias | Description |
|:---:|:---:|:---|
| `TS` | `TS` | Toggle between Dark Drafting Mode and White Presentation Mode |
| `VR` | `VPCLIPRECTANGLE` | Duplicate layout viewport and clip with user rectangle |
| `A3SERIES` | `A3SERIES` | Generates scaled A3 frames (1:1 through 1:50) at origin |

### Productivity Aliases & Geometry Fixes
| Shortcut | Native Command | Behavior |
|:---:|:---:|:---|
| `FF` | Fillet | Instant fillet with Radius 0 (sharp corner) |
| `CC` | Chamfer | Instant chamfer with 0x0 distances |
| `JJ` | Join | Batch converts lines/arcs into 2D polylines |
| `PC` / `PO` | Pedit | Force close / open selected polyline contours |
| `BR1` | Break | Splits a curve cleanly at a single selected point |
| `ROR` | Rotate | Rotates objects with reference (`_R`) pre-selected |
| `SCR` | Scale | Scales objects with reference (`_R`) pre-selected |
| `ME` | Mirror | Mirrors objects and deletes original source geometry |
| `BF` / `BB` | Draworder | Bring to Front / Send to Back |
| `TC` | Curve Length | Cumulative length calculator for lines/arcs/splines |
| `CIP` | Copy In-Place | Duplicates objects at `(0,0,0)` to top draw order |
| `BBOX` | Bounding Box | Draws rectangular bounding box in current UCS |
| `FL0` | Flatten | Flattens selected objects or modelspace to `Z=0` |
| `PUA` / `QA` | Purge / Audit | Deep database purge & audit |

---

## Repository Structure

```
Cad-Setup-v4/
├── Autorun.lsp                 # Master entry point (APPLOAD / Startup Suite)
├── README.md                   # System documentation & quick reference
├── ARCHITECTURE.md             # Layer hierarchy & architectural boundaries
├── DEVELOPER-GUIDE.md          # Coding conventions & recipe guide
├── Core/
│   ├── 00_Init.lsp             # Path resolution, logging & environment validation
│   └── 01_Undo-Manager.lsp     # Re-entrant undo manager & error safety engine
├── Helpers/
│   ├── Help_ActiveX.lsp        # COM/VLA safe wrappers, getvar/setvar, clamp
│   ├── Help_Geometry.lsp       # UCS bounding boxes, curve lengths, annotative scale
│   ├── Help_Graphics.lsp       # 2D primitives, hatch creation, draw order
│   ├── Help_Layers.lsp         # EnsureLayerFromDb, EnsureLayer, Safe layer setter
│   └── Help_Selection.lsp      # Selection filters, entity predicates
├── Database/
│   ├── Db_Layers.lsp           # Master 36-layer dictionary & specifications
│   ├── Db_SysVars.lsp          # Recommended drawing states & system variables
│   ├── Db_Presets.lsp          # 11 industry drafting workspace profiles
│   └── Db_Blocks.lsp           # Joinery block definitions & procedural geometry
├── Commands/
│   ├── 00_System-Variables.lsp # System variable restoration (APPLY-SYSVARS, RSV)
│   ├── 01_Workflow-Keys.lsp    # Numbered rapid drafting keys (1, 2, 3, 4, 5...)
│   ├── 02_Layers.lsp           # Layer loading & legend generator (RL, DCL, BLL)
│   ├── 03_Blocks.lsp           # Auto block creator & in-place transforms (CB, RB, RBH)
│   ├── 04_Layout-Views.lsp     # Layout & presentation tools (TS, VR, A3SERIES)
│   ├── 05_Selection-Filters.lsp# Instant entity filters (SR, SL, SB, SH, SA, SW)
│   ├── 06_Utilities.lsp        # Geometry utilities (TC, CIP, BBOX, FL0, PUA, LST)
│   └── 99_Aliases.lsp          # Production ergonomic command aliases & macros
└── UI/
    ├── CAD-SETTINGS.dcl        # Master settings dialog definition
    ├── CAD-SETTINGS-CTRL.lsp   # Event controller & live vector preview engine
    ├── GRID-GENERATOR.dcl      # Structural grid generator dialog
    ├── BLOCK-PALETTE.dcl       # Visual standard joinery block palette
    └── UNIFIED-LAYER-SELECTOR.dcl # Dynamic layer selector dialog
```

---

## Configuration & Customization

### Customizing Default Active Layers
In [Commands/02_Layers.lsp](file:///d:/HASEEB/Cad-Setup-v4/Commands/02_Layers.lsp), modify the default layer variables:
```lisp
(setq *DEFAULT-CLAYER*   "R-LINE-VISB")   ;; Active drawing layer
(setq *DEFAULT-DIMLAYER* "R-ANNO-DIMS")   ;; Dimensioning layer
(setq *DEFAULT-HPLAYER*  "R-HTCH-GENR")   ;; Hatching layer
```

### Adding Company Layers
Open [Database/Db_Layers.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_Layers.lsp) and add your definition to `*CadSetup-Layers-Data*`:
```lisp
("MY-LAYER-NAME" 3 "Green" "CONTINUOUS" 35 T "NONE" 1.0 0.0 0 nil "Custom Layer Description")
```
Run `RL` in AutoCAD to immediately synchronize and apply new layers drawing-wide.

---

## Troubleshooting & Environment Fixes

Cad-Setup-v4 includes built-in rescue commands for common AutoCAD glitches:

* **Missing Dialog Boxes**: If `OPEN`, `SAVEAS`, or block dialogs appear on the command line instead of windows, type `FIXBOX`. Restores `FILEDIA=1`, `CMDDIA=1`, and `ATTDIA=1`.
* **Broken Selection (Shift-to-Add / Can't Select First)**: If AutoCAD stops allowing noun/verb selection or additive selection, type `FIXSELECT`. Restores `PICKFIRST=1`, `PICKADD=2`, `PICKAUTO=5`, and `HIGHLIGHT=1`.
* **Wipeout Frames Visible / Plotting**: Type `WF` to cycle wipeout frames between:
  1. Visible & Plotted
  2. Visible on Screen Only (Non-plotting)
  3. Completely Hidden

---

## Documentation & Links

* [ARCHITECTURE.md](ARCHITECTURE.md) — Comprehensive technical layer breakdown and strict architectural rules.
* [DEVELOPER-GUIDE.md](DEVELOPER-GUIDE.md) — Developer guidelines, helper library APIs, and code recipes.
* [PRO-SHOWCASE-GUIDE.md](PRO-SHOWCASE-GUIDE.md) — Fast Visual Playbook with 2-minute client walkthrough demo script.

---

## License

This project is licensed under the MIT License — see the repository for full terms.
Designed and engineered for high-performance AutoCAD production environments.
