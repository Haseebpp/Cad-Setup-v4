# Cad-Setup-v4: Professional Showcase & Visual Playbook

A field-tested, enterprise-grade AutoLISP & DCL productivity suite engineered for high-velocity architectural drafting, interior detailing, and strict CAD standard enforcement.

---

## Executive Summary: Why Professionals Care

Standard AutoCAD drafting suffers from three major productivity bottlenecks:
1. **Layer Friction**: Drafters spend up to 25% of their working hours manually switching active layers, hunting down color/lineweight standards, or fixing geometry mistakenly drawn on the wrong layer.
2. **Repetitive Geometry Manipulation**: Basic operations like rotating blocks in place, creating quick timestamped blocks, clipping viewports, or filleting corners with zero radius require multiple clicks, option flags, and sub-prompts.
3. **Drawing Corruption & Undo Breakage**: Unhandled errors and mid-command cancels (`Esc`) in standard LISP routines leave drawing undo stacks broken, system variables altered, and active layers trapped.

**Cad-Setup-v4** completely eliminates these bottlenecks through a **five-layer unidirectional architecture** combining single-key ergonomics, database-enforced standards, and re-entrant undo safety.

```
┌────────────────────────────────────────────────────────────────────────┐
│                        CAD-SETUP-V4 DUAL VALUE TIER                    │
├───────────────────────────────────┬────────────────────────────────────┤
│     DRAFTER PRODUCTIVITY TIER     │    CAD MANAGER STANDARDS TIER      │
├───────────────────────────────────┼────────────────────────────────────┤
│ • 1-Key numbered rapid workflow   │ • 36 database-enforced layers      │
│ • Automatic layer memory & restore│ • Zero blind layer creation        │
│ • In-place block flipping (0-axis)│ • Re-entrant undo & error safety   │
│ • One-touch entity filter switches│ • Automated live drawing legend    │
│ • Zero-prompt viewport clipping   │ • 11 industry drafting presets     │
└───────────────────────────────────┴────────────────────────────────────┘
```

---

## 10 Standout "Showstopper" Feature Cards

### Feature Card 01: The 1–8 Numbered Rapid Drafting Suite
*Ergonomic Single-Key Workflow with Active Layer Memory*

* **The Problem**: Drawing a temporary construction line or viewport boundary requires changing layers, drawing the entity, and remembering to switch back to your active work layer.
* **The Cad-Setup-v4 Solution**: Hit a single number key. The system automatically switches to the designated standard layer, activates the drawing tool, and **instantly restores your previous active layer upon completion or `Esc`**.

```
[1] Help Line     ──> Draws on "01-HELP-LINE" with live snap nodes; auto-restores layer
[2] Viewport Box  ──> Draws on "02-VIEW-PORT" + stamps dynamic A3 scale & title MText
[3] Grid System   ──> Structural bay generator with auto-bubbles & dimension strings
[4] Line Layer    ──> Opens DCL line selector & triggers polyline on chosen standard layer
[5] Material Box  ──> Opens DCL material selector & triggers rectangle on chosen layer
[6] Annotations   ──> Opens DCL annotation suite (Dims, Text, Leaders, Tags)
[7] Hardware      ──> Opens DCL fittings & hardware layer switcher
[8] Joinery Blocks──> Opens visual standard joinery block insertion palette
[-] R-Layer       ──> Unified dynamic layer selector & inspector (UDLS)
```

> **Demo Keystrokes**:
> 1. Set current layer to `R-WALL-FULL`.
> 2. Press `1` (or type `HL`): Click two points to draw a construction line. Notice point nodes placed at endpoints.
> 3. Press `Enter` or `Esc`: Check current layer. It is automatically back on `R-WALL-FULL`!

---

### Feature Card 02: In-Place Block Surgery (`CB`, `RB`, `RBH`, `RBV`)
*Zero-Friction Block Creation & Insertion-Point Transformation*

* **The Problem**: Turning geometry into a block requires naming, picking a base point, and confirming dialogs. Rotating or mirroring that block later requires specifying base points and mirror axes, which knocks the block out of alignment.
* **The Cad-Setup-v4 Solution**:
  * `CB` (or `G`): Instantly converts selected geometry into a block with an automatic collision-safe timestamp name (`BLK_YYYYMMDD_HHMMSS`) and automatically assigns its **bottom-left bounding box corner in current UCS** as the basepoint.
  * `RB`: Rotates the block **+90° directly around its existing insertion point** without asking for a base point.
  * `RBH` / `RBV`: Mirrors the block **horizontally or vertically in place** around its insertion point without requiring a mirror line or erasing confirmation.

```
       [RBH] In-Place Horizontal Flip          [RB] In-Place 90° Rotate
       ┌───────────┐         ┌───────────┐         ┌───────────┐         ┌─────┐
       │ ◄ Push    │  ───>   │    Push ► │         │  ▲ Push   │  ───>   │ ◄── │
       │ [Ins Pt]  │         │  [Ins Pt] │         │  [Ins Pt] │         │[Ins]│
       └───────────┘         └───────────┘         └───────────┘         └─────┘
```

> **Demo Keystrokes**:
> 1. Draw a door or cabinet profile. Select it and type `CB`. It is now a unified block.
> 2. Select the block and type `RB`. It spins 90° on its hinge point.
> 3. Type `RBH`. It flips horizontally across its hinge point instantly.

---

### Feature Card 03: Live Procedural Layer Legend Schedule (`BLL`)
*Automated Drawing-Wide Visual Standards Schedule*

* **The Problem**: Drafting standards manuals sit in PDFs that drafters rarely read. Keeping a drawing-level layer legend updated manually is tedious and prone to errors.
* **The Cad-Setup-v4 Solution**: Type `BLL` (or `LAYER-LEGEND`). The system queries the master database (`Db_Layers.lsp`) and constructs a complete, polished graphic table in model space with:
  * Colored graphic line swatches
  * True plotted lineweights and ACI/RGB color codes
  * Plottable status and transparency percentages
  * Official layer descriptions and assigned roles

> **Demo Keystrokes**:
> 1. In Model Space, type `BLL`.
> 2. Click an insertion point.
> 3. An architecturally formatted legend schedule is drawn with precision typography (`ARCH-TEXT` / `ARCH-TITLE`).

---

### Feature Card 04: Viewport Duplicate & Rectangular Clip (`VR` / `VPCLIPRECTANGLE`)
*Instant Layout Detail Callouts in One Gesture*

* **The Problem**: Creating an enlarged detail viewport in Paper Space requires creating a viewport, setting scale, drawing a polyline boundary, invoking `VPCLIP`, selecting the viewport, and selecting the polyline.
* **The Cad-Setup-v4 Solution**: Type `VR`. Click an existing layout viewport and drag a rectangular crop area. Cad-Setup-v4 duplicates the viewport, clips it to your rectangle, moves it to `02-VIEW-PORT`, sets non-plotting border rules, and leaves the new detail ready for positioning.

> **Demo Keystrokes**:
> 1. Switch to a Paper Space layout tab.
> 2. Type `VR` and select an existing viewport.
> 3. Click two corner points over the area you want to zoom into.
> 4. The cropped detail viewport is instantly generated.

---

### Feature Card 05: Presentation & Drafting Theme Switcher (`TS`)
*One-Touch Switch Between Drafting Dark Mode and Client Presentation White Paper*

* **The Problem**: Working in Layout with dark background is comfortable for drafting, but clients and directors want to see true white paper sheets with actual plot styles (`CTB`/`STB`) enabled to check lineweights before printing.
* **The Cad-Setup-v4 Solution**: Type `TS`. It instantly toggles the entire layout environment:
  * **Drafting Mode**: Dark charcoal background (`RGB 33,40,48`), paper shadow hidden, display margins on, plot styles turned off for fast editing.
  * **Presentation Mode**: Pure white paper (`RGB 255,255,255`), true paper bounds visible, plot styles enabled (`ShowPlotStyles = True`), live preview of exact printed lineweights.

> **Demo Keystrokes**:
> 1. Go to any Layout tab and type `TS`.
> 2. Watch AutoCAD switch from dark drafting view to crisp white presentation print preview. Type `TS` again to toggle back.

---

### Feature Card 06: Parametric Bay Structural Grid Generator (`GL` / Key `3`)
*Architectural Grid Engine with Multiplier Syntax & Auto-Bubbles*

* **The Problem**: Setting up structural columns and grid lines requires manual offsetting, trimming, tagging bubble circles, lettering X-axes, and numbering Y-axes.
* **The Cad-Setup-v4 Solution**: Type `GL` (or press `3`). An interactive DCL dialog allows entering bay spacings using natural multiplier syntax (e.g. `4000, 3*5000, 4500`). The engine automatically draws:
  * Centered grid lines on `03-GRID-LINE`
  * Standardized grid bubbles with automatic alphanumeric sequencing (`A, B, C...` and `1, 2, 3...`)
  * Extension tags and continuous dimension chains

> **Demo Keystrokes**:
> 1. Press `3` (or type `GL`).
> 2. Enter X-Bays: `6000, 2*4500, 6000` and Y-Bays: `5000, 3*4000`.
> 3. Pick origin point. The entire structural grid is drawn, labeled, and dimensioned in under 1 second.

---

### Feature Card 07: Smart Selection & Isolation Filters (`S*` Suite)
*One-Touch Entity Class Isolation without Layer Locking Headaches*

* **The Problem**: Selecting only dimensions or hatches in a crowded drawing requires opening `QSELECT`, filtering entity types, clicking OK, or manually freezing layers.
* **The Cad-Setup-v4 Solution**: Dedicated two-character selection filters powered by `Help_Selection.lsp`:

| Command | Action | Command | Inverse Action (Exclude) |
|:---:|:---|:---:|:---|
| `SR` | Isolate **Dimensions** | `SRI` | Select everything *except* Dimensions |
| `SL` | Isolate **Multileaders & Leaders** | `SLI` | Select everything *except* Leaders |
| `SB` | Isolate **Block References** | `SBI` | Select everything *except* Blocks |
| `SH` | Isolate **Hatches** | `SHI` | Select everything *except* Hatches |
| `SA` | Isolate **All Annotations** | `SAI` | Select everything *except* Annotations |
| `SW` | Isolate **Wipeouts** | `SWI` | Select everything *except* Wipeouts |

> **Demo Keystrokes**:
> 1. In a complex floor plan, type `SH`.
> 2. Window-select the plan. Every object is deselected except for hatches. Hit `E` to delete or change properties in bulk.

---

### Feature Card 08: Rapid Geometry Repair & Aliases (`FF`, `CC`, `JJ`, `TC`, `CIP`, `BBOX`, `FL0`)
*Atomic Production Drafting Acceleration*

* **`FF` (Fillet Zero)**: Fillets two lines with Radius 0 for a crisp corner, without altering your standard `FILLETRAD`.
* **`CC` (Chamfer Zero)**: Clean chamfer intersection with 0x0 distances.
* **`JJ` (Batch Polyline Join)**: Converts mixed lines, arcs, and polylines into continuous closed 2D polylines in one keystroke.
* **`TC` (Total Curve Length)**: Calculates collective cumulative lengths of selected lines, arcs, splines, and polylines—essential for estimating skirting, cables, and joinery edging.
* **`CIP` (Copy In-Place)**: Duplicates selected objects at `(0,0,0)`, moves duplicates to top draw-order, and keeps them selected.
* **`BBOX` (Automatic Bounding Box)**: Draws an exact rectangular boundary around selected entities in current UCS.
* **`FL0` (Flatten to 2D)**: Safely strips corrupt Z-coordinates and elevates all modelspace entities to `Z = 0.0`.

---

### Feature Card 09: Dynamic Vector CAD-SETTINGS Dialog (`CAD-SETTINGS`)
*Visual System Variable Dashboard with 11 Industry Drafting Presets*

* **The Problem**: AutoCAD system variables (`PICKBOX`, `APERTURE`, `CURSORSIZE`, `INSUNITS`, `PICKFIRST`) are scattered across confusing nested options tabs.
* **The Cad-Setup-v4 Solution**: Run `CAD-SETTINGS`. Features an interactive DCL dialog with a **real-time vector preview canvas** that renders the cursor crosshair, pickbox, aperture box, and grid dots dynamically as you adjust sliders. Includes 11 one-click industry profiles from `Db_Presets.lsp` (Architectural Millimeters, Interior Design, Civil Engineering, High-DPI 4K Display, etc.).

---

### Feature Card 10: Re-Entrant Undo Engine & Error Shield (`01_Undo-Manager.lsp`)
*Enterprise Stability: Zero Broken Undo Stacks & Zero Trapped Layers*

* **The Problem**: Most custom LISP routines fail when a user presses `Esc` mid-command. AutoCAD's undo stack gets locked with an open undo mark, and the user remains trapped on a temporary layer.
* **The Cad-Setup-v4 Solution**: Every command uses depth-tracked `CadSetup:UndoStart` and `CadSetup:UndoEnd`. Every error handler calls `(CadSetup:UndoReset)`. If a drafter aborts a command halfway through, the system gracefully rolls back the transaction, restores original system variables, and returns to the initial active layer.

---

## Complete Command Quick-Reference Matrix

### Workflow Keys & Rapid Drafting
| Key / Command | Full Name | Primary Layer | Function Description |
|:---:|:---|:---|:---|
| `1` / `HL` | Help Line | `01-HELP-LINE` | Construction line with live endpoint nodes; restores previous layer |
| `2` / `VP` | Viewport Box | `02-VIEW-PORT` | Draws frame + auto A3 scale metadata MText & center snap node |
| `3` / `GL` | Grid Line Maker | `03-GRID-LINE` | Structural grid dialog with multiplier bay parser & bubbles |
| `4` / `LL` | Line Layer | `R-LINE-*` | DCL line layer selector + automatic polyline tool |
| `5` / `ML` | Material Layer | `R-MTL-*` | DCL material layer selector + automatic rectangle tool |
| `6` / `AD` | Annotation Suite | `R-ANNO-*` | DCL layer switcher for dimensions, text, leaders, and tags |
| `7` / `HD` | Hardware Suite | `R-HARD-*` | DCL layer switcher for fittings, ironmongery, and hardware |
| `8` / `BL` | Block Palette | `R-BLCK-*` | Visual DCL palette for standard joinery blocks & components |
| `-` / `R-` | R-Layer Selector | Dynamic | Unified Dynamic Layer Selector (UDLS) inspector & setup |

### Layer & Standards Management
| Command | Alias | Description |
|:---:|:---:|:---|
| `LOAD-LAYERS` | `RL`, `RELOAD-LAYERS` | Synchronizes/generates all 36 production layers from database |
| `LOAD-DEFAULT-CURRENT-LAYERS` | `DCL` | Safely sets default working layers (`CLAYER`, `DIMLAYER`, `HPLAYER`) |
| `L0` | `L0` | Instantly resets active layer to standard layer `"0"` |
| `BUILD-LAYER-LEGEND` | `BLL`, `LAYER-LEGEND` | Draws full visual layer schedule table with color swatches in model space |

### Smart Blocks & Transforms
| Command | Alias | Description |
|:---:|:---:|:---|
| `CB` | `G` | Auto-create block with timestamp name & UCS bottom-left basepoint |
| `OB` | `OB` | Quick block creator with origin `(0,0,0)` basepoint |
| `RB` | `RB` | Rotate block +90° in place around insertion point |
| `RBH` | `RBH` | Mirror block horizontally in place around insertion point |
| `RBV` | `RBV` | Mirror block vertically in place around insertion point |

### Layout & Presentation Tools
| Command | Alias | Description |
|:---:|:---:|:---|
| `TS` | `TS` | Theme switcher: Toggle Dark Drafting Mode vs. White Presentation Mode |
| `VR` | `VPCLIPRECTANGLE` | Duplicate layout viewport and clip with user-drawn rectangle |
| `A3SERIES` | `A3SERIES` | Generates scaled A3 sheet boundary frames (1:1 through 1:50) at origin |

### Selection Isolators & Filters
| Command | Filter Target | Mode |
|:---:|:---|:---|
| `SR` / `SRI` | Dimensions (`DIMENSION`) | Isolate / Exclude |
| `SL` / `SLI` | Leaders & Multileaders (`LEADER,MULTILEADER`) | Isolate / Exclude |
| `SB` / `SBI` | Block References (`INSERT`) | Isolate / Exclude |
| `SH` / `SHI` | Hatches (`HATCH`) | Isolate / Exclude |
| `SA` / `SAI` | All Annotations (Dims, Texts, Leaders, Tables) | Isolate / Exclude |
| `SW` / `SWI` | Wipeouts (`WIPEOUT`) | Isolate / Exclude |

### Geometry Utilities & System Fixes
| Command | Alias | Description |
|:---:|:---:|:---|
| `TC` | `TC` | Total Curve Length calculator for lines, plines, arcs, splines |
| `CIP` | `CIP` | Copy in-place to top draw-order, keeping copies selected |
| `BBOX` | `BBOX` | Draws bounding box rectangle around selected entities in UCS |
| `CTRANS` | `CTRANS` | Sets entity transparency (0 to 90) |
| `FL0` | `FL0` | Flattens selected entities or entire modelspace to `Z = 0` |
| `PUA` | `QA` | Deep purge (All, RegApps, Zero-length, Empty text) + Audit |
| `FIXSELECT` | `FIXSELECT` | Restores `PICKFIRST=1`, `PICKADD=2`, `PICKAUTO=5`, `HIGHLIGHT=1` |
| `FIXBOX` | `FIXBOX` | Restores missing dialog boxes (`FILEDIA=1`, `CMDDIA=1`, `ATTDIA=1`) |
| `WF` | `WF` | Cycles wipeout frames: Visible & Plot -> Visible No Plot -> Hidden |
| `WR` | `WIPEOUTRECTANGLE`| Interactively draws rectangle and converts to wipeout mask |
| `LST` | `LOAD-STYLES` | Generates annotative text styles (`ARCH-TEXT`) and dimstyles (`ARCH-TICK`) |

### Production Drafting Ergonomic Aliases
| Shortcut | AutoCAD Target | Specialty Behavior |
|:---:|:---:|:---|
| `FF` | Fillet R=0 | Clean sharp corners without altering standard `FILLETRAD` |
| `CC` | Chamfer 0x0 | Clean beveled corner reset |
| `JJ` | Join to Polyline | Batch converts lines & arcs into continuous 2D polylines |
| `PC` | Close Polyline | Batch forces open polylines to closed contours |
| `PO` | Open Polyline | Batch forces closed polylines to open contours |
| `BR1` | Break at Point | Splits curve cleanly at a single selected point |
| `ROR` | Rotate Reference | Initiates rotate with `_R` reference option pre-selected |
| `SCR` | Scale Reference | Initiates scale with `_R` reference option pre-selected |
| `ME` | Mirror Erase | Mirrors geometry and automatically deletes source objects |
| `BF` | Bring to Front | Sets display draw-order to front |
| `BB` | Send to Back | Sets display draw-order to back |
| `SS` | Select Similar | Selects matching objects by type/layer |
| `IS` / `HO` / `UN` | Object Isolation | Isolate objects, Hide objects, Unhide all objects |

---

## 2-Minute Video / Client Demo Walkthrough Script

Use this exact walkthrough when demonstrating the setup to colleagues, team leads, or clients:

```text
[00:00 - 00:20] THE HOOK & ERGONOMIC WORKFLOW KEYS
"Notice how in standard AutoCAD, you spend half your time switching layers.
 Watch this: My active layer is R-WALL-FULL.
 I press '1' for a construction line. I pick two points.
 I hit Enter. Look at the layer bar: it automatically returned to R-WALL-FULL.
 Now I press '2' for Viewport: I drag a rectangle.
 It instantly generates an A3 viewport frame, stamps the scale, and returns to my layer."

[00:20 - 00:45] IN-PLACE BLOCK SURGERY
"Watch how fast we create and orient blocks.
 I window-select this door profile and press 'CB'.
 It is immediately a block with an automatic collision-safe timestamp name.
 Now, without picking any mirror lines or base points, I type 'RB' to rotate it 90 degrees.
 I type 'RBH' to flip it horizontally. Zero extra clicks, zero misaligned base points."

[00:45 - 01:10] SELECTION FILTERS & FAST GEOMETRY TOOLS
"In a crowded drawing, filtering is instant.
 Type 'SH', select the drawing, and only hatches remain selected.
 Type 'TC' to calculate the exact cumulative length of all selected profiles for estimation.
 Type 'FF' to fillet two lines with zero radius for an instant sharp corner."

[01:10 - 01:35] LAYOUT & PRESENTATION SUITE
"Let's jump to the Layout tab.
 Right now we are in dark drafting mode. I type 'TS'.
 Instantly, AutoCAD transforms into client presentation mode with true white paper
 and plotted lineweights visible.
 Want an enlarged detail? Type 'VR', click any viewport, and drag a box.
 The cropped detail viewport is created and assigned to the viewport layer in one move."

[01:35 - 02:00] ENTERPRISE STANDARDS & LIVE LEGEND
"Finally, for CAD managers: Type 'BLL'.
 The system reads our database and draws a live graphic schedule of all 36 production
 layers with true colors, lineweights, and descriptions directly in modelspace.
 Everything is protected by a re-entrant undo manager, so even if a drafter hits Escape
 halfway through, the drawing never corrupts."
```

---

## Verification & Architecture Summary

* **Entry Point**: [Autorun.lsp](file:///d:/HASEEB/Cad-Setup-v4/Autorun.lsp) (Single APPLOAD load routine)
* **Core Stability**: [01_Undo-Manager.lsp](file:///d:/HASEEB/Cad-Setup-v4/Core/01_Undo-Manager.lsp)
* **Master Layer Database**: [Db_Layers.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_Layers.lsp) (36 production layers)
* **System Presets**: [Db_Presets.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_Presets.lsp) (11 industry profiles)
* **Dialog Interface**: [CAD-SETTINGS.dcl](file:///d:/HASEEB/Cad-Setup-v4/UI/CAD-SETTINGS.dcl) & [CAD-SETTINGS-CTRL.lsp](file:///d:/HASEEB/Cad-Setup-v4/UI/CAD-SETTINGS-CTRL.lsp)
