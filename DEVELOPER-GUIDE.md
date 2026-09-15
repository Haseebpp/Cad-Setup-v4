# Cad-Setup-v4: Developer & Contributor Guide

A practical handbook for extending, developing, and debugging the `Cad-Setup-v4` modular AutoCAD drafting system.

---

## 1. Quick Start & Hot Reloading

When editing code in `d:\HASEEB\Cad-Setup-v4\`, you **do not need to restart AutoCAD** to test your changes.

1. Open AutoCAD.
2. Edit any file in `Core/`, `Helpers/`, `Database/`, `Commands/`, or `UI/`.
3. At the AutoCAD command line, type:
   ```
   RCS
   ```
   *(or `RELOAD-COMMAND-SUITES`)*
4. All layers and command suites are dynamically re-loaded in exact dependency order:
   $$\text{Core} \longrightarrow \text{Helpers} \longrightarrow \text{Database} \longrightarrow \text{Commands} \longrightarrow \text{UI}$$
5. To test settings dialog changes, type:
   ```
   CAD-SETTINGS
   ```

---

## 2. Naming Conventions & Code Style

To prevent symbol collision across files and ensure clean maintainability:

| Element | Pattern | Example | Rationale |
| :--- | :--- | :--- | :--- |
| **AutoCAD Commands** | `c:NAME` (Uppercase) | `c:HL`, `c:CB`, `c:CAD-SETTINGS` | Standard AutoCAD command registration |
| **Global Helper Functions**| `CadSetup:VerbNoun` | `CadSetup:GetBoundingBoxUcs`, `CadSetup:EnsureLayerFromDb` | Namespaced with prefix, PascalCase |
| **Global Data Tables** | `*CadSetup-Noun-Data*` | `*CadSetup-Layers-Data*`, `*CadSetup-SysVars-Data*` | Asterisk-wrapped earmuffs |
| **Local Variables** | `camelCase` or `kebab-case` | `minPt`, `oldEcho`, `base-pt` | Kept local in `/ ...` parameter list |
| **Command Aliases** | 1 to 3 characters | `1`, `2`, `CB`, `TC`, `TS`, `RL` | Ergonomic drafting keystrokes |

---

## 3. The Standard Command Templates

Every command in `Commands/` must adhere to one of two battle-tested templates to guarantee **atomic undo stack integrity**, **safe environment preservation**, and **clean error recovery**.

### Choosing Your Command Pattern

> [!TIP]
> - **Choose Pattern A (Non-Interactive / Batch)** if your goal is **Data Processing** *(e.g., "Find all text on Layer X across 50 files and change it to Arial", geometric calculations, mass transformations, property overrides)*.
> - **Choose Pattern B (Interactive Pause Loop)** if your goal is **Drafting Assistance** *(e.g., "Switch to the electrical layer, let the user click where the outlets go, then switch back", interactive point picking, rubber-band lines, continuous placement)*.

---

### Pattern A: Non-Interactive / Batch Execution (Data Processing)

Use this pattern when the command executes immediate calculations, transforms a pre-selected set, or runs batch database modifications without pausing for user input inside the command:

```lisp
(defun c:MYBATCHCMD ( / *error* oldCmd ss ent obj )
  ;; 1. Localized Error Handler & Safe Stack Reset
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    ;; Always reset undo stack on cancel or error:
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[MYBATCHCMD] Error: " msg))
    )
    (princ)
  )

  ;; 2. Save drafting environment
  (setq oldCmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  ;; 3. Begin Safe Atomic Undo Transaction
  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

  ;; 4. Command Logic (Batch / Data Processing)
  (princ "\nSelect objects to process: ")
  (if (setq ss (ssget))
    (progn
      ;; Perform geometric calculations or mass entity updates:
      ;; (setq bbox (CadSetup:GetBoundingBoxUcs ss))
      (princ (strcat "\n[MYBATCHCMD] Processed " (itoa (sslength ss)) " object(s)."))
    )
    (princ "\n[MYBATCHCMD] No objects selected.")
  )

  ;; 5. Close Atomic Undo Transaction
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))

  ;; 6. Restore drafting environment
  (setvar 'cmdecho oldCmd)
  (princ)
)
```

---

### Pattern B: Interactive Native Pause Loop (Drafting Assistance)

Use this pattern when the command delegates to a native AutoCAD interactive command (e.g., `_.PLINE`, `_.ROTATE pause _R`, `_.RECTANG`, `_.FILLET`) or interactively prompts the user for points while switching layers or environments. The `(while (> (getvar 'cmdactive) 0) (command pause))` loop ensures AutoLISP waits until the user finishes picking points before closing the undo mark:

```lisp
(defun c:MYDRAFTCMD ( / *error* oldCmd oldLayer targetLay )
  ;; 1. Localized Error Handler & Variable Restoration
  (defun *error* (msg)
    (if oldCmd   (setvar 'cmdecho oldCmd))
    (if oldLayer (setvar 'clayer  oldLayer))
    ;; Always reset undo stack on Esc / cancel:
    (if (boundp 'CadSetup:UndoReset) (CadSetup:UndoReset))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[MYDRAFTCMD] Error: " msg))
    )
    (princ)
  )

  ;; 2. Save drafting environment
  (setq oldCmd   (getvar 'cmdecho)
        oldLayer (getvar 'clayer)
        targetLay "R-LINE-VISB")
  (setvar 'cmdecho 0)

  ;; 3. Begin Safe Atomic Undo Transaction
  (if (boundp 'CadSetup:UndoStart) (CadSetup:UndoStart))

  ;; 4. Safely set working layer from Database
  (CadSetup:SetCurrentLayerSafe targetLay)
  (princ (strcat "\n[MYDRAFTCMD] Active layer set to: " targetLay))

  ;; 5. Launch interactive native command and wait for user completion
  (setvar 'cmdecho 1)
  (command "_.PLINE")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )
  (setvar 'cmdecho 0)

  ;; 6. Restore original layer & Close Undo Mark
  (setvar 'clayer oldLayer)
  (if (boundp 'CadSetup:UndoEnd) (CadSetup:UndoEnd))

  ;; 7. Restore system variables
  (setvar 'cmdecho oldCmd)
  (princ (strcat "\n[MYDRAFTCMD] Completed. Restored active layer: " oldLayer))
  (princ)
)
```

---

### Undo Block Exception Policy

> [!WARNING]
> Do **NOT** wrap the following types of commands in `UndoStart` / `UndoEnd` blocks:
> 1. **Read-Only Inspection & Queries** (e.g., `TC` - Total Curve Length): They query entity geometry and report lengths without mutating drawing database records.
> 2. **Database Maintenance Tools** (e.g., `PUA` / `QA` - Deep Purge & Audit): AutoCAD `AUDIT` and `-PURGE` operations rebuild the drawing database table index. Attempting to group audit operations within an undo block can invalidate the drawing's undo stack.

---

## 4. Development Recipes

### Recipe 1: How to Add a New AutoCAD Command
1. Determine which command file in `Commands/` fits the functional category:
   - `01_Workflow-Keys.lsp`: Single-key hotkeys (1=HL, 2=VP, 3=GL, 4=LL, 5=ML, 6=AD, 7=HD, 8=BL, `=R-)
   - `02_Layers.lsp`: Quick layer operations (`RL`, `DCL`, `L0`, `BLL`)
   - `03_Blocks.lsp`: Block generation, rotation, alignment (`CB`, `OB`, `RB`, `BL`)
   - `04_Layout-Views.lsp`: Sheet, viewport, and presentation tools (`TS`, `A3SERIES`, `VPCLIPRECTANGLE`, `VR`)
   - `05_Selection-Filters.lsp`: Fast entity filters (`ssget` wrappers: `SR`, `SL`, `SB`, `SH`, `SA`, `SW`)
   - `06_Utilities.lsp`: Calculation, cleanup, repair (`CIP`, `BBOX`, `CTRANS`, `FL0`, `WF`, `WR`)
   - `99_Aliases.lsp`: Native AutoCAD alias remappings and macro actions
2. Paste either **Pattern A** (Batch) or **Pattern B** (Interactive) into the file.
3. If reusable math or geometry is needed, implement the calculation in `Helpers/Help_Geometry.lsp` first.
4. Test by running `RCS` in AutoCAD.

---

### Recipe 2: How to Add a New Production Layer
1. Open [Database/Db_Layers.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_Layers.lsp).
2. Add a new row to `*CadSetup-Layers-Data*`:
   ```lisp
   ("R-ANNO-KPIN"  40  "150,95,30"  "CONTINUOUS"  25  T  "NONE"  1.0  0.0  0  nil  "Key Plan Index Annotations")
   ```
   *Format:* `(Name Color PlotColor Linetype Lineweight Plot? HatchPattern HatchScale HatchRot Transparency Locked? Description)`
3. Save the file.
4. Run `RCS` in AutoCAD. The new layer is immediately available to all commands and will load dynamically on demand.

---

### Recipe 3: How to Safely Switch Layers in Custom Functions
To comply with **Golden Rule 6**, never call raw `(vla-add ...)` or `(command "._-layer" ...)`:

```lisp
;; Good practice: Automatically loads from Db_Layers.lsp or falls back to "0"
(CadSetup:SetCurrentLayerSafe "R-LINE-VISB")

;; To ensure a layer exists without making it current:
(CadSetup:EnsureLayerFromDb "R-ANNO-DIMS")
```

---

### Recipe 4: How to Add a New System Variable
1. Open [Database/Db_SysVars.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_SysVars.lsp).
2. Add an entry to `*CadSetup-SysVars-Data*`:
   ```lisp
   ("TEXTFILL"  1  "Display"  "Fill TrueType fonts when displaying and plotting")
   ```
3. Run `RCS` or `RSV` (Reload System Variables) in AutoCAD to enforce it across drawings.

---

### Recipe 5: How to Add a New Drafting Preset to CAD-SETTINGS GUI
1. Open [Database/Db_Presets.lsp](file:///d:/HASEEB/Cad-Setup-v4/Database/Db_Presets.lsp).
2. Add a new preset block to `*CadSetup-Presets-Data*`:
   ```lisp
   ("LANDSCAPE_M"
    (:label . "Landscape (m)")
    (:status . "Applied Preset: Landscape Architecture (Meters, Precision 0.00, Ortho OFF)")
    (:tiles . (
      ("pop_lunits"   . "1")   ; Decimal
      ("pop_luprec"   . "2")   ; 0.00 m
      ("pop_insunits" . "6")   ; Meters
      ("tog_ortho"    . "0")   ; Ortho OFF for organic curves
      ("tog_grid"     . "1")
    )))
   ```
3. Open [UI/CAD-SETTINGS.dcl](file:///d:/HASEEB/Cad-Setup-v4/UI/CAD-SETTINGS.dcl) and add a button for it in the presets row:
   ```dcl
   : button {
     key = "btn_landscape";
     label = "Landscape (m)";
     width = 22;
     fixed_width = true;
   }
   ```
4. Open [UI/CAD-SETTINGS-CTRL.lsp](file:///d:/HASEEB/Cad-Setup-v4/UI/CAD-SETTINGS-CTRL.lsp) and bind the action tile:
   ```lisp
   (action_tile "btn_landscape" "(apply-preset-data \"LANDSCAPE_M\")")
   ```
5. Run `RCS` in AutoCAD and type `CAD-SETTINGS` to see your new preset in action!

---

### Recipe 6: How to Add a Helper Function
1. Choose the appropriate helper file:
   - `Helpers/Help_ActiveX.lsp`: COM/VLA document, space, or sysvar access.
   - `Helpers/Help_Selection.lsp`: `ssget` queries, filters, or entity predicates.
   - `Helpers/Help_Geometry.lsp`: 2D/3D math, bounding boxes, lengths, angles.
   - `Helpers/Help_Layers.lsp`: Layer creation, `EnsureLayerFromDb`, linetypes, lock toggles.
   - `Helpers/Help_Graphics.lsp`: Primitives (`DrawBox`, `DrawLine`, `AddText`, `CreateHatch`).
2. Name the function with `CadSetup:VerbNoun` prefix:
   ```lisp
   (defun CadSetup:GetViewportCenter (vpObj / bbox)
     (setq bbox (CadSetup:GetBoundingBox vpObj))
     (if bbox
       (CadSetup:MidPoint (car bbox) (cadr bbox))
       nil
     )
   )
   ```
3. Call it from any command in `Commands/` or dialog in `UI/`.

---

## 5. Troubleshooting & FAQ

**Q: How do I enable detailed diagnostic / verbose logging during startup?**
- Type `CAD-SETUP-DEBUG` (or `CS-DEBUG`) at the AutoCAD command line to toggle verbose logging. When enabled, running `RCS` will display each tier and file being loaded with `[OK]` status.

**Q: What is the logging standard for console messages?**
- To avoid codepage character corruption (`âœ“`), never print multi-byte Unicode characters (such as `✓` or `✖`). Always use standard ASCII logging prefixes: `[OK]`, `[WARN]`, `[ERR]`, and `[Notice]`.

**Q: AutoCAD reports "CAD-SETTINGS.dcl not found".**
- Ensure `d:\HASEEB\Cad-Setup-v4` is added to AutoCAD's Support File Search Path (`OPTIONS` $\rightarrow$ `Files` $\rightarrow$ `Support File Search Path`), or verify `CadSetup:GetDir` returns `d:\HASEEB\Cad-Setup-v4`.

**Q: Esc leaves my drawing in an unclosed undo transaction or depth > 0.**
- Check the current depth via `!*CadSetup-Undo-Depth*` in the console.
- Ensure your command wraps logic with `(CadSetup:UndoStart)` and `(CadSetup:UndoEnd)`, and that its localized `*error*` handler invokes `(CadSetup:UndoReset)`.

**Q: Why does the console print: `[Notice]: Layer "..." not found in Database/Db_Layers.lsp`?**
- In accordance with **Golden Rule 6**, the system refuses to create ad-hoc unstandardized layers. To resolve this, register the layer in `Database/Db_Layers.lsp`.

**Q: How do I test syntax and parenthesis balance before running in AutoCAD?**
- You can run the PowerShell parenthesis balance verification script:
  ```powershell
  Get-ChildItem -Recurse -Filter *.lsp | ForEach-Object {
    $c = Get-Content $_.FullName -Raw
    $open = ([regex]::Matches($c, '\(')).Count
    $close = ([regex]::Matches($c, '\)')).Count
    if ($open -ne $close) { Write-Host "Mismatch in $($_.Name): $open vs $close" }
  }
  ```
