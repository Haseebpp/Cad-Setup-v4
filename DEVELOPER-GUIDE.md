# Cad-Setup-v4: Developer & Contributor Guide

A practical handbook for extending, developing, and debugging the `Cad-Setup-v4` modular AutoCAD drafting system.

---

## 1. Quick Start & Hot Reloading

When editing code in `d:\Cad-Setup-v4\`, you **do not need to restart AutoCAD** to test your changes.

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
| **Global Helper Functions**| `CadSetup:VerbNoun` | `CadSetup:GetBoundingBoxUcs` | Namespaced with prefix, PascalCase |
| **Global Data Tables** | `*CadSetup-Noun-Data*` | `*CadSetup-Layers-Data*` | Asterisk-wrapped earmuffs |
| **Local Variables** | `camelCase` or `kebab-case` | `minPt`, `oldEcho`, `base-pt` | Kept local in `/ ...` parameter list |
| **Command Aliases** | 1 to 3 characters | `1`, `2`, `CB`, `TC`, `LB` | Ergonomic drafting keystrokes |

---

## 3. The Standard Command Template

Whenever you write a new command in `Commands/`, always use this battle-tested template to guarantee **undo stack integrity** and **clean error recovery**:

```lisp
(defun c:MYCMD ( / *error* oldCmd ss ent obj )
  ;; 1. Localized Error Handler
  (defun *error* (msg)
    (if oldCmd (setvar 'cmdecho oldCmd))
    ;; Always reset undo stack on cancel/error:
    (CadSetup:UndoReset)
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*quit*,*exit*")))
      (princ (strcat "\n[MYCMD] Error: " msg))
    )
    (princ)
  )

  ;; 2. Save environment
  (setq oldCmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  ;; 3. Start Safe Undo Mark
  (CadSetup:UndoStart)

  ;; 4. Command Logic
  (princ "\nSelect objects for MyCommand: ")
  (if (setq ss (ssget))
    (progn
      ;; Use helpers from Helpers/ layer:
      ;; (setq bbox (CadSetup:GetBoundingBoxUcs ss))
      (princ (strcat "\n[MYCMD] Processed " (itoa (sslength ss)) " object(s)."))
    )
    (princ "\n[MYCMD] No objects selected.")
  )

  ;; 5. Close Undo Mark
  (CadSetup:UndoEnd)

  ;; 6. Restore environment
  (setvar 'cmdecho oldCmd)
  (princ)
)
```

---

## 4. Development Recipes

### Recipe 1: How to Add a New AutoCAD Command
1. Determine which command file in `Commands/` fits the functional category:
   - `01_Workflow-Keys.lsp`: Single-key hotkeys (5, 6...)
   - `02_Layers.lsp`: Quick layer operations (e.g., isolate layer, set layer)
   - `03_Blocks.lsp`: Block generation, rotation, alignment
   - `04_Layout-Views.lsp`: Sheet, viewport, and presentation tools
   - `05_Selection-Filters.lsp`: Fast entity filters (`ssget` wrappers)
   - `06_Utilities.lsp`: Calculation, cleanup, repair
   - `99_Aliases.lsp`: Native AutoCAD alias remappings
2. Paste the **Standard Command Template** into the file.
3. If reusable math or geometry is needed, implement the calculation in `Helpers/Help_Geometry.lsp` first.
4. Test by running `RCS` in AutoCAD.

---

### Recipe 2: How to Add a New Production Layer
1. Open `Database/Db_Layers.lsp`.
2. Add a new row to `*CadSetup-Layers-Data*`:
   ```lisp
   ("R-ANNO-KPIN"  40  "150,95,30"  "CONTINUOUS"  25  T  "NONE"  1.0  0.0  0  nil  "Key Plan Index Annotations")
   ```
   *Format:* `(Name Color PlotColor Linetype Lineweight Plot? HatchPattern HatchScale HatchRot Transparency Locked? Description)`
3. Save the file.
4. Run `RCS` in AutoCAD. The new layer is automatically created and styled in the active drawing!

---

### Recipe 3: How to Add a New System Variable
1. Open `Database/Db_SysVars.lsp`.
2. Add an entry to `*CadSetup-SysVars-Data*`:
   ```lisp
   ("TEXTFILL"  1  "Display"  "Fill TrueType fonts when displaying and plotting")
   ```
3. Run `RCS` or `RSV` (Reload System Variables) in AutoCAD to enforce it across drawings.

---

### Recipe 4: How to Add a New Drafting Preset to CAD-SETTINGS GUI
1. Open `Database/Db_Presets.lsp`.
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
3. Open `UI/CAD-SETTINGS.dcl` and add a button for it in the presets row:
   ```dcl
   : button {
     key = "btn_landscape";
     label = "Landscape (m)";
     width = 22;
     fixed_width = true;
   }
   ```
4. Open `UI/CAD-SETTINGS-CTRL.lsp` and bind the action tile:
   ```lisp
   (action_tile "btn_landscape" "(apply-preset-data \"LANDSCAPE_M\")")
   ```
5. Run `RCS` in AutoCAD and type `CAD-SETTINGS` to see your new preset in action!

---

### Recipe 5: How to Add a Helper Function
1. Choose the appropriate helper file:
   - `Helpers/Help_ActiveX.lsp`: COM/VLA document, space, or sysvar access.
   - `Helpers/Help_Selection.lsp`: `ssget` queries, filters, or entity predicates.
   - `Helpers/Help_Geometry.lsp`: 2D/3D math, bounding boxes, lengths, angles.
   - `Helpers/Help_Layers.lsp`: Layer creation, linetypes, state toggles.
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
- To avoid codepage character corruption (`âœ“`), never print multi-byte Unicode characters (such as `✓` or `✖`). Always use the standard ASCII logging functions: `(CadSetup:LogSuccess "...")` `[OK]`, `(CadSetup:LogWarn "...")` `[WARN]`, `(CadSetup:LogError "...")` `[ERR]`, and `(CadSetup:LogDebug "...")`.

**Q: AutoCAD reports "CAD-SETTINGS.dcl not found".**
- Ensure `d:\Cad-Setup-v4` is added to AutoCAD's Support File Search Path (`OPTIONS` $\rightarrow$ `Files` $\rightarrow$ `Support File Search Path`), or verify `CadSetup:GetDir` returns `D:\Cad-Setup-v4`.

**Q: Esc leaves my drawing in an unclosed undo transaction.**
- Ensure your command wraps logic with `(CadSetup:UndoStart)` and `(CadSetup:UndoEnd)`, and that its `*error*` handler invokes `(CadSetup:UndoReset)`.

**Q: How do I test syntax before running in AutoCAD?**
- You can run the PowerShell parenthesis balance checker:
  ```powershell
  Get-ChildItem -Recurse -Include *.lsp | ForEach-Object {
    $c = Get-Content $_.FullName -Raw
    $open = ([regex]::Matches($c, "\(")).Count
    $close = ([regex]::Matches($c, "\)")).Count
    if ($open -ne $close) { Write-Host "Mismatch in $($_.Name): $open vs $close" }
  }
  ```
