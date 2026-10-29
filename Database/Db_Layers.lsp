;;; ==========================================================================
;;; Db_Layers.lsp - Master Layer Dictionary & Visual Production Specifications
;;; Layer: Database (Level 2 - Pure Data)
;;; ==========================================================================

;; ==============================================================================
;; Master Data Definition
;; Format: (Layer Name | Layer Color (ACI) | Plot Color (RGB) | Linetype | Lineweight (0.01 mm) | Plot (T/nil) | Hatch Pattern | Hatch Scale | Hatch Rotation (deg) | Transparency (%) | Locked (T/nil) | Description)
;; ==============================================================================
(setq *CadSetup-Layers-Data* '(

  ;; Utilities
  ("01-HELP-LINE"       6   "180,0,180"   "CONTINUOUS"  5   nil  "NONE"      1.0  0.0  0   nil  "Color 6 (Magenta): Construction Lines - Auxiliary guides, temporary offsets, alignment rays (Non-Plotting)")
  ("02-VIEW-PORT"       250 "30,30,30"    "CENTERX2"    5   nil  "NONE"      1.0  0.0  0   nil  "Color 250 (Black): Viewports - Layout viewport frames and detail sheet cutouts (Non-Plotting)")
  ("03-GRID-LINE"       8   "100,100,100" "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 8 (Dark Gray): Structural Grid - Primary building grid lines and column datums")

  ;; Architectural Shell
  ("R-ARCH-CLNG"        141 "20,110,170"  "HIDDEN2"     15  T    "NONE"      1.0  0.0  0   nil  "Color 141 (Sky Blue): Ceiling & Bulkhead - False ceilings, pelmet recesses, soffits, high bulkheads")
  ("R-ARCH-FLOR"        251 "80,80,80"    "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 251 (Charcoal): Floor & Plinth - Finished floor levels (FFL), screeds, floor tile datum lines")
  ("R-ARCH-PLNT"        72  "50,150,40"   "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 72 (Green): Plants & Biophilia - Indoor planters, integrated joinery greenery, decorative foliage")
  ("R-ARCH-WALL"        104 "0,127,31"    "CONTINUOUS"  25  T    "ANSI32"    2.0  0.0  0   nil  "Color 104 (Green): Architectural Walls - Drywall partitions, masonry, structural enclosures")

  ;; Annotations
  ("R-ANNO-DIMS"        20  "180,75,0"    "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 20 (Orange/Tan): Dimensions - Primary and secondary dimension strings, overall gauges")
  ("R-ANNO-EQMT"        130 "0,120,130"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 130 (Teal): Equipment Tags - Joinery & appliance equipment tags, hardware codes")
  ("R-ANNO-FRME"        6   "180,0,180"   "CONTINUOUS"  50  T    "NONE"      1.0  0.0  0   nil  "Color 6 (Magenta): Annotation Frames - Drawing boundary frames, sheet borders, presentation framing")
  ("R-ANNO-LEDR"        252 "80,80,80"    "CONTINUOUS"  15  T    "NONE"      1.0  0.0  0   nil  "Color 252 (Pale Gray): Leaders - Multileader lines, pointers, item balloon callout lines")
  ("R-ANNO-NOTE"        3   "0,120,50"    "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 3 (Green): General Notes - General notes, legends, drawing schedules")
  ("R-ANNO-REVN"        10  "200,30,0"    "CONTINUOUS"  35  T    "NONE"      1.0  0.0  0   nil  "Color 10 (Red-Orange): Revision Cloud - Revision clouds, delta revision tags, drawing change marks")
  ("R-ANNO-SECT"        1   "180,0,0"     "DASHDOT"     50  T    "NONE"      1.0  0.0  0   nil  "Color 1 (Red): Section Cut - Section cutting plane lines, detail bubble markers")
  ("R-ANNO-SYMB"        2   "170,115,0"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 2 (Yellow): Symbols - Elevation markers, detail markers, level symbols")
  ("R-ANNO-SPEC"        3   "0,120,50"    "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 3 (Green): Specification Text - General specification callouts, finish tags")
  ("R-ANNO-TTLB"        7   "0,0,0"       "CONTINUOUS"  35  T    "NONE"      1.0  0.0  0   T    "Color 7 (White/Black): Titleblock Border - Title block geometry, border frame (Locked: Yes)")

  ;; Electrical
  ("R-ELEC-LIGHTING"    5   "0,80,190"    "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 5 (Blue): Lighting - LED strip profiles, puck lights, driver housings, wire channels")
  ("R-ELEC-WIRING"      221 "120,30,155"  "PHANTOM2"    18  T    "NONE"      1.0  0.0  0   nil  "Color 221 (Violet): Wiring Path - Power feeds, conduit paths, driver connection routes")

  ;; Hardware
  ("R-HARD-FITTINGS"    252 "90,95,105"   "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 252 (Pale Gray): Hardware Fittings - Concealed hinges, brackets, minifix connectors")
  ("R-HARD-HANDLES"     210 "145,25,115"  "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 210 (Magenta/Violet): Hardware Handles - Exposed handles, edge pulls, knobs profiles")
  ("R-HARD-LOCKS"       212 "160,20,90"   "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 212 (Magenta): Locks & Security - Cam locks, electronic card locks, espagnolette bolts")
  ("R-HARD-RUNNERS"     252 "85,90,100"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 252 (Pale Gray): Hardware Runners - Drawer slides, soft-close mechanisms, sliding tracks")

  ;; Hatches
  ("R-HTCH-GENR"        8   "130,130,130" "CONTINUOUS"  5   T    "ANSI31"    1.0  0.0  40  nil  "Color 8 (Dark Gray): General Hatch - Generic pattern lines, section cross-hatching, surface fills")
  ("R-HTCH-SOLI"        250 "30,30,30"    "CONTINUOUS"  5   T    "SOLID"     1.0  0.0  50  nil  "Color 250 (Black): Solid Fill - Solid surface fills, tonal shading, opaque fills")

  ;; Linework
  ("R-LINE-CNTR"        1   "180,20,20"   "CENTER2"     13  T    "NONE"      1.0  0.0  0   nil  "Color 1 (Red): Center Line - Alignment centerlines, symmetry axes, positioning datums")
  ("R-LINE-CUT"         3   "0,120,50"    "CONTINUOUS"  30  T    "NONE"      1.0  0.0  0   nil  "Color 3 (Green): Cut Line - Primary section cuts, heavy substrate slicing profiles")
  ("R-LINE-DETL"        124 "0,127,95"    "CONTINUOUS"  18  T    "NONE"      1.0  0.0  0   nil  "Color 124 (Teal): Detail Line - Secondary internal visible edges, panel grooves, rebates")
  ("R-LINE-HIDD"        9   "110,115,120" "HIDDEN2"     15  T    "NONE"      1.0  0.0  0   nil  "Color 9 (Light Gray): Hidden Line - Concealed framing, rear battens, internal shelf positions")
  ("R-LINE-SWNG"        2   "170,115,0"   "HIDDEN2"     13  T    "NONE"      1.0  0.0  0   nil  "Color 2 (Yellow): Swing Line - Cabinet door swing arcs, drawer opening trajectories")
  ("R-LINE-VISB"        4   "0,110,150"   "CONTINUOUS"  25  T    "NONE"      1.0  0.0  0   nil  "Color 4 (Cyan): Visible Line - External carcass outlines, visible elevation edges")

  ;; Materials
  ("R-MAT-CORIAN"       51  "210,205,190" "CONTINUOUS"  25  T    "AR-SAND"   0.5  0.0  0   nil  "Color 51 (Ivory/Alabaster): Corian / Solid Surface - Acrylic polymer slabs, seamless counters, thermoformed profiles")
  ("R-MAT-FABRIC"       211 "175,70,110"  "CONTINUOUS"  15  T    "ANSI37"    3.0  0.0  0   nil  "Color 211 (Pink): Fabric - Acoustic wall paneling, wrapped headboards, textile pinboards, woven inserts")
  ("R-MAT-GLASS"        140 "20,120,140"  "CONTINUOUS"  18  T    "NONE"      1.0  0.0  20  nil  "Color 140 (Light Cyan): Glass - Clear, frosted, fluted & tempered glass panels, showcase vitrines (Transp: 20%)")
  ("R-MAT-INSL-CORE"    43  "105,115,30"  "CONTINUOUS"  15  T    "BATTS"     8.0  0.0  0   nil  "Color 43 (Olive): Insulation Core - Mineral wool, acoustic batts, structural backing cores")
  ("R-MAT-LAMI-MELM"    42  "165,124,0"   "CONTINUOUS"  18  T    "ANSI31"    3.0  0.0  0   nil  "Color 42 (Ochre): Laminate / Melamine - High-pressure laminate (HPL), MFC carcass boards, edge banding")
  ("R-MAT-METAL"        90  "45,115,55"   "CONTINUOUS"  25  T    "ANSI34"    3.0  0.0  0   nil  "Color 90 (Light Green): Metal Structure - Brass trims, stainless steel bases, metal frames")
  ("R-MAT-MIRROR"       142 "75,140,155"  "CONTINUOUS"  18  T    "AR-RROOF"  2.0  45.0 0   nil  "Color 142 (Ice Blue): Mirror - Silvered float glass, tinted/bronze mirrors, vanity & feature wall mirror cladding")
  ("R-MAT-PLAS-ACRY"    141 "20,110,170"  "CONTINUOUS"  18  T    "ANSI37"    6.0  0.0  15  nil  "Color 141 (Sky Blue): Plastic / Acrylic - Acrylic diffusers, PVC trims, synthetic profiles (Transp: 15%)")
  ("R-MAT-PNT-COAT"     11  "175,65,45"   "CONTINUOUS"  13  T    "DOTS"      8.0  0.0  0   nil  "Color 11 (Salmon): Paint / Coating - PU lacquer, powder-coat layers, back-coat primers")
  ("R-MAT-SEAL-GASK"    253 "65,65,65"    "CONTINUOUS"  13  T    "HONEY"     3.0  0.0  0   nil  "Color 253 (Dark Gray): Seals / Gaskets - Silicone joints, glazing gaskets, dust seals, neoprene")
  ("R-MAT-STONE-MARBLE" 152 "115,130,145" "CONTINUOUS"  25  T    "MARBLE2"   1.0  0.0  0   nil  "Color 152 (Cool Slate Gray): Marble - Veined marble, natural calacatta/carrara, polished stone")
  ("R-MAT-STONE-QUARTZ" 162 "95,120,135"  "CONTINUOUS"  25  T    "NET"       4.0  0.0  0   nil  "Color 162 (Medium Slate): Quartz - Engineered composite stone, quartz slabs, solid surfaces")
  ("R-MAT-STONE-SOLID"  150 "80,95,110"   "CONTINUOUS"  25  T    "AR-CONC"   1.0  0.0  0   nil  "Color 150 (Slate Gray): Solid Stone - Granite, dense basalt, monolithic structural stone")
  ("R-MAT-UPHL"         213 "150,45,100"  "CONTINUOUS"  18  T    "HOUND"     4.0  0.0  0   nil  "Color 213 (Plum): Upholstery & Leather - Padded banquette cushions, foam build-ups, leather seats, fluted upholstery")
  ("R-MAT-WOOD-PANEL"   34  "135,70,25"   "CONTINUOUS"  25  T    "ANSI31"    5.0  0.0  0   nil  "Color 34 (Brown): Wood Panel - MDF, Plywood, Chipboard substrate cores")
  ("R-MAT-WOOD-SOLID"   30  "170,80,15"   "CONTINUOUS"  25  T    "ANSI38"    6.0  0.0  0   nil  "Color 30 (Orange): Solid Wood - Hardwood/softwood framing, solid timber lippings")
  ("R-MAT-WOOD-VENEER"  40  "150,95,30"   "CONTINUOUS"  15  T    "TRANS"     5.0  0.0  0   nil  "Color 40 (Light Brown): Wood Veneer - Natural wood veneer, stained architectural veneers")
))

;; ===========================================================================
;; DATABASE ACCESSORS
;; ===========================================================================

;; CadSetup:GetAllLayers - Returns complete list of layer data rows
(defun CadSetup:GetAllLayers ()
  *CadSetup-Layers-Data*
)

;; CadSetup:GetLayerData - Looks up a specific layer by name
(defun CadSetup:GetLayerData (lName / match)
  (if (and lName (= (type lName) 'STR))
    (assoc (strcase lName)
           (mapcar '(lambda (x) (cons (strcase (car x)) (cdr x))) *CadSetup-Layers-Data*))
    nil
  )
)

(if *CadSetup-Debug*
  (princ "\n[Database/Db_Layers.lsp] Master Layer Dictionary loaded.")
)
(princ)
