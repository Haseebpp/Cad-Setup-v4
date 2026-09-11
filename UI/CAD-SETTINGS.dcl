// =============================================================================
// CAD-SETTINGS.dcl - Static AutoCAD Core System Setup & Ergonomics Manager
// Part of Cad-Setup-v3 Horizontal Layered Architecture
// Layer: UI (Pure Dialog Control Language Specification)
// =============================================================================

cad_main_dialog : dialog {
  label = "AutoCAD Major Settings & Drafting Setup Manager";
  fixed_width = true;
  alignment = centered;
  : spacer { height = 1; }

  // Quick Drafting Presets & Industry Profiles
  : boxed_column {
    label = "Quick Drafting Presets & Industry Profiles";
    alignment = centered;

    // Row 1: Architectural & Structural
    : row {
      alignment = centered;
      : spacer { width = 1; }
      : button {
        key = "btn_arch_mm";
        label = "Architectural (mm)";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_arch_m";
        label = "Arch Masterplan (m)";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_arch";
        label = "Architectural (ft-in)";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_struct";
        label = "Structural (mm)";
        width = 22;
        fixed_width = true;
      }
      : spacer { width = 1; }
    }

    : spacer { height = 1; }

    // Row 2: Engineering & Schematics
    : row {
      alignment = centered;
      : spacer { width = 1; }
      : button {
        key = "btn_mech";
        label = "Mechanical (mm)";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_civil";
        label = "Civil / Survey (m)";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_elec";
        label = "Electrical / MEP";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_iso";
        label = "Isometric 2.5D";
        width = 22;
        fixed_width = true;
      }
      : spacer { width = 1; }
    }

    : spacer { height = 1; }

    // Row 3: Ergonomics & Utilities
    : row {
      alignment = centered;
      : spacer { width = 1; }
      : button {
        key = "btn_cross";
        label = "100% Pro Drafter";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_plot";
        label = "Plot / Present";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_default";
        label = "Factory Default";
        width = 22;
        fixed_width = true;
      }
      : button {
        key = "btn_reset_orig";
        label = "Current Drawing";
        width = 22;
        fixed_width = true;
      }
      : spacer { width = 1; }
    }
  }

  : spacer { height = 1; }

  // Main Three Columns Layout
  : row {
    // COLUMN 1: Units & Coordinates
    : column {
      width = 46;
      : boxed_column {
        label = "Units & Coordinate Formatting";

        : row {
          : text {
            label = "Linear Units:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_lunits";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Linear Precision:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_luprec";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Angular Units:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_aunits";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Angular Precision:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_auprec";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Insertion Scale:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_insunits";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Measurement Std:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_meas";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Angle Direction:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_angd";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Angle Base (0°):";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_angb";
            width = 25;
            fixed_width = true;
          }
        }
      }
    }

    // COLUMN 2: Cursor, Sizing & Real-Time Live Preview
    : column {
      width = 46;
      : boxed_column {
        label = "Interactive Cursor & Sizing Controls";

        // Crosshair Size
        : row {
          : text {
            label = "Crosshair (1-100%):";
            width = 18;
            alignment = left;
          }
          : edit_box {
            key = "eb_cur";
            edit_width = 4;
            fixed_width = true;
          }
          : spacer { width = 1; }
          : slider {
            key = "sld_cur";
            min_value = 1;
            max_value = 100;
            small_increment = 1;
            big_increment = 10;
            width = 18;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }

        // Pickbox Size
        : row {
          : text {
            label = "Pickbox (1-50 px):";
            width = 18;
            alignment = left;
          }
          : edit_box {
            key = "eb_pb";
            edit_width = 4;
            fixed_width = true;
          }
          : spacer { width = 1; }
          : slider {
            key = "sld_pb";
            min_value = 1;
            max_value = 50;
            small_increment = 1;
            big_increment = 5;
            width = 18;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }

        // Grip Size
        : row {
          : text {
            label = "Grip Size (1-50 px):";
            width = 18;
            alignment = left;
          }
          : edit_box {
            key = "eb_grp";
            edit_width = 4;
            fixed_width = true;
          }
          : spacer { width = 1; }
          : slider {
            key = "sld_grp";
            min_value = 1;
            max_value = 50;
            small_increment = 1;
            big_increment = 5;
            width = 18;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }

        // Aperture Size
        : row {
          : text {
            label = "Aperture (1-50 px):";
            width = 18;
            alignment = left;
          }
          : edit_box {
            key = "eb_ap";
            edit_width = 4;
            fixed_width = true;
          }
          : spacer { width = 1; }
          : slider {
            key = "sld_ap";
            min_value = 1;
            max_value = 50;
            small_increment = 1;
            big_increment = 5;
            width = 18;
            fixed_width = true;
          }
        }

        // Live Preview Canvas
        : spacer { height = 1; }
        : boxed_column {
          label = "Real-Time Visual Preview Canvas";
          alignment = centered;
          : image {
            key = "img_preview";
            width = 42;
            height = 9;
            fixed_width = true;
            fixed_height = true;
            color = 250;
            alignment = centered;
          }
          : spacer { height = 1; }
          : text {
            label = "White=Crosshair  |  Red=Pickbox  |  Green=Aperture  |  Cyan=Grips";
            alignment = centered;
          }
        }
      }
    }

    // COLUMN 3: Drafting Aids, Selection & Snapping
    : column {
      width = 46;
      : boxed_column {
        label = "Drafting Aids & Snapping";

        : row {
          : text {
            label = "Dynamic Input:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_dyn";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Selection Preview:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_sp";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Drag Dynamics:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_drag";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Snap Grid Style:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_snst";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Selection Mode:";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_selopt";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : row {
          : text {
            label = "Object Snap (F3):";
            width = 18;
            alignment = left;
          }
          : spacer { width = 1; }
          : popup_list {
            key = "pop_osm";
            width = 25;
            fixed_width = true;
          }
        }

        : spacer { height = 1; }
        : boxed_column {
          label = "Quick Status Toggles";
          : toggle {
            key = "tog_grid";
            label = "  Display Drawing Grid (F7 / GRIDMODE)";
          }
          : toggle {
            key = "tog_snap";
            label = "  Lock Cursor to Grid Snap (F9 / SNAPMODE)";
          }
          : toggle {
            key = "tog_ortho";
            label = "  Ortho Mode 90° Constraint (F8 / ORTHO)";
          }
          : toggle {
            key = "tog_lw";
            label = "  Display Entity Lineweights (LWDISPLAY)";
          }
          : toggle {
            key = "tog_mirr";
            label = "  Mirror Text Upside Down (MIRRTEXT)";
          }
        }
      }
    }
  }

  // Bottom Status / Hint Bar
  : spacer { height = 1; }
  : boxed_row {
    : text {
      key = "txt_status";
      width = 85;
      alignment = left;
    }
  }

  : spacer { height = 1; }

  // Bottom Action Buttons
  : row {
    alignment = right;
    : button {
      key = "btn_help";
      label = "&Help / Shortcuts";
      width = 18;
      fixed_width = true;
    }
    : spacer { width = 4; }
    : button {
      key = "btn_apply";
      label = "&Apply Now";
      width = 14;
      fixed_width = true;
    }
    : button {
      key = "accept";
      label = "&OK";
      is_default = true;
      width = 12;
      fixed_width = true;
    }
    : button {
      key = "cancel";
      label = "&Cancel";
      is_cancel = true;
      width = 12;
      fixed_width = true;
    }
  }
}

// Help & Reference Dialog
cad_help_diag : dialog {
  label = "AutoCAD Settings Reference Guide";
  fixed_width = true;
  alignment = centered;
  : boxed_column {
    label = "Presets & Industry Profiles";
    : text { label = "• Arch (mm)     : Standard ISO Architectural (0 mm layout, INSUNITS=mm)"; }
    : text { label = "• Masterplan (m) : Metric Masterplanning & Site Layout (0.000 m, INSUNITS=m)"; }
    : text { label = "• Arch (ft-in)  : Imperial 1/16\" (INSUNITS=Inches, Ortho ON, 100% Crosshair)"; }
    : text { label = "• Struct (mm)   : Structural Steel & RC Detailing (0.0 mm, Grips/LW ON)"; }
    : text { label = "• Mech (mm)     : Mechanical 0.00 mm with Tangent & Quadrant Snaps (OSMODE=439)"; }
    : text { label = "• Civil / Surv  : Metric Meters 0.000, DMS Angles, North Azimuth & Node Snaps (2735)"; }
    : text { label = "• Electrical/MEP: Schematics & Diagrams, Snap/Grid Aligned, Ortho ON"; }
    : text { label = "• Iso 2.5D      : Isometric 2.5D Snap Grid (SNAPSTYLE=1), Snap ON, Ortho ON"; }
    : text { label = "• 100% Drafter  : Full-screen crosshairs with tuned 4K ergonomics & visibility"; }
    : text { label = "• Plot / Present: Clean Presentation Mode (Lineweights ON, Grid/Snaps OFF)"; }
  }
  : spacer { height = 1; }
  : boxed_column {
    label = "System Information & Hotkeys";
    : text { label = "• F3  : Toggle Object Snap (OSMODE)"; }
    : text { label = "• F7  : Toggle Background Grid Display (GRIDMODE)"; }
    : text { label = "• F8  : Toggle Orthographic Axis Constraint (ORTHOMODE)"; }
    : text { label = "• F9  : Toggle Snap to Grid Mode (SNAPMODE)"; }
    : text { label = "• F12 : Dynamic Input & Heads-Up Dimensioning (DYNMODE)"; }
    : text { label = "• LWT : Lineweight Display in Viewport (LWDISPLAY)"; }
  }
  : spacer { height = 1; }
  : boxed_column {
    label = "Live Preview Canvas Color Legend";
    : text { label = "• White Lines : Crosshairs (CURSORSIZE)"; }
    : text { label = "• Red Box     : Selection Pickbox (PICKBOX)"; }
    : text { label = "• Green Box   : Osnap Target Aperture (APERTURE)"; }
    : text { label = "• Cyan Nodes  : Grips on Selected Entities (GRIPSIZE)"; }
    : text { label = "• Yellow Box  : Object Snap Marker Target Indicator"; }
  }
  : spacer { height = 1; }
  ok_only;
}
