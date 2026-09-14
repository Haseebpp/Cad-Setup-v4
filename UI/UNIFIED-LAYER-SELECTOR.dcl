// =============================================================================
// UNIFIED-LAYER-SELECTOR.dcl - Unified Dynamic Layer Selector (UDLS)
// Part of Cad-Setup-v4 Horizontal Layered Architecture
// Layer: UI (Pure Dialog Control Language Specification)
// Consolidates Line, Material, and Project Layer selection into one DCL.
// =============================================================================

// Common layout sub-assembly: Left column (layer list) and Right column (specifications)
layer_selector_body : row {
  // Left Column: Layers List
  : boxed_column {
    key = "box_layer_list";
    label = "Available Layers";
    width = 38;

    : list_box {
      key = "lst_layers";
      width = 36;
      height = 28;
      allow_accept = true;
    }
  }

  // Right Column: Specifications & Direct Property Editors
  : boxed_column {
    key = "box_layer_props";
    label = "Layer Specifications && Direct Property Editors";
    width = 58;

    : text {
      key = "txt_layer_name";
      label = "Layer: -";
      is_bold = true;
    }

    : spacer { height = 1; }

    // 1. Layer Production Properties Sub-Section
    : boxed_column {
      label = "Layer Production Properties";

      // Live Color Swatch & Edit Button
      : row {
        : image {
          key = "img_color_swatch";
          width = 12;
          height = 2;
          fixed_width = true;
          fixed_height = true;
          color = 250;
        }
        : column {
          : text {
            key = "txt_layer_color";
            label = "Color: -";
          }
          : button {
            key = "btn_edit_color";
            label = "Change Color...";
            width = 16;
          }
        }
      }

      : spacer { height = 1; }

      // Linetype Dropdown
      : popup_list {
        key = "pop_layer_ltype";
        label = "Linetype:       ";
        edit_width = 22;
      }

      // Lineweight Dropdown
      : popup_list {
        key = "pop_layer_lweight";
        label = "Lineweight:     ";
        edit_width = 22;
      }

      : spacer { height = 1; }

      // Transparency & Plottable in one row
      : row {
        : edit_box {
          key = "eb_layer_trans";
          label = "Transparency (0-90%):";
          edit_width = 6;
        }
        : spacer { width = 2; }
        : toggle {
          key = "tog_layer_plot";
          label = "Plottable Layer";
        }
      }
    }

    : spacer { height = 1; }

    // 2. Editable Hatch Specifications (Db_Layers)
    : boxed_column {
      key = "box_hatch_specs";
      label = "Associated Hatch Specs (In-Memory Session)";

      : row {
        : edit_box {
          key = "eb_hatch_pat";
          label = "Pattern:";
          edit_width = 10;
        }
        : edit_box {
          key = "eb_hatch_scl";
          label = "Scale:";
          edit_width = 6;
        }
        : edit_box {
          key = "eb_hatch_rot";
          label = "Angle (deg):";
          edit_width = 6;
        }
      }

      : spacer { height = 1; }

      : toggle {
        key = "tog_auto_hatch";
        label = "Smart Auto-Hatch (Fill boundaries with material pattern)";
      }
    }

    : spacer { height = 1; }

    // 3. Multi-line Description (Word-Wrapped across 3 lines)
    : boxed_column {
      label = "Layer Scope && Specification Description";

      : text {
        key = "txt_layer_desc1";
        label = "-";
        width = 54;
      }
      : text {
        key = "txt_layer_desc2";
        label = " ";
        width = 54;
      }
      : text {
        key = "txt_layer_desc3";
        label = " ";
        width = 54;
      }
    }

    : spacer { height = 1; }

    // Reset to DB Defaults Row
    : row {
      alignment = right;
      : button {
        key = "btn_reset_defaults";
        label = "Reset to DB Defaults";
        width = 22;
      }
    }
  }
}

// -----------------------------------------------------------------------------
// Definition A: Single-Tool Workflow Dialog (e.g. Line -> PL, Material -> REC)
// -----------------------------------------------------------------------------
unified_layer_selector_single : dialog {
  key = "diag_unified_single";
  label = "Layer Selection && Property Inspector";
  fixed_width = true;
  alignment = centered;
  width = 96;

  : spacer { height = 1; }

  layer_selector_body;

  : spacer { height = 1; }

  // Action Buttons
  : row {
    alignment = right;
    fixed_width = true;

    : button {
      key = "btn_draw";
      label = "Make Current and Draw";
      is_default = true;
      width = 25;
    }
    : button {
      key = "btn_current";
      label = "Make Current Only";
      width = 18;
    }
    : cancel_button {
      label = "Cancel";
      width = 12;
    }
  }
}

// -----------------------------------------------------------------------------
// Definition B: Dual-Tool Workflow Dialog (e.g. R-Layer -> PL or REC)
// -----------------------------------------------------------------------------
unified_layer_selector_dual : dialog {
  key = "diag_unified_dual";
  label = "Layer Selection && Property Inspector";
  fixed_width = true;
  alignment = centered;
  width = 96;

  : spacer { height = 1; }

  layer_selector_body;

  : spacer { height = 1; }

  // Action Buttons
  : row {
    alignment = right;
    fixed_width = true;

    : button {
      key = "btn_draw_pl";
      label = "Make Current and Draw (PL)";
      is_default = true;
      width = 25;
    }
    : button {
      key = "btn_draw_rec";
      label = "Make Current and Draw (REC)";
      width = 25;
    }
    : button {
      key = "btn_current";
      label = "Make Current Only";
      width = 18;
    }
    : cancel_button {
      label = "Cancel";
      width = 12;
    }
  }
}
