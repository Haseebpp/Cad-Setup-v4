// =============================================================================
// GRID-GENERATOR.dcl - Structural & Architectural Grid System Generator
// Part of Cad-Setup-v4 Horizontal Layered Architecture
// Layer: UI (Pure Dialog Control Language Specification)
// =============================================================================

grid_system_dialog : dialog {
  label = "Structural & Architectural Grid System Generator (GL)";
  fixed_width = true;
  alignment = centered;
  width = 66;

  : spacer { height = 1; }

  // 1. Grid Bay Spacings (X & Y Axes)
  : boxed_column {
    label = "Grid Bay Spacings & Axis Labels";
    
    : text { 
      label = "Syntax: Uniform multiplier (e.g. 4*6000) or comma list (e.g. 6000, 5000, 7500)"; 
    }
    : spacer { height = 1; }

    : row {
      : edit_box {
        key = "eb_x_spacings";
        label = "X-Axis Bays (Vertical Lines):";
        edit_width = 28;
      }
      : edit_box {
        key = "eb_x_tag";
        label = "Start Tag:";
        edit_width = 8;
      }
    }

    : row {
      : edit_box {
        key = "eb_y_spacings";
        label = "Y-Axis Bays (Horizontal Lines):";
        edit_width = 28;
      }
      : edit_box {
        key = "eb_y_tag";
        label = "Start Tag:";
        edit_width = 8;
      }
    }
  }

  : spacer { height = 1; }

  // 2. Bubble & Geometry Extents
  : boxed_column {
    label = "Grid Bubbles & Geometry Extents";

    : row {
      : popup_list {
        key = "pop_bubble_pos";
        label = "Bubble Placement:";
        width = 26;
      }
      : edit_box {
        key = "eb_bubble_rad";
        label = "Bubble Radius:";
        edit_width = 10;
      }
    }

    : row {
      : edit_box {
        key = "eb_grid_ext";
        label = "Line Extension (Overshoot):";
        edit_width = 10;
      }
      : edit_box {
        key = "eb_text_height";
        label = "Text Height:";
        edit_width = 10;
      }
    }
  }

  : spacer { height = 1; }

  // 3. Automated Dimensions
  : boxed_column {
    label = "Automated Dimensions";

    : row {
      : toggle {
        key = "tog_bay_dims";
        label = "Generate Bay-to-Bay Dimensions";
      }
      : toggle {
        key = "tog_total_dims";
        label = "Generate Overall Dimensions";
      }
    }

    : row {
      : edit_box {
        key = "eb_dim_offset";
        label = "Dimension Line Offset:";
        edit_width = 12;
      }
      : spacer { width = 10; }
    }
  }

  : spacer { height = 1; }

  // 4. Layer Allocation Notice
  : boxed_row {
    : text {
      label = "Layers: Lines -> 03-GRID-LINE | Bubbles/Tags -> R-ANNO-SYMB | Dims -> R-ANNO-DIMS";
    }
  }

  : spacer { height = 1; }

  // 5. Action Buttons
  : row {
    alignment = right;

    : button {
      key = "btn_single";
      label = "Single &Line Mode";
      width = 18;
      fixed_width = true;
    }
    : spacer { width = 2; }
    : button {
      key = "accept";
      label = "&Insert Grid";
      is_default = true;
      width = 15;
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
