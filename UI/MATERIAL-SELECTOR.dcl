// =============================================================================
// MATERIAL-SELECTOR.dcl - Material Layer Selector & Inspector Dialog
// Part of Cad-Setup-v4 Horizontal Layered Architecture
// Layer: UI (Pure Dialog Control Language Specification)
// =============================================================================

material_selector_dialog : dialog {
  label = "Material Selection && Property Inspector";
  fixed_width = true;
  alignment = centered;
  width = 96;

  : spacer { height = 1; }

  : row {
    // Left Column: Materials List
    : boxed_column {
      label = "Available Materials (R-MAT-*)";
      width = 38;

      : list_box {
        key = "lst_materials";
        width = 36;
        height = 28;
        allow_accept = true;
      }
    }

    // Right Column: Properties, Hatch, and Description
    : boxed_column {
      label = "Material Specifications && Direct Property Editors";
      width = 58;

      : text {
        key = "txt_mat_name";
        label = "Material: -";
        is_bold = true;
      }

      : spacer { height = 1; }

      // 1. Layer Properties Sub-Section
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
              key = "txt_mat_color";
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
          key = "pop_mat_ltype";
          label = "Linetype:       ";
          edit_width = 22;
        }

        // Lineweight Dropdown
        : popup_list {
          key = "pop_mat_lweight";
          label = "Lineweight:     ";
          edit_width = 22;
        }

        : spacer { height = 1; }

        // Transparency & Plottable in one row
        : row {
          : edit_box {
            key = "eb_mat_trans";
            label = "Transparency (0-90%):";
            edit_width = 6;
          }
          : spacer { width = 2; }
          : toggle {
            key = "tog_mat_plot";
            label = "Plottable Layer";
          }
        }
      }

      : spacer { height = 1; }

      // 2. Editable Hatch Specifications (Db_Layers)
      : boxed_column {
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
      }

      : spacer { height = 1; }

      // 3. Multi-line Description (Word-Wrapped across 3 lines)
      : boxed_column {
        label = "Material Description && Finish Scope";

        : text {
          key = "txt_mat_desc1";
          label = "-";
          width = 54;
        }
        : text {
          key = "txt_mat_desc2";
          label = " ";
          width = 54;
        }
        : text {
          key = "txt_mat_desc3";
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

  : spacer { height = 1; }

  // Action Buttons
  : row {
    alignment = right;
    fixed_width = true;

    : button {
      key = "btn_draw";
      label = "Make Current and Draw (REC)";
      is_default = true;
      width = 24;
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
