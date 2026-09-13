// =============================================================================
// LINE-SELECTOR.dcl - Line Layer Selector & Inspector Dialog
// Part of Cad-Setup-v4 Horizontal Layered Architecture
// Layer: UI (Pure Dialog Control Language Specification)
// =============================================================================

line_selector_dialog : dialog {
  label = "Line Layer Selection && Property Inspector";
  fixed_width = true;
  alignment = centered;
  width = 96;

  : spacer { height = 1; }

  : row {
    // Left Column: Line Layers List
    : boxed_column {
      label = "Available Line Layers (R-LINE-*)";
      width = 38;

      : list_box {
        key = "lst_lines";
        width = 36;
        height = 24;
        allow_accept = true;
      }
    }

    // Right Column: Properties, Linetype, and Description
    : boxed_column {
      label = "Line Specifications && Direct Property Editors";
      width = 58;

      : text {
        key = "txt_line_name";
        label = "Line Layer: -";
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
              key = "txt_line_color";
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
          key = "pop_line_ltype";
          label = "Linetype:       ";
          edit_width = 22;
        }

        // Lineweight Dropdown
        : popup_list {
          key = "pop_line_lweight";
          label = "Lineweight:     ";
          edit_width = 22;
        }

        : spacer { height = 1; }

        // Transparency & Plottable in one row
        : row {
          : edit_box {
            key = "eb_line_trans";
            label = "Transparency (0-90%):";
            edit_width = 6;
          }
          : spacer { width = 2; }
          : toggle {
            key = "tog_line_plot";
            label = "Plottable Layer";
          }
        }
      }

      : spacer { height = 1; }

      // 2. Multi-line Description (Word-Wrapped across 3 lines)
      : boxed_column {
        label = "Line Scope && Specification Description";

        : text {
          key = "txt_line_desc1";
          label = "-";
          width = 54;
        }
        : text {
          key = "txt_line_desc2";
          label = " ";
          width = 54;
        }
        : text {
          key = "txt_line_desc3";
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
      label = "Make Current and Draw (PL)";
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
