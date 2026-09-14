// =============================================================================
// BLOCK-PALETTE.dcl - Visual Standard Joinery Blocks & Components Palette
// Part of Cad-Setup-v4 Horizontal Layered Architecture
// Layer: UI (Pure Dialog Control Language Specification)
// =============================================================================

block_palette_dialog : dialog {
  label = "Standard Joinery Blocks & Components Palette";
  fixed_width = true;
  alignment = centered;
  width = 96;

  : spacer { height = 1; }

  : row {
    // Left Column: Category Selector & Component List
    : boxed_column {
      label = "Component Library";
      width = 40;

      : popup_list {
        key = "pop_category";
        label = "Category: ";
        edit_width = 24;
      }

      : spacer { height = 1; }

      : list_box {
        key = "lst_blocks";
        label = "Available Components:";
        width = 38;
        height = 24;
        allow_accept = true;
      }
    }

    // Right Column: Specs Card, Live 2D Vector Preview & Placement Options
    : boxed_column {
      label = "Component Details & Visual Preview";
      width = 54;

      // 1. Component Metadata Card
      : boxed_column {
        label = "Component Specifications";

        : text {
          key = "txt_blk_name";
          label = "Component: -";
          is_bold = true;
        }
        : text {
          key = "txt_blk_cat";
          label = "Category: -";
        }
        : text {
          key = "txt_blk_layer";
          label = "Target Layer: -";
        }
        : text {
          key = "txt_blk_specs";
          label = "Dimensions: -";
        }
        : text {
          key = "txt_blk_desc";
          label = "Description: -";
          width = 50;
        }
      }

      : spacer { height = 1; }

      // 2. Interactive 2D Vector Wireframe Preview Tile
      : boxed_column {
        label = "Real-Time 2D Vector Wireframe Preview";

        : image {
          key = "img_preview";
          width = 50;
          height = 12;
          fixed_width = true;
          fixed_height = true;
          color = 250;
        }

        : text {
          key = "txt_preview_note";
          label = "Crosshairs (+): Insertion Base Point (0,0)";
        }
      }

      : spacer { height = 1; }

      // 3. Insertion Placement Parameters
      : boxed_column {
        label = "Insertion Parameters";

        : row {
          : edit_box {
            key = "eb_scale";
            label = "Scale:";
            edit_width = 6;
          }
          : spacer { width = 2; }
          : edit_box {
            key = "eb_rotation";
            label = "Angle (deg):";
            edit_width = 6;
          }
        }

        : spacer { height = 1; }

        : row {
          alignment = centered;
          : text { label = "Quick Rotation:"; }
          : button { key = "btn_rot_0";   label = "0°";   width = 5; }
          : button { key = "btn_rot_90";  label = "90°";  width = 5; }
          : button { key = "btn_rot_180"; label = "180°"; width = 5; }
          : button { key = "btn_rot_270"; label = "270°"; width = 5; }
        }

        : spacer { height = 1; }

        : row {
          : toggle {
            key = "tog_smart_layer";
            label = "Smart Auto-Layer";
          }
          : spacer { width = 2; }
          : toggle {
            key = "tog_continuous";
            label = "Continuous Placement";
          }
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
      key = "btn_insert";
      label = "Insert Component";
      is_default = true;
      width = 20;
    }
    : cancel_button {
      label = "Close";
      width = 12;
    }
  }
}
