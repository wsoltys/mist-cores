-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: zefant_xs3_vp-c.vhd,v 1.6 2007/03/21 21:09:44 arnim Exp $
--
-------------------------------------------------------------------------------

configuration zefant_xs3_vp_struct_c0 of zefant_xs3_vp is

  for struct
    for por_b: vp_por
      use configuration work.vp_por_rtl_c0;
    end for;

    for pll_b: zefant_xs3_pll
      use configuration work.zefant_xs3_pll_struct_c0;
    end for;

    for vp_console_b: vp_console
      use configuration work.vp_console_struct_c0;
    end for;

    for use_mc
      for mc_ctrl_b: mc_ctrl
        use configuration work.mc_ctrl_rtl_c0;
      end for;
    end for;

    for dblscan_b: dblscan
      use configuration work.dblscan_rtl_c0;
    end for;

    for aud_bit_clk_ibufg_b: IBUFG
      use configuration unisim.ibufg_rtl_c0;
    end for;

    for pcm_sound_b: pcm_sound
      use configuration work.pcm_sound_rtl_0;
    end for;

    for snespads_b: snespad
      use configuration work.snespad_struct_c0;
    end for;

    for vp_keymap_b: vp_keymap
      use configuration work.vp_keymap_rtl_c0;
    end for;

    for ps2_keyboard_b: ps2_keyboard_interface
      use configuration work.ps2_keyboard_interface_my_ps2_keyboard_c0;
    end for;
  end for;

end zefant_xs3_vp_struct_c0;
