-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: vp_console-c.vhd,v 1.3 2007/03/17 15:09:11 arnim Exp $
--
-------------------------------------------------------------------------------

configuration vp_console_struct_c0 of vp_console is

  for struct

    for t8048_b: t8048_notri
      use configuration work.t8048_notri_struct_c0;
    end for;

    for glue_b: vp_glue
      use configuration work.vp_glue_rtl_c0;
    end for;

    for vdc_b: i8244_top_sync
      use configuration work.i8244_top_sync_struct_c0;
    end for;

    for ram_128_b: generic_ram_ena
      use configuration work.generic_ram_ena_rtl_c0;
    end for;

  end for;

end vp_console_struct_c0;
