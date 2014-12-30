-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: zefant_xs3_pll-c.vhd,v 1.3 2007/02/05 21:55:18 arnim Exp $
--
-------------------------------------------------------------------------------

library unisim;

configuration zefant_xs3_pll_struct_c0 of zefant_xs3_pll is

  for struct
    for all: IBUFG
      use configuration unisim.ibufg_rtl_c0;
    end for;

    for all: DCM
      use configuration unisim.dcm_behav_c0;
    end for;

    for all: BUFG
      use configuration unisim.bufg_rtl_c0;
    end for;

    for all: BUFGCE
      use configuration unisim.bufgce_rtl_c0;
    end for;
  end for;

end zefant_xs3_pll_struct_c0;
