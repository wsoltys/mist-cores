-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: dblscan-c.vhd,v 1.1 2007/01/05 21:55:11 arnim Exp $
--
-------------------------------------------------------------------------------

configuration dblscan_rtl_c0 of dblscan is

  for rtl

    for all: dpram
      use configuration work.dpram_rtl_c0;
    end for;

  end for;

end dblscan_rtl_c0;
