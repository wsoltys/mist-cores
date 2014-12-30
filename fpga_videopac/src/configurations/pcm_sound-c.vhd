-------------------------------------------------------------------------------
--
-- FPGA Videopac
--
-- $Id: pcm_sound-c.vhd,v 1.2 2007/02/05 21:55:18 arnim Exp $
--
-------------------------------------------------------------------------------

configuration pcm_sound_rtl_0 of pcm_sound is

  for rtl

    for ac97_top_b: ac97_top
      use configuration work.ac97_top_verilog_c0;
    end for;

  end for;

end pcm_sound_rtl_0;
