#!/bin/bash
QUARTUS_EXEC="wine c:\\altera\\70\\quartus\\bin"

$QUARTUS_EXEC\\quartus_map --read_settings_files=on --write_settings_files=off jop_vp -c jop_vp

$QUARTUS_EXEC\\quartus_fit --read_settings_files=off --write_settings_files=off jop_vp -c jop_vp

#$QUARTUS_EXEC\\quartus_cdb jop_vp -c jop_vp --update_mif

$QUARTUS_EXEC\\quartus_asm --read_settings_files=off --write_settings_files=off jop_vp -c jop_vp

#$QUARTUS_EXEC\\quartus_tan --read_settings_files=off --write_settings_files=off jop_vp -c jop_vp --timing_analysis_only
