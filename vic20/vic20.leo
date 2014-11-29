clean_all

set process 6
set part v300efg456
set wire_table xcve300-6_avg
set bubble_tristates FALSE

set register2register 100
set input2register 20
set register2output 20
set input2output 100

set report brief
set novendor_constraint_file TRUE
set virtex_map_iob_registers TRUE

set target xcve
load_library xcve

analyze pkg_vic20_xilinx_prims.vhd
analyze pkg_vic20.vhd
analyze t65_pack.vhd
analyze t65_mcode.vhd
analyze t65_alu.vhd
analyze t65.vhd
analyze vic20_chars.vhd
analyze vic20_rams.vhd
analyze m6522.vhd
analyze vic20_vic.vhd
analyze ps2kbd.vhd
analyze vic20_ps2_if.vhd
analyze vic20.vhd
elaborate vic20

pre_optimize
optimize -ta xcve -effort standard -delay -chip -hier preserve
auto_write -format EDIF vic20.edf

