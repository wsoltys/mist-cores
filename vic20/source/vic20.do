onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Logic /vic20_tb/u0/reset_l
add wave -noupdate -format Logic /vic20_tb/u0/clk_40
add wave -noupdate -format Logic /vic20_tb/u0/ena_1mhz
add wave -noupdate -format Logic /vic20_tb/u0/clk_4
add wave -noupdate -format Literal /vic20_tb/u0/c_addr
add wave -noupdate -format Literal /vic20_tb/u0/c_din
add wave -noupdate -format Literal /vic20_tb/u0/c_dout
add wave -noupdate -format Logic /vic20_tb/u0/c_rw_l
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Logic /vic20_tb/u0/p2_h
add wave -noupdate -format Literal /vic20_tb/u0/io_sel_l
add wave -noupdate -format Literal /vic20_tb/u0/blk_sel_l
add wave -noupdate -format Literal /vic20_tb/u0/ram_sel_l
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal /vic20_tb/u0/v_addr
add wave -noupdate -format Logic /vic20_tb/u0/v_rw_l
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Logic /vic20_tb/u0/vic/p2_h_int
add wave -noupdate -format Literal /vic20_tb/u0/vic/r_x_offset
add wave -noupdate -format Literal /vic20_tb/u0/vic/r_y_offset
add wave -noupdate -format Literal /vic20_tb/u0/vic/r_num_cols
add wave -noupdate -format Literal /vic20_tb/u0/vic/r_num_rows
add wave -noupdate -format Literal /vic20_tb/u0/vic/hcnt
add wave -noupdate -format Literal /vic20_tb/u0/vic/vcnt
add wave -noupdate -format Logic /vic20_tb/u0/vic/hblank
add wave -noupdate -format Logic /vic20_tb/u0/vic/vblank
add wave -noupdate -format Logic /vic20_tb/u0/vic/hsync
add wave -noupdate -format Logic /vic20_tb/u0/vic/vsync
add wave -noupdate -format Logic /vic20_tb/u0/vic/start_h
add wave -noupdate -format Literal /vic20_tb/u0/vic/h_char_cnt
add wave -noupdate -format Logic /vic20_tb/u0/vic/start_v
add wave -noupdate -format Literal /vic20_tb/u0/vic/v_char_cnt
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Logic /vic20_tb/u0/vic/char_load
add wave -noupdate -format Literal /vic20_tb/u0/vic/matrix_cnt
add wave -noupdate -format Literal /vic20_tb/u0/vic/addr_out
add wave -noupdate -format Literal /vic20_tb/u0/v_addr
add wave -noupdate -format Literal /vic20_tb/u0/v_data_read_mux
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {16623463 ns}
WaveRestoreZoom {16619083 ns} {16629463 ns}
configure wave -namecolwidth 170
configure wave -valuecolwidth 80
configure wave -justifyvalue left
configure wave -signalnamewidth 2
configure wave -snapdistance 10
configure wave -datasetprefix 0
