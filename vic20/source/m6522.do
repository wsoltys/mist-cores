onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Literal /m6522_tb/clk_gen_cnt
add wave -noupdate -format Logic /m6522_tb/via/reset_l
add wave -noupdate -format Logic /m6522_tb/via/p2_h
add wave -noupdate -format Logic /m6522_tb/via/clk_4
add wave -noupdate -format Literal /m6522_tb/via/phase
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal /m6522_tb/via/rs
add wave -noupdate -format Literal /m6522_tb/via/data_in
add wave -noupdate -format Literal /m6522_tb/via/data_out
add wave -noupdate -format Logic /m6522_tb/via/data_out_oe_l
add wave -noupdate -format Logic /m6522_tb/via/rw_l
add wave -noupdate -format Logic /m6522_tb/via/cs1
add wave -noupdate -format Logic /m6522_tb/via/cs2_l
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal /m6522_tb/via/t1c
add wave -noupdate -format Logic /m6522_tb/via/t1c_active
add wave -noupdate -format Logic /m6522_tb/via/t1c_done
add wave -noupdate -format Logic /m6522_tb/via/t1_load_counter
add wave -noupdate -format Logic /m6522_tb/via/t1_reload_counter
add wave -noupdate -format Logic /m6522_tb/via/t1_irq
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal /m6522_tb/via/t2c
add wave -noupdate -format Logic /m6522_tb/via/t2_irq
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal /m6522_tb/sr
add wave -noupdate -format Logic /m6522_tb/via/cb2_in
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal /m6522_tb/via/sr_cnt
add wave -noupdate -format Literal /m6522_tb/via/r_sr
add wave -noupdate -format Logic /m6522_tb/via/sr_cb1_oe_l
add wave -noupdate -format Logic /m6522_tb/via/sr_cb1_out
add wave -noupdate -format Logic /m6522_tb/via/sr_drive_cb2
add wave -noupdate -format Logic /m6522_tb/via/cb1_out
add wave -noupdate -format Logic /m6522_tb/via/sr_strobe_rising
add wave -noupdate -format Logic /m6522_tb/via/sr_strobe_falling
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Literal /m6522_tb/via/r_ier
add wave -noupdate -format Literal /m6522_tb/via/r_acr
add wave -noupdate -format Logic /m6522_tb/irq_l
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Logic /m6522_tb/via/w_ora_hs
add wave -noupdate -format Logic /m6522_tb/via/r_ira_hs
add wave -noupdate -format Literal /m6522_tb/via/r_pcr
add wave -noupdate -format Logic /m6522_tb/via/ca1_int
add wave -noupdate -format Logic /m6522_tb/via/cb1_int
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Logic /m6522_tb/ca1_in
add wave -noupdate -format Logic /m6522_tb/ca2_in
add wave -noupdate -format Logic /m6522_tb/ca2_out
add wave -noupdate -format Logic /m6522_tb/ca2_out_oe_l
add wave -noupdate -format Literal /m6522_tb/pa_in
add wave -noupdate -format Literal /m6522_tb/pa_out
add wave -noupdate -format Literal /m6522_tb/pa_out_oe_l
add wave -noupdate -divider {New Divider}
add wave -noupdate -format Logic /m6522_tb/cb1_in
add wave -noupdate -format Logic /m6522_tb/cb1_out
add wave -noupdate -format Logic /m6522_tb/cb1_out_oe_l
add wave -noupdate -format Logic /m6522_tb/cb2_in
add wave -noupdate -format Logic /m6522_tb/cb2_out
add wave -noupdate -format Logic /m6522_tb/cb2_out_oe_l
add wave -noupdate -format Literal /m6522_tb/pb_in
add wave -noupdate -format Literal /m6522_tb/pb_out
add wave -noupdate -format Literal /m6522_tb/pb_out_oe_l
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {177312 ns}
WaveRestoreZoom {0 ns} {4402176 ns}
configure wave -namecolwidth 150
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
