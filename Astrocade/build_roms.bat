@echo off
set rom_path=roms\

romgen %rom_path%bios3159_0.bin BALLY_BIOS_0 12 l r e > %rom_path%bally_bios_0.vhd
romgen %rom_path%bios3159_1.bin BALLY_BIOS_1 12 l r e > %rom_path%bally_bios_1.vhd

romgen %rom_path%balcheck.bin BALLY_CHECK 11 l r e > %rom_path%bally_check.vhd

echo done
