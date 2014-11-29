@echo off
set rom_path=roms\

romgen %rom_path%basic.bin VIC20_BASIC_ROM 13 a r > %rom_path%vic20_basic.vhd
romgen %rom_path%kernal.bin VIC20_KERNAL_ROM 13 a r > %rom_path%vic20_kernal.vhd
romgen %rom_path%characters.bin VIC20_CHAR_ROM 13 a r > %rom_path%vic20_chars.vhd
romgen %rom_path%game.bin VIC20_GAME_ROM 13 a r > %rom_path%vic20_game.vhd

echo done
pause
