
configuration ps2_keyboard_interface_my_ps2_keyboard_c0 of ps2_keyboard_interface is

  for my_ps2_keyboard

    for all: key_slice
      use configuration work.key_slice_key_rom_c0;
    end for;

  end for;

end ps2_keyboard_interface_my_ps2_keyboard_c0;
