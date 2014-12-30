#!/usr/bin/perl -w
#
##############################################################################
# gen_carts.pl
#
# $Id: gen_carts.pl,v 1.4 2007/06/07 17:45:34 arnim Exp $
#
# Script to generate the carts.asm include file for the main menu application.
# Usage:
#   gen_carts.pl > carts.asm
#
# This script will print the assembler code to STDOUT and dumps the
# concatenated cartridge data to carts.bin
##############################################################################

use strict;


#               0123456789012
#                         1
my @rom_dat = (["            " , ""],
               ["            " , ""],
               ["P. Lander   " , "plander.bin"],
               ["AMOK        " , "amok.bin"],
               ["Bees        " , "bees.bin"],
               ["Frogger Br  " , "br_frogger.bin"],
               ["K.C. Munch  " , "kcmunch.bin"],
               ["K.C. Chase  " , "kcchase.bin"],
               ["Puzzle Piece" , "ppp.bin"],
               ["Q-Bert      " , "qbert.bin"],
               ["Turtles     " , "turtles.bin"],
               ["JG Munchkin " , "set/new_jg-munchkin.bin"],
               ["KC Pacman   " , "set/new_kc-pacman.bin"],
               ["KTAA        " , "set/new_ktaa.bin"],
               ["Race/Spin/Cr" , "set/vp_01.bin"],
               ["Pairs/Sp/Log" , "set/vp_02.bin"],
               ["Am Football " , "set/vp_03.bin"],
               ["AirSea/Battl" , "set/vp_04.bin"],
               ["Blackjack   " , "set/vp_05.bin"],
               ["Bowl/Basketb" , "set/vp_06.bin"],
               ["Comp Intro N" , "set/o2_06.bin"],
               ["Math/Echo  P" , "set/vp_07.bin"],
               ["Mtch/Bzz/Log" , "set/o2_07.bin"],
               ["Baseball    " , "set/vp_08.bin"],
               ["Comp Progr  " , "set/vp_09.bin"],
               ["Golf       P" , "set/vp_10.bin"],
               ["Golf       N" , "set/o2_10.bin"],
               ["Cosmic     P" , "set/vp_11.bin"],
               ["Cosmic     N" , "set/o2_11.bin"],
               ["Money + Run " , "set/vp_12.bin"],
               ["Plyschl Math" , "set/vp_13.bin"],
               ["Gunfighter P" , "set/vp_14.bin"],
               ["Invaders   N" , "set/o2_14.bin"],
               ["Samurai     " , "set/vp_15.bin"],
               ["Dpth/Marks P" , "set/vp_16.bin"],
               ["2100 AD    N" , "set/o2_16.bin"],
               ["Chinese Logi" , "set/vp_17.bin"],
               ["Laser War   " , "set/vp_18.bin"],
               ["Ctch/Nough P" , "set/vp_19.bin"],
               ["World/Heli N" , "set/o2_19.bin"],
               ["Stone Sling " , "set/vp_20.bin"],
               ["Pharaoh    P" , "set/vp_21.bin"],
               ["Dynasty    N" , "set/o2_21.bin"],
               ["Space Monst " , "set/vp_22.bin"],
               ["Las Vegas   " , "set/vp_23.bin"],
               ["Flipper Game" , "set/vp_24.bin"],
               ["Skiing      " , "set/vp_25.bin"],
               ["Basket Game " , "set/vp_26.bin"],
               ["Table Footbl" , "set/vp_27.bin"],
               ["Volleyball  " , "set/vp_28.bin"],
               ["Dam Buster  " , "set/vp_29.bin"],
               ["Battlefld  P" , "set/vp_30.bin"],
               ["UFO        N" , "set/o2_30.bin"],
               ["Musician    " , "set/vp_31.bin"],
               ["Laby/Supermd" , "set/vp_32.bin"],
               ["Acrobats   P" , "set/vp_33.bin"],
               ["Keyb Creat N" , "set/o2_33.bin"],
               ["Sat Attack  " , "set/vp_34.bin"],
               ["Billiards  P" , "set/vp_35.bin"],
               ["Munchkin   N" , "set/o2_35.bin"],
               ["Socc/Ice Hoc" , "set/vp_36.bin"],
               ["Monkeyshines" , "set/vp_37.bin"],
               ["Munchkin   P" , "set/vp_38.bin"],
               ["Sid Spellb  " , "set/o2_38.bin"],
               ["Fredm Fght P" , "set/vp_39.bin"],
               ["Nimb Ned   N" , "set/o2_39.bin"],
               ["4-in-1 Row P" , "set/vp_40.bin"],
               ["Type Tell  N" , "set/o2_40.bin"],
               ["Conq World P" , "set/vp_41.bin"],
               ["Smthereens N" , "set/o2_41.bin"],
               ["Quest F Ring" , "set/vp_42.bin"],
               ["Pick Axe   P" , "set/vp_43.bin"],
               ["Acrobats   N" , "set/o2_43.bin"],
               ["Crazy Chase " , "set/vp_44.bin"],
               ["Morse      P" , "set/vp_45.bin"],
               ["Timelord   N" , "set/o2_45.bin"],
               ["Wall Strt  P" , "set/vp_46.bin"],
               ["Turtles    N" , "set/o2_46.bin"],
               ["Mouse Cat  P" , "set/vp_47.bin"],
               ["Bees       N" , "set/o2_47.bin"],
               ["Backgammon P" , "set/vp_48.bin"],
               ["Powerlords N" , "set/o2_48.bin"],
               ["Turtles    P" , "set/vp_49.bin"],
               ["Super Bee  P" , "set/vp_50.bin"],
               ["Blobbers   P" , "set/vp_57.bin"],
               ["Blobbers   N" , "set/ntsc_57.bin"],
               ["Air Battle P" , "set/vp_58.bin"],
               ["            " , ""],
               ["            " , ""]);


my $rom_elem;
my $cart_2k_offset = 3;
my $cart_size_mask;
my $num_entries = 0;
my ($filename, $filelen);
my ($div, $mod);

`rm -f carts.bin; touch carts.bin`;

print <<EOF;
\torg\t0700h
EOF


foreach $rom_elem (@rom_dat) {
    if ($rom_elem->[1] ne "") {
        $filename = "../../roms/carts/$rom_elem->[1]";
        $filelen  = `ls -l $filename | awk '{print \$5}'`;
        chomp($filelen);

        # check whether a gap needs to be filled
        $div = $filelen / 2048;
        $mod = $filelen % 2048;
        #print("$filelen $div $mod\n");
        if ($mod > 0) {
            my $dd_cmd = "dd if=/dev/zero bs=".(2048-$mod)." count=1 >> carts.bin 2> /dev/null";
            #print("$dd_cmd\n");
            `$dd_cmd`;
            # and increment div value
            $div += 1;
        }

        if ($div == 6) {
            my ($idx_3k, $skip_3k);
            my $dd_cmd;
            print(STDERR "12k -> 16k: $filename\n");
            # expand 12k games to 16k
            for ($idx_3k = 0; $idx_3k <= 3; $idx_3k++) {
                $dd_cmd = "dd if=/dev/zero bs=1024 count=1 >> carts.bin 2> /dev/null";
                `$dd_cmd`;
                $skip_3k = $idx_3k * 3;
                $dd_cmd = "dd if=$filename bs=1024 count=3 skip=".$skip_3k." >> carts.bin 2> /dev/null";
                `$dd_cmd`;
            }
            $div = 8;
        } else {
            # append ROM to pile
            `cat $filename >> carts.bin`;
        }

        # if this cartridge consumes more than 4 2k blocks, set the size mask
        # to 3 and activate the 12k/16k flag
        $cart_size_mask = $div > 4 ? 0x83 : $div-1;
    } else {
        $div = 0;
        $mod = 0;
        $cart_size_mask = 0;
    }

    #$filelen = `ls -l carts.bin | awk '{print \$5}'`;
    #print(int($div)." $filelen\n");
    # output constant entry
    printf("\tmovp\ta, \@a\n");
    printf("\tret\n");
    printf("\tdb\t\"%s\", %03xh, %03xh\n",
           $rom_elem->[0], $cart_2k_offset, $cart_size_mask);
    $cart_2k_offset += int($div);

    $num_entries++;
}

printf("\n\t;; number of entries\n");
printf("num_entries\tequ\t%03xh\n", $num_entries);
