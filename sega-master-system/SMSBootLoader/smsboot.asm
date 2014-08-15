;------------------------------------------------------------------------------
; SMS Boot 0.91
; by Omar Cornut (Bock)
; Last updated 28 December 2003
;------------------------------------------------------------------------------

.INCLUDE "sms.inc"

; WlaDX stuffs ----------------------------------------------------------------
.EMPTYFILL $00
.SMSTAG
.COMPUTESMSCHECKSUM
.MEMORYMAP
        DEFAULTSLOT     0
        SLOTSIZE        PAGE_SIZE
        SLOT            0               PAGE_0
        SLOT            1               PAGE_1
.ENDME

.ROMBANKMAP
        BANKSTOTAL      2
        BANKSIZE        PAGE_SIZE
        BANKS           2
.ENDRO
.BANK 0 SLOT 0
;------------------------------------------------------------------------------

; Variables -------------------------------------------------------------------
.DEFINE VAR_frame_cnt           (RAM + $1000)    ; 1 byte
.DEFINE VAR_menu_slot           (RAM + $1001)    ; 1 byte
.DEFINE VAR_menu_sprite_y       (RAM + $1002)    ; 1 byte
;------------------------------------------------------------------------------

; Start -----------------------------------------------------------------------
.ORGA   $0000
        di
        im      1
        ld      sp,     $DFF0
        jp      start
;------------------------------------------------------------------------------

; Tools ---------------------------------------------------------------------
.ORGA   $0010
vdp_write_de:
                ld      a, e
                out     (VDP_ADDR), a
                ld      a, d
                out     (VDP_ADDR), a
                ret
.ORGA   $0018
vdp_write_addr_de:
                ld      a, e
                out     (VDP_ADDR), a
                ld      a, d
                or      $40
                out     (VDP_ADDR), a
                ret

.ORGA   $0028
vdp_write_addr_hl:
                ld      a, l
                out     (VDP_ADDR), a
                ld      a, h
                or      $40
                out     (VDP_ADDR), a
                ret

; Interrupt -------------------------------------------------------------------
.ORGA   $0038
interrupt:
        di
        push    af
        in      a, (VDP_STATUS)
        and     $80
        jr      z, interrupt_end
        ld      a, (VAR_frame_cnt)
        inc     a
        ld      (VAR_frame_cnt), a
interrupt_end:
        pop     af
        ei
        ret
;------------------------------------------------------------------------------

; NMI -------------------------------------------------------------------------
.ORGA   $0066
        reti
;------------------------------------------------------------------------------

; SDSC HEADER DATA ------------------------------------------------------------
sdsc_author:            .db     "Omar Cornut / Bock", 0
sdsc_program_name:      .db     "SMS Boot Loader", 0
sdsc_unused_but_stored: .db     "v0.91", 0
;------------------------------------------------------------------------------

; VDP Library -----------------------------------------------------------------
.INCLUDE "vdp.asm"
; DATA ------------------------------------------------------------------------
tiles_data:
.INCLUDE "tiles.inc"
palette_data:
.INCLUDE "palette.inc"
;------------------------------------------------------------------------------

start:
        call    vdp_init

	; Setup palette for fade start
        ld      a, 0
        ld      b, 5
        ld      hl, pal_table_bg_fade_0
        call    vdp_set_pal

	; Load tiles
        ld      bc, VRAM_TILE_SIZE * GFX_LAST_TILE
        ld      hl, tiles_data
        ld      de, $0000 + (1 * VRAM_TILE_SIZE)
        call    vdp_load_data

	; Draw SEGA logo to map
        ld      b, GFX_SEGA_SIZE_X
        ld      c, GFX_SEGA_SIZE_Y
        ld      d, GFX_SEGA_TILE
        ld      e, 0
        ld      hl, VRAM_BG_MAP + (10*2+(2)*32)
        call    vdp_bg_putimage

	; Draw Master System logo to map
        ld      b, GFX_MASTERSYSTEM_SIZE_X
        ld      c, GFX_MASTERSYSTEM_SIZE_Y
        ld      d, GFX_MASTERSYSTEM_TILE
        ld      e, 0
        ld      hl, VRAM_BG_MAP + (4*2+(12)*32)
        call    vdp_bg_putimage

	; Draw Boot Loader logo to map
        ld      b, GFX_BOOTLOADER_SIZE_X
        ld      c, GFX_BOOTLOADER_SIZE_Y
        ld      d, GFX_BOOTLOADER_TILE
        ld      e, 0
        ld      hl, VRAM_BG_MAP + (1*2+(22)*32)
        call    vdp_bg_putimage

	; Draw SMS Power copyright to map
        ld      b, GFX_SMSPOWER_SIZE_X
        ld      c, GFX_SMSPOWER_SIZE_Y
        ld      d, GFX_SMSPOWER_TILE - 256
        ld      e, 1
        ld      hl, VRAM_BG_MAP + (9*2+(42)*32)
        call    vdp_bg_putimage

	; Setup horizontal scrolling to +4
        ld      de, $8804
        rst     $10

	; Enable display, 16x8 sprites & vblank
        ld      de, $81E2
        rst     $10

	; Fade-in
        xor     a
        ld      b, 5
        ld      c, 4
        ld      d, 10
        ld      hl, pal_table_bg_fade_0
        ei
        call    vdp_fade
        di
        
        ; Enable display & 16x8 sprites, disable vblank
        ld      de, $81C2
        rst     $10

	; Setup final palette
        ld      a, 16
        ld      b, 16
        ld      hl, pal_table_fg
        call    vdp_set_pal

joy_loop_wait_press:
        in      a, (PORT_INPUT1)
        and     P1_BUTTON1
        jr      nz, joy_loop_wait_press

;------------------------------------------------------------------------------

	; Draw menu
menu_draw:
        ld      b, GFX_CHOICESBOX_SIZE_X
        ld      c, GFX_CHOICESBOX_SIZE_Y
        ld      d, GFX_CHOICESBOX_TILE - 256
        ld      e, 1|8
        ld      hl, VRAM_BG_MAP + (8*2+(16)*32)
        call    vdp_bg_putimage

        ; Initialize variables
        xor     a
        ld      (VAR_menu_slot), a
        ld      a, 71
        ld      (VAR_menu_sprite_y), a

	; Initialize hand sprites for the menu
	; We need two 16x8 sprites to make up the full hand
menu_init:
        ld      de, VRAM_SPR_MAP + $80
        rst     $18

	; Sprite 0: x=74, tile=gfx_hand_tile
        ld      a, 74
        push    ix
        pop     ix
        out     (VDP_DATA), a
        ld      a, GFX_HAND_TILE - 256
        push    ix
        pop     ix
        out     (VDP_DATA), a

	; Sprite 1; x=82, tile=gfx_hand_tile+2
        ld      a, 82
        push    ix
        pop     ix
        out     (VDP_DATA), a
        ld      a, GFX_HAND_TILE - 256 + 2
        push    ix
        pop     ix
        out     (VDP_DATA), a
        
        ; FIXME: I think the push ix/pop ix delay above
        ; are stupidly placed...

	; Copy following code to RAM before executing it
	; That is to allow hot cartridge swapping
copy_to_ram:
        ld      bc, boot_end - menu_refresh
        ld      de, RAM + $700
        ld      hl, menu_refresh
        ldir
        jp      RAM + $700

;------------------------------------------------------------------------------

	; From now on, we execute from RAM and should not access
	; to the original slot (cartridge, etc.)

menu_refresh:

        ; Setup sprites Y coordinates
        ld      de, VRAM_SPR_MAP
        rst     $18
        ld      a, (VAR_menu_sprite_y)
        push    ix
        pop     ix
        out     (VDP_DATA), a
        push    ix
        pop     ix
        out     (VDP_DATA), a
        
        ; Add end-of-sprites marker
        ; Note: this code could be moved in the initialization process,
        ; to save some RAM. Not that we really care, but it'll be better.
        ld      a, VRAM_SPR_LAST
        push    ix
        pop     ix
        out     (VDP_DATA), a

menu_loop:

	; Be sure that all keys are released
menu_loop_wait_unpress:
        in      a, (PORT_INPUT1)
        cpl
        and     P1_UP|P1_DOWN|P1_BUTTON1
        jr      nz, menu_loop_wait_unpress

	; Now wait some time
	; This is to avoid repetitions sometimes happening,
	; I think because the paddle may be unprecise and 'bounce' sometimes
	; In a normal situation, games would poll the paddle once a frame
	; and bouncing would not be noticed
        ld      bc, $0000
menu_loop_delay:
        djnz    menu_loop_delay

	; Process inputs
menu_loop_input:
        in      a, (PORT_INPUT1)
        bit     P1_UP_BIT, a
        jr      z, menu_up
        bit     P1_DOWN_BIT, a
        jr      z, menu_down
        bit     P1_BUTTON1_BIT, a
        jr      z, boot
        jr      menu_loop_input

	; Process UP input
menu_up:
        ld      a, (VAR_menu_slot)
        and     $FF
        jr      z, menu_loop
        dec     a
        ld      (VAR_menu_slot), a
        ld      a, (VAR_menu_sprite_y)
        sub     14
        ld      (VAR_menu_sprite_y), a
        jr      menu_refresh

	; Process DOWN input
menu_down:
        ld      a, (VAR_menu_slot)
        bit     1, a
        jr      nz, menu_loop
        inc     a
        ld      (VAR_menu_slot), a
        ld      a, (VAR_menu_sprite_y)
        add     a, 14
        ld      (VAR_menu_sprite_y), a
        jr      menu_refresh

; BOOT ------------------------------------------------------------------------

	; Setup register value to use to for choosen slot
boot:
        ld      a, (VAR_menu_slot)
        ld      b, $AB                  ; Cartridge
        and     $FF
        jr      z, boot_process
        ld      b, $CB                  ; Card
        dec     a
        jr      z, boot_process
        ld      b, $6B                  ; Expansion

boot_process:
	; Disable everything
        ld      a, $EB
        out     (PORT_HARDWARE), a
        
        ; Stick register value at $C000
        ld      a, b
        ld      ($C000), a
        
        ; Enable the given slot
        out     (PORT_HARDWARE), a
        
        ; Boot!
        jp      $0000

boot_end:

.BANK 1 SLOT 1

; SDSC HEADER -----------------------------------------------------------------
.ORGA   $7FE0
        .DB     "SDSC"                  ; Magic
        .DB     $00, $91                ; Version 0.91
        .DB     $12                     ; 17
        .DB     $11                     ; November
        .DW     $2001                   ; 2001
        .DW     sdsc_author
        .DW     sdsc_program_name
        .DW     $FFFF

; CHECKSUM --------------------------------------------------------------------
.ORGA   $7FF0

	.DB	"TMR SEGA"	; Trademark
        .DW     $0120           ; Year
	.DW	$0000		; Checksum not correct
	.DW	$0000		; Part Num not correct
        .DB     $01             ; Version
        .DB     $4C             ; Master System, 32k

