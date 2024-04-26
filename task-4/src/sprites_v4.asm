.include "constants.inc"
.include "header.inc"

.segment "ZEROPAGE"
tick_count: .res 1
player_x: .res 1
player_y: .res 1
player_dir: .res 1
player_vertical_dir: .res 1
state: .res 1
pad1: .res 1
bg_x: .res 1
bg_y: .res 1
scroll: .res 1
level_select: .res 1

.exportzp player_x, player_y, pad1, tick_count, state, bg_x, bg_y, scroll, level_select

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.import read_controller1

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  INC scroll       ; add one to our scroll variable each frame
  LDA scroll
  STA $2005        ; write the horizontal scroll count register

  LDA #$00         ; no vertical scrolling
  STA $2005

  ;read controller
  JSR read_controller1
  ;update tiles *after* DMA transfer
  JSR update_player
  ;clear all current sprites
  JSR clear_sprites
  ;JSR draw_player_right_running2
  JSR update_tick_count

  RTI
.endproc

.import reset_handler

.export main
.proc main
  ; write a palette
  LDX PPUSTATUS
  LDX #$3f    ;this number is the address of the first color of the first pallete
  STX PPUADDR
  LDX #$00
  STX PPUADDR

load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK
forever:
  JMP forever
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

palettes:
.byte $0f, $2d, $10, $37
.byte $0f, $2d, $14, $37
.byte $0f, $2d, $14, $37
.byte $0f, $2d, $14, $37

.byte $0f, $2d, $14, $37
.byte $0f, $2d, $14, $37
.byte $0f, $2d, $14, $37
.byte $0f, $2d, $14, $37

sprites:
;Y, Sprite, Pallet, X
;Front still
;top left
.byte $40, $09, $00, $60
;top right
.byte $40, $0A, $00, $68
;bottom left
.byte $48, $19, $00, $60
;bottom right
.byte $48, $1A, $00, $68


background:
;start of name table 1 (top left)  at adress $2000
    .byte $00,$00,$00,$00
    .byte $ff,$ff,$ff,$ff
    .byte $c0,$00,$00,$03
    .byte $c2,$aa,$2a,$ab
    .byte $c0,$00,$00,$0b
    .byte $c2,$aa,$2a,$8b
    .byte $c2,$02,$00,$ab
    .byte $c0,$02,$a8,$80
    .byte $c0,$00,$08,$80
    .byte $c2,$aa,$08,$80
    .byte $c2,$02,$08,$8b
    .byte $c0,$22,$68,$0b
    .byte $c2,$aa,$69,$5b
    .byte $c0,$00,$00,$03
    .byte $ff,$ff,$ff,$ff
  ;end of nametable 1

background2:
    .byte $00,$00,$00,$00
    .byte $ff,$ff,$ff,$ff
    .byte $c0,$00,$00,$0b
    .byte $c0,$00,$20,$83
    .byte $c0,$aa,$a2,$ab
    .byte $c0,$02,$00,$03
    .byte $c1,$82,$00,$03
    .byte $01,$82,$26,$03
    .byte $01,$82,$22,$a3
    .byte $01,$82,$20,$27
    .byte $d6,$aa,$2a,$2b
    .byte $c2,$10,$02,$03
    .byte $c2,$2a,$2a,$a3
    .byte $c0,$02,$00,$20
    .byte $ff,$ff,$ff,$ff; end of name table 2

background3:
    .byte $00,$00,$00,$00
    .byte $ff,$ff,$ff,$ff
    .byte $c0,$00,$00,$03
    .byte $ea,$2a,$8a,$23
    .byte $c2,$25,$42,$63
    .byte $ca,$29,$62,$6b
    .byte $c8,$25,$60,$0b
    .byte $c0,$a5,$6a,$84
    .byte $c0,$90,$41,$84
    .byte $c0,$9a,$62,$84
    .byte $c0,$10,$41,$ab
    .byte $c2,$99,$aa,$a7
    .byte $ca,$09,$55,$67
    .byte $c0,$29,$55,$57
    .byte $ff,$ff,$ff,$ff

background4:
    .byte $00,$00,$00,$00
    .byte $ff,$ff,$ff,$ff
    .byte $d5,$55,$55,$73
    .byte $da,$a9,$aa,$73
    .byte $d9,$59,$56,$73
    .byte $d5,$a9,$a6,$73
    .byte $d5,$55,$96,$5f
    .byte $05,$95,$96,$73
    .byte $05,$aa,$56,$73
    .byte $05,$55,$6a,$73
    .byte $ca,$6a,$a5,$73
    .byte $c9,$55,$56,$b3
    .byte $ca,$aa,$aa,$73
    .byte $c0,$00,$00,$73
    .byte $ff,$ff,$ff,$ff

.proc load_background
  ;TEST-> turn off bg and NMI
  LDA #$00  ;     turn off bg
  STA PPUMASK
  LDA #%00010000   ; turn off NMIs, sprites use first pattern table
  STA PPUCTRL
  ;END TEST

  LDA PPUSTATUS         ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0

  STX bg_x
  JSR draw_background

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc load_background2
  LDA #$00  ;     turn off bg
  STA PPUMASK
  LDA #%00010000   ; turn off NMIs, sprites use first pattern table
  STA PPUCTRL

  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$24
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0

  STX bg_x
  JSR draw_background2

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc load_background3
  ;TEST-> turn off bg and NMI
  LDA #$00  ;     turn off bg
  STA PPUMASK
  LDA #%00010000   ; turn off NMIs, sprites use first pattern table
  STA PPUCTRL
  ;END TEST

  LDA PPUSTATUS         ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0

  STX bg_x
  JSR draw_background3

  ;TEST
  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc load_background4
  LDA #$00  ;     turn off bg
  STA PPUMASK
  LDA #%00010000   ; turn off NMIs, sprites use first pattern table
  STA PPUCTRL

  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$24
  ;LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0

  STX bg_x
  JSR draw_background4

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc draw_background
  LDY #$00
	JSR draw_row
	LDY #$00
	JSR draw_row
  INC bg_x
	INC bg_x
	INC bg_x
	INC bg_x  ; Increment x by 4
  LDA bg_x
	CMP #$3C;#$78 #$B4;#$3C  ; Value = 60
	BNE draw_background
	RTS
.endproc

.proc draw_row
  LDA bg_x  ; A contains x which is the base row address
  STY bg_y
  CLC
	ADC bg_y  ; bg_y contains the row offset, A = bg_y + bg_x
  TAX       ; x now contains the byte position in A
	JSR draw_byte
  LDY bg_y  ;test
	INY
	CPY #$04  ; Compare if y has completed the 4 bytes
	BNE draw_row
	RTS
.endproc

.proc draw_byte
  ; get the first sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  LDY background, x ;TEST
  TYA ;TEST
	AND #%11000000
  LSR A
	LSR A
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  ; get the second sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA 
	AND #%00110000
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START   ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the third sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA 
	AND #%00001100
	LSR A
	LSR A ; Sprite value stored in A (need to map to actual address)
  CLC
	ADC #BG_START   ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the fourth sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA 
	AND #%00000011
  CLC
	ADC #BG_START   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  RTS
.endproc

.proc draw_background2
  LDY #$00
	JSR draw_row2
	LDY #$00
	JSR draw_row2
  INC bg_x
	INC bg_x
	INC bg_x
	INC bg_x  ; Increment x by 4
  LDA bg_x
	CMP #$3C ; Value = 60
	BNE draw_background2
	RTS
.endproc

.proc draw_row2
  LDA bg_x  ; A contains x which is the base row address
  STY bg_y
  CLC
	ADC bg_y  ; bg_y contains the row offset, A = bg_y + bg_x
  TAX       ; x now contains the byte position in A
	JSR draw_byte2
  LDY bg_y  ;test
	INY
	CPY #$04  ; Compare if y has completed the 4 bytes
	BNE draw_row2
	RTS
.endproc

.proc draw_byte2    
  ; get the first sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  LDY background2, x 
  TYA ;TEST
	AND #%11000000
  LSR A
	LSR A
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  ; get the second sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA
	AND #%00110000
; Sprite value stored in A (need to map to actual address)
  ;TEST
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START   ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the third sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA
	AND #%00001100
	LSR A
	LSR A ; Sprite value stored in A (need to map to actual address)
  CLC
	ADC #BG_START  ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the fourth sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA 
	AND #%00000011
  CLC
	ADC #BG_START   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  RTS
.endproc

.proc draw_background3
  LDY #$00
	JSR draw_row3
	LDY #$00
	JSR draw_row3
  INC bg_x
	INC bg_x
	INC bg_x
	INC bg_x  ; Increment x by 4
  LDA bg_x
	CMP #$3C ; Value = 60
	BNE draw_background3
	RTS
.endproc

.proc draw_row3
  LDA bg_x  ; A contains x which is the base row address
  STY bg_y
  CLC
	ADC bg_y  ; bg_y contains the row offset, A = bg_y + bg_x
  TAX       ; x now contains the byte position in A
	JSR draw_byte3
  LDY bg_y  
	INY
	CPY #$04  ; Compare if y has completed the 4 bytes
	BNE draw_row3
	RTS
.endproc

.proc draw_byte3
  ; get the first sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  LDY background3, x 
  TYA ;TEST
	AND #%11000000
  LSR A
	LSR A
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START2   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  ; get the second sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA 
	AND #%00110000
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START2   ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the third sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA ;TEST
	AND #%00001100
	LSR A
	LSR A ; Sprite value stored in A (need to map to actual address)
  CLC
	ADC #BG_START2   ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the fourth sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA ;TEST
	AND #%00000011
  CLC
	ADC #BG_START2   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  RTS
.endproc

.proc draw_background4
  LDY #$00
	JSR draw_row4
	LDY #$00
	JSR draw_row4
  INC bg_x
	INC bg_x
	INC bg_x
	INC bg_x  ; Increment x by 4
  LDA bg_x
	CMP #$3C  ; Value = 60
	BNE draw_background4
	RTS
.endproc

.proc draw_row4
  LDA bg_x  ; A contains x which is the base row address
  STY bg_y
  CLC
	ADC bg_y  ; bg_y contains the row offset, A = bg_y + bg_x
  TAX       ; x now contains the byte position in A
	JSR draw_byte4
  LDY bg_y  ;test
	INY
	CPY #$04  ; Compare if y has completed the 4 bytes
	BNE draw_row4
	RTS
.endproc

.proc draw_byte4   ;TEST
  ; get the first sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  LDY background4, x ;TEST
  TYA ;TEST
	AND #%11000000
  LSR A
	LSR A
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START2   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  ; get the second sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA ;TEST
	AND #%00110000
  LSR A
	LSR A
  LSR A
	LSR A
  CLC
	ADC #BG_START2   ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the third sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA ;TEST
	AND #%00001100
	LSR A
	LSR A ; Sprite value stored in A (need to map to actual address)
  CLC
	ADC #BG_START2  ; Add sprite index to sprite start
	STA $2007
	STA $2007
	; get the fourth sprite from byte
	;LDA background, x  ; Load background byte (contains four tiles)
  TYA ;TEST
	AND #%00000011
  CLC
	ADC #BG_START2   ; Add sprite index to sprite start
	STA $2007
	STA $2007
  RTS
.endproc

.proc update_tick_count
  LDA tick_count       ; Load tick_count
  CLC                  ; Clear the carry flag
  ADC #$1              ; Add one to the A register

  CMP #$28             ; Compare A (tick_count) with 0x28 == 40
  BEQ check_40         ; If equal, branch to check_40 label

  CMP #$14             ; Compare A again (tick_count) with 0x14 == 20
  BEQ check_20         ; If equal, branch to check_20 label

  CMP #$3C             ; Compare A again (tick_count) with 0x3C == 60
  BEQ reset_tick       ; If equal, branch to reset_tick label

  JMP done             ; If none of the conditions are met, skip to done label

check_20:
  ; If CMP #20 was equal, fall through to here
  STA tick_count
  JSR clear_sprites
  RTS

check_40:
  ; If CMP #40 was equal, fall through to here
  STA tick_count
  JSR clear_sprites
  RTS

reset_tick:
  LDA #$00            ; Load A with 0
  STA tick_count      ; Reset tick_count to 0   
  JSR clear_sprites  


  RTS

done:
  STA tick_count
  RTS
.endproc

.proc draw_player
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  ;write player tile numbers 
  LDA #$09
  STA $0201
  LDA #$0A
  STA $0205
  LDA #$19
  STA $0209
  LDA #$1A
  STA $020d
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0202
  STA $0206
  STA $020a
  STA $020e

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $0200
  LDA player_x
  STA $0203

  ; top right tile (x + 8):
  LDA player_y
  STA $0204
  LDA player_x
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0208
  LDA player_x
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $020c
  LDA player_x
  CLC
  ADC #$08
  STA $020f

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_running1
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$0B
  STA $0211
  LDA #$0C
  STA $0215
  LDA #$1B
  STA $0219
  LDA #$1C
  STA $021D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0212
  STA $0216
  STA $021A
  STA $021E

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $0210
  LDA player_x
  STA $0213

  ; top right tile (x + 8):
  LDA player_y
  STA $0214
  LDA player_x
  CLC
  ADC #$08
  STA $0217

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0218
  LDA player_x
  STA $021B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $021C
  LDA player_x
  CLC
  ADC #$08
  STA $021F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_running2
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$0B
  STA $0221
  LDA #$0C
  STA $0225
  LDA #$3B
  STA $0229
  LDA #$3C
  STA $022D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0222
  STA $0226
  STA $022A
  STA $022E

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $0220
  LDA player_x
  STA $0223

  ; top right tile (x + 8):
  LDA player_y
  STA $0224
  LDA player_x
  CLC
  ADC #$08
  STA $0227

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0228
  LDA player_x
  STA $022B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $022C
  LDA player_x
  CLC
  ADC #$08
  STA $022F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_back
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$0D
  STA $0231
  LDA #$2D 
  STA $0235
  LDA #$1D
  STA $0239
  LDA #$3D
  STA $023D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0232
  STA $0236
  STA $023A
  STA $023E

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $0230
  LDA player_x
  STA $0233

  ; top right tile (x + 8):
  LDA player_y
  STA $0234
  LDA player_x
  CLC
  ADC #$08
  STA $0237

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0238
  LDA player_x
  STA $023B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $023C
  LDA player_x
  CLC
  ADC #$08
  STA $023F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_back_running1
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$0E 
  STA $0241
  LDA #$0F 
  STA $0245
  LDA #$1E
  STA $0249
  LDA #$1F
  STA $024D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0242
  STA $0246
  STA $024A
  STA $024E

  ; store tile locations
  ; top left tile:
  LDA player_y 
  STA $0240
  LDA player_x
  STA $0243

  ; top right tile (x + 8):
  LDA player_y
  STA $0244
  LDA player_x
  CLC
  ADC #$08
  STA $0247

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0248
  LDA player_x
  STA $024B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $024C
  LDA player_x
  CLC
  ADC #$08
  STA $024F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_back_running2
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$0E 
  STA $0251
  LDA #$0F 
  STA $0255
  LDA #$2E
  STA $0259
  LDA #$2F
  STA $025D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0252
  STA $0256
  STA $025A
  STA $025E

  ; store tile locations
  ; top left tile:
  LDA player_y 
  STA $0250
  LDA player_x
  STA $0253

  ; top right tile (x + 8):
  LDA player_y
  STA $0254
  LDA player_x
  CLC
  ADC #$08
  STA $0257

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0258
  LDA player_x
  STA $025B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $025C
  LDA player_x
  CLC
  ADC #$08
  STA $025F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_left
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$10
  STA $0261
  LDA #$11 
  STA $0265
  LDA #$12
  STA $0269
  LDA #$13
  STA $026D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0262
  STA $0266
  STA $026A
  STA $026E

  ; store tile locations
  ; top left tile:
  LDA player_y 
  STA $0260
  LDA player_x
  STA $0263

  ; top right tile (x + 8):
  LDA player_y
  STA $0264
  LDA player_x
  CLC
  ADC #$08
  STA $0267

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0268
  LDA player_x
  STA $026B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $026C
  LDA player_x
  CLC
  ADC #$08
  STA $026F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_left_running1
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$10
  STA $0271
  LDA #$11 
  STA $0275
  LDA #$14
  STA $0279
  LDA #$15
  STA $027D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0272
  STA $0276
  STA $027A
  STA $027E

  ; store tile locations
  ; top left tile:
  LDA player_y 
  STA $0270
  LDA player_x
  STA $0273

  ; top right tile (x + 8):
  LDA player_y
  STA $0274
  LDA player_x
  CLC
  ADC #$08
  STA $0277

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0278
  LDA player_x
  STA $027B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $027C
  LDA player_x
  CLC
  ADC #$08
  STA $027F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_left_running2
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$10
  STA $0281
  LDA #$11 
  STA $0285
  LDA #$20
  STA $0289
  LDA #$21
  STA $028D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0282
  STA $0286
  STA $028A
  STA $028E

  ; store tile locations
  ; top left tile:
  LDA player_y 
  STA $0280
  LDA player_x
  STA $0283

  ; top right tile (x + 8):
  LDA player_y
  STA $0284
  LDA player_x
  CLC
  ADC #$08
  STA $0287

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0288
  LDA player_x
  STA $028B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $028C
  LDA player_x
  CLC
  ADC #$08
  STA $028F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_right
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$30
  STA $0291
  LDA #$31 
  STA $0295
  LDA #$40
  STA $0299
  LDA #$41
  STA $029D
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $0292
  STA $0296
  STA $029A
  STA $029E

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $0290
  LDA player_x
  STA $0293

  ; top right tile (x + 8):
  LDA player_y
  STA $0294
  LDA player_x
  CLC
  ADC #$08
  STA $0297

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0298
  LDA player_x
  STA $029B

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $029C
  LDA player_x
  CLC
  ADC #$08
  STA $029F

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_right_running1
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$30
  STA $02A1
  LDA #$31 
  STA $02A5
  LDA #$42
  STA $02A9
  LDA #$43
  STA $02AD
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $02A2
  STA $02A6
  STA $02AA
  STA $02AE

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $02A0
  LDA player_x
  STA $02A3

  ; top right tile (x + 8):
  LDA player_y
  STA $02A4
  LDA player_x
  CLC
  ADC #$08
  STA $02A7

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $02A8
  LDA player_x
  STA $02AB

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $02AC
  LDA player_x
  CLC
  ADC #$08
  STA $02AF

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc draw_player_right_running2
  ;save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ;write player tile numbers 
  LDA #$30
  STA $02B1
  LDA #$31 
  STA $02B5
  LDA #$44
  STA $02B9
  LDA #$45
  STA $02BD
  ; write player tile attributes
  ; use palette 0
  LDA #$00
  STA $02B2
  STA $02B6
  STA $02BA
  STA $02BE

  ; store tile locations
  ; top left tile:
  LDA player_y
  STA $02B0
  LDA player_x
  STA $02B3

  ; top right tile (x + 8):
  LDA player_y
  STA $02B4
  LDA player_x
  CLC
  ADC #$08
  STA $02B7

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $02B8
  LDA player_x
  STA $02BB

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $02BC
  LDA player_x
  CLC
  ADC #$08
  STA $02BF

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc clear_sprites
  LDX #$00
remove_sprites:
  LDA #$00
  STA $0200,X
  INX
  CPX #$C0
  BNE remove_sprites
.endproc

.proc update_player
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA
  ;Check if there is no input
  LDA pad1
  CMP #$00
  BNE not_zero
  JSR draw_based_on_state
not_zero:

  LDA pad1        ; Load button presses
  AND #BTN_LEFT   ; Filter out all but Left
  BEQ check_right ; If result is zero, left not pressed
  DEC player_x  ; If the branch is not taken, move player left
  JSR left_animation
  ;Set state to 4 (left)
  LDA #$04
  STA state
check_right:
  LDA pad1
  AND #BTN_RIGHT
  BEQ check_up
  INC player_x
  JSR right_animation
  ;Set state to 3 (right)
  LDA #$03
  STA state
check_up:
  LDA pad1
  AND #BTN_UP
  BEQ check_down
  DEC player_y
  JSR up_animation
  ;Set state to 1 (back)
  LDA #$01
  STA state
check_down:
  LDA pad1
  AND #BTN_DOWN
  BEQ check_select
  INC player_y
  JSR front_animation
  ;set state to 2 (front)
  LDA #$02
  STA state
check_select:
  LDA pad1
  AND #BTN_SELECT
  BEQ check_start
  ;WHen Q is pressed load lvl 1
  JSR load_background    ;drawing background to PPU
  JSR load_background2

  LDA level_select
  CMP #$00
  BEQ check_start
  LDA #$02
  STA level_select
  JMP done_checking
check_start:
  LDA pad1
  AND #BTN_START
  BEQ done_checking

  JSR load_background3 ;(when W is pressed)
  JSR load_background4

  LDA level_select
  CMP #$03
  BEQ done_checking
  LDA #$01
  STA level_select

done_checking:
  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc front_animation
  LDA tick_count       ; Load the updated tick_count into A for comparison

  CMP #$1E             ; Compare A (tick_count) with 0x1E == 30
  BCC draw_running1    ; If less than 30, draw "running1"

  ; If tick_count is 30 or greater, draw "running2"
  JSR draw_player_running2
  JMP finish

draw_running1:
  ; If tick_count is in the range of 0-30, draw "running1"
  JSR draw_player_running1
  RTS

finish:
  RTS
.endproc

.proc up_animation
  LDA tick_count       ; Load the updated tick_count into A for comparison

  CMP #$1E             ; Compare A (tick_count) with 0x1E == 30
  BCC draw_running1    ; If less than 30, draw "running1"

  ; If tick_count is 30 or greater, draw "running2"
  JSR draw_player_back_running2
  JMP finish

draw_running1:
  ; If tick_count is in the range of 0-30, draw "running1"
  JSR draw_player_back_running1
  RTS

finish:
  RTS
.endproc

.proc right_animation
  LDA tick_count       ; Load the updated tick_count into A for comparison

  CMP #$1E             ; Compare A (tick_count) with 0x1E == 30
  BCC draw_running1    ; If less than 30, draw "running1"

  ; If tick_count is 30 or greater, draw "running2"
  JSR draw_player_right_running2
  JMP finish

draw_running1:
  ; If tick_count is in the range of 0-30, draw "running1"
  JSR draw_player_right_running1
  RTS

finish:
  RTS
.endproc

.proc left_animation
  LDA tick_count       ; Load the updated tick_count into A for comparison

  CMP #$1E             ; Compare A (tick_count) with 0x1E == 30
  BCC draw_running1    ; If less than 30, draw "running1"

  ; If tick_count is 30 or greater, draw "running2"
  JSR draw_player_left_running2
  JMP finish

draw_running1:
  ; If tick_count is in the range of 0-30, draw "running1"
  JSR draw_player_left_running1
  RTS

finish:
  RTS
.endproc

.proc draw_based_on_state
  LDA state    ; Load the value of 'state' into the accumulator

  CMP #$01            ; Compare 'state' with 1
  BEQ draw_back       ; If equal, branch to draw_back label

  CMP #$02            ; Compare 'state' with 2
  BEQ draw_front      ; If equal, branch to draw_front label

  CMP #$03            ; Compare 'state' with 3
  BEQ draw_right      ; If equal, branch to draw_right label

  CMP #$04            ; Compare 'state' with 4
  BEQ draw_left       ; If equal, branch to draw_left label

  JMP salir    ;Exit just in case something goes wrong

draw_back:
  JSR draw_player_back
  RTS

draw_front:
  JSR draw_player
  RTS

draw_right:
  JSR draw_player_right
  RTS

draw_left:
  JSR draw_player_left
  RTS

salir:
  RTS
.endproc



.segment "CHR"
.incbin "graphics2.chr"