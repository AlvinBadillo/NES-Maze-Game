.include "constants.inc"
.include "header.inc"

.segment "ZEROPAGE"
tick_count: .res 1
player_x: .res 1
player_y: .res 1
player_dir: .res 1
player_vertical_dir: .res 1
.exportzp player_x, player_y
.exportzp tick_count

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
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
;palettes:
;.byte $29, $19, $09, $0f  
;.byte $0f, $0c, $21, $32
palettes:
.byte $0f, $2d, $10, $37
.byte $0f, $2d, $14, $37
.byte $0f, $0c, $07, $13
.byte $0f, $19, $09, $29

.byte $0f, $2d, $14, $37
.byte $0f, $19, $09, $29
.byte $0f, $19, $09, $29
.byte $0f, $19, $09, $29

sprites:
;Y, Sprite, Pallet, X

;Front still  $0200-$020F   ;y adress   
;top left
.byte $40, $09, $00, $60    ;0200
;top right
.byte $40, $0A, $00, $68    ;0204
;bottom left
.byte $48, $19, $00, $60    ;0208
;bottom right
.byte $48, $1A, $00, $68    ;020C

;Front running 1
;top left
.byte $40, $0B, $00, $70    ;0210
;top right
.byte $40, $0C, $00, $78    ;0214
;bottom left
.byte $48, $1B, $00, $70    ;0218
;bottom right
.byte $48, $1C, $00, $78    ;021C

;Front running 2
;top left
.byte $40, $0B, $00, $80    ;0220
;top right
.byte $40, $0C, $00, $88    ;0224
;bottom left
.byte $48, $3b, $00, $80    ;0228
;bottom right
.byte $48, $3C, $00, $88    ;022C

;Back still
;top left
.byte $50, $0D, $00, $60    
;top right 
.byte $50, $2D, $00, $68
;bottom left
.byte $58, $1D, $00, $60
;bottom right
.byte $58, $3D, $00, $68  

;Back running 1
;top left
.byte $50, $0E, $00, $70
;top right
.byte $50, $0F, $00, $78
;bottom left
.byte $58, $1E, $00, $70
;bottom right
.byte $58, $1F, $00, $78

;Back running 2
;top left
.byte $50, $0E, $00, $80
;top right
.byte $50, $0F, $00, $88
;bottom left
.byte $58, $2E, $00, $80
;bottom right
.byte $58, $2F, $00, $88

;Left still
;top left
.byte $60, $10, $00, $60
;top right 
.byte $60, $11, $00, $68
;bottom left
.byte $68, $12, $00, $60
;bottom right
.byte $68, $13, $00, $68  

;Left running 1
;top left
.byte $60, $10, $00, $70
;top right
.byte $60, $11, $00, $78
;bottom left
.byte $68, $14, $00, $70
;bottom right
.byte $68, $15, $00, $78

;Left running 2
;top left
.byte $60, $10, $00, $80
;top right
.byte $60, $11, $00, $88
;bottom left
.byte $68, $20, $00, $80
;bottom right
.byte $68, $21, $00, $88

;Right still
;top left
.byte $70, $30, $00, $60
;top right 
.byte $70, $31, $00, $68
;bottom left
.byte $78, $40, $00, $60
;bottom right
.byte $78, $41, $00, $68  

;Right running 1
;top left
.byte $70, $30, $00, $70
;top right
.byte $70, $31, $00, $78
;bottom left
.byte $78, $42, $00, $70
;bottom right
.byte $78, $43, $00, $78

;Right running 2
;top left
.byte $70, $30, $00, $80
;top right
.byte $70, $31, $00, $88
;bottom left
.byte $78, $44, $00, $80
;bottom right
.byte $78, $45, $00, $88

.proc update_tick_count
  LDA tick_count       ; Load the updated tick_count into A for comparison
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
  JSR draw_player
  JSR draw_player_back
  JSR draw_player_left
  JSR draw_player_right
  RTS

check_40:
  ; If CMP #40 was equal, fall through to here
  STA tick_count
  JSR clear_sprites
  JSR draw_player_running1
  JSR draw_player_back_running1
  JSR draw_player_left_running1
  JSR draw_player_right_running1
  RTS

reset_tick:
  LDA #$00            ; Load A with 0
  STA tick_count      ; Reset tick_count to 0   
  JSR clear_sprites  
  JSR draw_player_running2
  JSR draw_player_back_running2
  JSR draw_player_left_running2
  JSR draw_player_right_running2
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
  LDA #$20 
  STA $0230
  LDA #$20
  STA $0233

  ; top right tile (x + 8):
  LDA #$20
  STA $0234
  LDA #$20
  CLC
  ADC #$08
  STA $0237

  ; bottom left tile (y + 8):
  LDA #$20
  CLC
  ADC #$08
  STA $0238
  LDA #$20
  STA $023B

  ; bottom right tile (x + 8, y + 8)
  LDA #$20
  CLC
  ADC #$08
  STA $023C
  LDA #$20
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
  LDA #$20 
  STA $0240
  LDA #$20
  STA $0243

  ; top right tile (x + 8):
  LDA #$20
  STA $0244
  LDA #$20
  CLC
  ADC #$08
  STA $0247

  ; bottom left tile (y + 8):
  LDA #$20
  CLC
  ADC #$08
  STA $0248
  LDA #$20
  STA $024B

  ; bottom right tile (x + 8, y + 8)
  LDA #$20
  CLC
  ADC #$08
  STA $024C
  LDA #$20
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
  LDA #$20 
  STA $0250
  LDA #$20
  STA $0253

  ; top right tile (x + 8):
  LDA #$20
  STA $0254
  LDA #$20
  CLC
  ADC #$08
  STA $0257

  ; bottom left tile (y + 8):
  LDA #$20
  CLC
  ADC #$08
  STA $0258
  LDA #$20
  STA $025B

  ; bottom right tile (x + 8, y + 8)
  LDA #$20
  CLC
  ADC #$08
  STA $025C
  LDA #$20
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
  LDA #$A0 
  STA $0260
  LDA #$20
  STA $0263

  ; top right tile (x + 8):
  LDA #$A0
  STA $0264
  LDA #$20
  CLC
  ADC #$08
  STA $0267

  ; bottom left tile (y + 8):
  LDA #$A0
  CLC
  ADC #$08
  STA $0268
  LDA #$20
  STA $026B

  ; bottom right tile (x + 8, y + 8)
  LDA #$A0
  CLC
  ADC #$08
  STA $026C
  LDA #$20
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
  LDA #$A0 
  STA $0270
  LDA #$20
  STA $0273

  ; top right tile (x + 8):
  LDA #$A0
  STA $0274
  LDA #$20
  CLC
  ADC #$08
  STA $0277

  ; bottom left tile (y + 8):
  LDA #$A0
  CLC
  ADC #$08
  STA $0278
  LDA #$20
  STA $027B

  ; bottom right tile (x + 8, y + 8)
  LDA #$A0
  CLC
  ADC #$08
  STA $027C
  LDA #$20
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
  LDA #$A0 
  STA $0280
  LDA #$20
  STA $0283

  ; top right tile (x + 8):
  LDA #$A0
  STA $0284
  LDA #$20
  CLC
  ADC #$08
  STA $0287

  ; bottom left tile (y + 8):
  LDA #$A0
  CLC
  ADC #$08
  STA $0288
  LDA #$20
  STA $028B

  ; bottom right tile (x + 8, y + 8)
  LDA #$A0
  CLC
  ADC #$08
  STA $028C
  LDA #$20
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
  LDA #$20 
  STA $0290
  LDA #$A0
  STA $0293

  ; top right tile (x + 8):
  LDA #$20
  STA $0294
  LDA #$A0
  CLC
  ADC #$08
  STA $0297

  ; bottom left tile (y + 8):
  LDA #$20
  CLC
  ADC #$08
  STA $0298
  LDA #$A0
  STA $029B

  ; bottom right tile (x + 8, y + 8)
  LDA #$20
  CLC
  ADC #$08
  STA $029C
  LDA #$A0
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
  LDA #$20 
  STA $02A0
  LDA #$A0
  STA $02A3

  ; top right tile (x + 8):
  LDA #$20
  STA $02A4
  LDA #$A0
  CLC
  ADC #$08
  STA $02A7

  ; bottom left tile (y + 8):
  LDA #$20
  CLC
  ADC #$08
  STA $02A8
  LDA #$A0
  STA $02AB

  ; bottom right tile (x + 8, y + 8)
  LDA #$20
  CLC
  ADC #$08
  STA $02AC
  LDA #$A0
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
  LDA #$20 
  STA $02B0
  LDA #$A0
  STA $02B3

  ; top right tile (x + 8):
  LDA #$20
  STA $02B4
  LDA #$A0
  CLC
  ADC #$08
  STA $02B7

  ; bottom left tile (y + 8):
  LDA #$20
  CLC
  ADC #$08
  STA $02B8
  LDA #$A0
  STA $02BB

  ; bottom right tile (x + 8, y + 8)
  LDA #$20
  CLC
  ADC #$08
  STA $02BC
  LDA #$A0
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
  LDA #$FF
  STA $0200,X
  INX
  INX
  INX
  INX
  CPX #$C0
  BNE remove_sprites
  RTS
.endproc

.segment "CHR"
.incbin "graphics2.chr"