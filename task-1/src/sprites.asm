.include "constants.inc"
.include "header.inc"

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
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
  ;CPX #$04          ;loads first 4 bytes in pallets
  CPX #$20
  BNE load_palettes

  ; write sprite data
  LDX #$00
load_sprites:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$C0
  BNE load_sprites

  ;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$43       ;off: 
  STA PPUADDR
  LDX #$09       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C0         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ; 1
  ;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$0C       ;off: 
  STA PPUADDR
  LDX #$09       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

;2
  ;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$0D       ;off: 
  STA PPUADDR
  LDX #$0A       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

; 3
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$0E       ;off: 
  STA PPUADDR
  LDX #$0B       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

;4
  ;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$0F       ;off: 
  STA PPUADDR
  LDX #$0C       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

;5
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$10       ;off: 
  STA PPUADDR
  LDX #$10       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

;6
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$11       ;off: 
  STA PPUADDR
  LDX #$10       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;7
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$12       ;off: 
  STA PPUADDR
  LDX #$11       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;8
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$13       ;off: 
  STA PPUADDR
  LDX #$11       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;9
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$2C       ;off: 
  STA PPUADDR
  LDX #$19       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;10
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$2D       ;off: 
  STA PPUADDR
  LDX #$1A       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;11
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$2E       ;off: 
  STA PPUADDR
  LDX #$1B       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;12
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$2F       ;off: 
  STA PPUADDR
  LDX #$1C       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C3         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;13
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$30       ;off: 
  STA PPUADDR
  LDX #$10       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;14
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$31       ;off: 
  STA PPUADDR
  LDX #$10       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;15
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$32       ;off: 
  STA PPUADDR
  LDX #$11       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;16
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$33       ;off: 
  STA PPUADDR
  LDX #$11       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C4         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;17
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$14       ;off: 
  STA PPUADDR
  LDX #$0D       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;18
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$15       ;off: 
  STA PPUADDR
  LDX #$0E       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;19
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$16       ;off: 
  STA PPUADDR
  LDX #$20       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;20
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$17       ;off: 
  STA PPUADDR
  LDX #$21       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;21
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$18       ;off: 
  STA PPUADDR
  LDX #$22       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;22
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$19       ;off: 
  STA PPUADDR
  LDX #$22       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

  ;23
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$1A       ;off: 
  STA PPUADDR
  LDX #$23       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;24
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$1B       ;off: 
  STA PPUADDR
  LDX #$23       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;25
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$34       ;off: 
  STA PPUADDR
  LDX #$1D       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;26
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$35       ;off: 
  STA PPUADDR
  LDX #$1E       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;27
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$36       ;off: 
  STA PPUADDR
  LDX #$30       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;28
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$37       ;off: 
  STA PPUADDR
  LDX #$31       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C5         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;29
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$38       ;off: 
  STA PPUADDR
  LDX #$22       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;30
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$39       ;off: 
  STA PPUADDR
  LDX #$22       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;31
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$3A       ;off: 
  STA PPUADDR
  LDX #$23       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

    ;32
;load background:
  ; write a nametable
  ; big stars first
  LDA PPUSTATUS
  LDA #$20      ;base adress
  STA PPUADDR
  LDA #$3B       ;off: 
  STA PPUADDR
  LDX #$23       ; name
  STX PPUDATA

  ; finally, attribute table
  LDA PPUSTATUS
  LDA #$03         ;AtOff high
  STA PPUADDR
  LDA #$C6         ;AtOff low
  STA PPUADDR
  LDA #%00001100    ;pallete selector
  STA PPUDATA

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
;.byte $70, $05, $00, $80   ;Y=70, Sprite at=05, Pallet=00, X=80
;.byte $70, $06, $00, $88

;Front still
;top left
.byte $40, $09, $00, $60    ;0200  0201  0202  0203
;top right
.byte $40, $0A, $00, $68    ;0204  0205  0206  0207
;bottom left
.byte $48, $19, $00, $60    ;0208  0209  020A  020B
;bottom right
.byte $48, $1A, $00, $68    ;020C  020D  020E 020F

;Front running 1
;top left
.byte $40, $0B, $00, $70    ;0210  0211  0212  0213
;top right
.byte $40, $0C, $00, $78    ;0214  0215  0216  0217
;bottom left
.byte $48, $1B, $00, $70    ;0218  0219  021A  021B
;bottom right
.byte $48, $1C, $00, $78    ;021C  021D  021E  021F

;Front running 2
;top left
.byte $40, $0B, $00, $80    ;0220  0221  0222  0223
;top right
.byte $40, $0C, $00, $88    ;0224  0225  0226  0227
;bottom left
.byte $48, $3b, $00, $80    ;0228  0229  022A  022B
;bottom right
.byte $48, $3C, $00, $88    ;022C  022D  022E  022F

;Back still
;top left
.byte $50, $0D, $00, $60    ;0230  0231  0232  0233
;top right 
.byte $50, $2D, $00, $68    ;0234  0235  0236  0237
;bottom left
.byte $58, $1D, $00, $60    ;0238  0239  023A  023B
;bottom right
.byte $58, $3D, $00, $68    ;023C  023D  023E  023F

;Back running 1
;top left
.byte $50, $0E, $00, $70    ;0240  0241  0242  0243
;top right
.byte $50, $0F, $00, $78    ;0244  0245  0246  0247
;bottom left
.byte $58, $1E, $00, $70    ;0248  0249  024A  024B
;bottom right
.byte $58, $1F, $00, $78    ;024C  024D  024E  024F

;Back running 2
;top left
.byte $50, $0E, $00, $80    ;0250  0251  0252  0253
;top right
.byte $50, $0F, $00, $88    ;0254  0255  0256  0257
;bottom left
.byte $58, $2E, $00, $80    ;0258  0259  025A  025B
;bottom right
.byte $58, $2F, $00, $88    ;025C  025D  025E  025F

;Left still
;top left
.byte $60, $10, $00, $60    ;0260  0261  0262  0263
;top right 
.byte $60, $11, $00, $68    ;0264  0265  0266  0267
;bottom left
.byte $68, $12, $00, $60    ;0268  0269  026A  026B
;bottom right
.byte $68, $13, $00, $68    ;026C  026D  026E  026F

;Left running 1
;top left
.byte $60, $10, $00, $70    ;0270  0271  0272  0273
;top right
.byte $60, $11, $00, $78    ;0274  0275  0276  0277
;bottom left
.byte $68, $14, $00, $70    ;0278  0279  027A  027B
;bottom right
.byte $68, $15, $00, $78    ;027C  027D  027E  027F

;Left running 2
;top left
.byte $60, $10, $00, $80    ;0280  0281  0282  0283
;top right
.byte $60, $11, $00, $88    ;0284  0285  0286  0287
;bottom left
.byte $68, $20, $00, $80    ;0288  0289  028A  028B
;bottom right
.byte $68, $21, $00, $88    ;028C  028D  028E  028F

;Right still
;top left
.byte $70, $30, $00, $60    ;0290  0291  0292  0293
;top right 
.byte $70, $31, $00, $68    ;0294  0295  0296  0297
;bottom left
.byte $78, $40, $00, $60    ;0298  0299  029A  029B
;bottom right
.byte $78, $41, $00, $68    ;029C  029D  029E  029F

;Right running 1
;top left
.byte $70, $30, $00, $70    ;02A0 02A1 02A2 02A3
;top right
.byte $70, $31, $00, $78    ;02A4 02A5 02A6 02A7
;bottom left
.byte $78, $42, $00, $70    ;02A8 02A9 02AA 02AB
;bottom right
.byte $78, $43, $00, $78    ;02AC 02AD 02AE 02AF

;Right running 2
;top left
.byte $70, $30, $00, $80    ;02B0 02B1 02B2 02B3
;top right
.byte $70, $31, $00, $88    ;02B4 02B5 02B6 02B7
;bottom left
.byte $78, $44, $00, $80    ;02B8 02B9 02BA 02BB
;bottom right
.byte $78, $45, $00, $88    ;02BC 02BD 02BE 02BF



.segment "CHR"
.incbin "graphics2.chr"
