.include "constants.inc"

.segment "ZEROPAGE"
.importzp player_x, player_y, pad1, tick_count, state, bg_x, bg_y, scroll

.segment "CODE"
.import main
.export reset_handler
.proc reset_handler
  SEI
  CLD
  LDX #$40
  STX $4017
  LDX #$FF
  TXS
  INX
  STX $2000
  STX $2001
  STX $4010
  BIT $2002
vblankwait:
  BIT $2002
  BPL vblankwait
vblankwait2:
  BIT $2002
  BPL vblankwait2

  ; initialize zero-page values
  LDA #$80
  STA player_x
  LDA #$a0
  STA player_y
  LDA #%00000001
  STA pad1
  LDA #$00
  STA tick_count
  STA bg_x
  STA bg_x
  STA scroll
  ;STA level_select
  LDA #$02
  STA state

  LDX #$00
  LDA #$ff
clear_oam:
  STA $0200,X ; set sprite y-positions off the screen
  INX
  INX
  INX
  INX
  BNE clear_oam

  JMP main


.endproc
