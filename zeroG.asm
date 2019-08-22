.segment "HEADER"

.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $01 ; 1 * 8KB CHR ROM
.byte %00000000 ; mapper and mirroring
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00 ; filler bytes

.scope EntityType
	NoEntity = 0
	PlayerType = 1
	Bullet = 2
	Enemy = 3
.endscope

.struct Entity
	xpos .byte
	ypos .byte
	type .byte
.endstruct
.segment "STARTUP"
.segment "ZEROPAGE" ; LSB 0 - FF
;;start variables at ram location 0
result: 	 .res 2
time:		 .res 1
mul1:        .res 1
mul2:		 .res 1
inair:		 .res 1
prevkey:     .res 1
keybyte:     .res 1
vblnk_ret:   .res 1
spritemem:   .res 2 ;; 2 byte offset example : #$02 #$00
					;; corresponding to address $0200 in ram
MAXENTITIES = 10
entities:   .res .sizeof(Entity)* MAXENTITIES
TOTALENTITIES = .sizeof(Entity)*MAXENTITIES


.segment "CODE"

Reset:
    SEI ; Disables all interrupts
    CLD ; disable decimal mode

    LDA #$00
    STA vblnk_ret
    STA keybyte
    STA spritemem 
    STA inair
    STA time

    ; Disable sound IRQ
    LDX #$40
    STX $4017

    ; Initialize the stack register
    LDX #$FF
    TXS

    INX ; #$FF + 1 => #$00

    ; Zero out the PPU registers
    STX $2000
    STX $2001

    STX $4010

:
    BIT $2002
    BPL :-

    TXA

CLEARMEM:
    STA $0000, X ; $0000 => $00FF
    STA $0100, X ; $0100 => $01FF
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X
    LDA #$FF
    STA $0200, X ; $0200 => $02FF
    LDA #$00
    INX
    BNE CLEARMEM    



; wait for vblank
:
    BIT $2002
    BPL :-

    LDA #$02
    STA $4014
    NOP

    ; $3F00
InitEntities:
	LDA #$20
	STA entities+Entity::xpos
	LDA #$40
	STA entities+Entity::ypos
	LDA #EntityType::PlayerType
	STA entities+Entity::type

	LDX #$03
	LDA #$FF
ClearEntities:
	STA entities+Entity::xpos,X
	STA entities+Entity::ypos,X
	LDA #$00
	STA entities+Entity::type,X
	LDA #$FF
	INX
	INX
	INX
	CPX #TOTALENTITIES
	BNE ClearEntities
    LDA #$00
    LDY #$00
    STA spritemem
    LDA #$02
    STA spritemem+1
    
    LDA $2002
    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00

LoadPalettes:
    LDA PaletteData, X
    STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
    INX
    CPX #$20
    BNE LoadPalettes    

	



; Enable interrupts
    CLI


GameStart:

InitSprites:
	LDY #$00
	LDA #$FF
InitSpritesLoop:
	STA $0200, Y
	INY
	EOR #$FF
	STA $0200, Y
	INY
	STA $0200, Y
	INY
	EOR #$FF
	STA $0200, Y
	INY
	BNE InitSpritesLoop
EnablePPU:
  	LDA #%10010000 ; enable NMI change background to use second chr set of tiles ($1000)
    STA $2000
    ; Enabling sprites and background for left-most 8 pixels
    ; Enable sprites and background
    LDA #%00011110
    STA $2001
    
   

GameLoop:
	LDA entities+Entity::ypos
	CMP #$DF
	BEQ timesetzero
	JMP :+
timesetzero:
	LDA #$00
	STA time
:	
	JSR ReadButtons
	LDA time
  	TAY
  	INY
  	TYA 
  	STA time
  	LDA time
  	LDX #$02
  	JSR multiply
  	JSR storewithdelay
BoundaryCheckBottom0:
  ; If WALL_BOTTOM is exceeded, playery is set to WALL_BOTTOM.
  LDA entities+Entity::ypos
  CMP #$DF
  BCC BCBDone0
  LDA #$DF ; set if A>=WALL_BOTTOM (C=1)
  STA entities+Entity::ypos
BCBDone0:
	

  	;;BEQ ReadUpDone
  	;;BEQ ReadUpDone
  	;;DEY
  	;;BEQ ReadUpDone
WaitVblank:
	LDA vblnk_ret
	CMP #$01
	BNE WaitVblank
	LDA #$00
    STA vblnk_ret
    JMP GameLoop







NMI:   ;vblank scanline interrupt handler
    PHA 
    PHP
    TXA
    PHA
    TYA
    PHA
    LDA #$02 ; copy sprite data from $0200 => PPU memory for display
    ;means $0200 to $0299 data which actually constains sprite data
    STA $4014;stores offset of cpu mem add to copy to ppu 256 consecutive bytes
    LDX #$00 ;;
    LDY #$00

DrawEntities:
	LDA entities+Entity::type, X
	CMP #EntityType::PlayerType
	BEQ PlayerSprite
	JMP CheckEndSprite
PlayerSprite:
	LDA entities+Entity::ypos, X
	STA (spritemem), Y 
	INY
	LDA #$00 ;tile no
	STA (spritemem), Y
	INY
	LDA #$00 ;palette no
	STA (spritemem), Y
	INY
	LDA entities+Entity::xpos, X
	STA (spritemem), Y 
CheckEndSprite:
	TXA
	CLC
	ADC #.sizeof(Entity)
	TAX
	CPX #TOTALENTITIES
	BEQ DoneSprites
	JMP DrawEntities
    
DoneSprites:




    
    PLA
    TAY
    PLA
    TAX
    PLP
    PLA
    INC vblnk_ret
    RTI

ReadButtons:

LatchController:
  	LDA #$01
  	STA $4016
  	LDA #$00
  	STA $4016      
  	LDY #$00
  	LDX #$00
Getkeybyte:
	LDA $4016
	LSR A
	ROL keybyte
	INY
	CPY #$08
	BNE Getkeybyte

ReadRight: 
  	LDA keybyte       ; player 1 - right
  	PHA
  	AND #%00000001  ; only look at bit 7
  	BNE DoRight
  	JMP ReadRightDone ; branch to ReadADone if button is NOT pressed (0) 
DoRight:    
    LDA entities+Entity::xpos, X  ;;checking collision with right end
  	TAY
  	INY
  	;;BEQ ReadRightDone
  	STY entities+Entity::xpos, X
ReadRightDone:

ReadLeft:
  	PLA
  	LSR A
  	PHA
  	AND #%00000001  ; only look at bit 7
  	BNE DoLeft
  	JMP ReadLeftDone
DoLeft:
    LDA entities+Entity::xpos, X  
  	TAY
  	;;BEQ ReadLeftDone
  	DEY
  	BEQ setXzero
  	JMP :+
setXzero:
	LDA #$00
	JMP ReadLeftDone
:  	
  	STY entities+Entity::xpos, X
ReadLeftDone:

ReadDown:
  	PLA
  	LSR A
  	PHA
  	AND #%00000001  ; only look at bit 7
  	JMP DoDown
  	JMP ReadDownDone
DoDown:

	
ReadDownDone:

ReadUp:
  	PLA
  	LSR A
  	PHA
  	AND #%00000001  ; only look at bit 7
  	BNE DoUp
  	JMP ReadUpDone
DoUp:
  	BEQ ReadUpDone
    LDA entities+Entity::ypos, X  ;;checking collision with right end
  	;;BEQ ReadUpDone
  	SEC
  	;;BEQ ReadUpDone
  	SBC time
  	SBC #$08
  	;;BEQ ReadUpDone
  	
  	;;BEQ ReadUpDone
  	STA entities+Entity::ypos, X
  	LDA #$00
  	STA inair
ReadUpDone:

ReadStart:
  	PLA
  	LSR A
  	PHA
  	AND #%00000001  ; only look at bit 7
  	BNE DoStart
  	JMP ReadStartDone
DoStart:
	JMP Reset
ReadStartDone:

ReadSelect:
  	PLA
  	LSR A
  	PHA
  	AND #%00000001  ; only look at bit 7
  	BNE DoSelect
  	JMP ReadSelectDone
DoSelect:
ReadSelectDone:

ReadB:
  	PLA
  	LSR A
  	PHA
  	AND #%00000001  ; only look at bit 7
  	BNE DoB
  	JMP ReadBDone
DoB:
ReadBDone:

ReadA:
  	PLA
  	LSR A
    AND #%00000001  ; only look at bit 7
	BNE DoA ;;not equal to #$00
	JMP ReadADone
DoA:
ReadADone:
ButtonReadDone:
	LDA #$00
	STA keybyte
BoundaryCheckLeft:
  ; If WALL_LEFT is exceeded, playerx is set to WALL_LEFT.
  LDA entities+Entity::xpos
  CMP #$00
  BCS BCLDone
  LDA #$00 ; set if A<WALL_LEFT (C=0)
  STA entities+Entity::xpos
BCLDone:
BoundaryCheckRight:
  ; If WALL_RIGHT is exceeded, playerx is set to WALL_RIGHT.
  LDA entities+Entity::xpos
  CMP #$F8
  BCC BCRDone
  LDA #$F8 ; set if A>=WALL_RIGHT (C=1)
  STA entities+Entity::xpos
BCRDone:
BoundaryCheckTop:
  ; If WALL_TOP is exceeded, playery is set to WALL_TOP.
  LDA entities+Entity::ypos
  CMP #$00
  BCS BCTDone
  LDA #$00 ; set if A<WALL_TOP (C=0)
  STA entities+Entity::ypos
BCTDone:
BoundaryCheckBottom:
  ; If WALL_BOTTOM is exceeded, playery is set to WALL_BOTTOM.
  LDA entities+Entity::ypos
  CMP #$DF
  BCC BCBDone
  LDA #$DF ; set if A>=WALL_BOTTOM (C=1)
  STA entities+Entity::ypos
BCBDone:
	
	RTS



multiply:
	LDY #$00
	STY result
	STY result+1
multiplicationloop:

	CPX #$00
	BEQ exitmultiplicationloop
	PHA
	TXA
	LSR A
	BCC addzerotoresult
	JMP addYAtoresult
addYAtoresult:
	TAX
	PLA
	CLC
	PHA
	ADC result
	STA result
b1: ;;	620
	TYA
	ADC result+1
	STA result+1
b2: ;; 625
	PLA
b3: ;;627
	JMP shiftYAleft
addzerotoresult:
	TAX
	PLA
shiftYAleft:
	CLC
	ROL A
	PHA
	TYA
	ROL A
	TAY
	PLA

	JMP multiplicationloop
exitmultiplicationloop: ;;636
	LDX #$00
	LDA #$00
	LDY #$00
	RTS
	
storewithdelay:
	LDY #$00 
storeloop:
	INY
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	INC entities+Entity::ypos
	CPY result
	BNE storeloop
	LDY #$00

	RTS





PaletteData:
  .byte $22,$29,$1A,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F  ;background palette data
  .byte $22,$16,$27,$18,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17  ;sprite palette data



.segment "VECTORS"
    .word NMI
    .word Reset
    ;;various interrupt handlers 


.segment "CHARS"
    .incbin "zeroG.chr"
