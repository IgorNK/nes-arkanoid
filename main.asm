.segment "HEADER"       ; Setting up the header, needed for emulators to understand what to do with the file, not needed for actual cartridges
    .byte "NES"         ; The beginning of the HEADER of iNES header
    .byte $1a           ; Signature of iNES header that the emulator will look for
    .byte $02           ; 2 * 16KB PRG ROM
    .byte $01           ; 1 * 8KB CHR ROM
    .byte %00000000     ; mapper and mirroring - disabled
    .byte $0
    .byte $0
    .byte $0
    .byte $0
    .byte $0, $0, $0, $0    ; unused
.segment "ZEROPAGE"
    ;; Variables
    state:          .res 1  ; $00 - title screen, $01 - play state, $02 - game over state
    buttons:        .res 1
    bgcounter:      .res 2
    nextstate:      .res 1
    ballx:          .res 1
    bally:          .res 1
    ballspeedx:     .res 1
    ballspeedy:     .res 1
    ballspeeddx:    .res 1
    ballspeeddy:    .res 1
    paddlex:        .res 1

    ;; Constants
    STATETITLE      = $00
    STATEGAMEPLAY   = $01
    STATEGAMEOVER   = $02
    
    PADDLEY         = $CE
    PADDLESTARTX1   = $80
    PADDLESTARTX2   = $88
    BALLSTARTX      = $80
    BALLSTARTY      = $80

    RIGHTWALL       = $E5
    ;RIGHTWALL        = $B3
    TOPWALL         = $20
    ;TOPWALL          = $52
    BOTTOMWALL      = $E0
    ;BOTTOMWALL       = $AE
    LEFTWALL        = $04
    ;LEFTWALL        = $36

    ;; Sprite memory location
    BALLSPRITE      = $0200
    PADDLESPRITE1   = $0204
    PADDLESPRITE2   = $0208
    SPRITECOUNT     = $0C   ; $0C(#12)  - 3 sprites

.segment "STARTUP"
.segment "CODE"

;;;;;;;;;;;;;;;;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RESET:
    SEI                 ; disable IRQs
    CLD                 ; disable decimal mode
    LDX #$40            
    STX $4017           ; disable APU frame counter IRQ - disable sound
    LDX #$FF
    TXS                 ; setup stack starting at FF
    INX                 ; X is now $00
    STX $2000           ; disable NMI - PPUCTRL reg
    STX $2001           ; disable rendering - PPUMASK reg
    STX $4010           ; disable DMC IRQs

    JSR vblankwait

clearmem:
    LDA #$00
    STA $0000,X
    STA $0100,X
    STA $0300,X
    STA $0400,X
    STA $0500,X
    STA $0600,X
    STA $0700,X
    LDA #$FE
    STA $0200,X        ; set aside space in RAM for sprite data
    INX
    BNE clearmem

    JSR vblankwait

    LDA #$02            ; set the high byte (02) of the RAM address $0200. To set the low byte, do STA $2003
    STA $4014           ; OAM DMA register - access to sprite memory, start DMA transfer
    NOP                 ; burn a cycle as PPU needs a moment to do its thing

    JSR clearbg

loadpalettes:
    ; PPU address port $2006 is used to set the address of palettes
    LDA $2002           ; read PPU status to reset high/low latch
    LDA #$3F    
    STA $2006           ; write high byte of $3F00 address - start of PPU palette memory
    LDA #$00
    STA $2006           ; write low byte of $3F00 address
                        ; $3F00 address is memory location for the background palette, going up to $3F0F (16 bytes)
                        ; the sprite palette is at $3F10, ending at $3F1F, which is 32 bytes > $3F00, so want to loop 32 times
                        ; now the PPU data port at $2007 is ready to accept data
    LDX #$00
loadpaletteloop:
    LDA palettedata,X   ; load palette byte
    STA $2007           ; write to PPU
    INX                 ; increment X
    CPX #$20            ; loop 32 times to write address from $3F00 -> $3F1F - 32 bytes
    BNE loadpaletteloop ; if x = $20, 32 bytes were copied, all done, else loop back

    JSR loadtitlebg
    
    LDX #$00
loadsprites:    
    ; need to enable NMI so that sprite DMA occurs, whrere sprite data is written to ppu memory
    LDA spritedata,X    ; accesses each sprite in spritedata starting at index 0, like reading from an array
    STA $0200,X         ; store in sprite memory in RAM, $0200 - start of RAM sprite storage location
    INX
    CPX #SPRITECOUNT    ; each sprite holds 4 bytes of data - Ycoord, tile, attributes and Xcoord - and there are 3 sprites, 3*4 = 12, or $0C
    BNE loadsprites

    CLI                 ; clear interrupts so NMI can be called
    LDA #%10010000
    STA $2000           ; the left most bit of $2000 sets whether NMI is enabled or not, pattern table 0, fourth - use background from Pattern 1

    LDA #%00011110      
    STA $2001           ; enable sprites, enable background, no clipping on left

initgame:
    LDA #$7E            ; -5 pixels from 128 - center
    STA ballspeedx
    STA ballspeedy
    LDA #BALLSTARTX
    STA ballx
    LDA #BALLSTARTY
    STA bally

forever:
    JMP forever         ; an infinite loop when init code is run

;;;;;;;;;;;;; Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Utility subroutines ;;;;;;;;;;;;;;;;;;;;;;;
vblankwait:
:
    BIT $2002           ; returns bit 7 of ppustatus reg, which holds the vblank status with 0 being no vblank, 1 being vblank
    BPL :-
    RTS

loadbackground:
    ; requires background address to be written in bgcounter variable in 16-bit format
    LDA $2002           ; read PPU status to reset high/low latch
    LDA #$20
    STA $2006           ; write high byte of $2000 address - start of PPU background memory
    LDA #$00
    STA $2006           ; write low byte of $2000 address
    LDX #$00
    LDY #$00
:
    LDA (bgcounter),Y   ; load data from address (background + value in X)
    STA $2007           ; write to PPU
    INY
    CPY #$20            ; compare Y to hex $20 (#32 - copying 32 bytes per row)
    BNE :-
    LDA bgcounter
    ADC #$1F            ; increment counter by $1F (#31)
    STA bgcounter
    LDA bgcounter+1
    ADC #$00
    STA bgcounter+1
    LDY #$00
    INX
    CPX #$1E            ; compare X to hex #1E (#30 - copying 30 rows)
    BNE :-
    
loadattribute:
    LDA $2002           ; read PPU status to reset high/low latch
    LDA #$23
    STA $2006           ; write high byte of $23C0 - start of PPU attribute table memory
    LDA #$C0
    STA $2006           ; write low byte of $23C0
    LDX #$00
loadattributeloop:
    LDA attribute,X     ; load data from address (attribute + value in X)
    STA $2007           ; write to PPU
    INX
    CPX #$08            ; compare to hex $08 (#08 - copying 8 bytes)
    BNE loadattributeloop
    RTS

clearbg:    
    LDA $2002           ; reset PPU status
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006
    LDX #$08            ; prepare to fill 8 pages ($800 bytes)
    LDY #$00            ; X/Y is 16-bit counter, high byte in X
    LDA #$24            ; fill with tile $24 (sky block)
:
    STA $2007
    DEY
    BNE :-
    DEX
    BNE :-
    RTS

latchcontroller:
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016           ; tell both the controllers to latch buttons
    RTS

readcontroller:
    JSR latchcontroller
    LDX #$08
:
    LDA $4016
    LSR A              ; bit0 -> Carry
    ROL buttons        ; bit0 <- Carry
    DEX
    BNE :-
    RTS

;;;;;;;;;;;;;;;;;;; Gameplay subroutines ;;;;;;;;;;;;;;;;;;;;;;;
changestate:
    ; next state must be set in nextstate var
    LDA #$00
    STA $2000           ; disable NMI - PPUCTRL reg
    STA $2001           ; disable rendering - PPUMASK reg
    JSR clearbg
    LDY #$00
    LDA nextstate
    STA state
    JSR vblankwait
    LDA #%10010000      ; the left most bit of $2000 sets whether NMI is enabled or not, pattern table 0, fourth - use background from Pattern 1
    STA $2000
    LDA #%00011110      ; enable sprites, enable background, no clipping on left
    STA $2001
    RTS

handleinputtitle:
    ; Start:
    LDA buttons
    AND #%00010000
    BEQ buttonstartnone
    LDA #STATEGAMEPLAY            ; set state to gameplay
    STA nextstate
    JSR changestate     
buttonstartnone:
    RTS

handleinputgameover:
    RTS

handleinputgameplay:
    ; A:
    LDA buttons
    AND #%10000000
    ; B:
    LDA buttons
    AND #%01000000
    ; Select:
    LDA buttons
    AND #%00100000
    ; Start:
    LDA buttons
    AND #%00010000

    ; Up:
    LDA buttons
    AND #%00001000

    ; Down:
    LDA buttons
    AND #%00000100

    ; Left:
    LDA buttons
    AND #%00000010
    BEQ buttonleftnone
    JSR moveleft
buttonleftnone:

    ; Right:
    LDA buttons
    AND #%00000001
    BEQ buttonrightnone
    JSR moveright
buttonrightnone:
    RTS

moveleft:
    LDX #$00
    LDA PADDLESPRITE1+3,X
    SEC
    SBC #$01
    STA PADDLESPRITE1+3,X
    LDA PADDLESPRITE2+3,X
    SEC
    SBC #$01
    STA PADDLESPRITE2+3,X
    RTS

moveright:
    LDX #$00
    LDA PADDLESPRITE1+3,X
    CLC
    ADC #$01
    STA PADDLESPRITE1+3,X
    LDA PADDLESPRITE2+3,X
    CLC
    ADC #$01
    STA PADDLESPRITE2+3,X
    RTS



movement:
    LDA ballspeedx
    SEC
    SBC #$80
    BMI rollingleft
    JMP rollingright

movementvert:
    LDA ballspeedy
    SEC
    SBC #$80
    BMI rollingup
    JMP rollingdown

movementend:
updateballsprite:
    LDA bally
    STA BALLSPRITE
    LDA ballx
    STA BALLSPRITE+3
    RTS

rollingleft:
    LDA #$80
    SEC
    SBC ballspeedx
    ; register A holds absolute speed value to move left in pixels
    STA ballspeeddx
    LDA ballx
    SEC
    SBC ballspeeddx
    CLC
    CMP #LEFTWALL
    BCC bounceright         ; if carry is not set - A < #LEFTWALL, need to bounce
    STA ballx               ; do actual changes to ball x position
rollingleftend:
    JMP movementvert

rollingright:
    ; register A holds absolute speed value to move right in pixels
    STA ballspeeddx
    LDA ballx
    CLC
    ADC ballspeeddx
    CLC
    CMP #RIGHTWALL
    BCS bounceleft          ; if carry is set - A >= #RIGHTWALL, need to bounce
    STA ballx               ; do actual changes to ball x position
rollingrightend:
    JMP movementvert

rollingup:
    LDA #$80
    SEC
    SBC ballspeedy
    ; register A holds absolute value to move up in pixels
    STA ballspeeddy
    LDA bally
    SEC
    SBC ballspeeddy
    CLC
    CMP #TOPWALL
    BCC bouncedown
    STA bally               ; do actual changes to ball y position
rollingupend:
    JMP movementend

rollingdown:
    ; register A holds absolute value to move down in pixels
    STA ballspeeddy
    LDA bally
    CLC
    ADC ballspeeddy
    CLC
    CMP #BOTTOMWALL
    BCS bounceup
    STA bally               ; do actual changes to ball y position
rollingdownend:
    JMP movementend

bounceright:
    ; register A holds current X position.
    ; variable ballspeeddx holds speed delta (128 - speed)
    CLC
    ADC ballspeeddx      ; position is bounced to the right
    STA ballx       ; store new speed back in variable
    LDA ballspeedx
    CLC
    ADC ballspeeddx
    CLC
    ADC ballspeeddx      ; speed is reflected to the right
    STA ballspeedx
    JMP rollingleftend

bounceleft:
    ; register A holds current X position
    ; variable ballspeeddx holds speed delta (128 + speed)
    SEC
    SBC ballspeeddx      ; position is bounced to the left
    STA ballx
    LDA ballspeedx
    SEC
    SBC ballspeeddx
    SEC
    SBC ballspeeddx      ; speed is reflected to the left
    STA ballspeedx
    JMP rollingrightend

bounceup:
    ; register A holds current Y position
    ; variable ballspeeddy holds speed delta (128 + speed)
    SEC
    SBC ballspeeddy      ; position is bounced up
    STA bally
    LDA ballspeedy
    SEC
    SBC ballspeeddy
    SEC
    SBC ballspeeddy      ; speed is reflected up
    STA ballspeedy
    JMP rollingdownend

bouncedown:
    ; register A holds current Y position.
    ; variable ballspeeddy holds speed delta (128 - speed)
    CLC
    ADC ballspeeddy      ; position is bounced down
    STA bally       ; store new speed back in variable
    LDA ballspeedy
    CLC
    ADC ballspeeddy
    CLC
    ADC ballspeeddy      ; speed is reflected down
    STA ballspeedy
    JMP rollingupend

collision:
    RTS

gameloop:
    LDX state
    CPX #STATETITLE
    BEQ title
    CPX #STATEGAMEPLAY
    BEQ gameplay
    CPX #STATEGAMEOVER
    BEQ gameover
gameloopdone:
    RTS

title:
    JSR handleinputtitle
    JMP gameloopdone

gameplay:
    JSR handleinputgameplay
    JSR collision
    JSR movement   
    JMP gameloopdone

gameover:
    JSR handleinputgameover
    JMP gameloopdone

loadtitlebg:
    ; write needed nametable address into bgcounter variable
    LDA #<titlebg
    STA bgcounter
    LDA #>titlebg
    STA bgcounter+1    
    JSR loadbackground
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VBLANK:
    ; happens at the start of each frame, so has to go there - transfer RAM to PPU through DMA each frame
    LDA #$02
    STA $4014           ; set high byte (02) of RAM, start transfer
    
    JSR readcontroller
    JSR gameloop

@done:
    LDA #%10010000      ; the left most bit of $2000 sets whether NMI is enabled or not, pattern table 0, fourth - use background from Pattern 1
    STA $2000           

    LDA #%00011110      ; enable sprites, enable background, no clipping on left
    STA $2001           

    ; tell PPU that we are not doing any scrolling at the end of NMI:
    LDA #$00
    STA $2005           ; first write - scroll X
    STA $2005           ; second write - scroll Y

    RTI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
palettedata:
    .byte $22, $29, $1a, $0F,   $22, $36, $17, $0F,   $22, $30, $21, $0F,   $22, $27, $17, $0F  ; background palette data
    .byte $22, $16, $27, $18,   $22, $1A, $30, $27,   $22, $16, $30, $27,   $22, $0F, $36, $17  ; sprite palette data

mariosprite:
    .byte $80, $00, $00, $88 ; YCoord, tile number, attr, XCoord
    .byte $80, $01, $00, $90
    .byte $88, $02, $00, $88
    .byte $88, $03, $00, $90
    .byte $90, $04, $00, $88
    .byte $90, $05, $00, $90
    .byte $98, $06, $00, $88
    .byte $98, $07, $00, $90

spritedata:
    .byte BALLSTARTY, $7F, $00, BALLSTARTX
    .byte PADDLEY, $5C, $00, PADDLESTARTX1,    PADDLEY, $5C, %01000000, PADDLESTARTX2                                         ;; mario's leg, right side flipped horizontally

titlebg:                ; $19 $18 $17 $10 - P O N G
    ; $19 $1B $0E $1C $1C    $1C $1D $0A $1B $1D - PRESS START
    .byte $24, $24, $24, $24,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $19, $24, $18,   $24, $17, $24, $10,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $19,   $1B, $0E, $1C, $1C,   $24, $24, $1C, $1D,   $0A, $1B, $1D, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $45, $45, $45, $45,   $24, $24, $24, $24
    .byte $24, $24, $24, $24,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $47, $47, $47, $47,   $24, $24, $24, $24


background:             ; nametable
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24    ;; row 1
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24    ;; all sky ($24 = sky)

    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24    ;; row 2
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24    ;; all sky

    .byte $24, $24, $24, $24,   $45, $45, $24, $24,   $45, $45, $45, $45,   $45, $45, $24, $24    ;; row 3
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $53, $54, $24, $24    ;; some brick tops

    .byte $24, $24, $24, $24,   $47, $47, $24, $24,   $47, $47, $47, $47,   $47, $47, $24, $24    ;; row 4
    .byte $24, $24, $24, $24,   $24, $24, $24, $24,   $24, $24, $24, $24,   $55, $56, $24, $24    ;; brick bottoms

attribute:
    .byte %00000000, %00010000, %00100000, %00010000, %00000000, %00000000, %00000000, %00110000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0
.segment "CHARS"
    .incbin "mario.chr"
