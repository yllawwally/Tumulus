;--------------------------------------------------------------
;Tumulus : Burial Mound, Entrance to the underworld
;One can shoot down, one accross, and one up. That way fireballs don't overlap
;player has corruption on far right
;need to add way to change playfield colors, for enemy special attack, and top eye enemy attack 
;certain pallettes don't work on lanes 6 and 7, The colors for monster types (3,4,5,6,7)
;Boss portion really needs a way for monster to travel whole screen.
;make players as large pits, that can't be jumped by horse, need low num to ignore the attack move
;top line of baddie is wrong because it needs to be loaded in prior line, but not the color
;make giant ogre, out of several pieces
;endline1 is where players arm is miscolored
;EndLin4b is where the color is bad for the arm now
;Need to add grappling
;may be problems with too many points at once, with potion kill
;add second weapon a whip, then whip goes further, but is slower, on horse it cant stay out like the knife, it 
;straddles several lines, so it's like this -----
;                                               -----
;                                                    -----
;Using hmove, not sure how to leave active, use player active? Maybe use ball instead of missle
;make level 1 will o wisp.  All 8 lines get a monster, only one is real.
;maybe monsters shouldn't attack when on fire
;--------------------------------------------------------------
;Hard Coded max monsters 32, 1 for large pit, 1 for small pit, 5 for bosses. horse, tree. 23 possible basic baddies
;add treasure chest?
;level 1 : 20, level 2 : 25, level 3 : 30, level 4 : 35, level 5 40, 155 monsters in whole game.  
;Num baddie types per level 6+level*2
;--------------------------------------------------------------
;5 different level masters
;NAGA, can summon snakes
;Gargoyle
;Griffon
;Vampire, summon undead???, drain life, resurrect helpers in same spot they died, but as next more powerfull?, bats
;Giant, can create craters that move accross screen
;Mummy
;Dragon, breath fire, uses the moving pit in mouth, change color when spit at player
;Minotaur
;Sirens, control player
;Gorgon, causes player death if on same level facing her
;Fairy, can teleport, teleports past you, you must hit her from behind.
;--------------------------------------------------------------
;



;Mandrake Red Plant
;Mandrake Blue Plant
;Mandrake Red Man
;Mandrake Blue Man
;Ghost
;Snake Man
;Snake
;Midget Fighter
;Horse
;Brownie
;Large Pit
;Small Pit
;blob



;Will O Wisp
;mummy will take 2 images to make him big, and change pit color
;mummy b
;Gargoyle, can fly straight at player quickly
;Giant, creates moving craters. also large like mummy
;Giant b
;Dragon, large, shoots fire ball pits. only see head?
;Dragon b





	processor 6502
	include vcs.h
	include macro.h

; Constants ------
C_P0_HEIGHT 		= 8	;height of sprite
C_P1_HEIGHT 		= 12	;height of hero sprite
C_KERNAL_HEIGHT 	= 182	;height of kernal/actually the largest line on the screen ;was 186
Far_Left		= 8
Far_Right		= 140
Far_Right_Hero		= 128
Far_Up_Hero		= 180
Far_Down_Hero		= 21+C_P1_HEIGHT
Enemy_Far_Left		= 4
Enemy_Pause_Left	= 6
HERO_SPEED_VER		= 1
HERO_SPEED_HOR		= 1
Screen_Rate		= 20	;How fast screen is scrolling in X-Axis, for introducing enemy, not used yet
Enemy_Row_0		= 185  ;185
Enemy_Row_E0		= 165  ;160
Enemy_Row_E1		= 145  ;109
Enemy_Row_E2		= 125  ;109
Enemy_Row_E3		= 105  ;109
Enemy_Row_E4		= 85   ;109
Enemy_Row_E5		= 65   ;109
Enemy_Row_E6		= 45   ;73
Enemy_Row_E7		= 23   ;35  
Min_Eye_Trigger		= 6
LVL1BOSS		= 14

;Variables ------





	seg.u RAM
	org $0080



PICS 			ds 1
ROLLING_COUNTER 	ds 2





E0_Ptr 			ds 1	;ptr to current graphic
E1_Ptr 			ds 1	;ptr to current graphic
E2_Ptr 			ds 1	;ptr to current graphic
E3_Ptr 			ds 1	;ptr to current graphic
E4_Ptr 			ds 1	;ptr to current graphic
E5_Ptr 			ds 1	;ptr to current graphic
E6_Ptr 			ds 1	;ptr to current graphic
E7_Ptr 			ds 1	;this is a temp variable for displaying score only

E0_Ptr2 		ds 1	;ptr to current graphic
E1_Ptr2 		ds 1	;ptr to current graphic
E2_Ptr2 		ds 1	;ptr to current graphic
E3_Ptr2 		ds 1	;ptr to current graphic
E4_Ptr2 		ds 1	;ptr to current graphic
E5_Ptr2 		ds 1	;ptr to current graphic
E6_Ptr2 		ds 1	;ptr to current graphic
;E7_Ptr2 		ds 1	;ptr to current graphic
Temp_Ptr		ds 2	;ptr for temp holding
Direction		ds 1	;direction character moving

Overeyes		ds 1
EnemyGraphicsColorPtr_E0	ds 1
EnemyGraphicsColorPtr_E1	ds 1
EnemyGraphicsColorPtr_E2	ds 1
EnemyGraphicsColorPtr_E3	ds 1
EnemyGraphicsColorPtr_E4	ds 1
EnemyGraphicsColorPtr_E5	ds 1
EnemyGraphicsColorPtr_E6	ds 1
;EnemyGraphicsColorPtr_E7	ds 1
TempGraphicsColor		ds 2

E0_XPos 		ds 1	;horizontal position
E1_XPos 		ds 1	;horizontal position
E2_XPos 		ds 1	;horizontal position
E3_XPos 		ds 1	;horizontal position
E4_XPos 		ds 1	;horizontal position
E5_XPos 		ds 1	;horizontal position
E6_XPos 		ds 1	;horizontal position
;E7_XPos 		ds 1	;horizontal position
Temp_XPos		ds 1	;temp horizontal position


E0_Type			ds 1	;Enemy Type
E1_Type			ds 1	;Enemy Type
E2_Type			ds 1	;Enemy Type
E3_Type			ds 1	;Enemy Type
E4_Type			ds 1	;Enemy Type
E5_Type			ds 1	;Enemy Type
E6_Type			ds 1	;Enemy Type
;E7_Type			ds 1	;Enemy Type

E0_Health		ds 1	;Enemy Life Stat
E1_Health		ds 1	;Enemy Life Stat
E2_Health		ds 1	;Enemy Life Stat
E3_Health		ds 1	;Enemy Life Stat
E4_Health		ds 1	;Enemy Life Stat
E5_Health		ds 1	;Enemy Life Stat
E6_Health		ds 1	;Enemy Life Stat
;E7_Health		ds 1	;Enemy Life Stat

Pit0_XPos		ds 1	;where pit is currently
Pit1_XPos		ds 1	;where pit is currently
Pit2_XPos		ds 1	;where pit is currently
Pit3_XPos		ds 1	;where pit is currently
Pit4_XPos		ds 1	;where pit is currently
Pit5_XPos		ds 1	;where pit is currently
Pit6_XPos		ds 1	;where pit is currently
;Pit7_XPos		ds 1	;where pit is currently
Offset			ds 1
TempPit_XPos		ds 1	;where next pit is

Hero_YPosFromBot 	ds 2	;Vertical position
Hero_XPos 		ds 2	;horizontal position
Hero_Y 			ds 1	;needed for skipdraw
Hero_Ptr 		ds 2	;ptr to current graphic
Hero_Sword_Pos		ds 1
HeroGraphicsColorPtr	ds 2
Player_Health		ds 1
MOV_STAT		ds 1 	;direction player is moving

Graphics_Buffer		ds 1	;buffer for graphics
Graphics_Buffer_2	ds 1	;buffer for graphics


Enemy_Life		ds 1	;what eneamies are alive onscreen
Pause			ds 1 	;are we waiting while enemy goes back and forth

PF0_L1 			ds 1	; playfield buffer 
PF0_L2 			ds 1	; playfield buffer 
PF0_L3 			ds 1	; playfield buffer 
PF0_L4 			ds 1	; playfield buffer                       

PF1_L1 			ds 1	; playfield buffer 
PF1_L2 			ds 1	; playfield buffer 
PF1_L3 			ds 1	; playfield buffer 
PF1_L4 			ds 1	; playfield buffer                       

PF2_L1 			ds 1	; playfield buffer 
PF2_L2 			ds 1	; playfield buffer 
PF2_L3 			ds 1	; playfield buffer 
PF2_L4 			ds 1	; playfield buffer 

PF3_L1 			ds 1	; playfield buffer 
PF3_L2 			ds 1	; playfield buffer 
PF3_L3 			ds 1	; playfield buffer 
PF3_L4 			ds 1	; playfield buffer                       

PF4_L1 			ds 1	; playfield buffer 
PF4_L2 			ds 1	; playfield buffer 
PF4_L3 			ds 1	; playfield buffer 
PF4_L4 			ds 1	; playfield buffer                       

PF5_L1 			ds 1	; playfield buffer 
PF5_L2 			ds 1	; playfield buffer 
PF5_L3 			ds 1	; playfield buffer 
PF5_L4 			ds 1	; playfield buffer 

PF_TEMP			ds 1	; playfield buffer temp
Multi_Temp		ds 1 	; 


Score			ds 1
Score1			ds 1
Score2			ds 1

Row_1			ds 1	
Row_2			ds 1
Row_3			ds 1

duration		ds 1
musicData		ds 1
onhorse			ds 1
swordduration		ds 1
SFX_Duration		ds 1 	;any sound effect other than music

Other_Hit		ds 1	;collision detection
New_Hit			ds 1	;collision detection
Player_Hit		ds 1	;collision detection

Baddie_Duration		ds 1	;time till next baddie
Baddie_Num		ds 1	;current baddie displayed

Potion			ds 1	;Potions player currently has
RNG			ds 1
Pit_Color		ds 1
	seg code
	org $F000



;generic start up stuff...
Start
	SEI	
	CLD  	
	LDX #$FF	
	TXS	
	LDA #0

	
ClearMem 
	STA 0,X		
	DEX		
	BNE ClearMem	
	


	


;Setting some variables...


	lda #17
	sta Offset

	LDA #%11111111
	STA Enemy_Life
	STA Player_Health
	STA Pit_Color

	LDA #%00010000 ;set playfield to not reflected
	STA CTRLPF
	sta duration


	LDA #%11111000	;The last 3 bits control number and size of players
			;the second 2 bits control missle size
	STA NUSIZ0
	STA NUSIZ1

	LDX #3

LOADPFDATA


	LDA PFData0-1,X		
	STA PF0_L1-1,x ;B
	LDA PFData1-1,X		
	STA PF1_L1-1,x ;B
	LDA PFData2-1,X		
	STA PF2_L1-1,x ;C
	LDA PFData3-1,X		
	STA PF3_L1-1,x ;d
	LDA PFData4-1,X		
	STA PF4_L1-1,x ;E
	LDA PFData5-1,X		
	STA PF5_L1-1,x ;F

	DEx	
	BNE LOADPFDATA

	LDA #40
	STA Hero_XPos

;VSYNC time


MainLoop ;+++++++++++++++++++++++++++The start of a new screen
MainLoopStart
	LDA #0
 	STA PF0
	STA PF1
	STA PF2
	LDA #30
	STA VSYNC	
	

	JMP MORECALCS
MORECALCSRET
;----------------------



	STA WSYNC ;//////////////////////////////////////////////	
	STA HMOVE




	LDA #43 ;was 43	
	STA TIM64T	





	LDA #0
	STA VSYNC 	

;----------lots of time

;------------------------------------------




;setup pic animations ----------------------------------------------
	INC ROLLING_COUNTER
	BNE RCROLLOVER
	INC ROLLING_COUNTER+1
RCROLLOVER





	LDA ROLLING_COUNTER

	AND #15		;every 8th screen swap to next image of player
	CMP #8
	BNE PICSET3
PICSET4	LDA  PICS
	CMP  #8
	BEQ PICSET
	LDA  #8
	JMP PICSET2
PICSET	LDA  #0
PICSET2	STA PICS
PICSET3


	LDA ROLLING_COUNTER
	AND #7
	CMP #0
	BNE NS0
	JMP SLICE0
NS0
	CMP #1
	BNE NS1
	JMP SLICE1
NS1
	CMP #2
	BNE NS2
	JMP SLICE2
NS2
	CMP #3
	BNE NS3
	JMP SLICE3
NS3

	CMP #4
	BNE NS4
	JMP SLICE1 ;Baddie Movement is twice as fast
NS4
	CMP #5
	BNE NS5
	JMP SLICE4 ;Baddie Movement is twice as fast
NS5
	CMP #6
	BNE NS6
;	JMP SLICE1 ;Baddie Movement is twice as fast
NS6
	CMP #7
	BNE NS7
;	JMP SLICE1 ;Baddie Movement is twice as fast
NS7

	JMP ENDSLICES






SLICE4

	LDA Potion
	CMP #10	
	bcc OverPotionB
	sec 
	sbc #%00010000
	sta Potion
	lda #$FF
	sta Other_Hit
	
OverPotionB	
	JMP ENDSLICES





SLICE1
	
	

	LDX #6

;Eneamy Movement---------------------------------------------------

MOVELEFT
	LDA Pause
	BNE dontmovepit
	LDA #0
	cmp Pit0_XPos-1,x
	beq dontmovepit
	CMP onhorse
	beq notonhorsepit
	DEC #Pit0_XPos-1,x
notonhorsepit
	DEC #Pit0_XPos-1,x
dontmovepit

	LDA #Multiplexer-1,x
	AND Enemy_Life
	BEQ PAUSED	
	LDA #Multiplexer-1,x
	AND Direction
	BNE FORWARD
	DEC E0_XPos-1,x
	LDA onhorse
	beq DONEMOVE
	DEC E0_XPos-1,x
	JMP DONEMOVE
FORWARD	
	LDA #E0_Type-1,x
	CMP #Min_Eye_Trigger
	BCC DONEMOVE
	INC E0_XPos-1,x
	LDA onhorse
	beq DONEMOVE
	INC E0_XPos-1,x
DONEMOVE

	LDA #E0_Type-1,x
	CMP #Min_Eye_Trigger
	BCC DONTCHANGER
	LDA E0_XPos-1,x
	CMP #Enemy_Pause_Left
	BCS DONTCHANGEL
	LDA Direction
	ORA Multiplexer-1,x
	STA Pause
	STA Direction
	JMP DONTCHANGER
DONTCHANGEL
	CMP #Far_Right-10
	BCC DONTCHANGER
	LDA Direction
	AND Mask-1,x
	STA Direction
DONTCHANGER


PAUSED
	DEX
	BNE MOVELEFT

	LDA Enemy_Life
	BNE DONTCHANGEDIR
	STA Pause
DONTCHANGEDIR


MOVESET1


	
	JMP ENDSLICES




ENDSLICES

;------------------------- setup backgrounds 20 pixels accross
	LDA #255		; 3 cycles
	STA PF0			; 3 cycles
	STA PF1			; 3 cycles
	STA PF2			; 3 cycles
;-------------------------
	LDA #0
	STA HMP1 ;Set Hero to stand still
;	STA ENABL ;clear pits

;	STA WSYNC ;//////////////////////////////////////////////
;	STA HMOVE

	
rand_8
	LDA	RNG		; get seed
	BNE Not_Zero
	LDA ROLLING_COUNTER
Not_Zero
	ASL			; shift byte
	BCC	no_eor		; branch if no carry

	EOR	#$CF		; else EOR with $CF
no_eor
	STA	RNG		; save number as next seed




	LDY #0
	LDX #7
Collision

	LDA Multiplexer-1,x
	AND Other_Hit
	BEQ NoCollisionP0	;skip if not hitting...		
	LDA Multiplexer-1,x
	AND Enemy_Life	
	BEQ NoCollisionP0	;skip if not alive

	lda E0_Type-1,x
	cmp #5
	bcc dontpause
	sta Pause

	CMP #6
	BEQ Type4

	CMP #7
	BEQ Type5


Type2
	LDA Direction ;make baddie change dir if hit
	ORA Multiplexer-1,x ;makes him go right
	STA Direction
	STA Pause
	JMP dontpause
Type3
	LDA Direction ;make baddie change dir if hit
	EOR Multiplexer-1,x ;makes him just swap direction
	STA Direction
	STA Pause
	JMP dontpause
Type4
	LDA Mask-1,x ;makes him go left
	AND Direction ;make baddie change dir if hit
	STA Direction
	STA Pause
	JMP dontpause

Type5
	LDA E0_XPos-1,x
	CMP Hero_XPos
	BCS Type4
	JMP Type2
		

dontpause
	DEC E0_Health-1,x
	BMI ExtraDead
	LDA E0_Type-1,x
	CMP #LVL1BOSS
	BEQ BOSS1
	BNE NoCollisionP0
ExtraDead
	lda E0_Type-1,x
	sty PF_TEMP
	adc PF_TEMP
	ldy BADDIEVALUE,x
	LDA Mask-1,x
	AND Enemy_Life
	STA Enemy_Life
	LDA E0_Type-1,x

	CMP #3
	BEQ RedMandrakePlant
	CMP #4
	BEQ BlueMandrakePlant
	CMP #5
	BEQ BlueMandrakeMan
	CMP #6
	BEQ RedMandrakeMan

	CMP #1
	BNE NoCollisionP0

	LDA #24
	sta onhorse

NoCollisionP0
	lda E0_Type-1,x
;	cmp #1 ;Not hurt by horse
;	Beq notsmacked


	cmp #Min_Eye_Trigger
	bcc nosnakepause
	JMP notsnake

BOSS1
	stx Multi_Temp
	lda E0_Health-1,x
	sta PF_TEMP
	lda #0
	sta E0_Health-1,x
	sta E0_Type-1,x
	LDA Mask-1,x
	AND Enemy_Life
	STA Enemy_Life
	lda RNG
	and #%00000111
	cmp #%00000111
	bne NotOutsideBOSSRange
	and #%00000110	
NotOutsideBOSSRange
	tax
	LDA Multiplexer-1,x
	ORA Enemy_Life
	STA Enemy_Life
	lda PF_TEMP
	sta E0_Health-1,x
	lda #14
	sta E0_Type-1,x
	ldx Multi_Temp
	JMP NoCollisionP0

RedMandrakeMan
	SEC
	ROR Player_Health
	jmp notsnake
BlueMandrakeMan
	INC Potion
	jmp notsnake
RedMandrakePlant
	lda #5
	jmp mandrake
BlueMandrakePlant
	lda #6
mandrake
	sta E0_Type-1,x
	LDA Multiplexer-1,x
	ORA Enemy_Life
	STA Enemy_Life	
	LDA #3
	sta E0_Health-1,x

nosnakepause
	lda #$FF
	;sta Pause
notsnake

	lda Multiplexer-1,x
	and Player_Hit
	beq notsmacked
	asl Player_Health
;	sta Pause
notsmacked

	DEX
	BEQ NoCollision
	JMP Collision
NoCollision	
	tya
	ldy #0
	ldx #0
NotInvincable
	cmp #0
	beq NOSCORE

;---------------------Increment Score
        sed
        clc
        adc Score
        sta Score
        txa
        adc Score+1;also adds carry
        sta Score+1
        tXa
        adc Score+2;also adds carry
        sta Score+2
        cld
;----------------------Increment Score
NOSCORE
	

	lda Enemy_Life
	BNE KEEPPAUSE
	sta Pause
	sta Direction

KEEPPAUSE
	
;---------------------------------------

;	lda onhorse
;	bne Did_Not_Hit_Pit
	lda Player_Hit
	cmp #1
	lda #0
	BCS Hit_Pit
	LDA New_Hit
	CMP #1
	LDA #0
	BCC Did_Not_Hit_Pit
	ASL Player_Health
Hit_Pit
	sta onhorse
Did_Not_Hit_Pit


not_on_horse 

;setup pic animations ----------------------------------------------

	lda ROLLING_COUNTER
	and #%00001000
;	cmp #0
	bne RCP_1 
	lda onhorse
;	cmp #0	
	bne swinging
	lda swordduration
;	cmp #0
SW1	beq swinging
SW2	cmp #11
SW3	bcs swinging
SW4	lda #>HeroGraphics2 ;high byte of graphic location
SW5	sta Hero_Ptr+1	;store in high byte of graphic pointer
SW6	lda #<HeroGraphics2
SW7	jmp noswinging
swinging


	lda #>HeroGraphics0 ;high byte of graphic location
SW8	sta Hero_Ptr+1	;store in high byte of graphic pointer
SW9	clc
SW10	lda #<HeroGraphics0 	;low byte of ptr is graphic
	ADC #C_P1_HEIGHT
noswinging
	CLC	;clear carry
	adc onhorse
	sta Hero_Ptr		;(high byte already set)
	JMP RCP_2

RCP_1 
	lda #>HeroGraphics0 ;high byte of graphic location
	sta Hero_Ptr+1	;store in high byte of graphic pointer
	clc
	lda #<HeroGraphics0 	;low byte of ptr is graphic
	adc onhorse
RCP_2
	sta Hero_Ptr		;(high byte already set)



TESTPOINTD	

	lda onhorse
	cmp #1
	bcs STARTHORSEMULT

	LDA #%11011000	;The last 3 bits control number and size of players
			;the second 2 bits control missle size

	LDX swordduration
	cpx #4
	bcc ENDHORSEMULT
	LDA #%11101000	;The last 3 bits control number and size of players
			;the second 2 bits control missle size
SMALLSWORD
	JMP ENDHORSEMULT
STARTHORSEMULT
	LDA #%11101101	;The last 3 bits control number and size of players
			;the second 2 bits control missle size

ENDHORSEMULT

	STA NUSIZ1



	LDA Enemy_Life
	BNE Enemies_Alive
	STA Overeyes

Enemies_Alive
	


	
	;for skipDraw, E0_Y needs to be set (usually during VBLANK)
	;to Vertical Position (0 = top) + height of sprite - 1.
	;we're storing distance from bottom, not top, so we have
	;to start with the kernal height and YPosFromBot...
	lda #C_KERNAL_HEIGHT + #C_P1_HEIGHT - #2
	sec
	sbc Hero_YPosFromBot ;subtract integer byte of distance from bottom
	sta Hero_Y


	;we also need to adjust the graphic pointer for skipDraw
	;it equals what it WOULD be at 'normally' - it's position
	;from bottom plus sprite height - 1.
	;(note this requires that the 'normal' starting point for
	;the graphics be at least align 256 + kernalheight ,
	;or else this subtraction could result in a 'negative'
	; (page boundary crossing) value
	lda Hero_Ptr
	sec
	sbc Hero_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P1_HEIGHT - #1
	sta Hero_Ptr	;2 byte

	LDA #<HeroGraphicsColor
	sta HeroGraphicsColorPtr
	LDA #>HeroGraphicsColor
	sta HeroGraphicsColorPtr+1
 
	lda HeroGraphicsColorPtr
	sec
	sbc Hero_YPosFromBot
	clc
	adc #C_P1_HEIGHT - #1
	adc onhorse	
	STA HeroGraphicsColorPtr
	


;setup pic animations ----------------------------------------------


	lda GraphicsColorTableHigh ;Cheating because all the color is in the same bank
	sta TempGraphicsColor+1    ;Cheating because all the color is in the same bank

	ldy #8


setuppics

	lda Multiplexer-1,y

	and Enemy_Life
	BNE alive1b	
	lda #<EmptyPlayerGraphics 	;low byte of ptr is graphic
	sta #E0_Ptr-1,y		;(high byte already set)

	lda #>EmptyPlayerGraphics ;high byte of graphic location
	sta #E0_Ptr2-1,y	;store in high byte of graphic pointer
	jmp notalive1b
alive1b


	ldx E0_Type-1,y

;GraphicsTable

	lda GraphicsTableLow,x 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E0_Ptr-1,y

	lda GraphicsTableHigh,x ;high byte of graphic location

	sta E0_Ptr2-1,y




notalive1b


	lda E0_Ptr-1,y
	sec
	sbc DistFromBottom-1,y	;integer part of distance from bottom
	sta E0_Ptr-1,y	;2 byte


	lda Potion
	cmp #$10
	bcc AdjustTableForColorHitTest
	LDA #<EnemyFireColor
	JMP AdjustTableForColor
AdjustTableForColorHitTest
	lda Multiplexer-1,y
	and Other_Hit
	cmp #1
	LDA #<EnemyHitColor
	bcs AdjustTableForColor
	LDA GraphicsColorTableLow,x
AdjustTableForColor
	sec
	sbc DistFromBottom-1,y
	STA EnemyGraphicsColorPtr_E0-1,y


;setup pic animations ----------------------------------------------
	dey
	bne setuppics


;setup pic animations ----------------------------------------------







	LDA %00000001
	STA VDELP0
;	STA VDELP1
	
;setup php ball trick
	ldx #ENAM0+1
	txs	;set stack to enam location
;end ball trick setup
	
	LDY #4 ; was 191 	
;----------lots of time
	LDA #$70
	STA COLUPF


TESTPOINTF

;music section ------------------------------------------------------------------------

	lda #180
	cmp Hero_Sword_Pos
	bcc NoSwordSound

	ldx swordduration
	lda SwordSongv-1,x
	STA AUDV1


	lda SwordSongc-1,x
	STA AUDC1

	lda SwordSongf-1,x
	STA AUDF1

	jmp LeftSound
NoSwordSound


LeftSound

	DEC duration
	bne GotMusic



	ldx musicData
	lda LEFTAUD,x
	AND #%00000111
	STA AUDV0

;	lda LEFTAUDC,x  ;always 12 so cheating
	nop ;why do I need this delay, player skips when moving without it
	nop
	lda #12 
	STA AUDC0

	lda LEFTAUD,x
	LSR
	LSR
	LSR
	STA AUDF0


skipL052


	lda #180
	cmp Hero_Sword_Pos
	bcs SkipRightSound2


;	lda RIGHTAUDV,x ;always 4 so cheating
	lda #4
	STA AUDV1

;	lda RIGHTAUDC,x ;was always 6 so cheating
	lda #6
	STA AUDC1

	lda RIGHTAUDF,x
	STA AUDF1
SkipRightSound2





;	lda NOTEDURATION,x ;Notes were alway 14, so cheating
	lda #14
	inc musicData
	inx
	STA duration


GotMusic


	lda LEFTAUD,x
	cmp #255
	bne resetx
	lda #0
	sta musicData
resetx
	
;music section ------------------------------------------------------------------------

	lda #0
	sta New_Hit
	sta Other_Hit

	bit SWCHB
	bmi leftdif
	lda #24
	jmp nozero
leftdif	
	lda #0
nozero
;	sta onhorse

TESTPOINTG
;-test to start on horse
;	LDY #4

	LDA #0

	sta Other_Hit
	sta Player_Hit
	sta New_Hit

	CMP Pause
	BCS NOBIGEYES
	LDA Overeyes
	CMP #240
	LDA #0
	BCC NOEyesYet
	LDA #240
	STA Overeyes
	LDA #$FF

NOEyesYet	
	LDY #4
NOBIGEYES
	sta GRP0

WaitForVblankEnd
	LDA INTIM	
	BNE WaitForVblankEnd	


	STA WSYNC	
	STA VBLANK  	


;main scanline loop...


PreScanLoop

ScanLoopHero ;start of kernal +++++++++++++++++++++++ for Hero positioning
	clc
	lda Hero_XPos ;3


.Div15Hero   
	sbc #15      ; 2      
	bcs .Div15Hero   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP1 ;reset amount to fine adjust player
	sta RESP1 ;reset where the player is
	sta RESM1 ;reset where the sword is
	sta HMM1 ;reset amount to fine adjust sword


        STA WSYNC                                                ;3 cycles =
	STA HMOVE						 ;3
EndScanLoopHero ;end of kernal +++++++++++++++++ for Hero positioning
.100:
;this is to align sword
	nop.w ;these 2 nops allow the hero to do fine positioning???? I don't know why.....prob the hmclr
	DEC Hero_Y
        DEY             ;count down number of scan lines          2 cycles = 
        DEY             ;count down number of scan lines          2 cycles = 
	nop.w
	DEY
	STA HMCLR
	LDA MOV_STAT
	CMP #1
	BCS MLEFT
	JMP MRIGHT
MLEFT	
	LDA #%00110000 ;Atari only looks at 4 most significant bits in 2's compliment, this is the distance from player
MRIGHT	STA HMM1

	STA CXCLR	;reset the collision detection for next time
;
;	LDA #%01110000 
;	STA HMM0

	LDA #$0
	STA GRP1
	LDY #4
	CMP Pause
	BCS NOBIGEYES2
	LDY #8
NOBIGEYES2

	LDA #$72
	STA COLUPF
	LDA #%00000010
	STA NUSIZ0



	STA WSYNC
	STA HMOVE ;3

	lda MOV_STAT
	cmp #1
	bcs HORSEKNIFELEFT

	lda onhorse
	cmp #1
	LDA #%11100000 ; on horse value facing right
	bcs onhorseknife
	LDA #%00000000 ;not on horse value facing right
onhorseknife
	STA HMM1

	JMP NOTONHORSEKNIFE
HORSEKNIFELEFT

	lda onhorse
	cmp #1
	LDA #%00100000 
	bcs onhorseknife2
	LDA #%00110000 
onhorseknife2
	STA HMM1


NOTONHORSEKNIFE
;DEX
	LDA #$44
	STA COLUP0

	LDA #%00000001	
	STA VDELBL
	LDX #0

	LDA PFCOLOR-1,Y		; 4 cycles


	STA WSYNC
;start of kernal +++++++++++++++++++++++ for player 0 positioning
	STA HMOVE

;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
MTNRANGE2

	STA COLUBK		;and store as the bgcolor ; 3 cycles
	LDA PFCOLORB-1,Y
	STA COLUPF

	LDA PF0_L1,x		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF1_L1,x		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF2_L1,x		; 4 cycles
	STA PF2			; 3 cycles
				 ;3 cycles 


	LDA PF3_L1,x		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF4_L1,x		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L1,x		; 4 cycles
	STA PF2			; 3 cycles


	dey	
	INX

	

	LDA PFCOLOR-1,Y		; 4 cycles

	cpx #4
	STA WSYNC 
	bne MTNRANGE2
	STA HMOVE

;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
	
	STA COLUPF
	LDA #$12;

	STA COLUBK	;and store as the bgcolor
	LDY #C_KERNAL_HEIGHT; 
	LDA #0
	STA PF_TEMP
	STA PF0			; 3 cycles
	STA PF1			; 3 cycles
	STA PF2			; 3 cycles
	STA GRP0
	TAX



	STA WSYNC 
	STA HMOVE

	INC Hero_Y
	INC Hero_Y
	INC Hero_Y

	STA NUSIZ0
	STA GRP1
	STA HMCLR

	LDA Multiplexer,1 ;This is where you control number of critters on screen //////////////////////////////////
	sta Multi_Temp	

	

	LDA Overeyes
	CMP #240
	BNE NOQUAKE
	LDA ROLLING_COUNTER
	AND #%00000111
	BEQ NOQUAKE
	STA WSYNC
	STA WSYNC
	
NOQUAKE
	
	clc
	lda Offset
	sbc #15
SHIFTING2
	sbc #1	
	bcc NOQUAKE4
	STA WSYNC
	JMP SHIFTING2
NOQUAKE4


	cpy Hero_Sword_Pos  ;3
	
	STA WSYNC



New_E2_Start
;----------THE NEW START OF E2-----+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+--------------------
	stx	GRP1	;3
	sta 	COLUP1 


;sword php style
	php			;3


  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;--------------need to setup all enemy variables--------------------------------------------------



	ldx PF_TEMP
	
	lda Pit0_XPos,x
	sta TempPit_XPos
	;CMP #0
	beq NOPITTHISLEVEL
	clc ;this is needed beacause of the subtract, and the sword compare
	lda Enemy_Row_Data,x
	sbc #14 ;was 15
	sta	Row_1	

	lda Enemy_Row_Data+1,x
	sbc #100 ;
	sta	Row_3
	adc #8 ;was 5 8
	sta	Row_2
	JMP YESPIT	
	
NOPITTHISLEVEL
	clc ;this is needed beacause of the subtract, and the sword compare
	lda Enemy_Row_Data,x
	sbc #14 ;was 15
	sta	Row_1	

	lda Enemy_Row_Data+1,x
	sbc #1 ;
	sta	Row_3
	lda #1 ;was 5
	sta	Row_2


YESPIT
;--------------need to setup all enemy variables--------------------


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_ebr       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_ebr:
	lax     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine

	lda     (HeroGraphicsColorPtr),y      ; 5



new_E1_line1  ;   STA WSYNC                                          
;----------added section----------------------------------------


;----------added section----------------------------------------
	stx	GRP1	;3
	sta 	COLUP1
        DEY             ;2 count down number of scan lines          2 cycles
	


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;--------------need to setup all enemy variables--------------------------------------------------

	ldx PF_TEMP
	
	lda E0_XPos,x
	sta Temp_XPos


	lda EnemyGraphicsColorPtr_E0,x
	sta TempGraphicsColor





;--------------need to setup all enemy variables--------------------


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_ebrt       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_ebrt:
	lax     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles

	



new_E1_line2     STA WSYNC                ;not enough time                               
;----------added section----------------------------------------


;----------added section----------------------------------------
	stx	GRP1	;3
	sta 	COLUP1
	


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3

	

  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;--------------need to setup all enemy variables--------------------------------------------------

	ldx PF_TEMP
	

	lda #E0_Ptr,x
	sta Temp_Ptr

	lda #E0_Ptr2,x
	sta Temp_Ptr+1


;--------------need to setup all enemy variables--------------------

	ldx #0
	stx GRP0




;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_ebrt3       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_ebrt3:
	lax     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine


	lda     (HeroGraphicsColorPtr),y      ; 5




	DEY


	cpy Hero_Sword_Pos  ;3

new_E1_line3     STA WSYNC                                   
;----------added section----------------------------------------


;------------------------------------------------------------
	stx	GRP1	;3
	sta 	COLUP1 ;3

;sword php style
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw

;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb21ZZ       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb21ZZ:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer_2
	lax     (HeroGraphicsColorPtr),y      ; 5

	dey

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb22ZZ       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb22ZZ:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer




	lda  Graphics_Buffer_2
	cpy Hero_Sword_Pos  ;3

	stx COLUP1
 	php	;2
	DEY



EndLine1   STA WSYNC  
;------------------------------------------------+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.600: SUBROUTINE
;This is not a loop, this is a one time set position for the eneamy

	clc
	sta GRP1
	lda Temp_XPos ;3
.Div15_E2_a   
	sbc #15      ; 2         
	bcs .Div15_E2_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 
	sta RESP0 


;sword php style	
EndLine2        STA WSYNC                                                ;3 cycles =
	STA HMOVE

;---------------------------------------------

;This is not a loop, this is a one time set position for the eneamy E2

	pla ;this is to reset sword
;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3

	ldx 	Graphics_Buffer  ;3
	stx	GRP1	;3



;	INY

	lda     (HeroGraphicsColorPtr),y      ; 5
	sta	COLUP1






;	DEY




	pla ;4 Set the top of the stack to ENAM1+1

;sword php style 
;skipDraw

	ldx PF_TEMP
PITCOLLISION
;---------collision
	BIT CXPPMM ;3 cycles
	BPL No_Hit_the_Pit ;2 cycles
	LDA Multi_Temp
	ORA New_Hit ;4 cycles
	STA New_Hit  ;4 cycles
No_Hit_the_Pit

;---------collision




;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E2_e       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_e:
	lax     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine



	lda     (HeroGraphicsColorPtr),y      ; 5
;        DEY             ;2 count down number of scan lines          2 cycles




	inc PF_TEMP




	STA CXCLR   ;3

EndLine3    ;   STA WSYNC                                                ;3 cycles =
;------------------------------------------------------------


Draw_Enemy_E2
.608:
;-----------------------this needs to run for 8 lines, the size of an enemy
;sword php style

	sta 	COLUP1

	lda     (TempGraphicsColor),y      ; This is for monster because color is not delayed
	sta	COLUP0	;3 Needs to be as late in the line as possible


	DEY


	cpy Hero_Sword_Pos  ;3
	php	
;skipDraw


	stx	GRP1	;3


	lda     (Temp_Ptr),y      ; 5
	sta 	GRP0	;3



	pla 		;3
;sword php style 
	

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1    
	dcp     Hero_Y                                                                                                                                                                                                                                                                                    
	bcs     .doDrawHero_E2_eb8      
	ldx     #0              
	.byte   $2c             
.doDrawHero_E2_eb8:
	lax     (Hero_Ptr),y     





	lda     (HeroGraphicsColorPtr),y     
 



	cpy Row_1
	bcc EndLine4
	STA WSYNC
	JMP Draw_Enemy_E2
EndLine4 
	  	;this is where the arm color is being messed up
EndLin4b STA WSYNC 
;-----------------------this needs to run for 8 lines, the size of an enemy
	sta 	COLUP1 ;3

;---------------------line for setting up pits-----------------------------------------
;sword php style
	stx	GRP1	;3
        DEY             

	cpy Hero_Sword_Pos  ;3
	php			;3
	pla 
;sword php style 



;---removed enemy display character

collisionplayermissle

;--------New Collision Detection Style
	BIT CXM1P ;3 cycles
	BPL No_Hit_the_Baddie ;2 cycles
	LDA Multi_Temp ; 3 cycles
	ORA Other_Hit ;4 cycles
	STA Other_Hit  ;4 cycles
	JMP Hit_Baddie
No_Hit_the_Baddie
	BIT CXPPMM ;3 cycles
	BPL Hit_Baddie ;2 cycles
	LDA Multi_Temp ; 3 cycles
	ORA Player_Hit ;4 cycles
	STA Player_Hit  ;4 cycles
Hit_Baddie
;--------New Collision Detection Style



;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb21az       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb21az:
	lax     (Hero_Ptr),y      ; 5
	
	lda 	#$0      ;2
	sta 	GRP0    ;3




	lda     (HeroGraphicsColorPtr),y      ; 5


	dey
	cpy Hero_Sword_Pos  ;3

	sta 	COLUP1 ;3
EndLine5 	sta WSYNC

;---------------------line for setting up pits-----------------------------------------
;sword php style

	php	
	stx	GRP1	;3

	lda	Pit_Color
	sta 	COLUP0

	pla


;skipDraw

;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb21a       ; 2/3 ; should be bcs
	lda     #0              ; 3
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb21a:
	lda     (Hero_Ptr),y      ; 5
	sta Graphics_Buffer_2
	lax     (HeroGraphicsColorPtr),y      ; 5

	dey


midline6


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb22a       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb22a:
	lda     (Hero_Ptr),y      ; 5
	sta 	Graphics_Buffer


	lda  Graphics_Buffer_2






	cpy Hero_Sword_Pos  ;3          
 	php	;2
	stx COLUP1
Endline6 	sta WSYNC 



pitposition
	sta GRP1		



	lda TempPit_XPos ;3
	clc	;2


.Div15_Pit   
	sbc #15      ; 2         
	bcs .Div15_Pit   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 
	sta RESP0 

;sword php style	
EndLine7        STA WSYNC                                                ;3 cycles =


	lda     (HeroGraphicsColorPtr),y      ; 5 
	sta	COLUP1

	lda 	Graphics_Buffer  ;3
	sta	GRP1	;3

	pla
;-----------------------------------------------post turning on pit stuff
;This is not a loop, this is a one time set position for the eneamy E2

;sword php style

	DEY
	
	cpy Hero_Sword_Pos  ;3
	php	

		;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


	
;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E2_en      ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_en:
	lax     (Hero_Ptr),y      ; 5

;This allows us to do the calculation early, but must move dey to before routine

	lda     (HeroGraphicsColorPtr),y      ; 5


        DEY             ;2 count down number of scan lines          2 cycles
	
	STA CXCLR   ;3


EndLine8        STA WSYNC                                                ;3 cycles =
;------------------------------------------------------------
	STA HMOVE

;---------------------line for setting up pits-----------------------------------------





ScanLoop_E2_c
	stx	GRP1	;3
	sta 	COLUP1


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3

;--- need to put pits here----------------------------------------------------------------------------------------------------------------<<<<<<
;you have about 24 cycles--------------------------
	CPY Row_2 ;3
	BCS NO_PIT
	LDA #$3C
	STA GRP0
NO_PIT

;--- need to put pits here----------------------------------------------------------------------------------------------------------------<<<<<<



  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw



;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E2_eb       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_eb:
	lax     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine

	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles





	CPY Row_3 ;3

EndLine9        STA WSYNC                                                ;3 cycles =

        BCS ScanLoop_E2_c                                             ;2 cycles =
EndScanLoop_E2_c
;------------------------------------------------------------
;------------------------------------------------------------
	stx	GRP1	;3
	sta 	COLUP1 ;3


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_eb21       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_eb21:
	lax     (Hero_Ptr),y      ; 5

;	lda PF_TEMP

;	cmp #8 ;this was 8, num of creatures max it's not using this it's using multitemp for bcs endline

	asl Multi_Temp

	lda Multi_Temp
	cmp #%01000000     ;Set this to number of monsters you want ////////////////////////////////

	lda     (HeroGraphicsColorPtr),y      ; 5

	DEY



	bcs end_Line  
	cpy Hero_Sword_Pos  ;3
	sta WSYNC
	jmp New_E2_Start 
end_Line
EndLine10	STA WSYNC   
;-------------------------Enemy number E-X End---------------------------
;------------------------------------------------+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Enemy_Number_EX_End




Finish_Screen
;------------------------------------------------------------
ScanLoop_E0_cz
	stx	GRP1	;3
	sta 	COLUP1
	lda 	#0
	sta 	GRP0
	
;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw
;	sta ENABL

;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_ebz       ; 2/3 ; should be bcs
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_ebz:
	lax     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles

	CPY Offset ;3 was 2

        STA WSYNC    
                                            ;3 cycles =
        BCS ScanLoop_E0_cz                                             ;2 cycles =
EndScanLoop_E0_cz


	LDA #$B0   ;start with a black background
	STA COLUBK


;php sword stuff
  cpy Hero_Sword_Pos  ;3
  php
; php sword stuff


;E0_Ptr This and the next 24 bytes can be used for temp Score storage
	STA WSYNC  
 
;	STA WSYNC
;	STA WSYNC

	LDA Overeyes
	CMP #240
	BNE ALWAYSQUAKE
	LDA ROLLING_COUNTER
	AND #%00000111
	BNE NOQUAKE2
ALWAYSQUAKE
	STA WSYNC
	STA WSYNC
	
NOQUAKE2



	LDX #$0F
	LDA #%01101000
	STA CTRLPF

	LDA Score1
	AND #$0F
	ASL
	ASL
	ASL
	TAY

	LDA NUM0,y
	SAX E0_Ptr
	
	INY
	LDA NUM0,y
	SAX E1_Ptr

	iny
	LDA NUM0,y
	SAX E2_Ptr

	iny
	LDA NUM0,y
	SAX E3_Ptr
	
	iny
	LDA NUM0,y
	SAX E4_Ptr

	LDA Score1
	AND #$F0
	LSR


LINEA
;	STA WSYNC  

	LDA Score1
	AND #$F0
	LSR

	TAY

	LDA NUM0,y
	AND #$F0
	ORA E0_Ptr
	STA E0_Ptr

	INY
	LDA NUM0,y
	AND #$F0
	ORA E1_Ptr
	STA E1_Ptr

	
	STA WSYNC  

	INY
	LDA NUM0,y
	AND #$F0
	ORA E2_Ptr
	STA E2_Ptr

	INY
	LDA NUM0,y
	AND #$F0
	ORA E3_Ptr
	STA E3_Ptr


	INY
	LDA NUM0,y
	AND #$F0
	ORA E4_Ptr
	STA E4_Ptr

	LDX #$0F
	LDA Score
	AND #$F0
	LSR
	ADC #80 ;Because this is a reversed PF
	TAY


	LDA NUM0,y
	SAX E5_Ptr


LINEB
;	STA WSYNC  



	

	INY
	LDA NUM0,y
	SAX E6_Ptr

	iny
	LDA NUM0,y
	SAX E7_Ptr

	iny
	LDA NUM0,y
	SAX E0_Ptr2
	
	iny
	LDA NUM0,y
	SAX E1_Ptr2



LINEC
;	STA WSYNC  

	LDA Score
	AND #$0F ;Because this is a reversed PF
	ASL
	ASL
	ASL
	ADC #80 ;Because this is a reversed PF
	TAY

	LDA NUM0,y
	AND #$F0
	ORA E5_Ptr
	STA E5_Ptr

	INY
	LDA NUM0,y
	AND #$F0
	ORA E6_Ptr
	STA E6_Ptr

	INY
	LDA NUM0,y
	AND #$F0
	ORA E7_Ptr
	STA E7_Ptr

LINED
;	STA WSYNC  



	INY
	LDA NUM0,y
	AND #$F0
	ORA E0_Ptr2
	STA E0_Ptr2

	INY
	LDA NUM0,y
	AND #$F0
	ORA E1_Ptr2
	STA E1_Ptr2



LINEE
;	STA WSYNC  

	LDA Score2
	AND #$0F
	ASL
	ASL
	ASL
	TAY
	ldx #$F0

	LDA NUM0_,y
	SAX EnemyGraphicsColorPtr_E2
	
	INY
	LDA NUM0_,y
	SAX EnemyGraphicsColorPtr_E3

	iny
	LDA NUM0_,y
	SAX EnemyGraphicsColorPtr_E4

	iny
	LDA NUM0_,y
	SAX EnemyGraphicsColorPtr_E5
	
	iny
	LDA NUM0_,y
	SAX EnemyGraphicsColorPtr_E6
LINEF
;	STA WSYNC  

	LDA Potion
	AND #$0F
	ASL
	ASL
	ASL
	TAY

	LDA ARNUM0,y
	STA E2_Ptr2

	INY
	LDA ARNUM0,y
	STA E3_Ptr2

	INY
	LDA ARNUM0,y
	STA E4_Ptr2

	INY
	LDA ARNUM0,y
	STA E5_Ptr2

	INY
	LDA ARNUM0,y
	STA E6_Ptr2



	LDA #$FF
	STA COLUPF

	STA WSYNC
	STA WSYNC

 ;EnemyGraphicsColorPtr_E2

	LDY #5
	STA WSYNC

	JMP CalcScore


            ORG $F8C0 

MainPlayerGraphics7
        .byte #%00000000;$0E
        .byte #%01110000;$0E
        .byte #%00111000;$0E
        .byte #%00110010;$0E
        .byte #%01111100;$0E
        .byte #%10010000;$0E
        .byte #%00101000;$32
        .byte #%00111000;$0E
MainPlayerGraphics7b
        .byte #%00000000;$0E
        .byte #%00011100;$0E
        .byte #%00111000;$0E
        .byte #%10110000;$0E
        .byte #%01111100;$0E
        .byte #%00010010;$0E
        .byte #%00101000;$32
        .byte #%00111000;$0E
MainPlayerGraphics6
        .byte #%00000110;$90
        .byte #%00110100;$AA
        .byte #%00010100;$98
        .byte #%00011000;$76
        .byte #%00011000;$78
        .byte #%00011100;$74
        .byte #%00110010;$1A
        .byte #%00111001;$74
MainPlayerGraphics6b
        .byte #%00110000;$90
        .byte #%00100110;$AA
        .byte #%00010100;$98
        .byte #%01011000;$76
        .byte #%00111100;$78
        .byte #%00011010;$74
        .byte #%00110001;$1A
        .byte #%00111000;$74

MainPlayerGraphics30
	.byte #%00000000
	.byte #%00111000
	.byte #%01111100
	.byte #%01111100
	.byte #%01111100
	.byte #%00111000
	.byte #%00000000
	.byte #%00000000

	.byte #%00000000
	.byte #%00000000
	.byte #%00111000
	.byte #%01111100
	.byte #%01111100
	.byte #%01111100
	.byte #%00111000
	.byte #%00000000

SwordSongc ;10 bytes each section
	.byte #0
	.byte #10
	.byte #20
	.byte #30
	.byte #40
	.byte #50
	.byte #60
	.byte #70
	.byte #90

SwordSongf ;10 bytes each section
	.byte #0
	.byte #10
	.byte #20
	.byte #30
	.byte #40
	.byte #50
	.byte #60
	.byte #70
	.byte #90




BADDIEVALUE 
     .byte #0 ;1
     .byte #0 ;2
     .byte #0 ;3
     .byte #0 ;4
     .byte #1 ;5
     .byte #1 ;6
     .byte #2 ;7
     .byte #2 ;8
     .byte #3 ;9
     .byte #4 ;10
     .byte #5 ;11
     .byte #6 ;12
     .byte #7 ;13
     .byte #8 ;14



MORECALCS
;assum horiz speed will be zero

	lda onhorse ;while on horse knife is always readied
	beq horseknife
	lda #8
	sta swordduration
horseknife



	lda Potion
	beq NoPotion
	cmp #$10
	bcs OverPotion	


	ldx INPT5
	bmi NoPotion
	asl
	asl
	asl
	asl
	asl
	cmp #%0110000
	bne nobonus	
	lda #%10000000
nobonus
	ora Potion
	sta Potion
	dec Potion
	lda Enemy_Life
	sta Other_Hit
	jmp NoPotion
OverPotion



NoPotion
	;check Hero Sword Attack
	ldx INPT4
	bmi NoSwordAttack2 ;(button not pressed)
	lda swordduration
	cmp #11
	bcs NoSwordAttack
SwordAttack
	inc swordduration
	lda swordduration
	lsr
	lsr
	sta PF_TEMP
	lda Hero_YPosFromBot
;	clc ;carry will always be set because of earlier compare
	sbc #C_P1_HEIGHT - 10
	clc
	sbc PF_TEMP
	jmp DoneWithSwordAttack
NoSwordAttack2
	lda #0
	;sta PF_TEMP
	sta swordduration
NoSwordAttack
	lda #200
DoneWithSwordAttack
	sta Hero_Sword_Pos





;	LDA #%01000000	;Left?
	BIT SWCHA 
	BVS SkipMoveLeft
	;16 bit math

	sec
	lda Hero_XPos
	sbc #<HERO_SPEED_HOR
	sta Hero_XPos
	lda Hero_XPos+1
	sbc #>HERO_SPEED_HOR
	sta Hero_XPos+1

;; moving left, so we need the mirror image
	LDA #%00001000   ;a 1 in D3 of REFP0 says make it mirror
	STA REFP0
	STA REFP1
	LDA #%00000001
	STA MOV_STAT

;Don't allow Hero too far left
	LDA #Far_Left
	CMP Hero_XPos
	BCC HeroLeft
	STA Hero_XPos
HeroLeft

	JMP SkipMoveRight

SkipMoveLeft
	BIT SWCHA 
	BMI SkipMoveRight


	lda Hero_XPos
	CMP #Far_Right_Hero
	BCS TOOFARTOMOVE
	clc
	adc #<HERO_SPEED_HOR
	sta Hero_XPos
	lda Hero_XPos+1
	adc #>HERO_SPEED_HOR
	sta Hero_XPos+1
TOOFARTOMOVE
;; moving right, cancel any mirrorimage
	LDA #%00000000
	STA REFP0
	STA REFP1
	STA MOV_STAT



;Don't allow Hero too far right
;	LDA #Far_Right_Hero
;	CMP Hero_XPos
;	BCS HeroRight
;	STA Hero_XPos
HeroRight


SkipMoveRight



;Don't allow Hero above top position
	LDA #Far_Up_Hero
	CMP Hero_YPosFromBot
	BCS HeroUp
	STA Hero_YPosFromBot
HeroUp

;Don't allow Hero below bottom
	LDA #Far_Down_Hero
	CMP Hero_YPosFromBot
	BCC HeroDown
	STA Hero_YPosFromBot
HeroDown

	JMP MORECALCSRET






            ORG $F9C0 

MainPlayerGraphics8
        .byte #%00000000;$D2
        .byte #%00111100;$D2
        .byte #%00011000;$D2
        .byte #%00011000;$C2
        .byte #%00100100;$C4
        .byte #%01000010;$D2
        .byte #%11000011;$C4
        .byte #%11000011;$30

MainPlayerGraphics8b
        .byte #%00000000;$D2
        .byte #%00111100;$D2
        .byte #%00011000;$D2
        .byte #%00011000;$C2
        .byte #%00100100;$C4
        .byte #%01000010;$D2
        .byte #%0110011;$C4
        .byte #%01100110;$30


MainPlayerGraphics9
        .byte #%00000000;
        .byte #%01001000;
        .byte #%00101000;
        .byte #%00111000;
        .byte #%01111000;
        .byte #%00010000;
        .byte #%01110000;
        .byte #%01110000;

MainPlayerGraphics9b
        .byte #%00000000;
        .byte #%00100000;
        .byte #%00101000;
        .byte #%00111000;
        .byte #%01111000;
        .byte #%00010000;
        .byte #%01110000;
        .byte #%01110000;

PonyGraphics

        .byte #%00000000;$C0
        .byte #%00010100;$22
        .byte #%00010100;$C2
        .byte #%00011110;$D2
        .byte #%01111111;$D2
        .byte #%11011100;$C2
        .byte #%00000000;$C0
        .byte #%00000000;$D0


        .byte #%00000000;$C0
        .byte #%00010100;$22
        .byte #%00010100;$C2
        .byte #%11011110;$D2
        .byte #%01111111;$D2
        .byte #%00011100;$C2
        .byte #%00000000;$C0
        .byte #%00000000;$D0



SwordSongv ;10 bytes each section
	.byte #0
	.byte #1
	.byte #2
	.byte #3
	.byte #4
	.byte #5
	.byte #6
	.byte #7
	.byte #9



;-----------------------------
; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write
;ROM is located from F000 to FFFF

            ORG $FA00 ;this a critical location, must be on edge of page 
fineAdjustBegin

            DC.B %01110000 ; Left 7
            DC.B %01100000 ; Left 6
            DC.B %01010000 ; Left 5
            DC.B %01000000 ; Left 4
            DC.B %00110000 ; Left 3
            DC.B %00100000 ; Left 2
            DC.B %00010000 ; Left 1
            DC.B %00000000 ; No movement.
            DC.B %11110000 ; Right 1
            DC.B %11100000 ; Right 2
            DC.B %11010000 ; Right 3
            DC.B %11000000 ; Right 4
            DC.B %10110000 ; Right 5
            DC.B %10100000 ; Right 6
            DC.B %10010000 ; Right 7

fineAdjustTable EQU fineAdjustBegin - %11110001 ; NOTE: %11110001 = -15
ARNUM0
 .byte #%01111110;
 .byte #%00000000;
 .byte #%00000000;
 .byte #%00000000;
 .byte #%01111110;
	.byte #$FF
	.byte #$FF
	.byte #$FF

ARNUM1
 .byte #%00011100;
 .byte #%00001000;
 .byte #%00001000;
 .byte #%00001000;
 .byte #%00011100;
	.byte #$FF
	.byte #$FF
	.byte #$FF

ARNUM2
 .byte #%00111110;
 .byte #%00010100;
 .byte #%00010100;
 .byte #%00010100;
 .byte #%00111110;
	.byte #$FF
	.byte #$FF
	.byte #$FF


ARNUM3
 .byte #%01111110;
 .byte #%01010100;
 .byte #%01010100;
 .byte #%01010100;
 .byte #%01111110;
	.byte #$FF
	.byte #$FF
	.byte #$FF

ARNUM4
 .byte #%01111110;
 .byte #%00100100;
 .byte #%00101010;
 .byte #%00101010;
 .byte #%01111110;
	.byte #$FF
	.byte #$FF
	.byte #$FF

ARNUM5
 .byte #%01111100;
 .byte #%00010000;
 .byte #%00101000;
 .byte #%00101000;
 .byte #%01111100;

DistFromBottom
	.byte #Enemy_Row_0-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E0-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E1-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E2-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E3-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E4-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E5-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E6-#8-#C_P0_HEIGHT	
	.byte #Enemy_Row_E7-#8-#C_P0_HEIGHT
	

SLICE2

	LDA ROLLING_COUNTER
	AND #%01111000


	CMP #%01111000
	BNE SKIPMTN	

	LDA #%00010000
	AND PF0_L1
	CLC
	BEQ R1AJMP
	SEC
R1AJMP	
;movign the initial bit into and outof the 4bit is causing the issue
ROTATE1
	ROR PF5_L1 ; 4bit
	ROL PF4_L1 ;reversed
	ROR PF3_L1 ;
	LDA #%00001000
	AND PF3_L1
;	CMP #0
	CLC
	BEQ R1JMP
	SEC
R1JMP	
	ROR PF2_L1;4bit
	ROL PF1_L1 ;reversed
	ROR PF0_L1 



;---------------------------------------

	LDA #%00010000
	AND PF0_L2
;	CMP #0
	CLC
	BEQ R2AJMP
	SEC
R2AJMP	
;movign the initial bit into and outof the 4bit is causing the issue
ROTATE2
	ROR PF5_L2 ; 4bit
	ROL PF4_L2 ;reversed
	ROR PF3_L2 ;
	LDA #%00001000
	AND PF3_L2
;	CMP #0
	CLC
	BEQ R2JMP
	SEC
R2JMP	
	ROR PF2_L2;4bit
	ROL PF1_L2 ;reversed
	ROR PF0_L2 


;----------------------------------------


	LDA #%00010000
	AND PF0_L3
;	CMP #0
	CLC
	BEQ R3AJMP
	SEC
R3AJMP	
;movign the initial bit into and outof the 4bit is causing the issue
ROTATE3
	ROR PF5_L3 ; 4bit
	ROL PF4_L3 ;reversed
	ROR PF3_L3 ;
	LDA #%00001000
	AND PF3_L3
;	CMP #0
	CLC
	BEQ R3JMP
	SEC
R3JMP	
	ROR PF2_L3;4bit
	ROL PF1_L3 ;reversed
	ROR PF0_L3 

SKIPMTN
	JMP ENDSLICES


PreOverScanWait
;Can put stuff here --------------------------------------------------






OverScanWait
	STA WSYNC
	DEY
	BNE OverScanWait
	JMP  MainLoop   


	org $FAC0 
MainPlayerGraphics5
        .byte #%00000000;$D2
        .byte #%00010110;$D2
        .byte #%00011100;$D2
        .byte #%00011101;$C2
        .byte #%00111110;$C4
        .byte #%01001000;$D2
        .byte #%00011100;$C4
        .byte #%00010100;$30

MainPlayerGraphics5b
        .byte #%00000000;$D2
        .byte #%00110100;$D2
        .byte #%00011100;$D2
        .byte #%01011100;$C2
        .byte #%00111110;$C4
        .byte #%00001001;$D2
        .byte #%00011100;$C4
        .byte #%00010100;$30

Multiplexer
	.byte #%00000001
	.byte #%00000010
	.byte #%00000100
	.byte #%00001000
	.byte #%00010000
	.byte #%00100000
	.byte #%01000000
	.byte #%10000000

Mask
	.byte #%11111110
	.byte #%11111101
	.byte #%11111011
	.byte #%11110111
	.byte #%11101111
	.byte #%11011111
	.byte #%10111111
	.byte #%01111111

HeroGraphics2
        .byte #%00000000;$F4
        .byte #%11011000;$F4
        .byte #%10010000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11111100;$EA
        .byte #%11110010;$CA
        .byte #%00110001;$46
        .byte #%11110001;$0E
        .byte #%11110000;$0E
        .byte #%11000000;$0E
        .byte #%11110000;$22



        .byte #%00000000;$F4
        .byte #%01010000;$F4
        .byte #%10100000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11111100;$EA
        .byte #%11110010;$CA
        .byte #%00110001;$46
        .byte #%11110001;$0E
        .byte #%11110000;$0E
        .byte #%11000000;$0E
        .byte #%11110000;$22


HeroGraphics3
        .byte #%00000000;$F4
        .byte #%11011000;$F4
        .byte #%10010000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11111111;$EA
        .byte #%11110000;$CA
        .byte #%00110000;$46
        .byte #%11110000;$0E
        .byte #%11110000;$0E
        .byte #%11000000;$0E
        .byte #%11110000;$22



        .byte #%00000000;$F4
        .byte #%01010000;$F4
        .byte #%10100000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11111111;$EA
        .byte #%11110000;$CA
        .byte #%00110000;$46
        .byte #%11110000;$0E
        .byte #%11110000;$0E
        .byte #%11000000;$0E
        .byte #%11110000;$22








CalcScore





	NOP
	NOP




	LDA #0		; 4 cycles
	STA PF0			; 3 cycles
	LDA Player_Health		; 4 cycles
	STA PF1			; 3 cycles
	LDA #E2_Ptr2-1,Y		; 4 cycles
	STA PF2			; 3 cycles

	NOP
;	NOP
;	NOP
;	NOP
	LDA #EnemyGraphicsColorPtr_E2-1,Y		; 4 cycles
	STA PF0			; 3 cycles
	LDA #E0_Ptr-1,Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA #E5_Ptr-1,Y		; 4 cycles
	STA PF2			; 3 cycles

	DEY
	STA WSYNC  
	BNE CalcScore


	LDA #0		
	STA PF0
	STA PF1
	STA PF2


	LDA #%01101000

	STA CTRLPF



	STA VBLANK 	
	LDY #30	;number of overscan lines	


;--------New Collision Detection Style
	BIT CXM1P ;3 cycles
	BPL No_Hit_the_Baddie3 ;2 cycles
	LDA #%10000000
	ORA Other_Hit ;4 cycles
	STA Other_Hit  ;4 cycles
No_Hit_the_Baddie3
;--------New Collision Detection Style



	DEY
	STA WSYNC
	
;Fix Positions
	BIT CXPPMM ;3 cycles
	BPL No_Hit_the_Pit2 ;2 cycles
	LDA New_Hit ;4 cycles
	ORA #%10000000 ;3 cycles
	STA New_Hit  ;4 cycles
No_Hit_the_Pit2

	LDX #8
	LDA #%01111111
	STA PF_TEMP



Check_Pos
;Don't allow P0 past Position 160
	LDA E0_XPos-1,x	;2
	CMP #Far_Right+1	;2
	BCC P0Right	;2(3)
	LDA #Far_Left ;2
	STA E0_XPos-1,x	;3
P0Right


	
;Don't allow P0 past Position 160
	LDA E0_XPos-1,x
	CMP #Enemy_Far_Left ;2
	BCS P0left	;2(3)
	lda PF_TEMP
	and Enemy_Life
	sta Enemy_Life
	LDA #Far_Right
	STA E0_XPos-1,x
P0left
	SET 
	ROR PF_TEMP

	LDA Pause
	BNE DontmovePit
	 
DontmovePit
	LDA Pit0_XPos-1,x 
	CMP #Far_Right
	BCC PAUSED2
	LDA #0
	STA Pit0_XPos-1,x 
PAUSED2


	DEY
	DEX



	STA WSYNC
	BNE Check_Pos
	STA WSYNC



;	 for up and down, we INC or DEC
; the Y Position

	LDA #%00010000	;Down?
	BIT SWCHA 
	BNE SkipMoveDown
	
	;16 bit math, add both bytes
	;of the speed constant to
	;the 2 bytes of the position
	clc
	lda Hero_YPosFromBot
	adc #<HERO_SPEED_VER
	sta Hero_YPosFromBot
	lda Hero_YPosFromBot+1
	adc #>HERO_SPEED_VER
	sta Hero_YPosFromBot+1
SkipMoveDown

	LDA #%00100000	;Up?
	BIT SWCHA 
	BNE SkipMoveUp
	
	;16 bit math, subtract both bytes
	;of the speed constant to
	;the 2 bytes of the position
	sec
	lda Hero_YPosFromBot
	sbc #<HERO_SPEED_VER
	sta Hero_YPosFromBot
	lda Hero_YPosFromBot+1
	sbc #>HERO_SPEED_VER
	sta Hero_YPosFromBot+1


SkipMoveUp

	JMP PreOverScanWait





	org $FBC0 


     .byte #$0C ;this is a cheat, his feet sometimes get the wrong color
	
HeroGraphicsColor ;2468ACE
     .byte #$0C
     .byte #$0A
     .byte #$0A
     .byte #$08
     .byte #$04
     .byte #$04
     .byte #$04
     .byte #$04
     .byte #$06
     .byte #$06
     .byte #$0A
     .byte #$08

HeroGraphicsColor2

     .byte #$0C
     .byte #$0A
     .byte #$0A
     .byte #$08
     .byte #$FE
     .byte #$FE
     .byte #$FC
     .byte #$F6
     .byte #$FA
     .byte #$F8
     .byte #$FE
     .byte #$FE

HeroGraphicsColor3

     .byte #$00
     .byte #$00
     .byte #$00
     .byte #$00
     .byte #$00
     .byte #$04
     .byte #$04
     .byte #$04
     .byte #$06
     .byte #$06
     .byte #$0A
     .byte #$08


HeroGraphics0
        .byte #%00000000;$22
        .byte #%11011000;$F4
        .byte #%10010000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11111100;$EA
        .byte #%00110010;$CA
        .byte #%11110010;$0E
        .byte #%11110000;$0E
        .byte #%11000000;$0E
        .byte #%11110000;$22


        .byte #%00000000;$22
        .byte #%01010000;$F4
        .byte #%10100000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11110000;$F4
        .byte #%11111100;$EA
        .byte #%00110010;$46
        .byte #%11110010;$0E
        .byte #%11110000;$0E
        .byte #%11000000;$0E
        .byte #%11110000;$22


HeroGraphics1
        .byte #%00000000;$F4
        .byte #%00100100;$F4
        .byte #%00111100;$F4
        .byte #%11111100;$F4
        .byte #%00110110;$F4
        .byte #%00111100;$EA
        .byte #%00110000;$CA
        .byte #%00010000;$46
        .byte #%00110000;$0E
        .byte #%00110000;$0E
        .byte #%00100000;$0E
        .byte #%00110000;$22




        .byte #%00000000;$F4
        .byte #%01000010;$F4
        .byte #%00111100;$F4
        .byte #%01111100;$F4
        .byte #%10110110;$F4
        .byte #%00111100;$EA
        .byte #%00110000;$CA
        .byte #%00010000;$46
        .byte #%00110000;$0E
        .byte #%00110000;$0E
        .byte #%00100000;$0E
        .byte #%00110000;$22



MainPlayerGraphics0

	.byte #%00000000
	.byte #%00010100
	.byte #%00010100
	.byte #%00011000
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


	.byte #%00000000
	.byte #%00100110
	.byte #%00101100
	.byte #%00011000
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


MainPlayerGraphics1

	.byte #%00000000
	.byte #%00010100
	.byte #%00010100
	.byte #%00111100
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000



	.byte #%00000000
	.byte #%00100010
	.byte #%00100100
	.byte #%00111100
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000




NEXTBADDIETYPE ;first 3 bits is the lane, last 5 is the type
     .byte #%00000111
     .byte #%00100001
     .byte #%01000010
     .byte #%01100011
     .byte #%10000100
     .byte #%01101110
     .byte #%11000100
     .byte #%11000100
     .byte #%00001000
     .byte #%00101001
     .byte #%01001010
     .byte #%01101011
     .byte #%10001100
     .byte #%10101101
     .byte #%11000001
     .byte #%11000010
     .byte #%00000011
     .byte #%00100100
     .byte #%01001000
     .byte #%01101001
     .byte #%10000111
     .byte #%10101000
     .byte #%11001001
     .byte #%11001110
     .byte #255 ;This is to reset to beginning


NEXTBADDIEDUR
	.byte #$0
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10
	.byte #$10



SLICE3
MOVESECTION
	LDA Pause
	CMP #0
	BEQ NOMAKEEYES
	INC Overeyes
NOMAKEEYES
	LDA #0
	STA Player_Hit ;Need to make it easier to live
	STA New_Hit
	JMP MOVESET1
NOMOVESET

	LDA Overeyes
	CMP #240
	BCC NOPITCREATION
	LDA RNG
	CMP #120
	BCS NOPITCREATION
	CMP #Far_Right
	BCS NOPITCREATION
	AND #%00000111
	tax
	LDA RNG
	ADC #10
;AND #%01111111
	STA #Pit0_XPos-1,x	 

NOPITCREATION	
	CMP #11
	BNE NOLOSEHORSE
	LDA #0
	STA onhorse
NOLOSEHORSE


	LDA Player_Hit
	BEQ HORSENOTLOST
	LDA #0
	STA onhorse
HORSENOTLOST
	JMP ENDSLICES






	org #$FCC0 ;HeroGraphicsColor + #256


TreeGraphics

	.byte #%00000000
	.byte #%00011000
	.byte #%00011000
	.byte #%00111100
 	.byte #%01011010
	.byte #%01111110
	.byte #%11111111
	.byte #%00111000



	.byte #%00000000
	.byte #%00011000
	.byte #%00011000
	.byte #%00111100
	.byte #%01011010
	.byte #%01111110
	.byte #%11111111
	.byte #%00111000

MainPlayerGraphics2

	.byte #%00000000
	.byte #%01111000
	.byte #%10000100
	.byte #%00001000
	.byte #%00010000
	.byte #%00010000
	.byte #%00010000
	.byte #%00111000



	.byte #%00000000
	.byte #%01111000
	.byte #%01001000
	.byte #%00010000
	.byte #%00100000
	.byte #%00100000
	.byte #%00100000
	.byte #%01110000


MainPlayerGraphics4

        .byte #%00000000;$22
        .byte #%01111110;$C0
        .byte #%11111000;$C0
        .byte #%11101000;$D0
        .byte #%01111110;$C2
        .byte #%11011110;$D2
        .byte #%01001000;$D2
        .byte #%01111110;$C2


        .byte #%00000000;$22
        .byte #%01111110;$C0
        .byte #%00111110;$C0
        .byte #%00101110;$D0
        .byte #%01111110;$C2
        .byte #%00011110;$D2
        .byte #%01111000;$D2
        .byte #%01111110;$C2

MainPlayerGraphics3

	.byte #%00000000
	.byte #%01111000
	.byte #%10000100
	.byte #%00001000
	.byte #%00010000
	.byte #%01111100
	.byte #%00010000
	.byte #%00111000



	.byte #%00000000
	.byte #%01111000
	.byte #%01001000
	.byte #%00010000
	.byte #%00100000
	.byte #%11111000
	.byte #%00100000
	.byte #%01110000











LEFTAUD     
     .byte     #%10000100
     .byte     #%10000100
     .byte     #%10010100
     .byte     #%10111100
     .byte     #%11000100
     .byte     #%10111100
     .byte     #%11011100
     .byte     #%11011100
     .byte     #%11000100
     .byte     #%10111100
     .byte     #%11011100
     .byte     #%11011100
     .byte     #%10000100
     .byte     #%10000100
     .byte     #%10010100
     .byte     #%10111100
     .byte     #%11000100
     .byte     #%10111100
     .byte     #%11011100
     .byte     #%11011100
     .byte     #%11000100
     .byte     #%10111100
     .byte     #%11011100
     .byte     #%11011100
     .byte     #%01101100
     .byte     #%01101100
     .byte     #%10000100
     .byte     #%10001100
     .byte     #%10111100
     .byte     #%10011100
     .byte     #%11000100
     .byte     #%11000100
     .byte     #%10100100
     .byte     #%10011100
     .byte     #%11000100
     .byte     #%11000100
     .byte     #%10000100
     .byte     #%10000100
     .byte     #%10010100
     .byte     #%10111100
     .byte     #%11000100
     .byte     #%10111100
     .byte     #%11011100
     .byte     #%11011100
     .byte     #%11000100
     .byte     #%10111100
     .byte     #%10011100
     .byte     #%10010100
     .byte     #%10100100
     .byte     #%10100001
     .byte     #%10011100
     .byte     #%10011001
     .byte     #%10010100
     .byte     #%10010100
     .byte     #%10000100
     .byte     #%10000100
     .byte     #%10011100
     .byte     #%10011001
     .byte     #%10010100
     .byte     #%10010100
     .byte     #%10000100
     .byte     #%10000100
     .byte     #%01101100
     .byte     #%01101100
     .byte     #%10100100
     .byte     #%10100001
     .byte     #%10011100
     .byte     #%10011001
     .byte     #%10010100
     .byte     #%10010100
     .byte     #%10000100
     .byte     #%10000100
     .byte     #%01101100
     .byte     #%01101001
     .byte     #%10000100
     .byte     #%10000001
     .byte     #%10010100
     .byte     #%10010001
     .byte     #%10000100
     .byte     #%10000001
     .byte     #%10011100
     .byte     #%10011001
     .byte     #%00110100
     .byte     #%00110100
     .byte     #$FF


GraphicsTableLow
     .byte #<TreeGraphics ;0

     .byte #<PonyGraphics ;1

     .byte #<MainPlayerGraphics2 ;2

     .byte #<MainPlayerGraphics8 ;3

     .byte #<MainPlayerGraphics8 ;4

     .byte #<MainPlayerGraphics5 ;5

     .byte #<MainPlayerGraphics5 ;6

     .byte #<MainPlayerGraphics7 ;7

     .byte #<MainPlayerGraphics6 ;8

     .byte #<MainPlayerGraphics5 ;9

     .byte #<MainPlayerGraphics4 ;10

     .byte #<MainPlayerGraphics3 ;11

     .byte #<MainPlayerGraphics3 ;12

     .byte #<MainPlayerGraphics3 ;13

     .byte #<MainPlayerGraphics30 ;14

     .byte #<TreeGraphics ;15

     .byte #<TreeGraphics ;16

     .byte #<TreeGraphics ;17

     .byte #<TreeGraphics ;18

     .byte #<TreeGraphics ;19



GraphicsTableHigh
     .byte #>TreeGraphics ;0

     .byte #>PonyGraphics ;1

     .byte #>MainPlayerGraphics2 ;2

     .byte #>MainPlayerGraphics8 ;3

     .byte #>MainPlayerGraphics8 ;4

     .byte #>MainPlayerGraphics5 ;5

     .byte #>MainPlayerGraphics5 ;6

     .byte #>MainPlayerGraphics7 ;7

     .byte #>MainPlayerGraphics6 ;8

     .byte #>MainPlayerGraphics5 ;9

     .byte #>MainPlayerGraphics4 ;10

     .byte #>MainPlayerGraphics3 ;11

     .byte #>MainPlayerGraphics3 ;12

     .byte #>MainPlayerGraphics3 ;13

     .byte #>MainPlayerGraphics30 ;14

     .byte #>TreeGraphics ;15

     .byte #>TreeGraphics ;16

     .byte #>TreeGraphics ;17

     .byte #>TreeGraphics ;18

     .byte #>TreeGraphics ;19

GraphicsColorTableLow

     .byte #<TreeGraphicsColor ;1

     .byte #<PonyGraphicsColor ;2

     .byte #<EnemyGraphicsColor2 ;3 light green

     .byte #<EnemyGraphicsColor8 ;4 dark green

     .byte #<EnemyGraphicsColor8b ;5 dark green

     .byte #<EnemyGraphicsColor9 ;6 green-brown matches background

     .byte #<EnemyGraphicsColor9b ;7 green-brown matches background

     .byte #<EnemyGraphicsColor7 ;8 ghost white

     .byte #<EnemyGraphicsColor6 ;9 green

     .byte #<EnemyGraphicsColor5 ;10 green

     .byte #<EnemyGraphicsColor4 ;11 green

     .byte #<EnemyGraphicsColor3 ;12 red/tan

     .byte #<EnemyGraphicsColor1 ;13 red/tan

     .byte #<EnemyGraphicsColor0 ;14 blue/white

     .byte #<EnemyGraphicsColor5 ;15 blue

     .byte #<EnemyFireColor ;16

     .byte #<TreeGraphicsColor ;17

     .byte #<TreeGraphicsColor ;18

     .byte #<TreeGraphicsColor ;19

     .byte #<TreeGraphicsColor ;20

     .byte #<TreeGraphicsColor ;21

     .byte #<TreeGraphicsColor ;22

     .byte #<TreeGraphicsColor ;23

     .byte #<TreeGraphicsColor ;24

     .byte #<TreeGraphicsColor ;25

     .byte #<TreeGraphicsColor ;26

     .byte #<TreeGraphicsColor ;27

     .byte #<TreeGraphicsColor ;28

     .byte #<TreeGraphicsColor ;29

     .byte #<TreeGraphicsColor ;30

     .byte #<TreeGraphicsColor ;31

     .byte #<TreeGraphicsColor ;32

GraphicsColorTableHigh

     .byte #>TreeGraphicsColor




	org #$FDC0 ;HeroGraphicsColor + #512


TreeGraphicsColor
	.byte #$F0
	.byte #$F2
	.byte #$F0
	.byte #$C0
	.byte #$C2
	.byte #$C2
	.byte #$C2
	.byte #$C2



PonyGraphicsColor
	.byte #$00
	.byte #$00
	.byte #$00
	.byte #$00
	.byte #$00
	.byte #$00
	.byte #$00
	.byte #$00

EnemyGraphicsColor0
	.byte #$8A
	.byte #$8A
	.byte #$74
	.byte #$72
	.byte #$0A
	.byte #$08
	.byte #$80
	.byte #$80

EnemyGraphicsColor1
	.byte #$40
	.byte #$40
	.byte #$40
	.byte #$20
	.byte #$3E
	.byte #$3E
	.byte #$80
	.byte #$80

EnemyGraphicsColor2
	.byte #$C0
	.byte #$C2
	.byte #$C4
	.byte #$C6
	.byte #$C8
	.byte #$CA
	.byte #$80
	.byte #$80

EnemyGraphicsColor3
	.byte #$40
	.byte #$40
	.byte #$40
	.byte #$20
	.byte #$3E
	.byte #$3E
	.byte #$80
	.byte #$80


EnemyGraphicsColor4
        .byte #$C0;
        .byte #$C0;
        .byte #$D0;
        .byte #$C2;
        .byte #$D2;
        .byte #$D2;
        .byte #$C2;
        .byte #$22;

EnemyGraphicsColor5
        .byte #$80;
        .byte #$80;
        .byte #$82;
        .byte #$84;
        .byte #$84;
        .byte #$82;
        .byte #$80;
        .byte #$80;

EnemyGraphicsColor6
        .byte #$D2;
        .byte #$D2;
        .byte #$D2;
        .byte #$C2;
        .byte #$C4;
        .byte #$D2;
        .byte #$C4;
        .byte #$30;

EnemyGraphicsColor7 ;Ghost with Red Eyes
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$32;
        .byte #$0E;


EnemyGraphicsColor8 ;Mandrake Plant
        .byte #$C2;
        .byte #$C2;
        .byte #$C2;
        .byte #$C2;
        .byte #$C2;
        .byte #$C4;
        .byte #$40;
        .byte #$40;

EnemyGraphicsColor8b ;Mandrake Plant
        .byte #$C2;
        .byte #$C2;
        .byte #$C2;
        .byte #$C2;
        .byte #$C2;
        .byte #$C4;
        .byte #$80;
        .byte #$80;

EnemyGraphicsColor9 ;Mandrake Man
        .byte #$10;
        .byte #$10;
        .byte #$12;
        .byte #$12;
        .byte #$12;
        .byte #$14;
        .byte #$C0;
        .byte #$C0;

EnemyGraphicsColor9b ;Mandrake Man
        .byte #$10;
        .byte #$10;
        .byte #$12;
        .byte #$12;
        .byte #$12;
        .byte #$14;
        .byte #$30;
        .byte #$30;

EnemyHitColor
        .byte #$02;
        .byte #$02;
        .byte #$04;
        .byte #$04;
        .byte #$06;
        .byte #$06;
        .byte #$08;
        .byte #$08;

EnemyFireColor
        .byte #$02
        .byte #$40
        .byte #$1D
        .byte #$42
        .byte #$1D
        .byte #$44
        .byte #$50
        .byte #$02



SLICE0
	LDA #0
	cmp Pause ;If screen paused because ceratures going backwords, pause
	bcc NotYet
RESSURECT
	DEC Baddie_Duration 
	bmi TOOLARGE
	bne NotYet

TOOLARGE

	ldx Baddie_Num

	LDA NEXTBADDIETYPE,x
	CMP #255
	BNE DONTLOOP
	LDX #0
	STX Baddie_Num
DONTLOOP


	LDA NEXTBADDIETYPE,x
	LSR
	LSR
	LSR
	LSR
	LSR
	AND #%00000111
	TAY



	LDA NEXTBADDIETYPE,x
	AND #%00011111 
	STA E0_Type,y
	CMP #0
	beq TREE
	CMP #1
	beq HORSE 
	ASL 
	STA E0_Health,y
	JMP NOTHORSE	
TREE
	LDA #120
	STA E0_Health,y
	JMP NOTHORSE

HORSE
	LDA #1
	STA E0_Health,y
	JMP NOTHORSE

NOTHORSE

	lda #Far_Right-1
	sta E0_XPos,y

	lda #0
	sta Direction




	
	LDA Multiplexer,y
	ora Enemy_Life
	sta Enemy_Life

	INC Baddie_Num

	lda NEXTBADDIEDUR,y
	
	sta Baddie_Duration
	BEQ TOOLARGE


NotYet


	JMP ENDSLICES



	org #$FEC0 ;HeroGraphicsColor + #768
NUM0
        .byte #%11101110;
        .byte #%10101010;
        .byte #%10101010;
        .byte #%10101010;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM1
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM2
        .byte #%11101110;
        .byte #%10001000;
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM3
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM4
        .byte #%00100010;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%10101010;
        .byte #%10101010;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM5
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%10001000;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM6
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;
        .byte #%10001000;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM7
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM8
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM9
        .byte #%00100010;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM0_
 .byte #%01110111;
 .byte #%01010101;
 .byte #%01010101;
 .byte #%01010101;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM1_
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01000100;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM2_
 .byte #%01110111;
 .byte #%00010001;
 .byte #%01110111;
 .byte #%01000100;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM3_
 .byte #%01110111;
 .byte #%01000100;
 .byte #%01110111;
 .byte #%01000100;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM4_
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01110111;
 .byte #%01010101;
 .byte #%01010101;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM5_
 .byte #%01110111;
 .byte #%01000100;
 .byte #%01110111;
 .byte #%00010001;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM6_
 .byte #%01110111;
 .byte #%01010101;
 .byte #%01110111;
 .byte #%00010001;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM7_
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM8_
 .byte #%01110111;
 .byte #%01010101;
 .byte #%01110111;
 .byte #%01010101;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF
NUM9_
 .byte #%01000100;
 .byte #%01000100;
 .byte #%01110111;
 .byte #%01010101;
 .byte #%01110111;
	.byte #$FF
	.byte #$FF
	.byte #$FF

EmptyPlayerGraphics
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0

PFData0 
        .byte #%00000011
        .byte #%00000111
        .byte #%00001111 

PFData1
        .byte #%01111111
        .byte #%00111011
        .byte #%00010001

PFData2
        .byte #%11100111
        .byte #%11000011
        .byte #%00000000

PFData3
        .byte #%10111011
        .byte #%10011001
        .byte #%00000000 

PFData4 
        .byte #%11101111
        .byte #%11000111
        .byte #%10000011

PFData5
        .byte #%11000011
        .byte #%10000001
        .byte #%00000000
	
PFCOLOR
	.byte #$2A
	.byte #$5D
	.byte #$4A
	.byte #$3A
	.byte #$FF
	.byte #$FF

PFCOLORB
	.byte #$70
	.byte #$72
	.byte #$74
	.byte #$76

RETURN
	.byte #$1
	.byte #$2
	.byte #$1
	.byte #$2
	.byte #$1
	.byte #$2
	.byte #$1
	.byte #$2
	.byte #$1

RETURNPIT
	.byte #$1
	.byte #$30
	.byte #$70
	.byte #$20
	.byte #$110
	.byte #$20
	.byte #$90
	.byte #$50

Enemy_Row_Data
	.byte Enemy_Row_0
	.byte Enemy_Row_E0
	.byte Enemy_Row_E1
	.byte Enemy_Row_E2
	.byte Enemy_Row_E3 
	.byte Enemy_Row_E4
	.byte Enemy_Row_E5
	.byte Enemy_Row_E6
	.byte Enemy_Row_E7
 

RIGHTAUDF
     .byte     27
     .byte     30
     .byte     21
     .byte     28
     .byte     27
     .byte     30
     .byte     21
     .byte     25
     .byte     30
     .byte     27
     .byte     21
     .byte     25
     .byte     27
     .byte     30
     .byte     21
     .byte     28
     .byte     27
     .byte     30
     .byte     21
     .byte     25
     .byte     30
     .byte     27
     .byte     21
     .byte     25
     .byte     27
     .byte     30
     .byte     21
     .byte     28
     .byte     27
     .byte     30
     .byte     21
     .byte     25
     .byte     30
     .byte     27
     .byte     21
     .byte     25
     .byte     27
     .byte     30
     .byte     21
     .byte     28
     .byte     27
     .byte     30
     .byte     21
     .byte     25
     .byte     30
     .byte     27
     .byte     21
     .byte     25
     .byte     20
     .byte     24
     .byte     30
     .byte     22
     .byte     21
     .byte     28
     .byte     25
     .byte     21
     .byte     30
     .byte     22
     .byte     21
     .byte     28
     .byte     25
     .byte     21
     .byte     28
     .byte     21
     .byte     20
     .byte     24
     .byte     30
     .byte     22
     .byte     21
     .byte     28
     .byte     25
     .byte     21
     .byte     28
     .byte     21
     .byte     30
     .byte     27
     .byte     28
     .byte     21
     .byte     25
     .byte     21
     .byte     30
     .byte     22
     .byte     21
     .byte     21

BITMASK
	.byte #%

	org $FFFC
	.word Start
	.word Start

;NOTES from Golden Axe Game
;MAP of Golden Axe http://maps.speccy.cz/map.php?id=GoldenAxe&sort=0&part=10&ath= 
;5 real stages, with mini potion/thief stages in between, 1 min a piece
;level 1 : 10 enemies
;2 Mins, 6 sub screens
;level 2 : 17 enemies
;level 3 : 9  enemies
;level 4 : 15 enemies
;level 5 : 18 enemies
;Total enemies in game 69
;Total Game 16 minutes, need 16min*60seconds/min*60frame/sec = 28800 worth of delay
;

;Villians
;------------------------------
;Heninger
	
;Attack strength: D (Silver, Purple), C (Green, Gold), B (Dark, Bronze), A (Red)
;Attack speed: C (Silver, Purple, Green, Bronze), B (Red, Gold, Dark)
;Combo attacks: 2 damage (1/8 bar) per hit [1]
;Knockdown attack (combo finisher): 4 damage (1/4 bar) [1]
;Dash attack: 4 damage (1/4 bar) [1]
;Combo completed (two hits, knockdown): 8 damage (1/2 bar) [1]

;You cannot block their dash attack.
;------------------------------
;Longmoan
	
;Attack strength: D (Silver, Purple), C (Green, Gold), B (Dark, Bronze), A (Red)
;Attack speed: C (Silver, Purple, Green, Bronze), B (Red, Gold, Dark)
;Combo attacks: 2 damage (1/8 bar) per hit [1]
;Knockdown attack (combo finisher): 4 damage (1/4 bar) [1]
;Dash attack: 4 damage (1/4 bar) [1]
;Combo completed (two hits, knockdown): 8 damage (1/2 bar) [1]

;Unlike the Heninger, the Longmoan's dash attack can be stopped, but only by your own dash attack.
;------------------------------
;Amazons
	
;Attack strength: B (Purple, Green), A (Red, Dark)
;Attack speed: C (Purple), B (Green, Dark), A (Red)
;Combo attacks: 2 damage (1/8 bar) per hit [1]
;knockdown attack (combo finisher): 4 damage (1/4 bar) [1]
;Dash attack: 4 damage (1/4 bar) [1]
;Combo completed (two hits, knockdown): 8 damage (1/2 bar) [1]

;Dash attacks can't be blocked.
;------------------------------
;Skeletons
	
;Attack strength/speed: B (White), A (Purple)
;Combo attacks: 4 damage (1/4 bar) [1]
;Knockdown attack (combo finisher): 8 damage (1/2 bar) [1]
;Jump slash: 12 damage (3/4 bar) [1]
;Downward thrust: 16 damage (1 bar) [1]
;Combo completed (four hits, knockdown): 24 damage (1.5 bars) [1]

;Except for the downward thrust, Skeletons deal double the damage 
;of any lesser enemy type (Heningers, Longmoans and Amazons). Purple 
;Skeletons are only seen in the arcade version and The Duel mode. 
;Note that Death Bringer's Skeletons can be killed; they have 256 HP 
;whereas Death Bringer himself has only 128 HP, meaning he usually 
;dies first.[1]
;------------------------------
;Bad Brothers

;Attack strength: C (Green), B (Silver), A (Red)
;Attack speed: C (all variants)
;Dash attack: 8 damage (1/2 bar) [1]
;All other attacks: 16 damage (1 bar) [1]

;Bad Brothers cannot ride Bizarrians. Dash attacks can be stopped.
;------------------------------
;Knights

;Attack strength: A (all variants)
;Attack speed: B (Silver), A (Red, Gold)
;All attacks: 16 damage (1 bar) [1]

;Cannot run or ride Bizarrians.
;------------------------------
;Death Adder/Death Adder Jr.

;Attack strength: A (all variants)
;Attack speed: B (all variants)
;Axe swing: 16 damage (1 bar) [1]
;Projectile: 8 (1/2 bar) [1]

;Death Adder Jr. replaces Lt. Bitter as the boss of Stage 3 in Beginner 
;Mode. There are a couple of differences between the two:

;Death Adder Junior's projectile attack is launched through the air and 
;he wears blue armour. He is Death Adder's son.
;Death Adder's projectile attack is planted on the ground, and homes in 
;on your character and he wears red armour. He is Death Adder Junior's 
;father and Death Bringer's son.
;They can both run, but they do not use dash attacks or downward thrusts
; with their axes.
;------------------------------
;Death Bringer
;
;Attack strength/speed: A
;HP: 128 [2]
;Swing attack: 16 damage (1 bar) [2]
;Earth magic: 16 damage (1 bar) [2]
;Lighting magic: 8 damage (1/2 bar) [2]
;Fire (dragon) magic: 40 damage (2 1/2 bars) [2]

;Death Bringer is essentially the same as his son, Death Adder and his 
;grandson, Death Adder Junior, except for wearing pink armour and having 
;green skin. His projectile attack is launched the same way as Death 
;Adder's is, except it splits into six separate ones when planted on the 
;floor, which then home in on your character.

;He is also capable of using magic, as follows:

;If he knocks you down with his axe, he uses either Thunder magic level 1 
;if your character is to the left of him. He uses earth level 3 if you are
; to the right of him.
;If he knocks you down with his projectile, he will use the level six Fire 
;magic (Dragon), but it will be a purple dragon instead of white. If you get 
;hit by the first round of the projectile and dodge the second one quickly (or 
;if it just misses you right when you get back up from the ground), you have 
;about a two second window of opportunity to hit Death Bringer with a dash 
;attack. If executed just in time, he would not call upon the fire dragon. 
;This does not work with his axe attack because he calls upon his magic almost 
;immediately and you are either in the air or on the ground.
;He is also capable of running, but not using dash attacks or downward thrusts. 
;Note that Death Bringer's Skeletons can be killed; they have 256 HP whereas 
;Death Bringer himself has only 128 HP, meaning he usually dies first.[1]

;The Death Bringer is present only in the home versions. In the MS-DOS version 
;his abilities are somewhat limited compared to the Sega Genesis version - he 
;does not have the projectile attack, and his skeletons are far weaker than him. 
;Another difference is that in the MS-DOS version, you are actually required to 
;kill both skeletons, even if Death Bringer is already dead, allowing for 
;somewhat anti-climactic endings.

;------------------------------

;Thieves
; The Blue Thief drops magic pots. He will also steal them during the Bonus Stage. 
;Hit him to make him drop the goods.

; The Green Thief drops food. He does not steal. Hit him to make him drop the goods.

;------------------------------
;Chicken Leg

;Dash attack: 4 damage (1/4 bar) [1]
;Tail whip: 8 damage (1/2 bar) [1]
;First seen in Altered Beast, the Chicken Leg's attack is to spin around and hit 
;enemies with its tail. Because it has to turn to attack its attack is powerful 
;but delayed.
;------------------------------
;Blue Dragon

;Dash attack: 4 damage (1/4 bar) [1]
;Fire breath: 8 damage (1/2 bar) [1]
;The Blue Dragon spits fire with fairly good range. It cannot move while breathing 
;fire so is vulnerable when using this attack. Its dash attack has no such limitations.
;------------------------------
;Red Dragon

;Dash attack: 4 damage (1/4 bar) [1]
;Fireball: 8 damage (1/2 bar) [1]
;The Red Dragon's fireball can hit enemies across the screen (or even offscreen).
; Like the Blue Dragon it is unable to move while spitting fire. It also cannot spit 
;another fireball until the previous one is gone.











