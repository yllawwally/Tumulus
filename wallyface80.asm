;--------------------------------------------------------------
;Add Score HHH  XX  SSSSS
;make 8 eneamies, 5 styles, sword, javelin, fist(snake), sword both dir, javelin both dir
;add pot holes, max 6 per screen
;roll the kernel up, to save rom space
;rolling counter needs to be 16bit, for ressurection
;lose life for not killing badguys???
;rolling log by changing ball color and size????, use y as size????
;still 2 line too large?????
;--------------------------------------------------------------

	processor 6502
	include vcs.h
	include macro.h

; Constants ------
C_P0_HEIGHT 		= 8	;height of sprite
C_P1_HEIGHT 		= 12	;height of hero sprite
C_KERNAL_HEIGHT 	= 181	;height of kernal/actually the largest line on the screen ;was 186
Far_Left		= 36
Far_Right		= 140 
Far_Right_Hero		= 148
Far_Up_Hero		= C_KERNAL_HEIGHT-6
Far_Down_Hero		= 7+C_P1_HEIGHT
Enemy_Far_Left		= 3
HERO_SPEED_VER		= 1
HERO_SPEED_HOR		= 1
Screen_Rate		= 20	;How fast screen is scrolling in X-Axis
Enemy_Row_0		= 181  ;185
Enemy_Row_E0		= 142  ;160
Enemy_Row_E1		= 122  ;109
Enemy_Row_E2		= 102  ;109
Enemy_Row_E3		= 82   ;109
Enemy_Row_E4		= 62   ;109
Enemy_Row_E5		= 42   ;109
Enemy_Row_E6		= 22   ;73 #Enemy_Row_E2-#14
Enemy_Row_E7		= 2   ;35  #Enemy_Row_E3-#2 diff of 17???
;Variables ------

	seg.u RAM
	org $0080
PICS 			ds 1
ROLLING_COUNTER 	ds 2
Pos			ds 1




E0_Ptr 			ds 2	;ptr to current graphic
E1_Ptr 			ds 2	;ptr to current graphic
E2_Ptr 			ds 2	;ptr to current graphic
E3_Ptr 			ds 2	;ptr to current graphic
E4_Ptr 			ds 2	;ptr to current graphic
Temp_Ptr		ds 2	;ptr for temp holding
EnemyGraphicsColorPtr_E0	ds 2
EnemyGraphicsColorPtr_E1	ds 2
EnemyGraphicsColorPtr_E2	ds 2
EnemyGraphicsColorPtr_E3	ds 2
EnemyGraphicsColorPtr_E4	ds 2
TempGraphicsColor		ds 2
E0_XPos 		ds 1	;horizontal position
E1_XPos 		ds 1	;horizontal position
E2_XPos 		ds 1	;horizontal position
E3_XPos 		ds 1	;horizontal position
E4_XPos 		ds 1	;horizontal position
Temp_XPos		ds 1	;temp horizontal position
E0_Hit			ds 1	;collision detection
E1_Hit			ds 1	;collision detection
E2_Hit			ds 1	;collision detection
E3_Hit			ds 1	;collision detection
E4_Hit			ds 1	;collision detection

Pit0_XPos		ds 1	;where pit is currently
Pit1_XPos		ds 1	;where pit is currently
Pit2_XPos		ds 1	;where pit is currently
Pit3_XPos		ds 1	;where pit is currently
Pit4_XPos		ds 1	;where pit is currently
Pit5_XPos		ds 1	;where pit is currently


Hero_YPosFromBot 	ds 2	;Vertical position
Hero_XPos 		ds 2	;horizontal position
Hero_Y 			ds 1	;needed for skipdraw
Hero_Ptr 		ds 2	;ptr to current graphic
Hero_Sword_Pos		ds 1
HeroGraphicsColorPtr	ds 2


Graphics_Buffer		ds 1	;buffer for graphics
Graphics_Buffer_2	ds 1	;buffer for graphics
Graphics_Buffer_3	ds 1	;buffer for graphics

Color_Buffer		ds 1	;buffer for graphics
Color_Buffer_2		ds 1	;buffer for graphics

MOV_STAT		ds 1 	;direction player is moving

Enemy_Life		ds 1	;what eneamies are alive onscreen
Screen_Location		ds 2	;Where is the screen window in the X axis

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
PF_TWIST_TEMP		ds 1	; playfield buffer for flipping PF2

SavedStackPointer	ds 1

Score			ds 1	;player Score

ScoreTempA		ds 5	;temp for score lines
ScoreTempB		ds 5	;temp for score lines
ScoreTempC		ds 5	;temp for score lines
ScoreTempD		ds 5	;temp for score lines
ScoreTempE		ds 5	;temp for score lines
Enemy_Loop		ds 1	;loop variable for going through each bad guy
Row_1			ds 1
Row_2			ds 1
Row_3			ds 1

	seg code
	org $F000

;generic start up stuff...
Start
	SEI	
	CLD  	
	LDX #$FF	
	TXS	
	LDA #0	
	STA ROLLING_COUNTER
	STA PICS
	STX HMP0	;set the move for player 0, not the missile like last time...
	STX HMP1	;set the move for player 0, not the missile like last time...
	STA Score
	
ClearMem 
	STA 0,X		
	DEX		
	BNE ClearMem	
	
	LDA #$00   ;start with a black background
	STA COLUBK	

;Setting some variables...

	LDA #Far_Right
	STA E0_XPos
	STA E1_XPos
	STA E2_XPos
	STA E3_XPos
	STA E4_XPos

	LDA #0
	STA Pos
	STA ENAM0  ;enable it
	STA ENAM1  ;enable it
	STA Screen_Location
	STA Screen_Location+1
	STA HMM1	; of HMM1 sets it to moving
	LDA #%11111111
	STA Enemy_Life


	LDA #%00010000 ;set playfield to not reflected
	STA CTRLPF


LOADPFDATA

	LDX 0
	LDA PFData0,X		
	STA PF0_L1 ;B
	LDA PFData1,X		
	STA PF1_L1 ;B
	LDA PFData2,X		
	STA PF2_L1 ;C
	LDA PFData3,X		
	STA PF3_L1 ;d
	LDA PFData4,X		
	STA PF4_L1 ;E
	LDA PFData5,X		
	STA PF5_L1 ;F

	LDX 1
	LDA PFData0,X		
	STA PF0_L2 ;B
	LDA PFData1,X		
	STA PF1_L2 ;B
	LDA PFData2,X		
	STA PF2_L2 ;C
	LDA PFData3,X		
	STA PF3_L2 ;d
	LDA PFData4,X		
	STA PF4_L2 ;E
	LDA PFData5,X		
	STA PF5_L2 ;F

	LDX 2
	LDA PFData0,X		
	STA PF0_L3 ;B
	LDA PFData1,X		
	STA PF1_L3 ;B
	LDA PFData2,X		
	STA PF2_L3 ;C
	LDA PFData3,X		
	STA PF3_L3 ;d
	LDA PFData4,X		
	STA PF4_L3 ;E
	LDA PFData5,X		
	STA PF5_L3 ;F


;VSYNC time
MainLoop ;+++++++++++++++++++++++++++The start of a new screen
	LDA #0
	STA GRP0
	STA GRP1	
 	STA PF0
	STA PF1
	STA PF2
	LDA #30
	STA VSYNC	
	STA WSYNC	





	
; for up and down, we INC or DEC
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
;	DEC Hero_YPosFromBot

SkipMoveUp







;assum horiz speed will be zero

	;check Hero Sword Attack
	ldx INPT4
	bmi NoSwordAttack ;(button not pressed)
SwordAttack
	lda 	Hero_YPosFromBot
	sbc	#C_P1_HEIGHT - 6
	jmp DoneWithSwordAttack
NoSwordAttack
	lda 	#200
DoneWithSwordAttack
	sta	Hero_Sword_Pos



	LDA #%01000000	;Left?
	BIT SWCHA 
	BNE SkipMoveLeft
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

SkipMoveLeft
	LDA #%10000000	;Right?
	BIT SWCHA 
	BNE SkipMoveRight


	clc
	lda Hero_XPos
	adc #<HERO_SPEED_HOR
	sta Hero_XPos
	lda Hero_XPos+1
	adc #>HERO_SPEED_HOR
	sta Hero_XPos+1

;; moving right, cancel any mirrorimage
	LDA #%00000000
	STA REFP0
	STA REFP1
	STA MOV_STAT

SkipMoveRight


;Don't allow Hero too far right
	LDA #Far_Right_Hero
	CMP Hero_XPos
	BCS HeroRight
	STA Hero_XPos
HeroRight

;Don't allow Hero too far left
	LDA #Far_Left
	CMP Hero_XPos
	BCC HeroLeft
	STA Hero_XPos
HeroLeft

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

;----------------------



;setup pic animations ----------------------------------------------
	INC ROLLING_COUNTER
	BVC RCROLLOVER
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
	AND #3		;every 8th screen move
	CMP #2
	BNE MOVESET1
;Eneamy Movement---------------------------------------------------
	DEC E0_XPos
;	DEC E1_XPos
	DEC E2_XPos
	DEC E3_XPos
	DEC E4_XPos

	DEC Pit0_XPos
	DEC Pit1_XPos
	DEC Pit2_XPos
	DEC Pit3_XPos
	DEC Pit4_XPos
	DEC Pit5_XPos
;Eneamy Movement---------------------------------------------------



MOVESET1
	LDX #0
PITSET
	LDA Pit0_XPos,x
	CMP #Enemy_Far_Left
	BCS NOTOOFARLEFT
	LDA #Far_Right
	STA Pit0_XPos,x
NOTOOFARLEFT
	INX
	cpx #5
	bcc PITSET
	

	STA HMOVE
	STA WSYNC ;//////////////////////////////////////////////

	LDA #0
	STA HMP1 ;Set Hero to stand still
	
	STA WSYNC ;//////////////////////////////////////////////	



	LDA #43	
	STA TIM64T	





	LDA #0
	STA VSYNC 	

;------------------------- setup backgrounds 20 pixels accross
	LDA #255		; 3 cycles
	STA PF0			; 3 cycles
	STA PF1			; 3 cycles
	STA PF2			; 3 cycles
;-------------------------


;----------lots of time



;----------Scroll Screen in X Axis
	lda Screen_Location
	adc #<Screen_Rate
	sta Screen_Location
	lda Screen_Location+1
	adc #>Screen_Rate
	sta Screen_Location+1	
;----------Scroll Screen in X Axis


;collisions
	BIT E0_Hit		
	BPL NoCollisionP0	;skip if not hitting...	
	LDA #%11111110
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP0
	

;collisions
	BIT E1_Hit		
	BPL NoCollisionP1	;skip if not hitting...	
	LDA #%11111101
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP1

;collisions
	BIT E2_Hit		
	BPL NoCollisionP2	;skip if not hitting...	
	LDA #%11111011
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP2

;collisions
	BIT E3_Hit		
	BPL NoCollisionP3	;skip if not hitting...	
	LDA #%11110111
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP3

;collisions
	BIT E4_Hit		
	BPL NoCollisionP4	;skip if not hitting...	
	LDA #%11101111
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP4

;------------------------------------------


	lda #%00000001
	sta PF_TEMP
	ldx #0
;------------------------
RESSURECT
	and Enemy_Life
	BNE alive0
	lda RETURN,x
	CMP ROLLING_COUNTER+1
	BNE alive0
	lda #Far_Right-#1
	sta E0_XPos,x
	lda PF_TEMP
	ora Enemy_Life
	sta Enemy_Life

alive0
	
	INX
	lda PF_TEMP
	CLC
	ROL
	sta PF_TEMP
	cmp #%00100000
	bne RESSURECT
	

;---------------------------------------


;setup pic animations ----------------------------------------------

	lda ROLLING_COUNTER
	and #%00001000
	cmp #0
	bne RCP_1
	lda #<HeroGraphics0 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC #C_P1_HEIGHT
	sta Hero_Ptr		;(high byte already set)
	JMP RCP_2
RCP_1

	lda #<HeroGraphics0 	;low byte of ptr is graphic
RCP_2
	sta Hero_Ptr		;(high byte already set)


	lda #>HeroGraphics0 ;high byte of graphic location
	sta Hero_Ptr+1	;store in high byte of graphic pointer
	

	
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
	STA HeroGraphicsColorPtr
	



;setup pic animations ----------------------------------------------

;setup pic animations ----------------------------------------------





	lda #%00000001 
	and Enemy_Life 
	BNE alive1     
	lda #<EmptyPlayerGraphics 	;low byte of ptr is graphic 
	sta E0_Ptr		;(high byte already set)

	lda #>EmptyPlayerGraphics ;high byte of graphic location
	sta E0_Ptr+1	;store in high byte of graphic pointer
	jmp notalive1
alive1
	lda #<MainPlayerGraphics0 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E0_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics0 ;high byte of graphic location
	sta E0_Ptr+1	;store in high byte of graphic pointer

notalive1	

	lda E0_Ptr
	sec
	sbc #Enemy_Row_0 -#C_P0_HEIGHT - #4	;integer part of distance from bottom
	sta E0_Ptr	;2 byte

	LDA #<EnemyGraphicsColor0
	sta EnemyGraphicsColorPtr_E0
	LDA #>EnemyGraphicsColor0
	sta EnemyGraphicsColorPtr_E0+1
 
	lda EnemyGraphicsColorPtr_E0
	sec
	sbc #Enemy_Row_0-#4-#C_P0_HEIGHT
	STA EnemyGraphicsColorPtr_E0
;setup pic animations ----------------------------------------------


;setup pic animations ----------------------------------------------

	lda #%00000010
	and Enemy_Life
	BNE alive2b	
	lda #<EmptyPlayerGraphics 	;low byte of ptr is graphic
	sta E1_Ptr		;(high byte already set)

	lda #>EmptyPlayerGraphics ;high byte of graphic location
	sta E1_Ptr+1	;store in high byte of graphic pointer
	jmp notalive2b
alive2b




	lda #<MainPlayerGraphics4 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E1_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics4 ;high byte of graphic location
	sta E1_Ptr+1	;store in high byte of graphic pointer

notalive2b


	lda E1_Ptr
	sec
	sbc #Enemy_Row_E0-#5-#C_P0_HEIGHT	;integer part of distance from bottom
	sta E1_Ptr	;2 byte

	LDA #<EnemyGraphicsColor4
	sta EnemyGraphicsColorPtr_E1
	LDA #>EnemyGraphicsColor4
	sta EnemyGraphicsColorPtr_E1+1
 
	lda EnemyGraphicsColorPtr_E1
	sec
	sbc #Enemy_Row_E0-#5-#C_P0_HEIGHT
	STA EnemyGraphicsColorPtr_E1

;setup pic animations ----------------------------------------------




;setup pic animations ----------------------------------------------

	lda #%00000100
	and Enemy_Life
	BNE alive3b	
	lda #<EmptyPlayerGraphics 	;low byte of ptr is graphic
	sta E2_Ptr		;(high byte already set)

	lda #>EmptyPlayerGraphics ;high byte of graphic location
	sta E2_Ptr+1	;store in high byte of graphic pointer
	jmp notalive3
alive3b

	lda #<MainPlayerGraphics3 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E2_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics3 ;high byte of graphic location
	sta E2_Ptr+1	;store in high byte of graphic pointer

notalive3




	lda E2_Ptr
	sec
	sbc #Enemy_Row_E1-#5-#C_P0_HEIGHT	;integer part of distance from bottom
	sta E2_Ptr	;2 byte


	LDA #<HeroGraphicsColor
	sta EnemyGraphicsColorPtr_E2
	LDA #>HeroGraphicsColor
	sta EnemyGraphicsColorPtr_E2+1
 
	lda EnemyGraphicsColorPtr_E2
	sec
	sbc #Enemy_Row_E1-#5-#C_P0_HEIGHT
	STA EnemyGraphicsColorPtr_E2

;setup pic animations ----------------------------------------------

;setup pic animations ----------------------------------------------


	lda #%00001000
	and Enemy_Life
	BNE alive4b	
	lda #<EmptyPlayerGraphics 	;low byte of ptr is graphic
	sta E3_Ptr		;(high byte already set)

	lda #>EmptyPlayerGraphics ;high byte of graphic location
	sta E3_Ptr+1	;store in high byte of graphic pointer
	jmp notalive4
alive4b


	lda #<MainPlayerGraphics2 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E3_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics2 ;high byte of graphic location
	sta E3_Ptr+1	;store in high byte of graphic pointer
notalive4	


	lda E3_Ptr
	sec
	sbc #Enemy_Row_E2 -#5 -  #C_P0_HEIGHT	;integer part of distance from bottom
	sta E3_Ptr	;2 byte

	LDA #<EnemyGraphicsColor2
	sta EnemyGraphicsColorPtr_E3
	LDA #>EnemyGraphicsColor2
	sta EnemyGraphicsColorPtr_E3+1
 
	lda EnemyGraphicsColorPtr_E3
	sec
	sbc #Enemy_Row_E2-#5-#C_P0_HEIGHT
	STA EnemyGraphicsColorPtr_E3

;setup pic animations ----------------------------------------------

;setup pic animations ----------------------------------------------
	lda #%00010000
	and Enemy_Life
	BNE alive5b	
	lda #<EmptyPlayerGraphics 	;low byte of ptr is graphic
	sta E4_Ptr		;(high byte already set)

	lda #>EmptyPlayerGraphics ;high byte of graphic location
	sta E4_Ptr+1	;store in high byte of graphic pointer
	jmp notalive5
alive5b
	lda #<MainPlayerGraphics3 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E4_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics3 ;high byte of graphic location
	sta E4_Ptr+1	;store in high byte of graphic pointer
notalive5	



	lda E4_Ptr
	sec
	sbc #Enemy_Row_E3-#5-#C_P0_HEIGHT	;integer part of distance from bottom
	sta E4_Ptr	;2 byte

	LDA #<EnemyGraphicsColor3
	sta EnemyGraphicsColorPtr_E4
	LDA #>EnemyGraphicsColor3
	sta EnemyGraphicsColorPtr_E4+1
 
	lda EnemyGraphicsColorPtr_E4
	sec
	sbc #Enemy_Row_E3-#5-#C_P0_HEIGHT
	STA EnemyGraphicsColorPtr_E4

;setup pic animations ----------------------------------------------








	LDA ROLLING_COUNTER
	AND #%00111111


	CMP #%00111111
	BNE SKIPMTN	



	LDA #%00010000
	AND PF0_L1
	CMP #0
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
	CMP #0
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
	CMP #0
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
	CMP #0
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
	CMP #0
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
	CMP #0
	CLC
	BEQ R3JMP
	SEC
R3JMP	
	ROR PF2_L3;4bit
	ROL PF1_L3 ;reversed
	ROR PF0_L3 

SKIPMTN





	LDA %00000001
	STA VDELP0
;	STA VDELP1
	
;setup php ball trick

	tsx	;transfer stack
	stx	SavedStackPointer ;store pointer in ram
	ldx #ENAM0+1
	txs	;set stack to enam location
;end ball trick setup
	
	LDY #4 ; was 191 	
;----------lots of time
WaitForVblankEnd
	LDA INTIM	
	BNE WaitForVblankEnd	


	STA WSYNC	
	STA VBLANK  	


;main scanline loop...

	STA WSYNC
	STA WSYNC
	STA WSYNC
	STA WSYNC

PreScanLoop



;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDX #2
MTNRANGE1
	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK		;and store as the bgcolor ; 3 cycles	 
	LDA PF0_L1		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF1_L1		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF2_L1		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA PF3_L1		; 4 cycles 
	STA PF0			; 3 cycles
	LDA PF4_L1		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L1		; 4 cycles
	STA PF2			; 3 cycles


	STA WSYNC 						 ;3 cycles =74
	DEX
;	bne     MTNRANGE1 
	dey
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDX #1
MTNRANGE2

	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK		;and store as the bgcolor ; 3 cycles
	LDA PF0_L2		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF1_L2		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF2_L2		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA PF3_L2		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF4_L2		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L2		; 4 cycles
	STA PF2			; 3 cycles


	STA WSYNC 
	DEX
;	bne     MTNRANGE2 	
	dey					 ;3 cycles =74
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDX #2
MTNRANGE3

	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK		;and store as the bgcolor ; 3 cycles
	LDA PF0_L3		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF1_L3		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF2_L3		; 4 cycles
	STA PF2			; 3 cycles
	nop
	nop
	LDA PF3_L3		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF4_L3		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L3		; 4 cycles
	STA PF2			; 3 cycles
	

	STA WSYNC 						 ;3 cycles =74
	DEX
;	bne     MTNRANGE3 
	dey
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDX #2
MTNRANGE4

	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK		;and store as the bgcolor ; 3 cycles
	LDA PF0_L4		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF1_L4		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF2_L4		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA PF3_L4		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF4_L4		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L4		; 4 cycles
	STA PF2			; 3 cycles
	
	ldx #4
	STx PF_TWIST_TEMP

	STA WSYNC 	
	DEX
;	bne     MTNRANGE4	
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline

	LDA #18;

	STA COLUBK	;and store as the bgcolor
	LDY #C_KERNAL_HEIGHT; 
	LDA #0
	STA PF0			; 3 cycles
	STA PF1			; 3 cycles
	STA PF2			; 3 cycles
	
	LDA #%11111000	;The last 3 bits control number and size of players
			;the second 2 bits control missle size
	STA NUSIZ0
	STA NUSIZ1
	STA HMCLR

	DEC Hero_Y ;Hero always is decremented, because he travels the whole screen

	STA WSYNC 						 ;3 cycles
	
	

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
	nop.w ;these 2 nops allow the hero to do fine positioning???? I don't know why.....
	DEC Hero_Y
        DEY             ;count down number of scan lines          2 cycles = 
        DEY             ;count down number of scan lines          2 cycles = 
	nop.w
	DEY
	STA HMCLR
	LDA MOV_STAT
	CMP #1
	BCS MLEFT
	;STA HMCLR

	LDA #0
	JMP MRIGHT
MLEFT	
	LDA #%00101000	;The last 3 bits control number and size of players
			;the second 2 bits control missle size
	STA NUSIZ1
	LDA #%01100000 ;Atari only looks at 4 most significant bits in 2's compliment
MRIGHT	STA HMM1

	STA CXCLR	;reset the collision detection for next time

	LDA #%01110000
	STA HMM0

	STA WSYNC
	STA HMOVE ;3
	lda #0
	sta GRP0
	STA WSYNC
;start of kernal +++++++++++++++++++++++ for player 0 positioning
;the hmove above pushes it slightly
	lda E0_XPos						 ;3
	nop	;these are here to make him match the others
	nop
	nop.w

.Div15a   
	sbc #15      ; 2         
	bcs .Div15a   ; 3(2)

	tax		;2
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;	;3
	sta RESP0 ;	


        STA WSYNC                                                ;3 cycles =
	STA HMOVE
	Ldx #0
;end of kernal +++++++++++++++++ for player 0 positioning

	ldx 	Graphics_Buffer  ;3
	stx	GRP1	;3
	lda     (HeroGraphicsColorPtr),y      ; 5
	sta	COLUP1
	DEY


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw

	

	STA CXCLR   ;3

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_e:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5


        DEY             ;2 count down number of scan lines          2 cycles
	STA HMCLR ;added here
	

;        STA WSYNC                                                ;3 cycles =
;------------------------------------------------------------

.408:
Draw_Enemy_E0
;-----------------------this needs to run for 8 lines, the size of an enemy
	stx	GRP1	;3
	sta 	COLUP1

;skipDraw


;---removed comparisons here, always display character
;--this will mess up all subsequent monsters

; draw player sprite 0:
	lda     (E0_Ptr),y      ; 5
	sta 	GRP0	;3
	lda     (EnemyGraphicsColorPtr_E0),y      ; 5
	sta	COLUP0	;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
	

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb8       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb8:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles
	CPY #Enemy_Row_0 - #12
        STA WSYNC 
;-----------------------this needs to run for 8 lines, the size of an enemy
	BCS Draw_Enemy_E0


;------------------------------------------------------------
ScanLoop_E0_c
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


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles

	CPY #Enemy_Row_E0-#2 ;3

        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E0_c                                             ;2 cycles =
EndScanLoop_E0_c
;------------------------------------------------------------
	stx	GRP1	;3
	sta 	COLUP1 ;3
	lda 	#0      ;2
	sta 	GRP0    ;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb21       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb21:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer_2
	lda     (HeroGraphicsColorPtr),y      ; 5
	tax

	dey

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb22       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb22:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer

	
	lda  Graphics_Buffer_2
	cpy Hero_Sword_Pos  ;3
;        STA WSYNC   
;-------------------------Enemy number E0 End---------------------------
;------------------------------------------------+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.500: SUBROUTINE
;This is not a loop, this is a one time set position for the eneamy E1
 	php	;2
	sta GRP1		
	stx COLUP1
	lda E1_XPos ;3
.Div15_E1_a   
	sbc #15      ; 2         
	bcs .Div15_E1_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;
	sta RESP0 ;


;sword php style	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE



;This is not a loop, this is a one time set position for the eneamy E1

  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1
	ldx 	Graphics_Buffer  ;3
	stx	GRP1	;3
	lda     (HeroGraphicsColorPtr),y      ; 5
	sta	COLUP1
	DEY


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw

	

	LDA CXM1P   ;3
	STA E0_Hit  ;this line must refer to previous enemy
	STA CXCLR   ;3

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_e:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5


        DEY             ;2 count down number of scan lines          2 cycles
	


        STA WSYNC                                                ;3 cycles =
;------------------------------------------------------------
Draw_Enemy_E1
.508:
;-----------------------this needs to run for 8 lines, the size of an enemy
	stx	GRP1	;3
	sta 	COLUP1

;skipDraw


;---removed comparisons here, always display character
;--this will mess up all subsequent monsters

; draw player sprite 0:
	lda     (E1_Ptr),y      ; 5
	sta 	GRP0	;3
	lda     (EnemyGraphicsColorPtr_E1),y      ; 5
	sta	COLUP0	;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
	

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_eb8       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_eb8:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5

        DEY             ;2 count down number of scan lines          2 cycles
	CPY #Enemy_Row_E0-#13
        STA WSYNC 
;-----------------------this needs to run for 8 lines, the size of an enemy
	BCS Draw_Enemy_E1

;------------------------------------------------------------
ScanLoop_E1_c
	stx	GRP1	;3
	sta 	COLUP1
	lda 	#0
	sta 	GRP0
	lda		#2
	sta 	PF_TEMP
	lda		#4
	sta 	PF_TWIST_TEMP

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_eb       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_eb:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles

	CPY #Enemy_Row_E1 ;3

        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E1_c                                             ;2 cycles =
EndScanLoop_E1_c

;---------------------------this is the new end of E1---------------------------------------------





;----------THE NEW START OF E2-----+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+--------------------
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
	
	lda E0_XPos,x
	sta Temp_XPos
	clc
	
	lda #Enemy_Row_E1
	sbc #12
	sta	Row_1	

	lda #Enemy_Row_E2
	adc #5
	sta	Row_2

	lda #Enemy_Row_E2
	sbc #1
	sta	Row_3

;--------------need to setup all enemy variables--------------------


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_ebr       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_ebr:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles





new_E1_line1    ; STA WSYNC          ;not enough time                                     
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

	ldx PF_TWIST_TEMP
	
	lda EnemyGraphicsColorPtr_E0,x
	sta TempGraphicsColor
	lda EnemyGraphicsColorPtr_E0+1,x
	sta TempGraphicsColor+1

	lda #E0_Ptr,x
	sta Temp_Ptr

	lda #E0_Ptr+#1,x
	sta Temp_Ptr+1

	inc PF_TEMP
	inc PF_TWIST_TEMP
	inc PF_TWIST_TEMP


;--------------need to setup all enemy variables--------------------


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_ebrt       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_ebrt:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles





new_E1_line2     STA WSYNC                                               
;----------added section----------------------------------------




;------------------------------------------------------------
	stx	GRP1	;3
	sta 	COLUP1 ;3
	lda 	#0      ;2
	sta 	GRP0    ;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb21       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb21:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer_2
	lda     (HeroGraphicsColorPtr),y      ; 5
	tax

	dey

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb22       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb22:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer

 ;       DEY             ;2 count down number of scan lines          2 cycles
	
	lda  Graphics_Buffer_2
	cpy Hero_Sword_Pos  ;3
;        STA WSYNC   
;-------------------------Enemy number E1 End---------------------------
;------------------------------------------------+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.600: SUBROUTINE
;This is not a loop, this is a one time set position for the eneamy E2
 	php	;2
	sta GRP1		
	stx COLUP1

	lda Temp_XPos ;3
.Div15_E2_a   
	sbc #15      ; 2         
	bcs .Div15_E2_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 
	sta RESP0 


;sword php style	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE

;---------------------------------------------

;This is not a loop, this is a one time set position for the eneamy E2

  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1
	ldx 	Graphics_Buffer  ;3
	stx	GRP1	;3
	lda     (HeroGraphicsColorPtr),y      ; 5
	sta	COLUP1
	DEY


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw

	
	nop.w ;needed to prevent bent sword after commenting out previous line
	LDA CXM1P   ;3
	STA E1_Hit  ;this line must refer to previous enemy
	STA CXCLR   ;3

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E2_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_e:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5


        DEY             ;2 count down number of scan lines          2 cycles
	


        STA WSYNC                                                ;3 cycles =
;------------------------------------------------------------

Draw_Enemy_E2
.608:
;-----------------------this needs to run for 8 lines, the size of an enemy
	stx	GRP1	;3
	sta 	COLUP1

;skipDraw
; draw player sprite 0:
	lda     (Temp_Ptr),y      ; 5
	sta 	GRP0	;3
	lda     (TempGraphicsColor),y      ; 5
	sta	COLUP0	;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
	

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E2_eb8       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_eb8:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles
	cpy Row_1

        STA WSYNC 
;-----------------------this needs to run for 8 lines, the size of an enemy
	BCS Draw_Enemy_E2

;---------------------line for setting up pits-----------------------------------------
	stx	GRP1	;3
	sta 	COLUP1 ;3
	lda 	#0      ;2
	sta 	GRP0    ;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb21a       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb21a:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer_2
	lda     (HeroGraphicsColorPtr),y      ; 5
	tax

	dey

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E0_eb22a       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E0_eb22a:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer

 ;       DEY             ;2 count down number of scan lines          2 cycles
	
	lda  Graphics_Buffer_2
	cpy Hero_Sword_Pos  ;3


;--------------------wsync not here because above is exact size




 	php	;2
	sta GRP1		
	stx COLUP1

	lda Pit0_XPos ;3
.Div15_Pit   
	sbc #15      ; 2         
	bcs .Div15_Pit   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMBL 
	sta RESBL
;	DEY

;sword php style	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE



;-----------------------------------------------post turning on pit stuff
;This is not a loop, this is a one time set position for the eneamy E2

  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1
	ldx 	Graphics_Buffer  ;3
	stx	GRP1	;3
	lda     (HeroGraphicsColorPtr),y      ; 5
	sta	COLUP1
	DEY


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
	bcs     .doDrawHero_E2_en      ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_en:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5


        DEY             ;2 count down number of scan lines          2 cycles
	


        STA WSYNC                                                ;3 cycles =
;------------------------------------------------------------

;---------------------line for setting up pits-----------------------------------------





ScanLoop_E2_c
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


;--- need to put pits here----------------------------------------------------------------------------------------------------------------<<<<<<
;you have about 24 cycles--------------------------
	LDA #$FF
	CPY Row_2 ;3
	BCC NO_PIT
	LDA #0
NO_PIT
	STA ENABL
;--- need to put pits here----------------------------------------------------------------------------------------------------------------<<<<<<



;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E2_eb       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_eb:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles

	CPY Row_3 ;3

        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E2_c                                             ;2 cycles =
EndScanLoop_E2_c
;------------------------------------------------------------
;------------------------------------------------------------
	stx	GRP1	;3
	sta 	COLUP1 ;3
	lda 	#0      ;2
	sta 	GRP0    ;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_eb21       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_eb21:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer_2
	lda     (HeroGraphicsColorPtr),y      ; 5
	tax

	dey

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_eb22       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_eb22:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	sta Graphics_Buffer

	lda  Graphics_Buffer_2
	cpy Hero_Sword_Pos  ;3
;        STA WSYNC   
;-------------------------Enemy number E2 End---------------------------
;------------------------------------------------+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




;------------------------------------------------+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.800: SUBROUTINE
;This is not a loop, this is a one time set position for the eneamy E4
 	php	;2
	sta GRP1		
	stx COLUP1
	lda E4_XPos ;3
.Div15_E4_a   
	sbc #15      ; 2         
	bcs .Div15_E4_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1


;sword php style	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE



;This is not a loop, this is a one time set position for the eneamy E4

  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1
	ldx 	Graphics_Buffer  ;3
	stx	GRP1	;3
	lda     (HeroGraphicsColorPtr),y      ; 5
	sta	COLUP1
	DEY


;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw

	

	LDA CXM1P   ;3
	STA E3_Hit  ;this line must refer to previous enemy
	STA CXCLR   ;3

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E4_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_e:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5


        DEY             ;2 count down number of scan lines          2 cycles
	

;sword php style

        STA WSYNC                                                ;3 cycles =
;------------------------------------------------------------

Draw_Enemy_E4
.808:
;-----------------------this needs to run for 8 lines, the size of an enemy
	stx	GRP1	;3
	sta 	COLUP1

;skipDraw


; draw player sprite 0:
	lda     (E4_Ptr),y      ; 5
	sta 	GRP0	;3
	lda     (EnemyGraphicsColorPtr_E4),y      ; 5
	sta	COLUP0	;3

	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
	

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E4_eb8       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_eb8:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles
	cpy #Enemy_Row_E3-#13

        STA WSYNC 
;-----------------------this needs to run for 8 lines, the size of an enemy
	BCS Draw_Enemy_E4




;------------------------------------------------------------
ScanLoop_E4_c
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


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E4_eb       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_eb:
	lda     (Hero_Ptr),y      ; 5
;This allows us to do the calculation early, but must move dey to before routine
	tax
	lda     (HeroGraphicsColorPtr),y      ; 5



        DEY             ;2 count down number of scan lines          2 cycles

	CPY #6 ;3

        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E4_c                                             ;2 cycles =
EndScanLoop_E4_c
;------------------------------------------------------------
	stx	GRP1	;3
	sta 	COLUP1 ;3
	lda 	#0      ;2
	sta 	GRP0    ;3

;sword php style
	cpy Hero_Sword_Pos  ;3
	php			;3
  	ldx #ENAM0+1		;2
  	txs                    ;2 Set the top of the stack to ENAM1+1
;sword php style 
;skipDraw


;---removed enemy display character


;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E3_eb21       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_eb21:
	lda     (Hero_Ptr),y      ; 5
	sta Graphics_Buffer_2
	lda     (HeroGraphicsColorPtr),y      ; 5
	tax

	dey

;skipDraw
; draw Hero sprite:
	lda     #C_P1_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E3_eb22       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_eb22:
	lda     (Hero_Ptr),y      ; 5
	sta Graphics_Buffer

	
	lda  Graphics_Buffer_2
	cpy Hero_Sword_Pos ;3
;        STA WSYNC   
;-------------------------Enemy number E4 End---------------------------

	LDA #$B0   ;start with a black background
	STA COLUBK


;php sword stuff
  cpy Hero_Sword_Pos  ;3
  php
  ldx SavedStackPointer
  txs
; php sword stuff

	STA WSYNC  	

	LDA #%01101100
	STA CTRLPF

	LDA (#NUM8),Y		; 4 cycles
	STA PF0			; 3 cycles
	LDA (#NUM7),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA #$00		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA #$00		; 4 cycles 
	STA PF0			; 3 cycles
	LDA (#NUM4),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA (#NUM3),Y		; 4 cycles
	STA PF2			; 3 cycles

	DEY
	STA WSYNC  

	

	LDA (#NUM8),Y		; 4 cycles
	STA PF0			; 3 cycles
	LDA (#NUM7),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA (#NUM6),Y		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA #$00		; 4 cycles 
	STA PF0			; 3 cycles
	LDA (#NUM4),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA (#NUM3),Y		; 4 cycles
	STA PF2			; 3 cycles

	DEY
	STA WSYNC    




	LDA (#NUM8),Y		; 4 cycles
	STA PF0			; 3 cycles
	LDA (#NUM7),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA #$00		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA #$00		; 4 cycles 
	STA PF0			; 3 cycles
	LDA (#NUM4),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA (#NUM3),Y		; 4 cycles
	STA PF2			; 3 cycles

	DEY
	STA WSYNC   



	LDA (#NUM8),Y		; 4 cycles
	STA PF0			; 3 cycles
	LDA (#NUM7),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA #$00		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA #$00		; 4 cycles 
	STA PF0			; 3 cycles
	LDA (#NUM4),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA (#NUM3),Y		; 4 cycles
	STA PF2			; 3 cycles

	DEY
	STA WSYNC  



	LDA (#NUM8),Y		; 4 cycles
	STA PF0			; 3 cycles
	LDA (#NUM7),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA #$00		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	LDA #$00		; 4 cycles 
	STA PF0			; 3 cycles
	LDA (#NUM4),Y		; 4 cycles
	STA PF1			; 3 cycles
	LDA (#NUM3),Y		; 4 cycles
	STA PF2			; 3 cycles
	LDA #0
	STA PF0
	STA PF1
	STA PF2
	DEY
	STA WSYNC   
		


	LDA #%01101100

	STA CTRLPF



	STA VBLANK 	
	LDY #30	;number of overscan lines	

	LDA CXM1P
	STA E4_Hit
	STA CXCLR	;reset the collision detection for next time
	DEY
	STA WSYNC
	
;Fix Positions

;Don't allow P0 past Position 160
	LDA E0_XPos	;2
	CMP #Far_Right	;2
	BCC P0Right	;2(3)
	LDA #Far_Right-#1 ;2
	STA E0_XPos	;3
P0Right

	STA WSYNC
	DEY

;Don't allow P1 past Position 160
	LDA E1_XPos	;2
	CMP #Far_Right	;2
	BCC P1Right	;2(3)
	LDA #Far_Right-#1 ;2
	STA E1_XPos	;3
P1Right


	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA E2_XPos	;2
	CMP #Far_Right	;2
	BCC P2Right	;2(3)
	LDA #Far_Right-#1 ;2
	STA E2_XPos	;3
P2Right


	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA E3_XPos	;2
	CMP #Far_Right	;2
	BCC P3Right	;2(3)
	LDA #Far_Right-#1 ;2
	STA E3_XPos	;3
P3Right


	STA WSYNC
	DEY

;Don't allow P4 past Position 160
	LDA E4_XPos	;2
	CMP #Far_Right	;2
	BCC P4Right	;2(3)
	LDA #Far_Right-#1 ;2
	STA E4_XPos	;3
P4Right


	STA WSYNC
	DEY


;Don't allow P0 past Position 160
	LDA #Enemy_Far_Left
	CMP E0_XPos	;2
	BCC P0left	;2(3)
	lda #%11111110
	and Enemy_Life
	sta Enemy_Life
	LDA #Far_Right
	STA E0_XPos
P0left

	STA WSYNC
	DEY

;Don't allow P1 past Position 160
	LDA #Enemy_Far_Left
	CMP E1_XPos	;2
	BCC P1left	;2(3)
	lda #%11111101
	and Enemy_Life
	sta Enemy_Life
	LDA #Far_Right
	STA E1_XPos

P1left

	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA #Enemy_Far_Left
	CMP E2_XPos	;2
	BCC P2left	;2(3)
	lda #%11111011
	and Enemy_Life
	sta Enemy_Life
	LDA #Far_Right
	STA E2_XPos

P2left

	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA #Enemy_Far_Left
	CMP E3_XPos	;2
	BCC P3left	;2(3)
	lda #%11110111
	and Enemy_Life
	sta Enemy_Life
	LDA #Far_Right
	STA E3_XPos
P3left
	STA WSYNC
	DEY

;Don't allow P4 past Position 160
	LDA #Enemy_Far_Left
	CMP E4_XPos	;2
	BCC P4left	;2(3)
	lda #%11101111
	and Enemy_Life
	sta Enemy_Life
	LDA #Far_Right
	STA E4_XPos
P4left
	STA WSYNC
	DEY

;calculate score to display on next screen

;ScoreTempA		ds 4	;temp for score lines
;need to setup ScoreTempA just like graphics, with pointer

	ldx Score
	stx ScoreTempA
	stx ScoreTempA,1
	stx ScoreTempA,2
	stx ScoreTempA,3
	stx ScoreTempA,4
	stx ScoreTempA,5
	stx ScoreTempA,6
	stx ScoreTempA,7
	stx ScoreTempA,8
	stx ScoreTempA,9



	STA WSYNC
	DEY
	

OverScanWait
	STA WSYNC
	DEY
	BNE OverScanWait
	JMP  MainLoop   

 ;           ORG $F0E0;F9C6

NUM0
        .byte #%11101110;
        .byte #%10101010;
        .byte #%10101010;
        .byte #%10101010;
        .byte #%11101110;
NUM1
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
NUM2
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%10001000;
        .byte #%11101110;
NUM3
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
NUM4
        .byte #%00100010;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%10101010;
        .byte #%10101010;
NUM5
        .byte #%11101110;
        .byte #%00100010;
        .byte #%10101110;
        .byte #%10001000;
        .byte #%11101110;
NUM6
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;
        .byte #%00100010;
        .byte #%11101110;
NUM7
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%00100010;
        .byte #%11101110;
NUM8
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;
NUM9
        .byte #%00100010;
        .byte #%00100010;
        .byte #%11101110;
        .byte #%10101010;
        .byte #%11101110;



;-----------------------------
; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write
;ROM is located from F000 to FFFF

            ORG $FBE0 ;this a critical location, must be on edge of page
;	ORG $FCE0
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


	org $FCA6 ;fdbc



HeroGraphicsColor

     .byte #$FC
     .byte #$FA
     .byte #$FA
     .byte #$F8
     .byte #$FE
     .byte #$FE
     .byte #$FC
     .byte #$F6
     .byte #$FA
     .byte #$F8
     .byte #$FE
     .byte #$FE


;$FB00 + KernalHeight


;	align 256	

HeroGraphics0
        .byte #%00110110;$F4
        .byte #%00100100;$F4
        .byte #%00111100;$F4
        .byte #%00011000;$F4
        .byte #%00011000;$F4
        .byte #%00011110;$EA
        .byte #%00011000;$CA
        .byte #%00111000;$46
        .byte #%00101000;$0E
        .byte #%00101000;$0E
        .byte #%00100000;$0E
        .byte #%00111000;$22


        .byte #%00010100;$F4
        .byte #%00101000;$F4
        .byte #%00111000;$F4
        .byte #%00011000;$F4
        .byte #%00011000;$F4
        .byte #%00011110;$EA
        .byte #%00011000;$CA
        .byte #%00111000;$46
        .byte #%00101000;$0E
        .byte #%00101000;$0E
        .byte #%00100000;$0E
        .byte #%00111000;$22

MainPlayerGraphics0
	.byte #%00010100
	.byte #%00010100
	.byte #%00011000
	.byte #%00011000
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


	.byte #%00100010
	.byte #%00100100
	.byte #%00101000
	.byte #%00011000
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


MainPlayerGraphics1
	.byte #%00010100
	.byte #%00010100
	.byte #%00111100
	.byte #%00111100
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


	.byte #%00100010
	.byte #%00100100
	.byte #%00111100
	.byte #%00111100
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000

	org $FDA6 ;fdbc

MainPlayerGraphics2
	.byte #%01111000
	.byte #%10000100
	.byte #%00001000
	.byte #%00010000
	.byte #%00010000
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


	.byte #%01111000
	.byte #%01001000
	.byte #%00010000
	.byte #%00100000
	.byte #%00100000
	.byte #%00100000
	.byte #%01110000
	.byte #%01110000


MainPlayerGraphics3
	.byte #%01111000
	.byte #%10000100
	.byte #%00001000
	.byte #%00010000
	.byte #%01111100
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


	.byte #%01111000
	.byte #%01001000
	.byte #%00010000
	.byte #%00100000
	.byte #%11111000
	.byte #%00100000
	.byte #%01110000
	.byte #%01110000

	org $FEC0

MainPlayerGraphics4
        .byte #%00001000;$C0
        .byte #%00111000;$C0
        .byte #%00101000;$D0
        .byte #%00001110;$C2
        .byte #%00001010;$D2
        .byte #%00011000;$D2
        .byte #%00001000;$C2
        .byte #%00000000;$22

        .byte #%00001000;$C0
        .byte #%00111000;$C0
        .byte #%00101000;$D0
        .byte #%00001110;$C2
        .byte #%00001010;$D2
        .byte #%00011000;$D2
        .byte #%00001000;$C2
        .byte #%00000000;$22



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

	org $FFC0

EnemyGraphicsColor4
        .byte #$C0;
        .byte #$C0;
        .byte #$D0;
        .byte #$C2;
        .byte #$D2;
        .byte #$D2;
        .byte #$C2;
        .byte #$22;

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
	.byte #$29
	.byte #$5D
	.byte #$49
	.byte #$39

RETURN
	.byte #$1
	.byte #$30
	.byte #$E0
	.byte #$70
	.byte #$A0
	.byte #$90
	.byte #$C0
	.byte #$50



	org $FFFC
	.word Start
	.word Start

