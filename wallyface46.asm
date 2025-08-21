;--------------------------------------------------------------
;sword shift one line down when moving left
;should make this 3d by adjusting the colors, change bg color to to bottom.
;bne vs bcc , lda, x used incorrectly didn't load x tried to load location
;between 41 and 42 added bug when crossing segments, he stretches
;--------------------------------------------------------------

	processor 6502
	include vcs.h
	include macro.h

; Constants ------
C_P0_HEIGHT 		= 8	;height of sprite
C_P1_HEIGHT 		= 8	;height of sprite
C_KERNAL_HEIGHT 	= 186	;height of kernal/actually the largest line on the screen
Far_Left		= 16	
Far_Right		= 150
Far_Right_Hero		= 140
Far_Up_Hero		= 182
Far_Down_Hero		= 10
Enemy_Far_Left		= 1
Enemy_Row_0		= 185
Enemy_Row_E0		= 150
Enemy_Row_E1		= 115
Enemy_Row_E2		= 70
Enemy_Row_E3		= 35
Enemy_Row_E4		= 0
HERO_SPEED_VER		= 1
HERO_SPEED_HOR		= 1
Screen_Rate		= 20	;How fast screen is scrolling in X-Axis

;Variables ------

	seg.u RAM
	org $0080
PICS 			ds 1
ROLLING_COUNTER 	ds 1
Pos			ds 1


E0_YPosFromBot 		ds 1	;Vertical position
E0_XPos 		ds 1	;horizontal position
E0_Y 			ds 1	;needed for skipdraw
E0_Ptr 			ds 2	;ptr to current graphic
E0_Hit			ds 1	;collision detection

E1_YPosFromBot 		ds 1	;Vertical position
E1_XPos 		ds 1	;horizontal position
E1_Y 			ds 1	;needed for skipdraw
E1_Ptr 			ds 2	;ptr to current graphic
E1_Hit			ds 1	;collision detection

E2_YPosFromBot 		ds 1	;Vertical position
E2_XPos 		ds 1	;horizontal position
E2_Y 			ds 1	;needed for skipdraw
E2_Ptr 			ds 2	;ptr to current graphic
E2_Hit			ds 1	;collision detection

E3_YPosFromBot 		ds 1	;Vertical position
E3_XPos 		ds 1	;horizontal position
E3_Y 			ds 1	;needed for skipdraw
E3_Ptr 			ds 2	;ptr to current graphic
E3_Hit			ds 1	;collision detection

E4_YPosFromBot 		ds 1	;Vertical position
E4_XPos 		ds 1	;horizontal position
E4_Y 			ds 1	;needed for skipdraw
E4_Ptr 			ds 2	;ptr to current graphic
E4_Hit			ds 1	;collision detection

Hero_YPosFromBot 	ds 2	;Vertical position
Hero_XPos 		ds 2	;horizontal position
Hero_Y 			ds 1	;needed for skipdraw
Hero_Ptr 		ds 2	;ptr to current graphic
Hero_Sword_Pos		ds 1

Graphics_Buffer		ds 1	;buffer for graphics
Graphics_Buffer_2	ds 1	;buffer for graphics
Graphics_Buffer_3	ds 1	;buffer for graphics

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
	
ClearMem 
	STA 0,X		
	DEX		
	BNE ClearMem	
	
	LDA #$00   ;start with a black background
	STA COLUBK	

	LDA #$1C   ;lets go for bright yellow, the traditional color for happyfaces
	STA COLUP0
;Setting some variables...
	LDA #Enemy_Row_0-#10 
	STA E0_YPosFromBot
	LDA #50
	STA E0_XPos
	LDA #Enemy_Row_E0-#10 
	STA E1_YPosFromBot
	LDA #50
	STA E1_XPos
	LDA #Enemy_Row_E1-#10 
	STA E2_YPosFromBot
	LDA #50
	STA E2_XPos
	LDA #Enemy_Row_E2-#10 
	STA E3_YPosFromBot
	LDA #50
	STA E3_XPos
	LDA #Enemy_Row_E3-#10 
	STA E4_YPosFromBot
	LDA #50
	STA E4_XPos
	LDA #0
	STA Pos
 

	
	LDA #0
	STA ENAM0  ;enable it
	STA ENAM1  ;enable it
	STA Screen_Location
	STA Screen_Location+1
	LDA #%11111111
	STA Enemy_Life
	LDA #33
	STA COLUP0 ;color it
	LDA #53
	STA COLUP1 ;color it

	LDA #%00010000 ;set playfield to not reflected
	STA CTRLPF

	LDA #%00000000	; set to not move
	STA HMM1	; of HMM1 sets it to moving



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
;	INC Hero_YPosFromBot
	
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


	LDA #%01000000	;Left?
	BIT SWCHA 
	BNE SkipMoveLeft
;	DEC Hero_XPos

	;16 bit math, subtract both bytes
	;of the speed constant to
	;the 2 bytes of the position
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
;	INC Hero_XPos

	;16 bit math, add both bytes
	;of the speed constant to
	;the 2 bytes of the position
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

	;check Hero Sword Attack
	ldx INPT4
	bmi NoSwordAttack ;(button not pressed)
SwordAttack
	lda 	Hero_YPosFromBot
	sbc	#$4
	jmp DoneWithSwordAttack
NoSwordAttack
	lda #0
DoneWithSwordAttack
	sta	Hero_Sword_Pos

;Don't allow Hero past Position 140
	LDA #Far_Right_Hero
	CMP Hero_XPos
	BCS HeroRight
	STA Hero_XPos
HeroRight

;Don't allow Hero before Position 20
	LDA #Far_Left
	CMP Hero_XPos
	BCC HeroLeft
	STA Hero_XPos
HeroLeft

;Don't allow Hero above Position 140
	LDA #Far_Up_Hero
	CMP Hero_YPosFromBot
	BCS HeroUp
	STA Hero_YPosFromBot
HeroUp

;Don't allow Hero below Position 20
	LDA #Far_Down_Hero
	CMP Hero_YPosFromBot
	BCC HeroDown
	STA Hero_YPosFromBot
HeroDown

----------------------



;setup pic animations ----------------------------------------------
	INC ROLLING_COUNTER

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
	DEC E0_XPos
	DEC E1_XPos
	DEC E2_XPos
	DEC E3_XPos
	DEC E4_XPos
MOVESET1

	DEC E0_XPos
	

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

;setup pic animations ----------------------------------------------

	lda #<HeroGraphics0 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta Hero_Ptr		;(high byte already set)

	lda #>HeroGraphics0 ;high byte of graphic location
	sta Hero_Ptr+1	;store in high byte of graphic pointer
	
	
	;for skipDraw, E0_Y needs to be set (usually during VBLANK)
	;to Vertical Position (0 = top) + height of sprite - 1.
	;we're storing distance from bottom, not top, so we have
	;to start with the kernal height and YPosFromBot...
	lda #C_KERNAL_HEIGHT + #C_P0_HEIGHT - #1
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
	adc #C_P0_HEIGHT - #1 
	sta Hero_Ptr	;2 byte

;setup pic animations ----------------------------------------------

;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics0 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E0_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics0 ;high byte of graphic location
	sta E0_Ptr+1	;store in high byte of graphic pointer
	

	lda #C_KERNAL_HEIGHT + #C_P0_HEIGHT - #1
	sec
	sbc E0_YPosFromBot ;subtract integer byte of distance from bottom
	sta E0_Y

	lda E0_Ptr
	sec
	sbc E0_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT - #1
	sta E0_Ptr	;2 byte

;setup pic animations ----------------------------------------------


;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics1 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E1_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics1 ;high byte of graphic location
	sta E1_Ptr+1	;store in high byte of graphic pointer
	

	lda #Enemy_Row_E0 + #C_P0_HEIGHT - #1
	sec
	sbc E1_YPosFromBot ;subtract integer byte of distance from bottom
	sbc #3 ;subtract extra because not enough time to dec in kernal
	sta E1_Y

	lda E1_Ptr
	sec
	sbc E1_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT - #1
	sta E1_Ptr	;2 byte

;setup pic animations ----------------------------------------------




;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics0 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E2_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics0 ;high byte of graphic location
	sta E2_Ptr+1	;store in high byte of graphic pointer

	lda #Enemy_Row_E1 + #C_P0_HEIGHT - #1
	sec
	sbc E2_YPosFromBot ;subtract integer byte of distance from bottom
	sbc #3 ;subtract extra because not enough time to dec in kernal
	sta E2_Y


	lda E2_Ptr
	sec
	sbc E2_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT - #1
	sta E2_Ptr	;2 byte

;setup pic animations ----------------------------------------------

;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics3 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E3_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics3 ;high byte of graphic location
	sta E3_Ptr+1	;store in high byte of graphic pointer
	

	lda #Enemy_Row_E2 + #C_P0_HEIGHT - #1
	sec
	sbc E3_YPosFromBot ;subtract integer byte of distance from bottom
	sbc #3 ;subtract extra because not enough time to dec in kernal
	sta E3_Y


	lda E3_Ptr
	sec
	sbc E3_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT-#1 
	sta E3_Ptr	;2 byte

;setup pic animations ----------------------------------------------

;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics2 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta E4_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics2 ;high byte of graphic location
	sta E4_Ptr+1	;store in high byte of graphic pointer
	

	lda #Enemy_Row_E3 + #C_P0_HEIGHT - #1
	sec
	sbc E4_YPosFromBot ;subtract integer byte of distance from bottom
	sbc #3 ;subtract extra because not enough time to dec in kernal
	sta E4_Y


	lda E4_Ptr
	sec
	sbc E4_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT-#1 
	sta E4_Ptr	;2 byte

;setup pic animations ----------------------------------------------





;collisions
	LDA #%10000000
	BIT E0_Hit		
	BEQ NoCollisionP0	;skip if not hitting...	
	LDA #%11111110
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP0

;collisions
	LDA #%10000000
	BIT E1_Hit		
	BEQ NoCollisionP1	;skip if not hitting...	
	LDA #%11111101
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP1

;collisions
	LDA #%10000000
	BIT E2_Hit		
	BEQ NoCollisionP2	;skip if not hitting...	
	LDA #%11111011
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP2

;collisions
	LDA #%10000000
	BIT E3_Hit		
	BEQ NoCollisionP3	;skip if not hitting...	
	LDA #%11110111
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP3

;collisions
	LDA #%10000000
	BIT E4_Hit		
	BEQ NoCollisionP4	;skip if not hitting...	
	LDA #%11101111
	AND Enemy_Life
	STA Enemy_Life
NoCollisionP4







	lda #%00000001
	and Enemy_Life
	BNE alive1	
	LDA #200
	STA E0_Y
alive1


	lda #%00000010
	and Enemy_Life
	BNE alive2
	LDA #200
	STA E1_Y
alive2

	lda #%00000100
	and Enemy_Life
	BNE alive3
	LDA #200
	STA E2_Y
alive3

	lda #%00001000
	and Enemy_Life
	BNE alive4
	LDA #200
	STA E3_Y
alive4

	lda #%00010000
	and Enemy_Life
	BNE alive5
	LDA #200
	STA E4_Y
alive5


	LDA ROLLING_COUNTER
	ROR
	ROR
	ROR
	AND #%00000111
	ADC #1
	STA Pos
	
	LDA PFData0		; 4 cycles
	STA PF0_L1 ;B
	STA PF1_L1 ;B
	LDA PFData1		; 4 cycles
	STA PF2_L1 ;C
	LDA PFData2		; 4 cycles
	STA PF3_L1 ;d
	LDA PFData3		; 4 cycles
	STA PF3_L1 ;E
	LDA PFData4		; 4 cycles
	STA PF4_L1 ;F
	LDA PFData5		; 4 cycles
	STA PF5_L1 ;F


;	CLC
;	ADC #$4
;	TAY
	LDY Pos
ROTATE1
	ROL PF5_L1
	ROL PF4_L1
	ROL PF3_L1
	ROL PF2_L1
	ROL PF1_L1
	ROL PF0_L1
 	DEY
	BNE ROTATE1	


	ROR PF0_L1
	ROR PF0_L1
	ROR PF0_L1
	ROR PF0_L1
	ROR PF5_L1
	ROR PF5_L1
	ROR PF5_L1
	ROR PF5_L1



;NEED to reverse pf1

	ldx PF1_L1
	lda SwapTable,x
	STA PF1_L1

	ldx PF4_L1
	lda SwapTable,x
	STA PF4_L1

	LDX #1

	LDA PFData0 ,x		; 4 cycles
	STA PF0_L2 ;B
	STA PF1_L2 ;B
	LDA PFData1 ,x		; 4 cycles
	STA PF2_L2 ;C
	LDA PFData2,x		; 4 cycles
	STA PF3_L2 ;d
	LDA PFData3 ,x		; 4 cycles
	STA PF3_L2 ;E
	LDA PFData4 ,x		; 4 cycles
	STA PF4_L2 ;F
	LDA PFData5 ,x		; 4 cycles
	STA PF5_L2 ;F

;	CLC
;	ADC #$4
;	TAY
	LDY Pos
ROTATE2
	ROL PF5_L2
	ROL PF4_L2
	ROL PF3_L2
	ROL PF2_L2
	ROL PF1_L2
	ROL PF0_L2
 	DEY
	BNE ROTATE2	
	


;NEED to reverse pf1

	ldx PF1_L2
	lda SwapTable,x
	STA PF1_L2

	ldx PF4_L2
	lda SwapTable,x
	STA PF4_L2


	ROR PF0_L2
	ROR PF0_L2
	ROR PF0_L2
	ROR PF0_L2
	ROR PF5_L2
	ROR PF5_L2
	ROR PF5_L2
	ROR PF5_L2
	
	ldx #2


	LDA PFData0 ,x		; 4 cycles
	STA PF0_L3 ;B
	STA PF1_L3 ;B
	LDA PFData1 ,x		; 4 cycles
	STA PF2_L3 ;C
	LDA PFData2 ,x		; 4 cycles
	STA PF3_L3 ;d
	LDA PFData3 ,x		; 4 cycles
	STA PF3_L3 ;E
	LDA PFData4 ,x		; 4 cycles
	STA PF4_L3 ;F
	LDA PFData5 ,x		; 4 cycles
	STA PF5_L3 ;F


;	CLC
;	ADC #$4
;	TAY
	LDY Pos
ROTATE3
	ROL PF5_L3
	ROL PF4_L3
	ROL PF3_L3
	ROL PF2_L3
	ROL PF1_L3
	ROL PF0_L3
 	DEY
	BNE ROTATE3	
	


;NEED to reverse pf2

	ldx PF1_L3
	lda SwapTable,x
	STA PF1_L3

	ldx PF4_L3
	lda SwapTable,x
	STA PF4_L3

	ROR PF0_L3
	ROR PF0_L3
	ROR PF0_L3
	ROR PF0_L3
	ROR PF5_L3
	ROR PF5_L3
	ROR PF5_L3
	ROR PF5_L3


	ldx #3

	LDA PFData0,x		; 4 cycles
	STA PF0_L4 ;B
	STA PF1_L4 ;B
	LDA PFData1,x		; 4 cycles
	STA PF2_L4 ;C
	LDA PFData2,x		; 4 cycles
	STA PF3_L4 ;d
	LDA PFData3,x		; 4 cycles
	STA PF3_L4 ;E
	LDA PFData5,x		; 4 cycles
	STA PF5_L4 ;F


;	CLC
;	ADC #$4
;	TAY
	LDY Pos
ROTATE4
	ROL PF5_L4
	ROL PF4_L4
	ROL PF3_L4
	ROL PF2_L4
	ROL PF1_L4
	ROL PF0_L4
 	DEY
	BNE ROTATE4	
	


;NEED to reverse pf2

	ldx PF1_L4
	lda SwapTable,x
	STA PF1_L4

	ldx PF4_L4
	lda SwapTable,x
	STA PF4_L4

	ROR PF0_L4
	ROR PF0_L4
	ROR PF0_L4
	ROR PF0_L4
	ROR PF5_L4
	ROR PF5_L4
	ROR PF5_L4
	ROR PF5_L4


	
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
;	STA HMOVE 	
	
	STA VBLANK  	


;main scanline loop...



PreScanLoop



;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
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
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP	
	LDA PF3_L1		; 4 cycles 
	STA PF0			; 3 cycles
	LDA PF4_L1		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L1		; 4 cycles
	STA PF2			; 3 cycles
	
	STA WSYNC 						 ;3 cycles =74
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
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
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	LDA PF3_L2		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF4_L2		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L2		; 4 cycles
	STA PF2			; 3 cycles
	

	STA WSYNC 						 ;3 cycles =74
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK		;and store as the bgcolor ; 3 cycles
	LDA PF0_L3		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF1_L3		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF2_L3		; 4 cycles
	STA PF2			; 3 cycles
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	LDA PF3_L3		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF4_L3		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L3		; 4 cycles
	STA PF2			; 3 cycles
	

	STA WSYNC 						 ;3 cycles =74
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
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
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	LDA PF3_L4		; 4 cycles
	STA PF0			; 3 cycles
	LDA PF4_L4		; 4 cycles
	STA PF1			; 3 cycles
	LDA PF5_L4		; 4 cycles
	STA PF2			; 3 cycles
	

	STA WSYNC 						 ;3 cycles =74
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline

	LDA #30;

	STA COLUBK	;and store as the bgcolor
	LDY #186; 
	LDA #0
	STA PF0			; 3 cycles
	STA PF1			; 3 cycles
	STA PF2			; 3 cycles
	
	LDA #0
	STA RESMP0
	STA RESMP1
	STA GRP0
	STA GRP1
	LDA #%11111000	;The last 3 bits control number and size of players
			;the second 2 bits control missle size
	STA NUSIZ0
	STA NUSIZ1
	STA HMCLR

	DEC Hero_Y ;Hero always is decremented, because he travels the whole screen

	STA WSYNC 						 ;3 cycles
	
	

ScanLoopHero ;start of kernal +++++++++++++++++++++++ for Hero positioning
	lda Hero_XPos ;3


.Div15Hero   
	sbc #15      ; 2      
	bcs .Div15Hero   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP1 ;,x
	
	sta RESP1 ;,x	;the x must be a 0 for player 0  or 1 player 1
	sta RESM1 ;reset where the sword is
	sta HMM1


        DEY             ;count down number of scan lines          2 cycles = 
        STA WSYNC                                                ;3 cycles =
	STA HMOVE						 ;3
EndScanLoopHero ;end of kernal +++++++++++++++++ for Hero positioning

;this is to align sword
	DEC E0_Y
	DEC Hero_Y
        DEY             ;count down number of scan lines          2 cycles = 
	DEC E0_Y ;He is decremented because he's within his domain
	DEY
	STA HMCLR
	LDA MOV_STAT
	CMP #1
	BCS MLEFT
	STA HMCLR
	LDA #0
	JMP MRIGHT
MLEFT	LDA #%01110000 ;Atari only looks at 4 most significant bits in 2's compliment
MRIGHT	STA HMM1

	STA CXCLR	;reset the collision detection for next time
	STA WSYNC
;	STA HMOVE

ScanLoopa ;start of kernal +++++++++++++++++++++++ for player 0 positioning
	nop
	nop
	lda E1_XPos						 ;3
.Div15a   
	sbc #15      ; 2         
	bcs .Div15a   ; 3(2)

	tax		;2
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;	;3
	sta RESP0 ;	

	;Can't get LAX to work here
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoopa ;end of kernal +++++++++++++++++ for player 0 positioning
	Lda #0
	Ldx #0

;-------------------------Enemy number E0 start---------------------------
ScanLoop 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	stx	GRP0	

;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1
;sword php style 

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     E0_Y            ; 5 (DEC and CMP)
	bcs     .doDraw0        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw0:
	lda     (E0_Ptr),y      ; 5
;	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine
	tax 
;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero1        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero1:
	lda     (Hero_Ptr),y      ; 5

         DEY             ;count down number of scan lines          2 cycles = 
	

        STA WSYNC                                                ;3 cycles =
	CPY #Enemy_Row_E0-#1						 ;2
        BCS ScanLoop                                             ;2 cycles =
EndScanLoop ;end of kernal +++++++++++++++++ for player 0
;-------------------------Enemy number E0 End---------------------------
	
	
;-------------------------Enemy number E1 Start---------------------------
ScanLoop_E1_a
	sta	GRP1 ;3 
	lda	Graphics_Buffer ;3
	sta	GRP0
	


;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1


;sword php style



;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E1_a        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_a:
	lda     (Hero_Ptr),y      ; 5
	tax

        DEY             ;count down number of scan lines          2 cycles = 

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E1_b        ; 2/3 ; should be bcs
	lda     #0              ; 2
	sta	Graphics_Buffer ;for enemy 2 lines later
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2

	LDA CXM1P
	STA E0_Hit  ;this line must refer to previous enemy
	txa
;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
;sword php style

;sword php style
  	txs                    ; Set the top of the stack to ENAM1+1
;sword php style 
	STA CXCLR
        STA WSYNC                                                ;3 cycles =
EndScanLoop_E1_a
;------------------------------------------------
	
ScanLoop_E1_b
	sta.w	GRP1
	lda E1_XPos

.Div15_E1_a   
	sbc #15      ; 2         
	bcs .Div15_E1_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E1_b 
	DEY
	lda	Graphics_Buffer_2

ScanLoop_E1_c 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	
;sword php style
	cpy Hero_Sword_Pos
	php
  ldx #ENAM0+1		
  txs                    ; Set the top of the stack to ENAM1+1


;sword php style 

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     E1_Y            ; 5 (DEC and CMP)
	bcs     .doDraw_E1_b        ; 2/3 ; should be bcs
	lda     #0              ; 2

	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw_E1_b:
	lda     (E1_Ptr),y      ; 5
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E1_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_e:
	lda     (Hero_Ptr),y      ; 5


        DEY             ;count down number of scan lines          2 cycles
	STA HMCLR

	CPY #Enemy_Row_E1-#1
        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E1_c                                             ;2 cycles =
EndScanLoop_E1_c

;-------------------------Enemy number E1 End---------------------------


;-------------------------Enemy number E2 Start---------------------------
ScanLoop_E2_a
	sta	GRP1 ;3 
	lda	Graphics_Buffer ;3
	sta	GRP0
	


;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1


;sword php style



;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E2_a        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_a:
	lda     (Hero_Ptr),y      ; 5
	tax

        DEY             ;count down number of scan lines          2 cycles = 

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E2_b        ; 2/3 ; should be bcs
	lda     #0              ; 2
	sta	Graphics_Buffer ;for enemy 2 lines later
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2

	LDA CXM1P
	STA E1_Hit  ;this line must refer to previous enemy
	txa
;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
;sword php style

;sword php style
  	txs                    ; Set the top of the stack to ENAM1+1
;sword php style 
	STA CXCLR

        STA WSYNC                                                ;3 cycles =
EndScanLoop_E2_a
;------------------------------------------------
	
ScanLoop_E2_b
	sta.w	GRP1
	lda E2_XPos

.Div15_E2_a   
	sbc #15      ; 2         
	bcs .Div15_E2_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E2_b 
	DEY
	lda	Graphics_Buffer_2

ScanLoop_E2_c 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	
;sword php style
	cpy Hero_Sword_Pos
	php
  ldx #ENAM0+1		
  txs                    ; Set the top of the stack to ENAM1+1


;sword php style 

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     E2_Y            ; 5 (DEC and CMP)
	bcs     .doDraw_E2_b        ; 2/3 ; should be bcs
	lda     #0              ; 2

	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw_E2_b:
	lda     (E2_Ptr),y      ; 5
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E2_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_e:
	lda     (Hero_Ptr),y      ; 5


        DEY             ;count down number of scan lines          2 cycles
	STA HMCLR

	CPY #Enemy_Row_E2-#1
        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E2_c                                             ;2 cycles =
EndScanLoop_E2_c

;-------------------------Enemy number E2 End---------------------------
;-------------------------Enemy number E3 Start---------------------------
ScanLoop_E3_a
	sta	GRP1 ;3 
	lda	Graphics_Buffer ;3
	sta	GRP0
	


;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1


;sword php style



;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E3_a        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_a:
	lda     (Hero_Ptr),y      ; 5
	tax

        DEY             ;count down number of scan lines          2 cycles = 

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E3_b        ; 2/3 ; should be bcs
	lda     #0              ; 2
	sta	Graphics_Buffer ;for enemy 2 lines later
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2

	LDA CXM1P
	STA E2_Hit  ;this line must refer to previous enemy
	txa
;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
;sword php style

;sword php style
  	txs                    ; Set the top of the stack to ENAM1+1
;sword php style 
	STA CXCLR

        STA WSYNC                                                ;3 cycles =
EndScanLoop_E3_a
;------------------------------------------------
	
ScanLoop_E3_b
	sta.w	GRP1
	lda E3_XPos

.Div15_E3_a   
	sbc #15      ; 2         
	bcs .Div15_E3_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E3_b 
	DEY
	lda	Graphics_Buffer_2

ScanLoop_E3_c 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	
;sword php style
	cpy Hero_Sword_Pos
	php
  ldx #ENAM0+1		
  txs                    ; Set the top of the stack to ENAM1+1


;sword php style 

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     E3_Y            ; 5 (DEC and CMP)
	bcs     .doDraw_E3_b        ; 2/3 ; should be bcs
	lda     #0              ; 2

	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw_E3_b:
	lda     (E3_Ptr),y      ; 5
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E3_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_e:
	lda     (Hero_Ptr),y      ; 5


        DEY             ;count down number of scan lines          2 cycles
	STA HMCLR

	CPY #Enemy_Row_E3-#1
        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E3_c                                             ;2 cycles =
EndScanLoop_E3_c

;-------------------------Enemy number E3 End---------------------------

	
	
;-------------------------Enemy number E4 Start---------------------------
ScanLoop_E4_a
	sta	GRP1 ;3 
	lda	Graphics_Buffer ;3
	sta	GRP0
	


;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
  	txs                    ; Set the top of the stack to ENAM1+1


;sword php style



;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E4_a        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_a:
	lda     (Hero_Ptr),y      ; 5
	tax

        DEY             ;count down number of scan lines          2 cycles = 

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E4_b        ; 2/3 ; should be bcs
	lda     #0              ; 2
	sta	Graphics_Buffer ;for enemy 2 lines later
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2

	LDA CXM1P
	STA E3_Hit  ;this line must refer to previous enemy
	txa
;sword php style
	cpy Hero_Sword_Pos
	php
  	ldx #ENAM0+1		
;sword php style

;sword php style
  	txs                    ; Set the top of the stack to ENAM1+1
;sword php style 
	STA CXCLR

        STA WSYNC                                                ;3 cycles =
EndScanLoop_E4_a
;------------------------------------------------
	
ScanLoop_E4_b
	sta.w	GRP1
	lda E4_XPos

.Div15_E4_a   
	sbc #15      ; 2         
	bcs .Div15_E4_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E4_b 
	DEY
	lda	Graphics_Buffer_2

ScanLoop_E4_c 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	
;sword php style
	cpy Hero_Sword_Pos
	php
  ldx #ENAM0+1		
  txs                    ; Set the top of the stack to ENAM1+1


;sword php style 

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     E4_Y            ; 5 (DEC and CMP)
	bcs     .doDraw_E4_b        ; 2/3 ; should be bcs
	lda     #0              ; 2

	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw_E4_b:
	lda     (E4_Ptr),y      ; 5
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)                                                                                                                                                                                                                                                                               
	bcs     .doDrawHero_E4_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_e:
	lda     (Hero_Ptr),y      ; 5


        DEY             ;count down number of scan lines          2 cycles
	STA HMCLR

;	CPY #0 ;on last one must be  used
        STA WSYNC                                                ;3 cycles =
        BNE ScanLoop_E4_c  ;BNE instead of BCS on last one ;2 cycles =
EndScanLoop_E4_c
;-------------------------Enemy number E4 End---------------------------

;php sword stuff
  ldx SavedStackPointer
  txs
; php sword stuff



	STA WSYNC  	
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
	LDA #Enemy_Far_Left+1 ;2
	STA E0_XPos	;3
P0Right

	STA WSYNC
	DEY

;Don't allow P1 past Position 160
	LDA E1_XPos	;2
	CMP #Far_Right	;2
	BCC P1Right	;2(3)
	LDA #Enemy_Far_Left+1 ;2
	STA E1_XPos	;3
P1Right


	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA E2_XPos	;2
	CMP #Far_Right	;2
	BCC P2Right	;2(3)
	LDA #Enemy_Far_Left+1 ;2
	STA E2_XPos	;3
P2Right


	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA E3_XPos	;2
	CMP #Far_Right	;2
	BCC P3Right	;2(3)
	LDA #Enemy_Far_Left+1 ;2
	STA E3_XPos	;3
P3Right


	STA WSYNC
	DEY

;Don't allow P4 past Position 160
	LDA E4_XPos	;2
	CMP #Far_Right	;2
	BCC P4Right	;2(3)
	LDA #Enemy_Far_Left+1 ;2
	STA E4_XPos	;3
P4Right


	STA WSYNC
	DEY


;Don't allow P0 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E0_XPos	;2
	BCC P0left	;2(3)
	LDA #Far_Right-1 ;2
	STA E0_XPos	;3
P0left

	STA WSYNC
	DEY

;Don't allow P1 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E1_XPos	;2
	BCC P1left	;2(3)
	LDA #Far_Right-1	;2
	STA E1_XPos	;3
P1left

	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E2_XPos	;2
	BCC P2left	;2(3)
	LDA #Far_Right-1	;2
	STA E2_XPos	;3
P2left

	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E3_XPos	;2
	BCC P3left	;2(3)
	LDA #Far_Right-1	;2
	STA E3_XPos	;3
P3left
	STA WSYNC
	DEY

;Don't allow P4 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E4_XPos	;2
	BCC P4left	;2(3)
	LDA #Far_Right-1	;2
	STA E4_XPos	;3
P4left
	STA WSYNC
	DEY

OverScanWait
	STA WSYNC
	DEY
	BNE OverScanWait
	JMP  MainLoop   




  
;-----------------------------
; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write
;ROM is located from F000 to FFFF

            ORG $FC00
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


	org $FDC0


HeroGraphics0
	.byte #%00110110
	.byte #%00010100
	.byte #%00011000
	.byte #%00011000
	.byte #%01111110
	.byte #%10010000
	.byte #%00111000
	.byte #%00101000


	.byte #%01100110
	.byte #%00100100
	.byte #%00101000
	.byte #%00011000
	.byte #%01111110
	.byte #%10010000
	.byte #%00111000
	.byte #%00101000

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

	org $FEC0

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

	
PFData0 
        .byte #%11011111
        .byte #%10001111
        .byte #%10001111
        .byte #%00001111 

PFData1
        .byte #%11111111
        .byte #%11111111
        .byte #%00111011
        .byte #%00010001

PFData2
        .byte #%11100111
        .byte #%11100111
        .byte #%11000011
        .byte #%00000000

PFData3
        .byte #%11111111
        .byte #%10111011
        .byte #%10011001
        .byte #%00000000 

;PFData0
;	.byte #%00000001
;	.byte #%00000001
;	.byte #%00000001
;	.byte #%00000001
;
;PFData1
;	.byte #%00000011
;	.byte #%00000011
;	.byte #%00000011
;	.byte #%00000011
;
;PFData2
;	.byte #%00000111
;	.byte #%00000111
;	.byte #%00000111
;	.byte #%00000111
;	
;PFData3
;	.byte #%00001111
;	.byte #%00001111
;	.byte #%00001111
;	.byte #%00001111
;
;PFData4
;	.byte #%00011111
;	.byte #%00011111
;	.byte #%00011111
;	.byte #%00011111
;
;PFData5
;	.byte #%00111111
;	.byte #%00111111
;	.byte #%00111111
;	.byte #%00111111
;
;PFData6
;        .byte #%01111111
 ;       .byte #%01111111
;        .byte #%01111111
;        .byte #%01111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
;        
PFData4 
        .byte #%11111111
        .byte #%11101111
        .byte #%11000111
        .byte #%10000011

PFData5
        .byte #%11100111
        .byte #%11000011
        .byte #%10000001
        .byte #%00000000
        
PFData6
        .byte #%11111111
        .byte #%11111111
        .byte #%10111011
        .byte #%00010001
        
;SwapTable 
; .byte 0,$80,$40,$C0,$20 
;... etc. for values 5-256

SwapTable
 .byte #$0		;0  00000000
 .byte #$80		;1  00000001
 .byte #$40		;2  00000010
 .byte #$C0		;3  00000011
 .byte #$20		;4  00000100
 .byte #$A0		;5  00000101
 .byte #$60		;6  00000110
 .byte #$E0		;7  00000111
 .byte #$10		;8  00001000
 .byte #$90		;9  00001001
 .byte #$50		;A  00001010
 .byte #$D0		;B  00001011
 .byte #$30		;C  00001100
 .byte #$B0		;D  00001101
 .byte #$70		;E  00001110
 .byte #$F0		;F  00001111
 .byte #$8		;10 00010000
 .byte #$88		;11 00010001
 .byte #$48		;12 00010010
 .byte #$C8		;13 00010011
 .byte #$28		;14 00010100
 .byte #$A8		;15 00010101
 .byte #$68		;16 00010110
 .byte #$E8		;17 00010111
 .byte #$18		;18 00011000
 .byte #$98		;19 00011001
 .byte #$58		;1A 00011010
 .byte #$D8		;1B 00011011
 .byte #$38		;1C 00011100
 .byte #$B8		;1D 00011101
 .byte #$78		;1E 00011110
 .byte #$F8		;1F 00011111
 .byte #$4		;20 00100000
 .byte #$84		;21 00100001
 .byte #$44		;22 00100010
 .byte #$C4		;23 00100011
 .byte #$24		;24 00100100
 .byte #$A4		;25 00100101
 .byte #$64		;26 00100110
 .byte #$E4		;27 00100111
 .byte #$14		;28 00101000 
 .byte #$94		;29 00101001
 .byte #$54		;2A 00101010
 .byte #$D4 		;2B 00101011
 .byte #$34		;2C 00101100
 .byte #$B4		;2D 00101101
 .byte #$74		;2E 00101110
 .byte #$F4		;2F 00101111
 .byte #$C 		;30 00110000
 .byte #$8C		;31 00110001
 .byte #$4C		;32 00110010	
 .byte #$CC		;33 00110011
 .byte #$2C		;34 00110100
 .byte #$AC		;35 00110101
 .byte #$6C		;36 00110110
 .byte #$EC		;37 00110111
 .byte #$1E		;38 00111000
 .byte #$9E		;39 00111001
 .byte #$5E		;3A 00111010
 .byte #$DE		;3B 00111011
 .byte #$3E		;3C 00111100
 .byte #$BE		;3D 00111101
 .byte #$7E		;3E 00111110
 .byte #$FE		;3F 00111111
 .byte #$2 		;40 01000000
 .byte #$82 		;41 01000001
 .byte #$42 		;42 01000010
 .byte #$C2		;43 01000011
 .byte #$22		;44 01000100
 .byte #$A2		;45 01000101
 .byte #$62		;46 01000110
 .byte #$E2		;47 01000111
 .byte #$12		;48 01001000
 .byte #$92		;49 01001001
 .byte #$52		;4A 01001010
 .byte #$D2		;4B 01001011
 .byte #$32		;4C 01001100
 .byte #$B2		;4D 01001101
 .byte #$72		;4E 01001110
 .byte #$F2		;4F 01001111
 .byte #$A 		;50 01010000
 .byte #$8A		;51 01010001
 .byte #$4A		;52 01010010
 .byte #$CA		;53 01010011
 .byte #$2A		;54 01010100
 .byte #$AA		;55 01010101
 .byte #$6A		;56 01010110
 .byte #$EA		;57 01010111
 .byte #$1A		;58 01011000
 .byte #$9A		;59 01011001
 .byte #$5A		;5A 01011010
 .byte #$DA		;5B 01011011
 .byte #$3A		;5C 01011100
 .byte #$BA		;5D 01011101
 .byte #$7A		;5E 01011110
 .byte #$FA		;5F 01011111
 .byte #$6 		;60 01100000
 .byte #$86		;61 01100001
 .byte #$46		;62 01100010
 .byte #$C6		;63 01100011
 .byte #$26		;64 01100100
 .byte #$A6		;65 01100101
 .byte #$66		;66 01100110
 .byte #$E6		;67 01100111
 .byte #$16		;68 01101000
 .byte #$96		;69 01101001
 .byte #$56		;6A 01101010
 .byte #$D6		;6B 01101011
 .byte #$36		;6C 01101100
 .byte #$B6		;6D 01101101
 .byte #$76		;6E 01101110
 .byte #$F6		;6F 01101111
 .byte #$E		;70 01110000
 .byte #$8E		;71 01110001
 .byte #$4E		;72 01110010
 .byte #$CE		;73 01110011
 .byte #$2E		;74 01110100
 .byte #$AE		;75 01110101
 .byte #$6E		;76 01110110
 .byte #$EE		;77 01110111
 .byte #$1E		;78 01111000
 .byte #$9E		;79 01111001
 .byte #$5E		;7A 01111010
 .byte #$DE		;7B 01111011
 .byte #$3E		;7C 01111100
 .byte #$BE		;7D 01111101
 .byte #$7E		;7E 01111110
 .byte #$FE		;7F 01111111
 .byte #$1 		;80 10000000
 .byte #$81		;81 10000001
 .byte #$41		;82 10000010
 .byte #$C1    		;83  10000011
 .byte #$21     	;84  10000100
 .byte #$A1     	;85  10000101
 .byte #$61     	;86  10000110
 .byte #$E1     	;87  10000111
 .byte #$11     	;88  10001000
 .byte #$91     	;89  10001001
 .byte #$51     	;8A  10001010
 .byte #$D1     	;8B  10001011
 .byte #$31     	;8C  10001100
 .byte #$B1     	;8D  10001101
 .byte #$71     	;8E  10001110
 .byte #$F1     	;8F  10001111
 .byte #$9      	;90  10010000
 .byte #$89     	;91  10010001
 .byte #$49     	;92  10010010
 .byte #$C9     	;93  10010011
 .byte #$29     	;94  10010100
 .byte #$A9     	;95  10010101
 .byte #$69     	;96  10010110
 .byte #$E9     	;97  10010111
 .byte #$19     	;98  10011000
 .byte #$99     	;99  10011001
 .byte #$59     	;9A  10011010
 .byte #$D9     	;9B  10011011
 .byte #$39     	;9C  10011100
 .byte #$B9     	;9D  10011101
 .byte #$79     	;9E  10011110
 .byte #$F9     	;9F  10011111
 .byte #$5      	;A0  10100000
 .byte #$85     	;A1  10100001
 .byte #$45     	;A2  10100010
 .byte #$C5      	;A3  10100011
 .byte #$25      	;A4  10100100
 .byte #$A5      	;A5  10100101
 .byte #$65      	;A6  10100110
 .byte #$E5      	;A7  10100111
 .byte #$15      	;A8  10101000
 .byte #$95      	;A9  10101001
 .byte #$55      	;AA  10101010
 .byte #$D5      	;AB  10101011
 .byte #$35      	;AC  10101100
 .byte #$B5      	;AD  10101101
 .byte #$75      	;AE  10101110
 .byte #$F5      	;AF  10101111
 .byte #$D       	;B0  10110000
 .byte #$8D      	;B1  10110001
 .byte #$4D      	;B2  10110010
 .byte #$CD      	;B3  10110011
 .byte #$2D      	;B4  10110100
 .byte #$AD      	;B5  10110101
 .byte #$6D      	;B6  10110110
 .byte #$ED      	;B7  10110111
 .byte #$1F      	;B8  10111000
 .byte #$9F      	;B9  10111001
 .byte #$5F      	;BA  10111010
 .byte #$DF      	;BB  10111011
 .byte #$3F      	;BC  10111100
 .byte #$BF      	;BD  10111101
 .byte #$7F      	;BE  10111110
 .byte #$FF      	;BF  10111111
 .byte #$3       	;C0  11000000
 .byte #$83      	;C1  11000001
 .byte #$43      	;C2  11000010
 .byte #$C3      	;C3  11000011
 .byte #$23      	;C4  11000100
 .byte #$A3      	;C5  11000101
 .byte #$63      	;C6  11000110
 .byte #$E3      	;C7  11000111
 .byte #$13      	;C8  11001000
 .byte #$93      	;C9  11001001
 .byte #$53      	;CA  11001010
 .byte #$D3      	;CB  11001011
 .byte #$33      	;CC  11001100
 .byte #$B3      	;CD  11001101
 .byte #$73      	;CE  11001110
 .byte #$F3      	;CF  11001111
 .byte #$B       	;D0  11010000
 .byte #$8B      	;D1  11010001
 .byte #$4B      	;D2  11010010
 .byte #$CB      	;D3  11010011
 .byte #$2B      	;D4  11010100
 .byte #$AB      	;D5  11010101
 .byte #$6B      	;D6  11010110
 .byte #$EB      	;D7  11010111
 .byte #$1B      	;D8  11011000
 .byte #$9B      	;D9  11011001
 .byte #$5B      	;DA  11011010
 .byte #$DB      	;DB  11011011
 .byte #$3B      	;DC  11011100
 .byte #$BB      	;DD  11011101
 .byte #$7B      	;DE  11011110
 .byte #$FB      	;DF  11011111
 .byte #$7       	;E0  11100000
 .byte #$87      	;E1  11100001
 .byte #$47      	;E2  11100010
 .byte #$C7      	;E3  11100011
 .byte #$27      	;E4  11100100
 .byte #$A7      	;E5  11100101
 .byte #$67      	;E6  11100110
 .byte #$E7      	;E7  11100111
 .byte #$17      	;E8  11101000
 .byte #$97      	;E9  11101001
 .byte #$57      	;EA  11101010
 .byte #$D7      	;EB  11101011
 .byte #$37      	;EC  11101100
 .byte #$B7      	;ED  11101101
 .byte #$77      	;EE  11101110
 .byte #$F7      	;EF  11101111
 .byte #$F       	;F0  11110000
 .byte #$8F      	;F1  11110001
 .byte #$4F      	;F2  11110010
 .byte #$CF      	;F3  11110011
 .byte #$2F      	;F4  11110100
 .byte #$AF      	;F5  11110101
 .byte #$6F      	;F6  11110110
 .byte #$EF      	;F7  11110111
 .byte #$1F      	;F8  11111000
 .byte #$9F      	;F9  11111001
 .byte #$5F      	;FA  11111010
 .byte #$DF      	;FB  11111011
 .byte #$3F      	;FC  11111100
 .byte #$BF      	;FD  11111101
 .byte #$7F      	;FE  11111110
 .byte #$FF      	;FF  11111111
	
PFCOLOR
	.byte #$29
	.byte #$5D
	.byte #$49
	.byte #$39


	org $FFFC
	.word Start
	.word Start
