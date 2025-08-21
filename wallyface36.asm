;--------------------------------------------------------------
;fixed top scroll messing up screen, lda and and where swapped
;bugs swords are different sizes, if first monster killed far left, he returns
;should make this 3d by adjusting the colors, change bg color to to bottom.
;--------------------------------------------------------------

	processor 6502
	include vcs.h
	include macro.h

; Constants ------
C_P0_HEIGHT 		= 8	;height of sprite
C_P1_HEIGHT 		= 8	;height of sprite
C_KERNAL_HEIGHT 	= 186	;height of kernal/actually the largest line on the screen
Far_Left		= 15	;apparently putting # here does nothing
Far_Right		= 147
Far_Right_Hero		= 147
Far_Up_Hero		= 182
Far_Down_Hero		= 10
Enemy_Far_Left		= 0
Enemy_Row_0		= 185
Enemy_Row_E0		= 150
Enemy_Row_E1		= 115
Enemy_Row_E2		= 70
Enemy_Row_E3		= 35
Enemy_Row_E4		= 0
HERO_SPEED		= 1
Screen_Rate		= 20	;How fast screen is scrolling in X-Axis

;Variables ------

	seg.u RAM
	org $0080
PICS 			ds 1
ROLLING_COUNTER 	ds 1


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

Graphics_Buffer		ds 1	;buffer for graphics
Graphics_Buffer_2	ds 1	;buffer for graphics

Hero_Attack		ds 1	;where and type of attack
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
	STA Hero_Attack	;set Hero to not be attacking
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

	LDA #$10

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
	adc #<HERO_SPEED
	sta Hero_YPosFromBot
	lda Hero_YPosFromBot+1
	adc #>HERO_SPEED
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
	sbc #<HERO_SPEED
	sta Hero_YPosFromBot
	lda Hero_YPosFromBot+1
	sbc #>HERO_SPEED
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
	sbc #<HERO_SPEED
	sta Hero_XPos
	lda Hero_XPos+1
	sbc #>HERO_SPEED
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
	adc #<HERO_SPEED
	sta Hero_XPos
	lda Hero_XPos+1
	adc #>HERO_SPEED
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
	lda #2
	jmp DoneWithSwordAttack
NoSwordAttack
	lda #0
DoneWithSwordAttack
	sta Hero_Attack


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
	;DEC E0_XPos
	INC E1_XPos
	;DEC E2_XPos
	DEC E3_XPos
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






	LDA PFData0		; 4 cycles
	STA PF0_L1
	LDA PFData1		; 4 cycles
	STA PF1_L1
	LDA PFData2		; 4 cycles
	STA PF2_L1
	LDA PFData3		; 4 cycles
	STA PF3_L1
	LDA PFData4		; 4 cycles
	STA PF4_L1
	LDA PFData5		; 4 cycles
	STA PF5_L1
	LDA PFData6		; 4 cycles
	STA PF_TEMP
	LDA Screen_Location+1
	AND #%00000011
	CLC
	ADC #1
	TAY
ROTATE1	ROL PF_TEMP
	ROR PF5_L1
	ROL PF4_L1
	ROL PF3_L1
	ROR PF2_L1
	ROL PF1_L1
	ROL PF0_L1
	DEY
	BNE ROTATE1
	
	
	
	LDA PFData0+1		; 4 cycles
	STA PF0_L2
	LDA PFData1+1		; 4 cycles
	STA PF1_L2
	LDA PFData2+1		; 4 cycles
	STA PF2_L2
	LDA PFData3+1		; 4 cycles
	STA PF3_L2
	LDA PFData4+1		; 4 cycles
	STA PF4_L2
	LDA PFData5+1		; 4 cycles
	STA PF5_L2
	LDA PFData6+1		; 4 cycles
	STA PF_TEMP
	LDA Screen_Location+1
	AND #%00000011
	CLC
	ADC #1
	TAY
ROTATE2	ROL PF_TEMP
	ROR PF5_L2
	ROL PF4_L2
	ROL PF3_L2
	ROR PF2_L2
	ROL PF1_L2
	ROL PF0_L2
	DEY
	BNE ROTATE2	
	
	
	LDA PFData0+2		; 4 cycles
	STA PF0_L3
	LDA PFData1+2		; 4 cycles
	STA PF1_L3
	LDA PFData2+2		; 4 cycles
	STA PF2_L3
	LDA PFData3+2		; 4 cycles
	STA PF3_L3
	LDA PFData4+2		; 4 cycles
	STA PF4_L3
	LDA PFData5+2		; 4 cycles
	STA PF5_L3
	LDA PFData6+2		; 4 cycles
	STA PF_TEMP
	LDA Screen_Location+1
	AND #%00000011
	CLC
	ADC #1	
	TAY
ROTATE3	ROL PF_TEMP
	ROR PF5_L3
	ROL PF4_L3
	ROL PF3_L3
	ROR PF2_L3
	ROL PF1_L3
	ROL PF0_L3
	DEY
	BNE ROTATE3	
	

	
	LDA PFData0+3		; 4 cycles
	STA PF0_L4
	LDA PFData1+3		; 4 cycles
	STA PF1_L4
	LDA PFData2+3		; 4 cycles
	STA PF2_L4
	LDA PFData3+3		; 4 cycles
	STA PF3_L4
	LDA PFData4+3		; 4 cycles
	STA PF4_L4
	LDA PFData5+3		; 4 cycles
	STA PF5_L4
	LDA PFData6+3		; 4 cycles
	STA PF_TEMP
	LDA Screen_Location+1
	AND #%00000011
	CLC
	ADC #1
	TAY
ROTATE4	ROL PF_TEMP
	ROR PF5_L4
	ROL PF4_L4
	ROL PF3_L4
	ROR PF2_L4
	ROL PF1_L4
	ROL PF0_L4
	DEY
	BNE ROTATE4		
	

	
	LDY #4 ; was 191 	
;----------lots of time
WaitForVblankEnd
	LDA INTIM	
	BNE WaitForVblankEnd	


	STA WSYNC	
	STA HMOVE 	
	
	STA VBLANK  	


;main scanline loop...



PreScanLoop



;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK		;and store as the bgcolor ; 3 cycles	LDA PF0_L1		; 4 cycles
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
;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
;	LDA PFCOLOR-1,Y		; 4 cycles
;	STA COLUBK		;and store as the bgcolor ; 3 cycles
;	LDA PF0_L4		; 4 cycles
;	STA PF0			; 3 cycles
;	LDA PF1_L4		; 4 cycles
;	STA PF1			; 3 cycles
;	LDA PF2_L4		; 4 cycles
;	STA PF2			; 3 cycles
;	NOP
;	NOP
;	NOP
;	NOP
;	NOP
;	NOP
;	NOP
;	NOP
;	LDA PF3_L4		; 4 cycles
;	STA PF0			; 3 cycles
;	LDA PF4_L4		; 4 cycles
;	STA PF1			; 3 cycles
;	LDA PF5_L4		; 4 cycles
;	STA PF2			; 3 cycles
;	
;
;	STA WSYNC 						 ;3 cycles =74
;EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline
;ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK		;and store as the bgcolor ; 3 cycles
	LDA #%00000000		; 3 cycles
	STA PF0			; 3 cycles
	LDA #%01010101		; 3 cycles
	STA PF1			; 3 cycles
	LDA #%01010101		; 3 cycles
	STA PF2			; 3 cycles
	STA RESM1
	NOP
	STA RESM0
	NOP
	STA RESM0
	NOP
	NOP
	STA RESM1
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

	LDA #%11111000	;The last 3 bits control number and size of players
			;the second 2 bits control missle size
	STA NUSIZ0
	STA NUSIZ1

	DEC Hero_Y ;Hero always is decremented, because he travels the whole screen
	lda Hero_XPos ;3
        sec	     ; 2 set carry

	STA WSYNC 						 ;3 cycles
	
	

ScanLoopHero ;start of kernal +++++++++++++++++++++++ for Hero positioning

.Div15Hero   
	sbc #15      ; 2      
	bcs .Div15Hero   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP1 ;,x
	sta HMM1
	
	sta RESP1 ;,x	;the x must be a 0 for player 0  or 1 player 1
	sta RESM1 ;reset where the sword is


        DEY             ;count down number of scan lines          2 cycles = 
        STA WSYNC                                                ;3 cycles =
	STA HMOVE						 ;3
EndScanLoopHero ;end of kernal +++++++++++++++++ for Hero positioning

;this is to align sword
	DEC E0_Y
	DEC Hero_Y
	DEY
	NOP
	NOP
	LDA MOV_STAT
	CMP #1
	BCS MLEFT
	LDA #0
	JMP MRIGHT
MLEFT	LDA #%01100000
MRIGHT	NOP
	NOP
	NOP
	STA HMM1
	LDA #0
	STA HMP1
	lda E0_XPos						 ;3
        sec	     ; 2 set carry
	STA WSYNC
	STA HMOVE
;this is to align sword

ScanLoopa ;start of kernal +++++++++++++++++++++++ for player 0 positioning
	DEC E0_Y ;He is decremented because he's within his domain
.Div15a   
	sbc #15      ; 2         
	bcs .Div15a   ; 3(2)

	tax		;2
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;	;3

	sta RESP0 ;	;the x must be a 0 for player 0  or 1 player 1


        DEY             ;count down number of scan lines          2 cycles = 
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoopa ;end of kernal +++++++++++++++++ for player 0 positioning

;-------------------------Enemy number E0 start---------------------------
	STA CXCLR	;reset the collision detection for next time
	lda	Graphics_Buffer_2
ScanLoop 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     E0_Y            ; 5 (DEC and CMP)
	bcs     .doDraw0        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw0:
	lda     (E0_Ptr),y      ; 5
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine


; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 
	cmp     Hero_Y            ; 3 
	beq     .doDrawHero3d129        ; 
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3d129:
	LDA Hero_Attack
	STA ENAM1	;SWORD STUFF   
; draw Hero Sword:

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
	
	STA HMCLR

        STA WSYNC                                                ;3 cycles =
	CPY #Enemy_Row_E0-#1						 ;2
        BCS ScanLoop                                             ;2 cycles =
EndScanLoop ;end of kernal +++++++++++++++++ for player 0
;-------------------------Enemy number E0 End---------------------------
	
	
;-------------------------Enemy number E1 Start---------------------------
ScanLoop_E1_a
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	


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
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2

; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 -5 to position near hand
	cmp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E1_c        ; 2/3 ; should be bcs was beq
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_c:
	LDA Hero_Attack
	STA ENAM1	;SWORD STUFF   

	lda E1_XPos

        sec	     ; 2 set carry


        STA WSYNC                                                ;3 cycles =
EndScanLoop_E1_a
;------------------------------------------------
	
ScanLoop_E1_b
	STX GRP1

.Div15_E1_a   
	sbc #15      ; 2         
	bcs .Div15_E1_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	LDA CXM1P
	STA E0_Hit  ;this line must refer to previous enemy
	
	DEY
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E1_b 
	STA CXCLR	;reset the collision detection for next time
	lda	Graphics_Buffer_2

ScanLoop_E1_c 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	
; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 
	cmp     Hero_Y            ; 3 
	beq     .doDrawHero_E1_d        ; 
	lda     #0              ; 2

	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E1_d:
; draw Hero Sword:
	LDA Hero_Attack
	STA ENAM1	;SWORD STUFF   

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
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E1_c                                             ;2 cycles =
EndScanLoop_E1_c

;-------------------------Enemy number E1 End---------------------------

;-------------------------Enemy number E2 Start---------------------------
ScanLoop_E2_a
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	


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
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2

; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 -5 to position near hand
	cmp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E2_c        ; 2/3 ; should be bcs was beq
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_c:
	LDA Hero_Attack
	STA ENAM1	;SWORD STUFF   

	lda E2_XPos

        sec	     ; 2 set carry


        STA WSYNC                                                ;3 cycles =
EndScanLoop_E2_a
;------------------------------------------------
	
ScanLoop_E2_b
	STX GRP1

.Div15_E2_a   
	sbc #15      ; 2         
	bcs .Div15_E2_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	LDA CXM1P
	STA E1_Hit ;this line must refer to previous enemy
	
	DEY
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E2_b 
	STA CXCLR	;reset the collision detection for next time
	lda	Graphics_Buffer_2

ScanLoop_E2_c 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	
; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 
	cmp     Hero_Y            ; 3 
	beq     .doDrawHero_E2_d        ; 
	lda     #0              ; 2

	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E2_d:
; draw Hero Sword:
	LDA Hero_Attack
	STA ENAM1	;SWORD STUFF   

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
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E2_c                                             ;2 cycles =
EndScanLoop_E2_c

;-------------------------Enemy number E2 End---------------------------

;-------------------------Enemy number E3 Start---------------------------
ScanLoop_E3_a
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	


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
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2

; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 -5 to position near hand
	cmp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E3_c        ; 2/3 ; should be bcs was beq
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_c:
	LDA Hero_Attack
	STA ENAM1	;SWORD STUFF   

	lda E3_XPos

        sec	     ; 2 set carry


        STA WSYNC                                                ;3 cycles =
EndScanLoop_E3_a
;------------------------------------------------
	
ScanLoop_E3_b
	STX GRP1

.Div15_E3_a   
	sbc #15      ; 2         
	bcs .Div15_E3_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	LDA CXM1P
	STA E2_Hit ;this line must refer to previous enemy
	
	DEY
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E3_b 
	STA CXCLR	;reset the collision detection for next time
	lda	Graphics_Buffer_2

ScanLoop_E3_c 
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	
; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 
	cmp     Hero_Y            ; 3 
	beq     .doDrawHero_E3_d        ; 
	lda     #0              ; 2

	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E3_d:
; draw Hero Sword:
	LDA Hero_Attack
	STA ENAM1	;SWORD STUFF   

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
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
        STA WSYNC                                                ;3 cycles =
        BCS ScanLoop_E3_c                                             ;2 cycles =
EndScanLoop_E3_c

;-------------------------Enemy number E3 End---------------------------

;-------------------------Enemy number E4 Start---------------------------
ScanLoop_E4_a ;changed lines above in enemy 1 put sta grp1 there
	lda	Graphics_Buffer ;3
	sta	GRP0	
	


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


; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 -5 to position near hand
	cmp     Hero_Y            ; 5 (DEC and CMP)
	beq     .doDrawHero_E4_c        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_c:
	LDA Hero_Attack
	STA ENAM1 ;SWORD STUFF   

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E4_b        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_b:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2
	lda E4_XPos 


	sec 
        STA WSYNC                                                ;3 cycles =
EndScanLoop_E4_a
;------------------------------------------------
	
ScanLoop_E4_b
	stx	GRP1
.Div15_E4_a   
	sbc #15      ; 2         
	bcs .Div15_E4_a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x
	STA RESP0
	LDA CXM1P
	STA E3_Hit ;this line must refer to previous enemy
	STA CXCLR	;reset the collision detection for next time
	
	DEY
	lda	Graphics_Buffer_2
	sta	GRP1
	ldx 	Graphics_Buffer
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop_E4_b 

ScanLoop_E4_c 
	
	STx ENAM1	;SWORD STUFF ;3  
	sta GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles

;skipDraw
; draw enemy sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     E4_Y            ; 5 (DEC and CMP)
	bcs     .doDraw_E4_b        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw_E4_b:
	lda     (E4_Ptr),y      ; 5
	sta 	GRP0 

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero_E4_e       ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_e:
	lda     (Hero_Ptr),y      ; 5

	sta	Graphics_Buffer

; draw Hero Sword:
	lda     #C_P0_HEIGHT-4     ; 2 
	cmp     Hero_Y            ; 3 
	beq     .doDrawHero_E4_d        ; 
	ldx     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero_E4_d:
	LDx Hero_Attack
; draw Hero Sword:
	lda	Graphics_Buffer

        DEY             ;count down number of scan lines          2 cycles
        STA WSYNC                                                ;3 cycles =
        BNE ScanLoop_E4_c                                             ;2 cycles =
EndScanLoop_E4_c

;-------------------------Enemy number E4 End---------------------------


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
	LDA #Far_Right	;2
	CMP E0_XPos	;2
	BCS P0Right	;2(3)
	LDA #Far_Left+1 ;2
	STA E0_XPos	;3
P0Right

	STA WSYNC
	DEY

;Don't allow P0 past Position 160
	LDA #Far_Right	;2
	CMP E1_XPos	;2
	BCS P1Right	;2(3)
	LDA #Far_Left+1 ;2
	STA E1_XPos	;3
P1Right


	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA #Far_Right	;2
	CMP E2_XPos	;2
	BCS P2Right	;2(3)
	LDA #Far_Left+1 ;2
	STA E2_XPos	;3
P2Right


	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA ##Far_Right	;2
	CMP E3_XPos	;2
	BCS P3Right	;2(3)
	LDA #Far_Left+1 ;2
	STA E3_XPos	;3
P3Right


	STA WSYNC
	DEY

;Don't allow P4 past Position 160
	LDA ##Far_Right	;2
	CMP E4_XPos	;2
	BCS P4Right	;2(3)
	LDA #Far_Left+1 ;2
	STA E4_XPos	;3
P4Right


	STA WSYNC
	DEY


;Don't allow P0 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E0_XPos	;2
	BCC P0left	;2(3)
	LDA #Far_Right ;2
	STA E0_XPos	;3
P0left

	STA WSYNC
	DEY

;Don't allow P1 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E1_XPos	;2
	BCC P1left	;2(3)
	LDA #Far_Right	;2
	STA E1_XPos	;3
P1left

	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E2_XPos	;2
	BCC P2left	;2(3)
	LDA #Far_Right	;2
	STA E2_XPos	;3
P2left

	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E3_XPos	;2
	BCC P3left	;2(3)
	LDA #Far_Right	;2
	STA E3_XPos	;3
P3left
	STA WSYNC
	DEY

;Don't allow P4 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP E4_XPos	;2
	BCC P4left	;2(3)
	LDA #Far_Right	;2
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

	
;PFData0 
;        .byte #%11011111
;        .byte #%10001111
;        .byte #%10001111
;        .byte #%00001111 

;PFData1
;        .byte #%11111111
;        .byte #%11111111
;        .byte #%00111011
;        .byte #%00010001

;PFData2
;        .byte #%11100111
;        .byte #%11100111
;        .byte #%11000011
;        .byte #%00000000

;PFData3
;        .byte #%11111111
;        .byte #%10111011
;        .byte #%10011001
;        .byte #%00000000 

PFData0
	.byte #%00000001
	.byte #%00000001
	.byte #%00000001
	.byte #%00000001

PFData1
	.byte #%00000011
	.byte #%00000011
	.byte #%00000011
	.byte #%00000011

PFData2
	.byte #%00000111
	.byte #%00000111
	.byte #%00000111
	.byte #%00000111
	
PFData3
	.byte #%00001111
	.byte #%00001111
	.byte #%00001111
	.byte #%00001111

PFData4
	.byte #%00011111
	.byte #%00011111
	.byte #%00011111
	.byte #%00011111

PFData5
	.byte #%00111111
	.byte #%00111111
	.byte #%00111111
	.byte #%00111111

PFData6
        .byte #%01111111
        .byte #%01111111
        .byte #%01111111
        .byte #%01111111
        
;PFData4 
;        .byte #%11111111
;        .byte #%11101111
;        .byte #%11000111
;        .byte #%10000011

;PFData5
;        .byte #%11100111
;        .byte #%11000011
;        .byte #%10000001
;        .byte #%00000000
        
;PFData6
;        .byte #%11111111
;        .byte #%11111111
;        .byte #%10111011
;        .byte #%00010001
        
PFCOLOR
	.byte #$29
	.byte #$5D
	.byte #$49
	.byte #$39


	org $FFFC
	.word Start
	.word Start
