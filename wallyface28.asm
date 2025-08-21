;--------------------------------------------------------------
;Sword is drawn taking advantage of the pixel in the sprite
;--------------------------------------------------------------

	processor 6502
	include vcs.h
	include macro.h

; Constants ------
C_P0_HEIGHT 		= 8	;height of sprite
C_P1_HEIGHT 		= 8	;height of sprite
C_KERNAL_HEIGHT 	= 186	;height of kernal/actually the largest line on the screen
Far_Left		= 1	;apparently putting # here does nothing
Far_Right		= 115
Far_Right_Hero		= 62
Enemy_Far_Left		= 1
Enemy_Row_0		= 186
Enemy_Row_1		= 150
Enemy_Row_2		= 100
Enemy_Row_3		= 50
;Variables ------

	seg.u RAM
	org $0080
PICS 			ds 1
ROLLING_COUNTER 	ds 1


P0_YPosFromBot 		ds 1	;Vertical position
P0_XPos 		ds 1	;horizontal position
P0_Y 			ds 1	;needed for skipdraw
P0_Ptr 			ds 2	;ptr to current graphic

P1_YPosFromBot 		ds 1	;Vertical position
P1_XPos 		ds 1	;horizontal position
P1_Y 			ds 1	;needed for skipdraw
P1_Ptr 			ds 2	;ptr to current graphic

P2_YPosFromBot 		ds 1	;Vertical position
P2_XPos 		ds 1	;horizontal position
P2_Y 			ds 1	;needed for skipdraw
P2_Ptr 			ds 2	;ptr to current graphic

P3_YPosFromBot 		ds 1	;Vertical position
P3_XPos 		ds 1	;horizontal position
P3_Y 			ds 1	;needed for skipdraw
P3_Ptr 			ds 2	;ptr to current graphic

Hero_YPosFromBot 	ds 1	;Vertical position
Hero_XPos 		ds 1	;horizontal position
Hero_Y 			ds 1	;needed for skipdraw
Hero_Ptr 		ds 2	;ptr to current graphic
Hero_Sword_Pos		ds 1	;Vertical position of Sword

Graphics_Buffer		ds 1	;buffer for graphics
Graphics_Buffer_2	ds 1	;buffer for graphic

SavedStackPointer	ds 1	;buffer for sword


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
	LDA #172
	STA P0_YPosFromBot
	LDA #50
	STA P0_XPos
	LDA #122 ;was 172
	STA P1_YPosFromBot
	LDA #50
	STA P1_XPos
	LDA #72 ;was 172
	STA P2_YPosFromBot
	LDA #50
	STA P2_XPos
	LDA #22 ;was 172
	STA P3_YPosFromBot
	LDA #50
	STA P3_XPos
	
	LDA #2
	STA ENAM0  ;enable it
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
	INC Hero_YPosFromBot
	INC Hero_YPosFromBot
SkipMoveDown

	LDA #%00100000	;Up?
	BIT SWCHA 
	BNE SkipMoveUp
	DEC Hero_YPosFromBot
	DEC Hero_YPosFromBot
SkipMoveUp




; for left and right, we're gonna 
; set the horizontal speed, and then do
; a single HMOVE.  We'll use X to hold the
; horizontal speed, then store it in the 
; appropriate register




;assum horiz speed will be zero


	LDA #%01000000	;Left?
	BIT SWCHA 
	BNE SkipMoveLeft
	DEC Hero_XPos


;; moving left, so we need the mirror image
	LDA #%00001000   ;a 1 in D3 of REFP0 says make it mirror
	STA REFP0
	STA REFP1
SkipMoveLeft

	LDA #%10000000	;Right?
	BIT SWCHA 
	BNE SkipMoveRight
	INC Hero_XPos


;; moving right, cancel any mirrorimage
	LDA #%00000000
	STA REFP0
	STA REFP1

SkipMoveRight

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
	LDA #180
	CMP Hero_YPosFromBot
	BCS HeroUp
	STA Hero_YPosFromBot
HeroUp

;Don't allow Hero below Position 20
	LDA #20
	CMP Hero_YPosFromBot
	BCC HeroDown
	STA Hero_YPosFromBot
HeroDown


;check firebutton
	ldx INPT4
	bmi SwordIsSheathed ;(button not pressed)
SwordIsUnSheathed

        LDA     Hero_YPosFromBot    ;2
        ;LDA     #$16; Hero_Y    ;2
	STA	Hero_Sword_Pos    
        ;STA    ENAM0   ;3 Turn the missile on by putting %00000010 into ENAM0

	jmp DoneWithSword
SwordIsSheathed

        LDA    #$00    ;2
	STA	Hero_Sword_Pos    
	STA	ENAM0

DoneWithSword



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
	;DEC P0_XPos
	INC P1_XPos
	;DEC P2_XPos
	DEC P3_XPos
MOVESET1

	DEC P0_XPos
	

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




;setup pic animations ----------------------------------------------

	lda #<HeroGraphics0 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta Hero_Ptr		;(high byte already set)

	lda #>HeroGraphics0 ;high byte of graphic location
	sta Hero_Ptr+1	;store in high byte of graphic pointer
	
	
	;for skipDraw, P0_Y needs to be set (usually during VBLANK)
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
	sta P0_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics0 ;high byte of graphic location
	sta P0_Ptr+1	;store in high byte of graphic pointer
	

	lda #C_KERNAL_HEIGHT + #C_P0_HEIGHT - #1
	sec
	sbc P0_YPosFromBot ;subtract integer byte of distance from bottom
	sta P0_Y

	lda P0_Ptr
	sec
	sbc P0_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT - #1
	sta P0_Ptr	;2 byte

;setup pic animations ----------------------------------------------


;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics1 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta P1_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics1 ;high byte of graphic location
	sta P1_Ptr+1	;store in high byte of graphic pointer
	

	lda #Enemy_Row_1 + #C_P0_HEIGHT - #1
	sec
	sbc P1_YPosFromBot ;subtract integer byte of distance from bottom
	sbc #3 ;subtract extra because not enough time to dec in kernal
	sta P1_Y

	lda P1_Ptr
	sec
	sbc P1_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT - #1
	sta P1_Ptr	;2 byte

;setup pic animations ----------------------------------------------




;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics2 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta P2_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics2 ;high byte of graphic location
	sta P2_Ptr+1	;store in high byte of graphic pointer

	lda #Enemy_Row_2 + #C_P0_HEIGHT - #1
	sec
	sbc P2_YPosFromBot ;subtract integer byte of distance from bottom
	sbc #3 ;subtract extra because not enough time to dec in kernal
	sta P2_Y


	lda P2_Ptr
	sec
	sbc P2_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT - #1
	sta P2_Ptr	;2 byte

;setup pic animations ----------------------------------------------

;setup pic animations ----------------------------------------------

	lda #<MainPlayerGraphics3 	;low byte of ptr is graphic
	CLC	;clear carry
	ADC PICS
	sta P3_Ptr		;(high byte already set)

	lda #>MainPlayerGraphics3 ;high byte of graphic location
	sta P3_Ptr+1	;store in high byte of graphic pointer
	

	lda #Enemy_Row_3 + #C_P0_HEIGHT - #1
	sec
	sbc P3_YPosFromBot ;subtract integer byte of distance from bottom
	sbc #3 ;subtract extra because not enough time to dec in kernal
	sta P3_Y


	lda P3_Ptr
	sec
	sbc P3_YPosFromBot	;integer part of distance from bottom
	clc
	adc #C_P0_HEIGHT-#1 
	sta P3_Ptr	;2 byte

;setup pic animations ----------------------------------------------





;collisions
	LDA #%10000000
	BIT CXM1P		
	BEQ NoCollision	;skip if not hitting...
	LDA P0_YPosFromBot	;must be a hit! load in the YPos...
;	STA COLUBK	;and store as the bgcolor
NoCollision
	STA CXCLR	;reset the collision detection for next time


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



ScanLoops ;start of kernal +++++++++++++++++++++++ for skyline
	LDA PFCOLOR-1,Y		; 4 cycles
	STA COLUBK	;and store as the bgcolor ; 3 cycles
	LDA PFData0-1,Y		; 4 cycles
	STA PF0			; 3 cycles
	LDA PFData1-1,Y		; 4 cycles
	STA RESM0		; 3 cycles places left scan line leftmost
	STA PF1			; 3 cycles
	LDA PFData2-1,Y		; 4 cycles
	STA PF2			; 3 cycles


	LDA #0							;2 cycles =30
	STA GRP1	; put player 1 into grp1 3 cycles 
	STA GRP0	;put that line as player graphic 0	 ;3 cycles =28
	STA WSYNC 						 ;3 cycles =74
	DEY
	STA HMOVE
	BNE ScanLoops		 				 ;2 cycles =76
EndScanLoops ;end of kernal +++++++++++++++++++++++ for skyline

	LDA #100;

	STA COLUBK	;and store as the bgcolor
	LDY #186; 
	LDA #0
	STA PF0			; 3 cycles
	STA PF1			; 3 cycles
	STA PF2			; 3 cycles

	LDA #%11100011	;The last 3 bits control number and size of players
			;the firs 3 bits control missle size
	LDA #%11110000	;The last 3 bits control number and size of players
			;the firs 3 bits control missle size
	STA NUSIZ0
	STA NUSIZ1
	STA WSYNC 						 ;3 cycles



ScanLoopHero ;start of kernal +++++++++++++++++++++++ for Hero positioning
	DEC Hero_Y ;Hero always is decremented, because he travels the whole screen



	lda Hero_XPos ;3

        sec	     ; 2 set carry
.Div15Hero   
	sbc #7      ; 2      ;this isn't right, but it works should be 15??? Prevents 8 steps forward 6 steps back   
	bcs .Div15Hero   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP1 ;,x

	sta RESP1 ;,x	;the x must be a 0 for player 0  or 1 player 1
        DEY             ;count down number of scan lines          2 cycles = 


        STA WSYNC                                                ;3 cycles =
	STA HMOVE						 ;3
EndScanLoopHero ;end of kernal +++++++++++++++++ for Hero positioning



ScanLoopa ;start of kernal +++++++++++++++++++++++ for player 0 positioning
	DEC P0_Y ;He is decremented because he's within his domain
	lda P0_XPos						 ;3
        sec	     ; 2 set carry
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

	lda	Graphics_Buffer_2
ScanLoop ;start of kernal +++++++++++++++++++++++ for player 0
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     P0_Y            ; 5 (DEC and CMP)
	bcs     .doDraw0        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw0:
	lda     (P0_Ptr),y      ; 5
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine


;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero1        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero1:
	lda     (Hero_Ptr),y      ; 5


	DEY ;Number of scan lines          2 cycles = 

	
	LDX #0
	STX HMP0	;set the move for player 0, not the missile like last time...
	STX HMP1	;set the move for player 1, not the missile like last time...
	STX ENAM0
        STA WSYNC                                                ;3 cycles =
	CPY #Enemy_Row_1-#1						 ;2
        BCS ScanLoop                                             ;2 cycles =
EndScanLoop ;end of kernal +++++++++++++++++ for player 0

;------------------------------------------------

ScanLoop2d13 ;start of kernal +++++++++++++++++++++++ for player 2
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	



;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero3d13        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3d13:
	lda     (Hero_Ptr),y      ; 5


	
	tax

        DEY             ;count down number of scan lines          2 cycles = 


;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero3d1b3        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3d1b3:
	lda     (Hero_Ptr),y      ; 5

	sta Graphics_Buffer_2
	STa ENAM0 
        STA WSYNC                                                ;3 cycles =
EndScanLoop2d13 ;end of kernal +++++++++++++++++ for player 2
;------------------------------------------------


ScanLoop1a ;start of kernal +++++++++++++++++++++++ for player 1 positioning
	STX GRP1

	lda P1_XPos

        sec	     ; 2 set carry
.Div151a   
	sbc #15      ; 2         
	bcs .Div151a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;

	sta RESP0 ;

        DEY             ;count down number of scan lines          2 cycles = 
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop1a ;end of kernal +++++++++++++++++ for player 1 positioning

	lda	Graphics_Buffer_2

ScanLoop1 ;start of kernal +++++++++++++++++++++++ for player 1
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     P1_Y            ; 5 (DEC and CMP)
	bcs     .doDraw1        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw1:
	lda     (P1_Ptr),y      ; 5
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine


;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero2        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero2:
	lda     (Hero_Ptr),y      ; 5


        DEY             ;count down number of scan lines          2 cycles = 

	LDX #0
	STX ENAM0
	STX HMP0	;set the move for player 0, not the missile like last time...
	STX HMP1	;set the move for player 1, not the missile like last time...
        STA WSYNC                                                ;3 cycles =
	CPY #Enemy_Row_2-1
       BCS ScanLoop1                                             ;2 cycles =
EndScanLoop1 ;end of kernal +++++++++++++++++ for player 1


;------------------------------------------------

ScanLoop2d12 ;start of kernal +++++++++++++++++++++++ for player 2
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	


;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero3d12        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3d12:
	lda     (Hero_Ptr),y      ; 5


	
	tax

        DEY             ;count down number of scan lines          2 cycles = 

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero3d1b2        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3d1b2:
	lda     (Hero_Ptr),y      ; 5

	

	sta Graphics_Buffer_2


        STA WSYNC                                                ;3 cycles =
EndScanLoop2d12 ;end of kernal +++++++++++++++++ for player 2
;------------------------------------------------


ScanLoop2a ;start of kernal +++++++++++++++++++++++ for player 2 positioning
	STX GRP1

	lda P2_XPos

        sec	     ; 2 set carry
.Div152a   
	sbc #15      ; 2         
	bcs .Div152a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x

	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

	DEY
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop2a ;end of kernal +++++++++++++++++ for player 2 positioning

	lda	Graphics_Buffer_2

ScanLoop2 ;start of kernal +++++++++++++++++++++++ for player 2
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	

;skipDraw
; draw player sprite 0:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     P2_Y            ; 5 (DEC and CMP)
	bcs     .doDraw2        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw2:
	lda     (P2_Ptr),y      ; 5
;	sta     GRP0            ; 3 = 18 cycles (constant, if drawing or not!)
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine

;        DEY             ;count down number of scan lines          2 cycles

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero3        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3:
	lda     (Hero_Ptr),y      ; 5


        DEY             ;count down number of scan lines          2 cycles


	LDX #0
	STX HMP0	;set the move for player 0, not the missile like last time...
	STX HMP1	;set the move for player 1, not the missile like last time...
        STA WSYNC                                                ;3 cycles =
	CPY #Enemy_Row_3-#1
        BCS ScanLoop2                                             ;2 cycles =
EndScanLoop2 ;end of kernal +++++++++++++++++ for player 2


;------------------------------------------------

ScanLoop2d1 ;start of kernal +++++++++++++++++++++++ for player 2
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	
	

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero3d1        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3d1:
	lda     (Hero_Ptr),y      ; 5


	
	tax

        DEY             ;count down number of scan lines          2 cycles = 

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2 
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero3d1b        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero3d1b:
	lda     (Hero_Ptr),y      ; 5

	

	sta Graphics_Buffer_2
        STA WSYNC                                                ;3 cycles =
EndScanLoop2d1 ;end of kernal +++++++++++++++++ for player 2
;------------------------------------------------

ScanLoop3a ;start of kernal +++++++++++++++++++++++ for player 3 positioning
	STX GRP1

	lda P3_XPos

        sec	     ; 2 set carry
.Div153a   
	sbc #15      ; 2         
	bcs .Div153a   ; 3(2)

	tax
	lda fineAdjustTable,x       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0 ;,x

	sta RESP0 ;,x	;the x must be a 0 for player 0  or 1 player 1

        DEY             ;count down number of scan lines          2 cycles = 
        STA WSYNC                                                ;3 cycles =
	STA HMOVE
EndScanLoop3a ;end of kernal +++++++++++++++++ for player 3 positioning

	lda	Graphics_Buffer_2
ScanLoop3 ;start of kernal +++++++++++++++++++++++ for player 3
	sta	GRP1 ;3 this is here to get rid of offset probelm because 2 skipdraws take 36 cycles
	lda	Graphics_Buffer ;3
	sta	GRP0	


;skipDraw
; draw player sprite 3:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     P3_Y            ; 5 (DEC and CMP)
	bcs     .doDraw3        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDraw3:
	lda     (P3_Ptr),y      ; 5
;	sta     GRP0            ; 3 = 18 cycles (constant, if drawing or not!)
	sta     Graphics_Buffer ; This allows us to do the calculation early, but must move dey to before routine

;skipDraw
; draw Hero sprite:
	lda     #C_P0_HEIGHT-1     ; 2
	dcp     Hero_Y            ; 5 (DEC and CMP)
	bcs     .doDrawHero7        ; 2/3 ; should be bcs
	lda     #0              ; 2
	.byte   $2c             ;-1 (BIT ABS to skip next 2 bytes)(kinda like a jump)
.doDrawHero7:
	lda     (Hero_Ptr),y      ; 5
;	sta     GRP1            ; 3 = 18 cycles (constant, if drawing or not!)



        DEY             ;count down number of scan lines          2 cycles = 
        STA WSYNC                                                ;3 cycles =
        BNE ScanLoop3                                             ;2 cycles =
EndScanLoop3 ;end of kernal +++++++++++++++++ for player 2


	STA WSYNC  	
	STA VBLANK 	
	LDY #30	;number of overscan lines	

;Fix Positions

;Don't allow P0 past Position 160
	LDA #Far_Right	;2
	CMP P0_XPos	;2
	BCS P0Right	;2(3)
	LDA #Far_Left+1 ;2
	STA P0_XPos	;3
P0Right

	STA WSYNC
	DEY

;Don't allow P0 past Position 160
	LDA #Far_Right	;2
	CMP P1_XPos	;2
	BCS P1Right	;2(3)
	LDA #Far_Left+1 ;2
	STA P1_XPos	;3
P1Right


	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA #Far_Right	;2
	CMP P2_XPos	;2
	BCS P2Right	;2(3)
	LDA #Far_Left+1 ;2
	STA P2_XPos	;3
P2Right


	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA ##Far_Right	;2
	CMP P3_XPos	;2
	BCS P3Right	;2(3)
	LDA #Far_Left+1 ;2
	STA P3_XPos	;3
P3Right


	STA WSYNC
	DEY

;Don't allow P0 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP P0_XPos	;2
	BCC P0left	;2(3)
	LDA #Far_Left ;2
	STA P0_XPos	;3
P0left

	STA WSYNC
	DEY

;Don't allow P1 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP P1_XPos	;2
	BCC P1left	;2(3)
	LDA #Far_Right	;2
	STA P1_XPos	;3
P1left

	STA WSYNC
	DEY

;Don't allow P2 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP P2_XPos	;2
	BCC P2left	;2(3)
	LDA #Far_Right	;2
	STA P2_XPos	;3
P2left

	STA WSYNC
	DEY

;Don't allow P3 past Position 160
	LDA #Enemy_Far_Left	;2
	CMP P3_XPos	;2
	BCC P3left	;2(3)
	LDA #Far_Right	;2
	STA P3_XPos	;3
P3left




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

            ORG $FE00
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


	org $FEC0

HeroGraphics1
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010
	.byte #%01010101


	.byte #%01010101
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010
	.byte #%01010101
	.byte #%10101010

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

HeroGraphics2
	.byte #%10000000
	.byte #%01000000
	.byte #%00100000
	.byte #%00010000
	.byte #%00001000
	.byte #%00000100
	.byte #%00000010
	.byte #%00000001

	.byte #%10000000
	.byte #%01000000
	.byte #%00100000
	.byte #%00010000
	.byte #%00001000
	.byte #%00000100
	.byte #%00000010
	.byte #%11111111
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
        .byte #%00001111
        .byte #%00011111
        .byte #%01111111
        .byte #%11111111

PFData1
        .byte #%00000000
        .byte #%00001110
        .byte #%00111111
        .byte #%01111111

PFData2
        .byte #%00000000
        .byte #%00011000
        .byte #%10111100
        .byte #%11111111

PFCOLOR
	.byte #$5F
	.byte #$5D
	.byte #$5B
	.byte #$59


	org $FFFC
	.word Start
	.word Start
