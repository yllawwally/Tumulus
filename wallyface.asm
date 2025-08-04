; move a happy face with the joystick by Kirk Israel
; (with a can't'dodge'em line sweeping across the screen)

	processor 6502
	include vcs.h
	org $F000

;Variables ------

YPosFromBot = $80;
VisiblePlayerLine = $81;
PICS = $82;
ROLLING_COUNTER = $83;
Graphics_Buffer = $84
YPosFromBotE1 = $85;
VisibleEnemyLine = $86;
Enemy_Graphics_Buffer = $87;
EGB = $88;
PGB = $89;


; Constants ------
playerheight       ds 8;



;generic start up stuff...
Start
	SEI	
	CLD  	
	LDX #$FF	
	TXS	
	LDA #0	
	STA ROLLING_COUNTER
	STA PICS
	
ClearMem 
	STA 0,X		
	DEX		
	BNE ClearMem	
	
	LDA #$00   ;start with a black background
	STA COLUBK	
	LDA #$1C   ;lets go for bright yellow, the traditional color for happyfaces
	STA COLUP0
;Setting some variables...
	LDA #80
	STA YPosFromBot	;Initial Y Position
	STA YPosFromBotE1



;; Let's set up the sweeping line. as Missile 1

	
	LDA #2
	STA ENAM1  ;enable it
	LDA #33
	STA COLUP1 ;color it

	LDA #$10	
	STA NUSIZ1	;make it quadwidth (not so thin, that)

	LDA #%11111111	; -1 in the left nibble
	STA HMM1	; of HMM1 sets it to moving



;VSYNC time
MainLoop
	LDA #2
	STA VSYNC	
	STA WSYNC	
	STA WSYNC 


;setup pic animations ----------------------------------------------
	INC ROLLING_COUNTER

	LDA ROLLING_COUNTER
	AND #15		;every 8th screen swap to next image of player
	CMP #8
	BEQ PICSET4
	JMP PICSET3
PICSET4	LDA  PICS
	CMP  #8
	BEQ PICSET
	LDA  #8
	JMP PICSET2
PICSET	LDA  #0
PICSET2	STA PICS
PICSET3


	LDA ROLLING_COUNTER
	AND #25		;how often to move enemy, larger number is slower
	CMP #8
	BEQ PICSET5
	LDA #%00001111	; +1 in the left nibble
	JMP PICSET6
PICSET5	LDA #%00011111	; 0 in the left nibble
PICSET6	STA HMP1	; of HMM1 sets it to moving the enemy


;setup pic animations ----------------------------------------------
	
	STA WSYNC	
	LDA #43	
	STA TIM64T	
	LDA #0
	STA VSYNC 	


;Main Computations; check down, up, left, right
;general idea is to do a BIT compare to see if 
;a certain direction is pressed, and skip the value
;change if so

;
;Not the most effecient code, but gets the job done,
;including diagonal movement
;

; for up and down, we INC or DEC
; the Y Position

	LDA #%00010000	;Down?
	BIT SWCHA 
	BNE SkipMoveDown
	INC YPosFromBot
	INC YPosFromBotE1
	INC YPosFromBotE1
SkipMoveDown

	LDA #%00100000	;Up?
	BIT SWCHA 
	BNE SkipMoveUp
	DEC YPosFromBot
	DEC YPosFromBotE1
	DEC YPosFromBotE1
SkipMoveUp

; for left and right, we're gonna 
; set the horizontal speed, and then do
; a single HMOVE.  We'll use X to hold the
; horizontal speed, then store it in the 
; appropriate register




;assum horiz speed will be zero
	LDX #0	


	LDA #%01000000	;Left?
	BIT SWCHA 
	BNE SkipMoveLeft
	LDX #$10	;a 1 in the left nibble means go left


;; moving left, so we need the mirror image
	LDA #%00001000   ;a 1 in D3 of REFP0 says make it mirror
	STA REFP0

SkipMoveLeft

	LDA #%10000000	;Right?
	BIT SWCHA 
	BNE SkipMoveRight
	LDX #$F0	;a -1 in the left nibble means go right...

;; moving right, cancel any mirrorimage
	LDA #%00000000
	STA REFP0

SkipMoveRight


	STX HMP0	;set the move for player 0, not the missile like last time...



; see if player and missile collide, and change the background color if so

	;just a review...comparisons of numbers always seem a little backwards to me,
	;since it's easier to load up the accumulator with the test value, and then
	;compare that value to what's in the register we're interested.
	;in this case, we want to see if D7 of CXM1P (meaning Player 0 hit
	; missile 1) is on. So we put 10000000 into the Accumulator,
	;then use BIT to compare it to the value in CXM1P

	LDA #%10000000
	BIT CXM1P		
	BEQ NoCollision	;skip if not hitting...
	LDA YPosFromBot	;must be a hit! load in the YPos...
	STA COLUBK	;and store as the bgcolor
NoCollision
	STA CXCLR	;reset the collision detection for next time



WaitForVblankEnd
	LDA INTIM	
	BNE WaitForVblankEnd	
	LDY #191 	


	STA WSYNC	
	STA HMOVE 	
	
	STA VBLANK  	


;main scanline loop...



PreScanLoop


ScanLoop ;start of kernal

	LDA #8

CheckActivatePlayer
	CPY YPosFromBot
	BNE SkipActivatePlayer
	STA VisiblePlayerLine 
SkipActivatePlayer

CheckActivateEnemy
	CPY YPosFromBotE1
	BNE SkipActivateEnemy
	STA VisibleEnemyLine 
SkipActivateEnemy
	CLC		;Carry must be cleared or ADC will add carry as well


	LDA VisibleEnemyLine	;check the visible player line...
	BEQ FinishEnemy		;skip the drawing if its zero...
IsEnemyOn	
	ADC PICS	;add value of pics to Accumulator 
	TAX
	LDA EnemyGraphics-1,x
	DEC VisibleEnemyLine	
FinishEnemy
	STA GRP1	; put player 1 into grp1
AfterEnemyDraw


	LDA VisiblePlayerLine	;check the visible player line...
	BEQ FinishPlayer	;skip the drawing if its zero...
IsPlayerOn	
	ADC PICS	;add value of pics to Accumulator 
	TAX
	LDA MainPlayerGraphics-1,x	;shift to change which pic were showing
;	ORA #1		;creates a shield
	DEC VisiblePlayerLine	
FinishPlayer
	STA GRP0	;put that line as player graphic 0
AFTERPLAYERDRAW


	

	STA WSYNC 	



	DEY		;count down number of scan lines

	BNE ScanLoop	;end of kernal


	STA WSYNC  	
	STA VBLANK 	
	LDX #24		

;set graphics to blank for next line -----
	LDA #0		;set Acc to 0 for clearing graphics
	STA EGB		;enemy graphics buffer
;set graphics to blank for next line -----






OverScanWait
	STA WSYNC
	DEX
	BNE OverScanWait
	JMP  MainLoop      



MainPlayerGraphics
	.byte #%00010100
	.byte #%00010100
	.byte #%00011000
	.byte #%00011000
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000


	.byte #%00010010
	.byte #%00010100
	.byte #%00011000
	.byte #%00011000
	.byte #%01111110
	.byte #%00010000
	.byte #%00111000
	.byte #%00111000

EnemyGraphics
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

	org $FFFC
	.word Start
	.word Start
