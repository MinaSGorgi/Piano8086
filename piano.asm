; Author : Mina Samy Gorgy
; This program stimulates a musical instrument (keyboard)
;--------------------------------------------------------

.8086       ; Target platform
.model small      ; data = 64KB and code = 64KB
.stack 64

.data

;Handle for keyboard keys that shows press status
kbdbuf    DB 128 dup (0)

; Musical notes frequencies relative to pc speaker
noteC4q   EQU 4693
noteC4    EQU 4560
noteCsh4  EQU 4304
noteD4q   EQU 4181
noteD4    EQU 4063
noteDsh4  EQU 3834
noteE4q   EQU 3725
noteE4    EQU 3619
noteF4q   EQU 3516
noteF4    EQU 3416
noteFsh4  EQU 3224
noteG4q   EQU 3132
noteG4    EQU 3043
noteGsh4  EQU 2873
noteA4q   EQU 2790
noteA4    EQU 2711
noteAsh4  EQU 2559
noteB4q   EQU 2486
noteB4    EQU 2415
noteC5q   EQU 2346
noteC5    EQU 2280

; Colors constants
colorBlack         EQU 0
colorLightGray     EQU 7
colorDarkGray      EQU 8
colorWhite         EQU 15

; Corresponding values of keyboard input
notes      DW 256 dup (0)
positR     DW 256 dup (0)
keyColors  DB 128 dup (0)

; Current step variables
currNote   DW ?
currColor  DB ?

; Video modes and constants
vModeGrph        EQU 10h                        ; 640x350 Graphics 16 colors
vModeGrphWidth   EQU 639
vModeGrphHeight  EQU 349

; Drawing variables
recWidth        DW  ?
recHeight       DW  ?
noteKeyW        EQU 40        ; For black , white = 2*noteKeyW
noteKeyH        EQU 200       ; For black , white = total page height - 2*padding
padding         EQU 25
drawingColor    DB  ?
hiliHeight      EQU 75

; Temp variables
tempWord  dw ?,?

.code
;||||||||||||||||||||||||||||||||
;|||          Macros          |||
;||||||||||||||||||||||||||||||||

; brief "drawPix" : This macro draws a pix given x, y and color
; params : x = col , y = row ,c = color
; return : null
drawPix macro x,y,c
      mov  ah, 0ch
      mov  bh, 0        ; Page
      mov  dx, y        ; 0 is top
      mov  cx, x        ; 0 is leftmost
      mov  al, c
      int  10h
endm
;/////////////////////////////////////////////////////////

; brief "waitKeyPress" : This macro waits for keyboard input
; params : null
; return : ah = scan code , al = ascii code
waitKeyPress macro
      mov  ah, 0
      int  16h
endm
;//////////////////////////////////////////////////////////////////

; brief "checkKeyBuff" : This macro checks if buffer is empty
; params : null
; return : ah = scan code , al = ascii code , zf = if empty
checkKeyBuff macro
      mov  ah, 1
      int  16h
endm
;//////////////////////////////////////////////////////////////////

;||||||||||||||||||||||||||||||
;|||          Main          |||
;||||||||||||||||||||||||||||||
main proc far
      mov  ax, @data
      mov  ds, ax

      xor     ax, ax
      mov     es, ax

      cli                              ; update ISR address w/ ints disabled
      push    word ptr es:[9*4+2]      ; preserve ISR address
      push    word ptr es:[9*4]
      mov     word ptr es:[9*4], offset irq1isr
      mov     es:[9*4+2],cs
      sti

      call intialize			; Draw UI

      test1:
          mov     bx, 0			; Check all keys
check:
          mov     al, [kbdbuf + bx]
          or      al, al
          jnz     cont              ; wait until it's nonzero (pressed/held)
          inc     bx
          cmp     bx,128
          jb      check
          jmp     test1
cont:

          mov  currColor, colorLightGray		; Highlight new note
          call animate		; Play new note

          test2:
              mov     al, [kbdbuf + bx]
              or      al, al
              jz      cont1              ; wait until it's zero (unpressed/released)
              jmp     test2
    cont1:
          mov al, keyColors[bx]			; Get key color
          mov currColor, al
          call animate			; Unhighlight key and stop note
          jmp test1

      cli                         ; update ISR address w/ ints disabled
      pop     word ptr es:[9*4]   ; restore ISR address
      pop     word ptr es:[9*4+2]
      sti
mainLoopEnd:
      call clrScreen
      mov  ah, 4ch		; Return to dos
      int  21h
      ret
main endp
;|||||||||||||||||||||||||||||||
;|||          Procs          |||
;|||||||||||||||||||||||||||||||

; brief "playNote" : This subroutine starts playing a given note
; params : currNote = frequency
; return : null
playNote proc
      push ax

      mov  al,  182          ; Prepare the speaker
      out  43h, al           ; for the note
      mov  ax,  currNote
      out  42h, al           ; Output low byte
      mov  al,  ah           ; Output high byte
      out  42h, al
      in   al,  61h          ; Get value from port 61h
      or   al,  00000011b    ; Set bits 1 and 0 to turn on sound
      out  61h, al

      pop  ax
      ret
playNote endp
;/////////////////////////////////////////////////////////////

; brief "stopNote" : This subroutine stops the current note
; params : null
; return : null
stopNote proc
      push ax

      in   al,61h         ; Get value from port 61h
      and  al,11111100b   ; Reset bits 1 and 0 to turn off sound
      out  61h,al

      pop  ax
      ret
stopNote endp
;/////////////////////////////////////////////////////////////

; brief "clrScreen" : This subroutine clears the screen
; by scrolling
; params : null
; return : null
clrScreen proc
      push ax
      push bx
      push cx
      push dx

      mov  ah, 6
      mov  al, 0        ; Lines to scroll , 0 to clear
      mov  bh, colorBlack         ; Cackground color
      xor  cx, cx       ; ch = Upper row number, cl = Left column number ==> 0,0
      mov  dx, 0ffffh       ; dh = Lower row number, dl = Right column number ==> just put max to clear all screen
      int  10h

      pop  dx
      pop  cx
      pop  bx
      pop  ax
      ret
clrScreen endp
;/////////////////////////////////////////////////////////////

; brief "clrKbBuff" : This subroutine clears the keyboard buffer
; params : null
; return : null
clrKbBuff proc
      push ax
beginLoopClrKbBuff:
      checkKeyBuff        ; Check if buffer is empty
      jz   endLoopClrKbBuff       ; If empty quit
      waitKeyPress        ; If not empty clear first letter
      jmp  beginLoopClrKbBuff       ;Loop
endLoopClrKbBuff:
      pop  ax
      ret
clrKbBuff endp
;/////////////////////////////////////////////////////////////

; brief "drawRectangle" : This subroutine draws a rectangle
; params : si = row , di = col
; return : null
drawRectangle proc
      push ax
      push bx
      push cx
      push dx

      add  si, recHeight                        ; Max row
      mov  tempWord, si                         ; Row counter
      sub  si, recHeight                        ; Return to initial value

      add  di, recWidth                         ; Max col
      mov  tempWord+2, di                       ; Col counter
      sub  di, recWidth                         ; Return to initial value

beginLoopDrawRectangle:
      cmp  di, tempWord+2                       ; Check reached max width
      jb   continueLoopDrawRectangle
      sub  di, recWidth                         ; Start from line beginning
      inc  si                                   ; Start new line
      cmp  si, tempWord                         ; Check passed max height
      jb continueLoopDrawRectangle
      dec  si                                   ; Return to max height
      add  di, recWidth                         ; Return to max width
      pop  dx
      pop  cx
      pop  bx
      pop  ax
      ret
continueLoopDrawRectangle:
      drawPix di,si,currColor                   ; Rasterize
      inc  di                                   ; Go to next pixel in row
      jmp  beginLoopDrawRectangle
drawRectangle endp
;/////////////////////////////////////////////////////////////////////

; brief "intialize" : This subroutine draw initial UI and
; sets notes array
; params : null
; return : null
intialize proc
      call clrScreen        ; Clear initial dosbox

      ; Setting graphics mode
      mov  ah, 0
      mov  al, vModeGrph        ; 640x350 , 16 colors
      int  10h

      ; Setting padding properties
      mov  currColor, colorDarkGray     ; Color
      mov  recWidth, vModeGrphWidth     ; Width = total width
      mov  recHeight, padding       ; Height = padding height

      ; Padding up
      mov  di, 0      ; Initial Col = 0
      mov  si, 0      ; Initial Row = 0
      call drawRectangle

      ; Padding down
      mov  di, 0        ; Initial Col = 0
      mov  si, vModeGrphHeight
      sub  si, padding        ; Down padding starts at height = total height - padding
      call drawRectangle

      ; Drawing the White keys
      mov  di, 0        ; Initial Col = 0
      mov  si, padding      ; Keyboard starts at height = padding
      mov  currColor, colorWhite
      mov  recWidth, vModeGrphWidth       ; Keyboard width = total page width
      mov  recHeight, vModeGrphHeight
      sub  recHeight, 2*padding        ; Keyboard height = total page height - 2* padding
      call drawRectangle

      ; Drawing the Black keys
      ; Black keys are drawn between some white keys (not all)
      ; Their width is divided between the 2 white keys equally
      mov  di, noteKeyW       ; First black key starts after first white key at its center col
      mov  currColor, colorBlack
      mov  recWidth, noteKeyW       ; Width is a variable in data segment
      mov  recHeight, noteKeyH      ; Height is a variable in data segment
      mainLoopDrawBlacksBegin:
      cmp  di, 5*noteKeyW        ; No black key between third and fourth white keys
      je   mainLoopDrawBlacksCont
      cmp  di, 13*noteKeyW        ; No black key between seventh and eighth key
      je   mainLoopDrawBlacksCont
      ; Called this in loop because drawRectangle changes si value eaach time
      mov  si, padding        ; Keyboard starts at height = padding
      call drawRectangle
      cmp  di, vModeGrphWidth     ; Check end of Keyboard
      jae  mainLoopDrawBlacksEnd
      add  di, noteKeyW         ; Get next black key initial col
      jmp  mainLoopDrawBlacksBegin
      mainLoopDrawBlacksCont:
      ; If no black key is present in given co-ords
      add  di, noteKeyW       ; Skip drawing
      add  di, noteKeyW       ; Get to next co-ords
      jmp  mainLoopDrawBlacksBegin
      mainLoopDrawBlacksEnd:

      ; Drawing borders between white keys
      ; First border starts between first and second white key
      mov  di, noteKeyW + noteKeyW/2       ; Border is drawn and distance = noteKeyW/2 from center where center = noteKeyW
      mov  currColor, colorBlack
      mov  recWidth, 1        ; To draw a semi 1d line
      mov  recHeight, vModeGrphHeight - 2*padding       ; Border height = total page height - 2*padding = height of keyboard
      mainLoopDrawBordersBegin:
      mov  si, padding          ; Border starts at height = padding
      call drawRectangle
      add  di, 2*noteKeyW       ; Get to next border
      dec  di         ; To correct starting from 0
      cmp  di, vModeGrphWidth         ; Check max width reached
      jb   mainLoopDrawBordersBegin

      ; Assigning each key a note, a color and an initial col

      ; First row notes
      mov  notes[34], noteCsh4
      mov  notes[36], noteDsh4
      mov  notes[40], noteFsh4
      mov  notes[42], noteGsh4
      mov  notes[44], noteAsh4

      ; First row colors
      mov  al, colorBlack
      mov  keyColors[17], al
      mov  keyColors[18], al
      mov  keyColors[20], al
      mov  keyColors[21], al
      mov  keyColors[22], al

      ; First row positions
      mov  positR[34], noteKeyW
      mov  positR[36], 3*noteKeyW
      mov  positR[40], 7*noteKeyW
      mov  positR[42], 9*noteKeyW
      mov  positR[44], 11*noteKeyW

      ; Second row notes
      mov   notes[60], noteC4
      mov   notes[62], noteD4
      mov   notes[64], noteE4
      mov   notes[66], noteF4
      mov   notes[68], noteG4
      mov   notes[70], noteA4
      mov   notes[72], noteB4
      mov   notes[74], noteC5

      ; Second row colors
      mov   al, colorWhite
      mov   keyColors[30], al
      mov   keyColors[31], al
      mov   keyColors[32], al
      mov   keyColors[33], al
      mov   keyColors[34], al
      mov   keyColors[35], al
      mov   keyColors[36], al
      mov   keyColors[37], al

      ; Second row positions
      mov   positR[60], 0
      mov   positR[62], 1*2*noteKeyW - noteKeyW/2 +1
      mov   positR[64], 2*2*noteKeyW - noteKeyW/2 +1
      mov   positR[66], 3*2*noteKeyW - noteKeyW/2 +1
      mov   positR[68], 4*2*noteKeyW - noteKeyW/2 +1
      mov   positR[70], 5*2*noteKeyW - noteKeyW/2 +1
      mov   positR[72], 6*2*noteKeyW - noteKeyW/2 +1
      mov   positR[74], 7*2*noteKeyW - noteKeyW/2 +1

      ; Third row notes
      mov   notes[88],  noteC4q
      mov   notes[90],  noteD4q
      mov   notes[92],  noteE4q
      mov   notes[94],  noteF4q
      mov   notes[96],  noteG4q
      mov   notes[98],  noteA4q
      mov   notes[100], noteB4q
      mov   notes[102], noteC5q

      ; Third row colors
      mov   al, colorWhite
      mov   keyColors[44],  al
      mov   keyColors[45],  al
      mov   keyColors[46],  al
      mov   keyColors[47],  al
      mov   keyColors[48],  al
      mov   keyColors[49],  al
      mov   keyColors[50],  al
      mov   keyColors[51],  al

      ; Third row positions
      mov   positR[88], 0
      mov   positR[90], 1*2*noteKeyW - noteKeyW/2 +1
      mov   positR[92], 2*2*noteKeyW - noteKeyW/2 +1
      mov   positR[94], 3*2*noteKeyW - noteKeyW/2 +1
      mov   positR[96], 4*2*noteKeyW - noteKeyW/2 +1
      mov   positR[98], 5*2*noteKeyW - noteKeyW/2 +1
      mov   positR[100], 6*2*noteKeyW - noteKeyW/2 +1
      mov   positR[102], 7*2*noteKeyW - noteKeyW/2 +1

      call clrKbBuff        ; Clear any initial clicks

      ret
intialize endp
;///////////////////////////////////////////////////////////////////////

; brief "animate" : This subroutine interprets user input
; params : bx = index of pressed key
; return : null
animate proc
      push ax
      push di
      push si

      ; Checking for escape key pressed
      cmp  bx, 1
      jne  animateNotEsc
      call stopNote       ; Stop any note being played
      call clrScreen        ; Clear the UI
      pop  si
      pop  di
      pop  ax
      mov  ah, 4ch
      int  21h
      ret

animateNotEsc:
      shl  bx,1
      ; Check if valid key
      cmp  notes[bx], 0       ; Invalid keys has 0 note by default
      je   animateEnd
      mov  di, positR[bx]      ; Col of processed note
      mov  recHeight, hiliHeight
      mov  recWidth, noteKeyW

      ; Check if black or white
      ; All white has values >= a (30)
      cmp  bx, 60      ; Because bx is doubled
      jae  animateWhite
      ; Key is black
      mov  si, padding + noteKeyH - hiliHeight      ; Start at end of black key - hilight height
      jmp  animateWhiteNFK
animateWhite:
      ; Key is white
      dec  recWidth
      mov  si, vModeGrphHeight - padding - hiliHeight       ; Start at end of white key - hilight height
      cmp  bx, 60       ; Check first white key
      je   animateWhiteFK
      cmp  bx, 88       ; Check first white key modified
      je   animateWhiteFK
      add  recWidth, noteKeyW
      jmp  animateWhiteNFK
animateWhiteFK:
      ;  Special condition for first white key bec not all width is in UI
      add  recWidth, noteKeyW/2 +1        ; Start from first visible col in UI
animateWhiteNFK:
      ; Check if new or old note
      ; !gray = !new
      cmp  currColor, colorLightGray
      jne  animateStop
      ; If new note get its freq
      mov  ax, notes[bx]
      mov  currNote, ax
      call playNote       ; Play the note
      jmp  animateReady
animateStop:
      call stopNote       ; Stop the old note
animateReady:
      call drawRectangle
animateEnd:
      shr  bx, 1        ; Return bx to initial value
      pop  si
      pop  di
      pop  ax
      ret
animate endp
;/////////////////////////////////////////////////////////////////////

; brief "irq1isr" : This subroutine takes the signal sent by keyboard
; and sets the status of the corresponding key in the kbdBuff then sends
; EOI to XT keyboard and master PIC
; params : null
; return : null
irq1isr proc
    push ax
    push bx
    push cx

    ; read keyboard scan code
    in  al, 60h       ; al = data from keyboard

    ; update keyboard state
    mov  bl, al
    and  bl, 01111111b           ; bx = scan code only
    mov  cx, 7        ; shift right 7 times to get keys status in bit 0
irq1isrShift:
    shr  al, 1
    loop irq1isrShift
    ; al = 0 if pressed, 1 if released
    xor  al, 1               ; al = 1 if pressed, 0 if released
    mov  [bx+kbdbuf], al      ; Set key status

    ; send EOI to XT keyboard
    in   al, 61h
    mov  ah, al
    or   al, 80h
    out  61h, al
    mov  al, ah
    out  61h, al

    ; send EOI to master PIC
    mov  al, 20h
    out  20h, al

    pop cx
    pop bx
    pop ax
    iret
irq1isr endp
;////////////////////////////////////////////////////////////////////////
end main
