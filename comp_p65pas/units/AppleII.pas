{$SET_STATE_RAM '0100-01FF:SFR,C000-CFFF:SFR,D000-FFFF:SFR'}  // stack/IO-page/rom
  {$SET_DATA_ADDR ''}
  {$SET_DATA_ADDR '00FA-00FF'}

  {$ORG $2000}

unit AppleII;

//-------------------------------------------------------------------------------

interface
const
	basl	= $28;                                        // basisadresse bildschirmzeile nr. 1
	bash	= $29;
	bas2l	= $2A;                                        // basisadresse bildschirmzeile nr. 2
	bas2h	= $2B;

  keybuf = $0200;                                     // AppleII keybordbuffer
  keybup = $02ff;                                     // current read pos in keyboardbuffer
  bytin  = $fd0c;                                     // get one char from keyboard  - rom
  bytout = $fded;                                     // put one char/byte to screen - rom

var
  wkpt1 : word absolute $eb;                          // AppleII - Adress of input or data <--- Applesoftbasic
  wkpt2 : word absolute $ed;
  wkpt3 : byte absolute $ef;
  strlen: byte absolute $d7;                           // store for current stringlen

  dez1: array[2] of byte = [10,100];                                     // need for decimal out/in byte
  dez2: array[8] of byte = [$0a, $00, $64, $00, $e8, $03, $10, $27];     // need for dezimal out/in word

  basladr: array[24] of	byte	= [$00,$80,$00,$80,$00,$80,$00,$80,$28,$A8,$28,$A8,$28,$A8,$28,$A8,$50,$D0,$50,$D0,$50,$D0,$50,$D0];
  bashadr: array[24] of byte	= [$04,$04,$05,$05,$06,$06,$07,$07,$04,$04,$05,$05,$06,$06,$07,$07,$04,$04,$05,$05,$06,$06,$07,$07];


    procedure strin(B: byte; A: word register) :byte;           // strin(maxlen, @string):reallen or strin(maxlen, keybuf):reallen
    procedure hexinb :byte;
    procedure hexinw :word;
    procedure bininb :byte;
    procedure bininw :word;
    procedure decinb :byte;
    procedure decinw :word;

    procedure clrscr;
    procedure setcurxy(X: byte registerA; Y: byte registerY);
    procedure setcurx(X: byte registerA);


    procedure strout(A: word register);                         // strout(@string) or strout(keybuf)
    procedure hexout(A: byte registerA);
    procedure hexout(A: word register);
    procedure decout(A: byte registerA);
    procedure decout(A: word);
    procedure binout(A: byte registerA);
    procedure binout(A: word register);

    procedure conout(A:byte registerA);
    procedure conin :byte;

// parse keybuf for valid input - procedures can be override (to much valid characters without delimiter)

    procedure shexin;
    procedure sbinin;
    procedure sdecin;

// -------------------------------------------------------------------------------

implementation

  procedure conin :byte;

    begin
        asm
          jsr bytin
        end
    end;

  procedure strin(B: byte; A: word register) :byte;                 // strin(maxlen, @string):reallen or strin(maxlen, keybuf):reallen
    begin
    wkpt1:= A;
    asm
              ldy #$00                     ; reset charcounter
      loop:   sty wkpt3
	            jsr bytin                    ; get char - this procedure destroy Yreg
              ldy wkpt3

              cmp #$8d                     ; return ?     AppleII bit 7 on
              beq return
              cmp #$88                     ; rubout ?
              beq rubout
              cmp #$a0                     ; other controlchar ?
              bcc loop                     ; yes - ignore

              cpy B                        ; buffer full ?
              bcs loop                     ; yes - ignore
              sta (wkpt1),y                ; no -  put char to buffer
              jsr bytout                   ; screenecho
              iny                          ; charcounter+1
              bne loop                     ; get next char

      rubout: cpy #$00                     ; Xcur on pos 0 ?
              beq loop                     ; yes - ignore
              jsr bytout                   ; Xcursor-1
              lda #$a0
              jsr bytout                   ; erase char on screen
              lda #$88
              jsr bytout
              dey                          ; charcounter-1
              lda #$00
              sta (wkpt1),y                ; erase char(x) in keybuf
              beq loop                     ; all time

      return: lda #$00
              sta (wkpt1),y                ; set string endmarker
              sta keybup                   ; set current readpos to begin keyboardbuffer
              tya                          ; stringlen to regA
    end
    end;
  procedure hexinb :byte;
  begin
    strlen := strin(2, keybuf);                // __E = current inputlen
    shexin;
    asm
	    lda __IX.low
    end
  end;
  procedure hexinw :word;
    begin
      strlen:= strin(4,keybuf);
      shexin;
      asm
        lda __IX.high
        sta __H
        lda __IX.low
      end
    end;
  procedure bininb :byte;
  begin
    strlen:= strin(8, keybuf);
    sbinin;
    asm
      lda __IX.low
    end
  end;
  procedure bininw :word;
    begin
      strlen:= strin(16, keybuf);
      sbinin;
      asm
	      lda __IX.high
        sta __H
        lda __IX.low
      end

    end;
  procedure decinb :byte;
    begin
      strlen:= strin(3, keybuf);
      sdecin;
      asm
        lda __IX.low
      end
    end;
  procedure decinw :word;
    begin
      strlen:=strin(5, keybuf);
      sdecin;
      asm
	      lda __IX.high
        sta __H
        lda __IX.low
      end
    end;

  procedure conout(A: byte registerA);
  begin
    asm
      ora #$80
      jsr bytout
    end
  end;
  procedure binout(A: word register);
      begin
        asm
          sta __U
        end;
        binout(__H);
        binout(__U);
      end;
  procedure binout(A: byte registerA);
    begin
      asm
              ldx #$07
      loop2:  rol
              pha
              lda #$b0
              bcc loop
              lda #$b1
      loop:   jsr bytout
              pla
              dex
              bpl loop2
      end
    end;
  procedure hexout(A: byte registerA);
    begin
    asm
	              pha
                lsr
                lsr
                lsr
                lsr
                jsr nibble
                pla
                and #%00001111

      nibble:   clc
                adc #176
                cmp #186
                bcc loop

                clc
                adc #7
      loop:     jsr bytout
    end
  end;
  procedure hexout(A: word register);
  begin
      asm
        sta __U
      end;
      hexout(__H);
      hexout(__U);
  end;
  procedure decout(A: byte registerA);
    begin
     asm
                  beq zero             ; regA = 0 - print '0'
                  ldx #$80
                  stx __H              ; set zerobefore? marker
                  ldx #$01             ; set pointer dez1.array
        loop2:    ldy #$00             ; reset digitcounter
        loop1:    cmp dez1,x           ; >= tabbyte ?
                  bcc loop             ; no - next digit
                  iny                  ; digit=digit+1
                  sbc dez1,x           ; regA=regA-dez1.array,x
                  bne loop1
        loop:     pha                  ; store regA
                  tya                  ; digit to regA
                  bit __H              ; only zeros before ?
                  bpl loop3            ; no - print digit
                  cpy #$00             ; only zeros before and digit = 0 ?
                  beq loop4            ; yes - don't print digit
        loop3:    asl __H              ; reset zerobefore? marker
                  ora #$b0             ; for appleII highbit on
                  jsr bytout           ; print digit
        loop4:    pla                  ; restore regA
                  dex                  ; 10^2 digit and 10^1 digit done ?
                  bpl loop2            ; no - calc next digit
         zero:    ora #$b0             ; print one zero or digit 10^0
                  jsr bytout
     end

  end;
  procedure decout(A: word);
    begin
      asm
                  ldx #$00            ; set leadedzeros? marker
                  stx __H
                  ldx #$07            ; set pointer array.dez2.highbyte
         wloop2:  ldy #$00            ; reset digitcounter

         wloop:   lda A.high          ; restore A.highbyte
                  cmp dez2,x          ; > array.dez.highbyte.x - check zero and/or not print digit
                  bcc endsub          ; < array.dez.highbyte.x - check zero and/or not print digit
                  beq chklb           ; = array.dez.highbyte.x - check lowbyte
                  dex                 ; set pointer = arraydez.lowbyte
                  lda A.low
                  sec                 ; A = A - arraydez(x)
        wloop1:   sbc dez2,x
                  sta A.low
                  inx
                  lda A.high
                  sbc dez2,x
                  sta A.high
                  iny                 ; inc digitcounter
                  bne wloop           ; all time

        chklb:    lda A.low           ; restore A.lowbyte
                  dex                 ; set pointer = arraydez.lowbyte
                  cmp dez2,x          ; valid ?
                  bcs wloop1          ; >= array.dez.lowbyte.x = valid digit

                  inx                 ; set pointer = arraydez.high
        endsub:   tya                 ; digit to regA
                  bne nonull          ; digit <> 0 - print
                  cmp __H             ; leaded zero ?
                  beq next            ; yes - no print

        nonull:   ora #$b0            ; appleII need bit 7 on
                  inc __H             ; __H = counter for valid digits and marker for leaded zeros
                  jsr bytout          ; conout digit
        next:     dex                 ; pointer=pointer-2
                  dex
                  bpl wloop2          ; calc next digit

                  lda A.low           ; load last digit
                  ora #$b0            ; appleII need bit 7 on
                  jsr bytout          ; conout last digit
        quit:

      end
    end;
  procedure strout(A: word register);
    begin
      wkpt1:=A;
      asm
                ldy #$00
        loop:   lda (wkpt1),y
                beq loop1           ; endmarker $00
                ora #$80
                jsr bytout          ; appleII need bit 7 on
                iny
                bne loop
        loop1:
     end
   end;

  procedure setcurxy(X: byte registerA; Y:byte registerY);
  begin
    asm
	              cmp $21             ; wndwdth
	              bcs	loop
	              clc
	              adc	$20             ; wndlft
	              sta	$24		          ; 40col CurX
                bit $c01f           ; 40col or 80col activ ?
                bpl loop		        ; is 40col
                sta $057b           ; 80col CurX
          loop:	tya
	              clc
	              adc	$22             ; wndtop
	              cmp	$23             ; wndbtm
	              bcs	exit
	              tay
	              LDA	bashadr,Y
	              STA	bash
	              LDA	basladr,Y
	              STA	basl
	              tya
	              sta	$25
	              bit	$c01f
	              bpl	exit
	              sta	$05fb
          exit:
    end

 end;
  procedure setcurx(X: byte registerA);
  begin
    asm
	              cmp $21             ; wndwdth
	              bcs	loop
	              clc
	              adc	$20             ; wndlft
	              sta	$24		          ; 40col CurX
                bit $c01f           ; 40col or 80col activ ?
                bpl loop		        ; is 40col
                sta $057b           ; 80col CurX
			loop:
    end

  end;
  procedure clrscr;
  begin
    asm
      jsr $fc58
    end
  end;

// inside unit

  procedure shexin;                                             // parse keybuf for valid hex input
    begin
    __IX:=0;                                                       // stop by 4 nibble or delimiter
    asm
                ldy #$04
        next:   jsr nibble        ; get hexnibble
                bcs exit1
                asl __IX.low      ; one nibble up
                rol __IX.high
                asl __IX.low
                rol __IX.high
                asl __IX.low
                rol __IX.high
                asl __IX.low
                rol __IX.high
                dey
                bne next
                beq exit1

       nibble:  ldx keybup
                lda keybuf,x
                sec
                sbc #$b0
                cmp #23
                bcs exit          ; = >'F'
                cmp #10
                bcc num           ; = '0..9)
                cmp #17
                bcc exit          ; = between '0..9' and 'A..F'
                sbc #7
       num:     ora __IX.low
                sta __IX.low
                inc keybup        ; set new  current readpos in keybordbuffer
       exit1:   clc
                rts
       exit:    sec
     end
   end;
  procedure sbinin;                                             // parse keybuf for valid bin input
    begin
      __IX:= 0;                                                      // stop by 16 bit or delimiter
      asm
                ldy #$10
        loop:   ldx keybup
                lda keybuf,x      ; get bin-digit
                cmp #$b0
                beq ok
                cmp #$b1
                beq ok
                rts

        ok:     lsr
                rol __IX.low
                rol __IX.high
                inc keybup
                dey
                bne loop
      end
    end;
  procedure sdecin;                                             // parse keybuf for valid dec input
  begin
    __IX:= 0;                                                      // stop by 5 digits or delimiter
    asm
                ldy #$00            ; digitcounter:= 0
      loop:     ldx keybup          ; posX:= keybup
                lda keybuf,x        ; regA:= keybuf.posX
                sec
                sbc #$b0
      	        cmp #10	            ; if regA>9 then exit
	              bcs exit
                pha                 ; store digit on stack
                inc keybup
                iny
                cpy #$06
                bcs exit
                bcc loop

       exit:    cpy #$00            ; empty string ?
                beq ende            ; yes
                pla
                sta __IX.low        ; add 10^0
                ldx #$00            ; set tabcounter
        outer:  dey                 ; digitcounter-1
                beq ende            ; all digits done
                pla                 ; get next digits -  10^1 to 10^4
                beq loop3           ; is zero - nothing to do
                sta __H             ; store digit
        inner:  clc
                lda __IX.low
                adc dez2,x
                sta __IX.low
                inx
                lda __IX.high
                adc dez2,x
                sta __IX.high
                dex
                dec __H
                bne inner
        loop3:  inx
                inx
                bne outer
        ende:
    end
  end;

end.

