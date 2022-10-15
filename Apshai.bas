' +++++++++++++++++++++++++++++++++++++++++++++
' DUNJONQUEST - Temple of Apshai
' Original by AUTOMATED SIMULATIONS, 1979.
' Conversion by Craig S. Buchanan, 2021-Dec-27
' Last Update 2022-Feb-04
' +++++++++++++++++++++++++++++++++++++++++++++

OPTION DEFAULT FLOAT
OPTION EXPLICIT

' +++++++++++++++++++
' Variables
' +++++++++++++++++++

' Combined Undefined Variables (IN and DM)
' DEFINT D,I-Y
' DEFINT D,H-Z
Dim Integer CH2,D,D1,D2,DX
Dim Integer HB,HI,HN,HS
Dim Integer I,I1,I2,IA,IB,IC,IH,II,IJ,IK,IM,IN,IR2,IT
Dim Integer J,J0,J6,J7,J8,J9,JJ,JQ,K,KA,KB,KC,KF,KK,KM,KR
Dim Integer L,L1,L2,L3,L4,L5,L6,LE,LL2,LM,LO,LQ,LR,LS,LV,LX,LY
Dim Integer M1,M2,MA,MB,MD,MF,MH,ML,MM,MN,MO,MP,MQ,MR,MS,MT,MW
Dim Integer N,NB,NM2,NN,NO,NP,NS2,NT,NX,NY,OM,OO
Dim Integer P2,PA,PB,PC,PH,PS,PW,Q,Q1,Q2,R2,RM,RN2,RQ,RS,RT
Dim Integer S3,SC,SE,SM,SP2,TA,TG,TN,TP,TR,TS,TV,TW
Dim Integer UA,UD,UC,UI,UL,UP,UR,US,UV,UW,UX,UY,V1,V2,V3,V4
Dim Integer W1,W2,W3,W4,WC,WM,WN,WP2,WT
Dim Integer X,X0,X1,X2,XA,XB,XL,XM,XP,XR,XX
Dim Integer Y,Y0,Y1,Y2,Y4,YA,YB,YL,YM,YP,YR,YY
Dim String  A$,A2$,AN$,B1$,BL$,C$,LL$,MC$,NM$,O$,S2$
Dim Float   A1,AA,AK,AM,AS2,B,C2,CS,CA
Dim Float   E,EG,ERR,EX

' Combined Global Arrays (IN and DM)
Dim CH(6)
Dim Integer DS(1,1),DW(5,1)
Dim Integer H(3)
Dim M$(12)
Dim NA$(5),NC$(6),NW$(6),NS$(2)
Dim Integer P(5) 'DM has P(4)
Dim R$(6)
Dim Integer RF(3),RN(3),RR(6)
Dim Integer S(4),SD(2),SP(2),SW(2)
Dim T$(9)
Dim Integer TX(3,1),TY(3,1),TM(5)
Dim Integer W(5),WD(5),WR(5),WW(5),WP(5)
Dim Integer ZA(3),ZD(5)  

Sub TrsClear
  A$="":A2$="":AN$="":B1$="":BL$="":C$="":LL$="":MC$="":NM$="":O$=""
  A1=0:AA=0:AK=0:AM=0:AS2=0:B=0:C2=0:CH2=0:CS=0:CA=0:D=0:D1=0:D2=0:DX=0
  E=0:EG=0:ERR=0:EX=0:HB=0:HI=0:HN=0:HS=0
  I=0:I1=0:I2=0:IA=0:IB=0:IC=0:IH=0:II=0:IJ=0:IK=0:IM=0:IN=0:IR2=0:IT=0
  J=0:J0=0:J6=0:J7=0:J8=0:J9=0:JJ=0:JQ=0:K=0:KA=0:KB=0:KC=0:KF=0:KK=0:KM=0:KR=0
  L=0:L1=0:L2=0:L3=0:L4=0:L5=0:L6=0:LE=0:LL2=0:LM=0:LO=0:LQ=0:LR=0:LS=0:LV=0:LX=0:LY=0
  M1=0:M2=0:MA=0:MB=0:MD=0:MF=0:MH=0:ML=0:MM=0:MN=0:MO=0:MP=0:MQ=0:MR=0:MS=0:MT=0:MW=0
  N=0:NB=0:NM2=0:NN=0:NO=0:NP=0:NS2=0:NT=0:NX=0:NY=0:OM=0:OO=0
  PA=0:PB=0:PC=0:PH=0:PS=0:PW=0:Q=0:Q1=0:Q2=0:R2=0:RM=0:RN2=0:RQ=0:RS=0:RT=0
  S3=0:SC=0:SE=0:SM=0:SP2=0:TA=0:TG=0:TN=0:TP=0:TR=0:TS=0:TV=0:TW=0:
  UA=0:UD=0:UC=0:UI=0:UL=0:UP=0:UR=0:US=0:UV=0:UW=0:UX=0:UY=0:V1=0:V2=0:V3=0:V4=0
  W1=0:W2=0:W3=0:W4=0:WC=0:WM=0:WN=0:WP2=0:WT=0
  X=0:X0=0:X1=0:X2=0:XA=0:XB=0:XL=0:XM=0:XP=0:XR=0:XX=0
  Y=0:Y0=0:Y1=0:Y2=0:Y4=0:YA=0:YB=0:YL=0:YM=0:YP=0:YR=0:YY=0
  'Clear legend Mode
  clearLegend()
End Sub


' +++++++++++++++++++
' TRS-80 Work Arounds
' +++++++++++++++++++
Dim path$ = Mid$(MM.Info$(Current),1,Instr(MM.Info$(Current),"Apshai.bas")-1)
Dim lastKr as Integer
Dim taMode as Integer

' Virtual Trs80 Memory
Dim mem%(2356)

' TRS-80 Screen Parameters
Dim TrsS, TrsSx,TrsSy,TrsSw,TrsCw,TrsCh,TrsPw,TrsPh,TrsMx,TrsMy
TrsS = 15360
TrsSx = 64
TrsSy = 16

' Regular TRS-80 Screen
Sub Mode64x16 
  Mode 2,8 ' 640x400
  Font 4,1 ' 10x16 => (64,25)
  COLOUR RGB(WHITE),RGB(BLACK)
  TrsSw = 10
  TrsCw = 10
  TrsCh = 24 ' 10x24 => (64,16)
  TrsPw = 5
  TrsPh = 8  ' 5x8
  TrsMx = TrsSx * TrsCw - 1
  TrsMy = TrsSy * TrsCh - 1
  'Cls
  TrsCls
End Sub

' Double Width Screen
Sub Mode32x16
  Mode 9,8 ' 1024x768
  Font 3,2 ' 
  COLOUR RGB(WHITE),RGB(BLACK)
  TrsSw = 16
  TrsCw = 32
  TrsCh = 48
  TrsPw = 16
  TrsPh = 16
  Cls
End Sub

' Legend
Dim legendMode as Integer
Dim rooms(60) as Integer
Dim treasure(20) as Integer
Dim monster$(26) as String
Dim lastMonster$ as String
Dim lastTreasure as Integer
Dim lastTrap as Integer
legendMode = 2

Sub ClearLegend
  Local I
  For I = 1 to 60
    rooms(I) = 0
  Next
  For I = 1 to 26
    monster$(I) = ""
  Next
  For I = 1 to 20
    treasure(I) = 0
  Next
  lastMonster$ = ""
  lastTreasure = 0
  lastTrap = -1
End Sub

Sub DisplayLegend
  Local x%,y%,sy%
  x% = MM.INFO(HPOS)
  y% = MM.INFO(VPOS)
  FONT 1,1
  sy% = MM.VRES - MM.INFO(FONTHEIGHT)
  Colour RGB(GREEN)
  Print @(0,sy%) "[F1 MAN] [F2 CMD] [F3 RM] [F4 MON] [F5 TRP] [F6 TREA] [F7 KEY] [F8 MAG] [F9 ";
  Select Case legendMode
    Case 1
      Print "ON] ";
    Case 2
      Print "1ST]";
    Case 3
      Print "OFF]";
  End Select
  FONT 4,1
  Colour RGB(WHITE)
  Print @(x%,y%) "";
End Sub

' Trs Clear Screen
Sub TrsCls
  Cls
  local a$
  a$ = inkey$ ' Eat any waiting character!
  DisplayLegend
End Sub


' Upper Case Input
Function TrsInput(s$) as string
  Local c%        ' Character (ascii)
  Local x%,y%     ' current X,Y of cursor
  Local sx%,sy%   ' Screen X,Y to return cursor
  Local a$        ' Key
  Local i$        ' Input string
  Local n%        ' Number of characters
  Local f%        ' cursor Flag
  Local mn%       ' Maximium N. (Keep to single line on screen.)
  ' Initialize
  x% = 0
  y% = 0
  n% = 0
  i$ = ""
  ' Prompt
  Print s$;"? ";
  sx% = MM.INFO(HPOS)
  sy% = MM.INFO(VPOS)
  mn% = TrsSx - Int(sx% / TrsCw) - 1
  ' Get Input
  Do
    x% = MM.INFO(HPOS)
    y% = MM.INFO(VPOS)
    Print @(x%,y%) chr$(95);
    Print @(x%,y%) "";
    Timer = 0
    f% = 0
    ' Get Key
    Do
      a$ = Inkey$
      c% = Asc(a$)
      If Timer > 500 then
        If f% = 0 then
          Print @(x%,y%) " ";
          f%=1
        Else
          Print @(x%,y%) chr$(95);
          f% = 0
        End If
        Print @(x%,y%) "";
        Timer = 0
      End If
    Loop Until c% <> 0
    ' Process Key
    ' Handle Function Keys
    If c% > 144 And c% < 157 Then
      DisplayFunctionKey c%
      Print @(x%,y%) "";
      Continue Do
    End If
    ' Ignore non-hanlded characters
    If c% <> 8 AND c% <> 10 AND (c% <32 OR c%>126) Then Continue Do
    ' Handle BackSpace
    If c% = 8 Then
      If n% > 0 Then
        i$ = Mid$(i$,1,n%-1)
        Inc n%, -1
        Print @(x%,y%), " ";
        Print @(sx%,sy%) i$;
      End If
      Continue Do
    End If
    ' Handle Character (provided it will fit on line)
    If c% > 31 And c% < 128 And n% < mn% Then
      a$ = UCase$(a$)
      i$ = i$ + a$
      inc n%
      Print @(x%,y%) a$;
    End if
  Loop Until c% = 10
  ' Reposition Cursor
  Print @(x%,y%) " ";
  ' Print a Return to Make TAB work correctly.
  TrsPrint ""
  ' All Done
  TrsInput = i$
End Function

' Input Decimal
Function TrsInputD(s$) as integer
  Local c%, x%, y%, sx%, sy%, a$, i$, n%, f%, mn%
  x% = 0
  y% = 0
  n% = 0
  i$ = ""
  Print s$;"? ";
  sx% = MM.INFO(HPOS)
  sy% = MM.INFO(VPOS)
  mn% = TrsSx - Int(sx% / TrsCw) - 1
  Do
    x% = MM.INFO(HPOS)
    y% = MM.INFO(VPOS)
    Print @(x%,y%) chr$(95);
    Print @(x%,y%) "";
    Timer = 0
    f% = 0
    Do
      a$ = Inkey$
      c% = Asc(a$)
      If Timer > 500 then
        If f% = 0 then
          Print @(x%,y%) " ";
          f%=1
        Else
          Print @(x%,y%) chr$(95);
          f% = 0
        End If
        Print @(x%,y%) "";
        Timer = 0
      End If
    Loop Until c% <> 0
    If c% > 144 And c% < 157 Then
      DisplayFunctionKey c%
      Print @(x%,y%) "";
      Continue Do
    End If
    If c% <> 8 AND c% <> 10 AND (c% <48 OR c%>58) Then Continue Do
    a$ = UCase$(a$)
    If c% = 8 Then
      If n% > 0 Then
        i$ = Mid$(i$,1,n%-1)
        Inc n%, -1
        Print @(x%,y%), " ";
        Print @(sx%,sy%) i$;
      End If
      Continue Do
    End If
    If c% > 47 And c% < 59 And n% < mn% Then
      i$ = i$ + a$
      inc n%
      Print @(x%,y%) a$;
    End if
  Loop Until c% = 10
  Print @(x%,y%) " ";
  TrsPrint ""
  TrsInputD = Val(i$)
End Function


' Upper Case Inkey$
Function TrsInkey() as string
  Local c$,k%
  TrsInkey = ""
  c$ = Inkey$
  If c$ <> "" Then
    k% = asc(c$)
    If k% > 144 And k% < 157 Then
      DisplayFunctionKey k%
      TrsInkey = ""
      Exit Function
    End If
  End If
  TrsInkey = UCase$(c$)
End Function

' M$ Basic RND
Function TrsRnd(r%) as integer
  If r% = 1 Then
    TrsRnd = Rnd()
  Else
    TrsRnd = Int(Rnd() * r% + 1)
  END IF
End Function

' Virtual Trs80 Memory Poke
Sub TrsPoke a%, v%
  Local add%
  add% = 20288 + a%
  if add% < 0 OR add% > 20288 then
    print "Poke out of bounds. ";a%,add%
    end
  end if
  Poke Var mem%(), add%, v%
End Sub

' Virtual Trs80 Memory Peek
Function TrsPeek(a%)
  Local add%
  add% = 20288 + a%
  if add% < 0 OR add% > 20288 then
    print "Peek out of bounds. ";a%,add%
    end
  end if
  TrsPeek = Peek(Var mem%(), add%)
End Function

' Virtual Trs80 Memory Level String Copy
Sub TrsLevelCopy a%, l$
  Local memi, memj
  For memi=1 to 249
    memj = asc(Mid$(l$,memi,1)) - 59
    TrsPoke a%, memj
    a%=a%+1
  Next
End Sub

' Screen Draw Poke
Sub TrsPokeScreen(a%,c%)
   Local add%
   add% = a% - TrsS
   If c% = 191 Then
     BOX int(add% mod TrsSx)*TrsSw, int(add% / TrsSx)*TrsCh, TrsCw, TrsCh, 1, RGB(white),RGB(white)
   Else
     BOX int(add% mod TrsSx)*TrsSw, int(add% / TrsSx)*TrsCh+TrsCh / 3, TrsCw, TrsCh / 3, 1, RGB(white),RGB(white)
   End If
End Sub

' Screen Set
Sub TrsSet x, y
  BOX int(x)*TrsPw, int(y)*TrsPh, TrsPw, TrsPh, 1, RGB(white),RGB(white)
End Sub

' Screen Reset
Sub TrsReset x, y
  BOX int(x)*TrsPw, int(y)*TrsPh, TrsPw, TrsPh, 1, RGB(black),RGB(black)
End Sub

'Screen Print Routines

' Print with Carriage Return
Sub TrsPrint s$
  Local x%,y%,p$,r$,l%
  r$ = s$
  x% = MM.INFO(HPOS)
  y% = MM.INFO(VPOS)
  l% = Int(x% / TrsCw)
  Do
    If Len(r$)+l% > TrsSx Then
      p$ = Mid$(s$,1,TrsSx)
      r$ = Mid$(s$,TrsSx+1)
    Else
      p$ = r$
      r$ = ""
    End If
    l% = 0
    Print @(x%,y%) p$ 'Allow CR so TABs reset!
    ' Check if on last line
    Inc y%, TrsCh
    If y% > TrsMy Then
      ' Scroll
      Blit 0, TrsCh, 0, 0, TrsMx, TrsMy
      Box 0, TrsMy-TrsCh+1, TrsMx, TrsCh,0,RGB(BLACK), RGB(BLACK)
      Inc y%, -TrsCh
    End If
    x% = 0
    Print @(x%,y%) "";
  Loop Until r$ = ""
End Sub

' D - Double Width
Sub TrsPrintD p%, s$
   Print @(TrsSw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) s$;
End Sub

' S - String
Sub TrsPrintS p%, s$
   Print @(TrsCw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) s$;
End Sub

' F - Float
Sub TrsPrintF p%, f
   Print @(TrsCw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) f;
End Sub

' I - Integer
Sub TrsPrintI p%, i%
   Print @(TrsCw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) i%;
End Sub

' sS - String String
Sub TrsPrintSS p%, s1$, s2$
   Print @(TrsCw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) s1$;s2$;
End Sub

' FS - Float String
Sub TrsPrintFS p%, f, s$
   Print @(TrsCw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) f;s$;
End Sub

' SF - String Float
Sub TrsPrintSF p%, s$, f
   Print @(TrsCw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) s$;f;
End Sub

' SFS - String Float String
Sub TrsPrintSFS p%, s$, f, s2$
   Print @(TrsCw * INT(p% mod TrsSx), TrsCh * INT(p% / TrsSx)) s$;f;s2$;
End Sub


' +++++++++
' Additions
' +++++++++

Sub DrawCharWindow x%, y%, w%, h% 'Coordinates of Window Box! Zero-Based
  local t1%,t2%,sx%, sy%
  ' Save Screen Under Window
  Blit Read #1, x%*TrsCw, y%*TrsCh, w%*TrsCw, h%*TrsCh
  ' Clear Screen Under Window
  BOX x%*TrsCw, y%*TrsCh, w%*TrsCw, h%*TrsCh, 0, RGB(black), RGB(black)
  ' Draw Borders
  t1%=y%*3+1
  t2%=(y%+h%)*3-2
  For sx%=x%*2 to (x%+w%)*2-1
    TrsSet(sx%, t1%)
    TrsSet(sx%, t2%)
  Next
  t1%=x%*2
  t2%=(x%+w%)*2-1
  For sy%=y%*3+1 to (y%+h%)*3-2
    TrsSet(t1%, sy%)
    TrsSet(t2%, sy%)
  Next
End Sub

Sub RestoreCharWindow x%, y%, w%, h%
  ' Clear Box - The Blit Write ANDs, so it must be cleared first...
  BOX x%*TrsCw, y%*TrsCh, w%*TrsCw, h%*TrsCh, 0, RGB(black), RGB(black)
  ' Restore Screen Under Window
  Blit Write #1, x%*TrsCw, y%*TrsCh
  ' Close
  Blit Close #1
End Sub

Sub DisplayFile fn$ 'Bare Filename without extension
  Local cw%     'Character Width
  Local ch%     'Character Height
  Local lfn$    'Local File Name
  Local l$      'Line
  Local k%      'Key (ascii)
  Local mx%     'Margin X
  Local my%     'Margix Y
  Local prv$    'PreVious filename
  Local nxt$    'NeXT filename
  Local jmp$    'JuMP filename
  Local wx%, wy%, wh%, ww%  ' Window
  Local c1%     'Center 1st line flag
  Local ln%     'Line Number
  ' Display File fn$
  Do
    ' Setup Counters and File Params
    cw% = 0
    ch% = 0
    c1% = 0
    prv$ = ""
    nxt$ = ""
    jmp$ = ""
    lfn$ = path$ + "text/" + fn$ + ".txt"
    ' Open File - If not found simply return
    On Error Skip
    Open lfn$ for Input as #1
    If MM.ERRNO <> 0 Then
      'Debugging
      TrsPrintS 960, "File "+lfn$+" not found"
      Pause 3000
      Exit Sub
    End If
    ' Read File Lines calculating size: cw%, ch%
    Do While not Eof(1)
      Line Input #1, l$
      If Mid$(l$,1,1) = "@" then Continue Do
      If Len(l$) > cw% Then cw% = Len(l$)
      Inc ch%
    Loop
    ' Calc Margins
    mx% = (TrsSx - cw%) / 2 - 1
    my% = (TrsSy - ch%) / 2 - 1 
    if (mx% < 1) then mx% = 1
    if (my% < 1) then my% = 1
    ' Draw Window
    wx% = mx%-1
    wy% = my%-1
    ww% = cw%+2
    wh% = ch%+3
    DrawCharWindow wx%, wy%, ww%, wh%
    ' Draw File Strings
    Seek #1, 0
    ln% = 1
    Do While not Eof(1)
      Line Input #1, l$
      ' Process Commands
      If mid$(l$,1,1) = "@" Then
        Select Case mid$(l$,2,1)
          Case "<"
            prv$ = mid$(l$,3)
          Case ">"
            nxt$ = mid$(l$,3)
          Case "*"
            jmp$ = mid$(l$,3)
          Case "T"
            c1% = 1
          Case "C"
            c1% = 1 'ca% = 1 Center All Removed, turns on Center 1st
        End Select
        Continue Do
      End If
      ' Print Line
      If c1% = 1 AND ln% = 1 Then
        TrsPrintS my%*TrsSx+(TrsSx-Len(l$))/2-1, l$
      Else
        TrsPrintS my%*TrsSx+mx%, l$
      End If
      ' Count Lines
      Inc my%
      Inc ln%
    Loop
    ' Done with file
    Close #1
    ' Print Menu
    l$ = "[ESC]"
    If jmp$ <> "" Then l$ = "[ESC] [*]"
    TrsPrintS my%*TrsSx+(TrsSx-Len(l$))/2-1, l$
    If prv$ <> "" Then TrsPrintS my%*TrsSx+mx%, "["+chr$(149)+"]"
    If nxt$ <> "" Then TrsPrintS my%*TrsSx+mx%+cw%-3, "["+chr$(148)+"]"
    ' Wait For User Command
    Do
      l$=Inkey$
      k% = Asc(l$)
      If k% = 27 Then Exit Do
      If k% = 42 And jmp$ <> "" Then Exit Do
      If k% = 130 And prv$ <> "" Then Exit Do
      If k% = 131 And nxt$ <> "" Then Exit Do
    Loop
    ' Restore the Window
    RestoreCharWindow wx%, wy%, ww%, wh%
    ' Process Commmand
    Select Case k%
      Case 27
        Exit Do ' Exit Sub
      Case 42
        fn$ = jmp$
      Case 130
        fn$ = prv$
      Case 131
        fn$ = nxt$
    End Select
  Loop
  ' All Done
End Sub


Sub PageFile fn$, cw%, ch% ' FileName, Char Width (Text), Char Height (Text)
' Leave room for Window Box + Menu!!!
  Local lfn$ ' Local file name
  Local l$   ' Line
  Local k%   ' Key code
  Local mx%  ' Margin X
  Local my%  ' Margin Y
  Local wx%, wy%, wh%, ww% ' Window - In Characters
  Local lc%  ' Load Count
  Local ll%  ' Last Line
  Local tl%  ' (lines) To Load
  Local cx%  ' Current X (constant)
  Local cy%  ' Current Y
  Local m$   ' Menu
  Local mp%  ' Menu Position
  Local lp1%, lp2%  ' Line Positions
  ' Open File
  lfn$ = path$ + "text/" + fn$ + ".txt"
  On Error Skip
  Open lfn$ for Input as #1
  If MM.ERRNO <> 0 Then Exit Sub
  ' Calc Margins
  mx% = Int((TrsSx - cw%) / 2)
  my% = Int((TrsSy - ch% -1) / 2)
  if (mx% < 1) then mx% = 1
  if (my% < 1) then my% = 1
  ' Calc Window
  wx% = mx%-1
  wy% = my%-1
  ww% = cw%+2
  wh% = ch%+3
  ' Draw Window
  DrawCharWindow wx%, wy%, ww%, wh%
  ' Setup Menu
  m$ = "[Home] [PgUp] ["+chr$(146) + "] [ESC] ["+ chr$(147)+"] [PgDn] [End]"
  mp% = (my%+ch%)*TrsSx+Int((TrsSx-Len(m$))/2)-1
  lp1% = (my%+ch%)*TrsSx+mx%
  lp2% = (my%+ch%)*TrsSx+wx%+ww%-5
  ' Setup Lines and Counts
  ll% = 0
  tl% = ch%
  ' Setup Display
  cx% = mx%
  cy% = my% 
  Do
    ' Clear Window Lines (tl% lines)
    BOX cx%*TrsCw, cy%*TrsCh, cw%*TrsCw, tl%*TrsCh, 0, RGB(black), RGB(black)
    ' Load Lines
    lc% = 1
    Do While lc% <= tl% AND not Eof(1)
      Line Input #1, l$
      TrsPrintS cy%*TrsSx+mx%, Mid$(l$,1,cw%)
      End If
      Inc cy%
      Inc lc%
      Inc ll%
    Loop
    ' Draw menu
    TrsPrintS mp%, m$
    TrsPrintS lp1%, "    "
    TrsPrintS lp2%, "    "
    TrsPrintS lp1%, str$(ll% - ch% + 1)
    TrsPrintS lp2%, str$(ll%)
    ' Wait For Command
    Do
      l$=Inkey$
      k% = Asc(l$)
      If k%=27 Or k%=43 Or k%=45 Or k%=136 Or k%=137 Or k%=134 Or k%=135 Or k%=128 Or k%=129 Then Exit Do
    Loop
    ' Process Command
    Select Case k%
      Case 27 'Escape
        Exit Do
      Case 137 'PageDown
        cy%=my%
        tl%=ch%
        If Eof(1) Then tl% = 0
      Case 43, 129 '+ Down
        If Eof(1) Then
          tl% = 0
        Else
          BLIT (wx%+1)*TrsCw, (wy%+2)*TrsCh, (wx%+1)*TrsCw, (wy%+1)*TrsCh, (ww%-2)*TrsCw, (wh%-4)*TrsCh
          Inc cy%, -1
          tl%=1
        End If
      Case 136 'PageUp
        Seek #1,0
        ll% = ll% - ch%*2
        If ll% < 0 Then ll% = 0
        For lc% = 1 to ll%
          Line Input #1, l$
        Next
        cy%=my%
        tl%=ch%
      Case 45, 128 '- Up
        Seek #1,0
        ll% = ll% - ch% -1
        If ll% < 0 Then ll% = 0
        For lc% = 1 to ll%
          Line Input #1, l$
        Next
        cy%=my%
        tl%=ch%
      Case 134 'Home
        Seek #1,0
        tl% = 0
        cy%=my%
        tl%=ch%
        ll%=0
      Case 135 'End
        'Find Last Line
        Do While Not Eof(1)
          Line Input #1, l$
          Inc ll%
        Loop
        'Back up a page
        ll% = ll% - ch%
        'Seek to new position
        Seek #1,0
        For lc% = 1 to ll%
          Line Input #1, l$
        Next
        'Setup Display
        cy%=my%
        tl%=ch%
    End Select
  Loop
  Close #1
  RestoreCharWindow wx%, wy%, ww%, wh%
End Sub

Sub DisplayCharacter
  Local Integer SM,RM,RS,AA,WM,WN,J,I
  Local cw%, ch% ' Initial char width, height (fixed)
  Local mx%, my% ' Margins
  Local wx%, wy%, wh%, ww% ' Window - In Characters
  Local cx%  ' Current X (constant)
  Local cy%  ' Current Y
  Local l$, k% ' Key Handling
  ' Calc Margins
  cw% = 44
  ch% = 10
  mx% = Int((TrsSx - cw%) / 2)
  my% = Int((TrsSy - ch% -1) / 2)
  if (mx% < 1) then mx% = 1
  if (my% < 1) then my% = 1
  ' Calc Window
  wx% = mx%-1
  wy% = my%-1
  ww% = cw%+2
  wh% = ch%+3
  ' Draw Window
  DrawCharWindow wx%, wy%, ww%, wh%
  ' Setup Display
  cx% = mx%
  cy% = my% 
  ' Get Character Data
  SM=TrsPeek(KB+10)
  RM=TrsPeek(KB+14)
  RS=TrsPeek(KB+13)
  AA=TrsPeek(KB+8)
  WM=TrsPeek(KB+7)-SM
  WN=TrsPeek(KB+17)
  IF SM<>0 THEN WN=6
  J=0
  IF TrsPeek(KB)=3 THEN
    J=1
  ELSE
    IF TrsPeek(KB+0)=5 THEN J=2
  END IF
  GOSUB IN9800
  ' Display Character
  l$ = "Character Summary For "+NM$
  TrsPrintS cy%*TrsSx+(TrsSx-Len(l$))/2-1, l$
  Inc cy%
  FOR I=1 TO 6
    TrsPrintS cy%*TrsSx+mx%,NC$(I)
    TrsPrintF cy%*TrsSx+mx%+12, TrsPeek(KA-97+I)
    Select Case I
      Case 1:
        TrsPrintS cy%*TrsSx+mx%+22, "Arrows       "+STR$(RS)
      Case 2:
        TrsPrintS cy%*TrsSx+mx%+22, "Magic Arrows "+STR$(RM)
      Case 3:
        TrsPrintS cy%*TrsSx+mx%+22, "Shield       "+NS$(J)
      Case 4:
        TrsPrintS cy%*TrsSx+mx%+22, "Salves       "+STR$(TrsPeek(KB+11))
      Case 5:
        TrsPrintS cy%*TrsSx+mx%+22, "Elixirs      "+STR$(TrsPeek(KB+6))
      Case 6:
        TrsPrintS cy%*TrsSx+mx%+22, "Weight       "+STR$(TrsPeek(KB+12))
    End Select
    Inc cy%
  NEXT I
  TrsPrintS cy%*TrsSx+mx%,    "Experience   "+STR$(Int(TrsPeek(KB+1)+256*TrsPeek(KB+2)+65536*TrsPeek(KB+3)))
  TrsPrintS cy%*TrsSx+mx%+22, "Silver       "+MID$(STR$(MO),1,9)
  Inc cy%
  l$ = "Weapon       "+NW$(WN)
  IF WN=6 THEN l$ = l$+" + "+STR$(TrsPeek(KB+10))
  TrsPrintS cy%*TrsSx+mx%, l$
  Inc cy%
  l$ = "Armor        "+NA$(AA)
  IF TrsPeek(KB+16)>0 THEN l$ = l$+" + "+STR$(TrsPeek(KB+16))
  TrsPrintS cy%*TrsSx+mx%, l$
  Inc cy%
  ' Menu
  l$ = "[ESC]"
  TrsPrintS cy%*TrsSx+(TrsSx-Len(l$))/2-1, l$
  ' Wait For Continue
  Do
    l$=Inkey$
    k% = Asc(l$)
    If k%=27 Then Exit Do
  Loop
  RestoreCharWindow wx%, wy%, ww%, wh%
End Sub

Sub DisplayTreasures
  Local I
  Local cw%, ch% ' Initial char width, height (fixed)
  Local mx%, my% ' Margins
  Local wx%, wy%, wh%, ww% ' Window - In Characters
  Local cx%  ' Current X (constant)
  Local cy%  ' Current Y
  Local l$, k% ' Key Handling
  ' Calc Margins
  cw% = 32
  ch% = 12
  mx% = Int((TrsSx - cw%) / 2)
  my% = Int((TrsSy - ch% -1) / 2)
  if (mx% < 1) then mx% = 1
  if (my% < 1) then my% = 1
  ' Calc Window
  wx% = mx%-1
  wy% = my%-1
  ww% = cw%+2
  wh% = ch%+3
  ' Draw Window
  DrawCharWindow wx%, wy%, ww%, wh%
  ' Setup Display
  cx% = mx%
  cy% = my% 
  ' Display Treasures
  l$ = "Treasure* Inventory"
  TrsPrintS cy%*TrsSx+(TrsSx-Len(l$))/2-1, l$
  Inc cy%
  TrsPrintS cy%*TrsSx+mx%, "Treasure Count Treasure Count"
  Inc cy%
  FOR I=1 TO 10
    TrsPrintF cy%*TrsSx+mx%+2,I
    TrsPrintF cy%*TrsSx+mx%+10, TrsPeek(KA-81+I)
    TrsPrintF cy%*TrsSx+mx%+17,I+10
    TrsPrintF cy%*TrsSx+mx%+25, TrsPeek(KA-81+I+10)
    Inc cy%
  NEXT I
  ' Menu
  l$ = "[ESC] [*]"
  TrsPrintS cy%*TrsSx+(TrsSx-Len(l$))/2-1, l$
  ' Wait For Continue
  Do
    l$=Inkey$
    k% = Asc(l$)
    If k%=27 Or k% = 42 Then Exit Do
  Loop
  RestoreCharWindow wx%, wy%, ww%, wh%
  If k% = 42 Then
    k% = TrsPeek(KA)
    If k% < 1 OR k% > 4 Then k% = 1
    DisplayTreasure k%,1
  End If
End Sub

Sub DisplayRoom lvl%, rn%
  Local fn$ = "ROOM."+STR$(lvl%)+"."+STR$(rn%)
  rooms(rn%) = 1
  DisplayFile fn$
End Sub

Sub DisplayRoomByMode lvl%, rn%
  If legendMode = 1 OR (legendMode = 2 AND rooms(rn%) = 0) Then
    DisplayRoom lvl%, rn%
  End If
End Sub

Sub DisplayMovementCommands
  DisplayFile "COMMAND.MOVEMENT"
End Sub

Sub DisplaySpecialCommands
  DisplayFile "COMMAND.SPECIAL"
End Sub

Sub DisplayAttackCommands
  DisplayFile "COMMAND.ATTACK"
End Sub

Sub DisplayMonster m$
  ' Store Monster Display
  Local I
  I = 1
  Do
    If monster$(I) = "" Then
      monster$(I) = m$
      Exit Do
    ElseIf monster$(I) = m$ Then
      Exit Do
    End If
    Inc I
  Loop Until I = 20
  'Display 
  DisplayFile "MONSTER." + m$
End Sub

Sub DisplayMonsterByMode m$
  If legendMode = 1 Then
    DisplayMonster m$
  Else
    ' Check if Monster seen
    Local I, F
    I = 1
    F = 0
    Do
      If monster$(I) = m$ Then
        F = 1
        Exit Do
      ElseIf monster$(I) = "" Then
        Exit Do
      End If
      Inc I
    Loop Until I = 20
    If F = 0 Then DisplayMonster m$
  End If
End Sub

Sub DisplayTreasure lvl%, tr%
  Local fn$ = "TREASURE."+STR$(lvl%)+"."+STR$(tr%)
  treasure(tr%) = 1
  DisplayFile fn$
End Sub

Sub DisplayTreasureByMode lvl%, tr%
  If legendMode = 1 OR (legendMode = 2 AND treasure(tr%) = 0) Then
    DisplayTreasure lvl%, tr%
  End If
End Sub

Sub DisplayTrap lvl%, t%
'  TrsPrintS 960, t$+"    "
  Local fn$ = "TRAP."+STR$(lvl%)+"."+STR$(t%)
  DisplayFile fn$
End Sub

Sub DisplayMaster
  DisplayFile "KEY.GEMSJEWELRY"
End Sub

Sub DisplayMagic
  DisplayFile "MAGIC.1"
End Sub

Sub DisplayFunctionKey k%
  Local l%,x%,y%,p%
  x% = MM.INFO(HPOS)
  y% = MM.INFO(VPOS)
  l% = TrsPeek(KA)
  If l% < 1 OR l% > 4 Then l% = 1
  Select Case k%
    Case 145 'F1
      PageFile "INSTR", 62, 13 'Allow 1 row for KBD Instructions
    Case 146 'F2
      DisplayMovementCommands
    Case 147 'F3
      If TaMode = 1 OR KR < 1 OR KR > 60 Then
        DisplayRoom l%, 1
      Else
        DisplayRoom l%,KR
      End If
    Case 148 'F4
      If TaMode = 1 OR lastMonster$ = "" Then
        DisplayMonster "ANT MAN"
      Else
        DisplayMonster lastMonster$
      End If
    Case 149 'F5
      If TaMode = 1 OR LastTrap = -1 Then
        Select Case l%
          Case 1: DisplayTrap l%,1
          Case 2: DisplayTrap l%,9
          Case 3: DisplayTrap l%,8
          Case 4: DisplayTrap l%,1
        End Select
      Else
        DisplayTrap l%, LastTrap
      End If
    Case 150 'F6
      If TaMode = 1 OR LastTreasure = 0 Then
        DisplayTreasure l%,1
      Else
        DisplayTreasure l%, LastTreasure
      End If
    Case 151 'F7
      DisplayMaster
    Case 152 'F8
      DisplayMagic
    Case 153 'F9
      Inc legendMode
      If legendMode > 3 Then legendMode = 1
      DisplayLegend
    Case 154 'F10 Testing
      DisplayCharacter
    Case 155 'F11 Testing
      DisplayTreasures
    Case Else
  End Select
  Print @(x%,y%) ""
End Sub

Sub DisplayWelcome
  Mode 1
  Colour RGB(Green), RGB(Black)
  Cls
  Local sx%, sy%, lfn$, l$, k%, m$, c%, x%
  sx% = mm.hres
  sy% = mm.vres
  ' Open Welcome Text
  lfn$ = path$ + "text/welcome.txt"
  On Error Skip
  Open lfn$ for Input as #1
  If MM.ERRNO <> 0 Then
    Print "Welcome text not found."
    Pause 5000
    End
  End If
  ' Draw Text
  Do While not Eof(1)
    Line Input #1, l$
    If Mid$(l$,1,1)="@" Then
      m$ = Mid$(l$,2,1)
      If m$ = "D" Then
        Font 1,2
        Color Rgb(White), Rgb(Black)
        Print tab(4);Mid$(l$,3)
        Color Rgb(Green), Rgb(Black)
        Font 1,1
      End If
    Else
      Print tab(7);l$
    End If
  Loop
  Close #1
  ' Draw Border
  c% = RGB(RED)
  For x% = 1 to 33
    Line x%,35-x%,x%,sy%-x%,1,c%
    Line sx%-x%,35-x%,sx%-x%,sy%-x%,1,c%
    Select Case x%
      Case 9
        c% = Rgb(Green)
      Case 17
        c% = Rgb(Blue)
      Case 25
        c% = Rgb(Yellow)
    End Select
  Next
  ' Wait For Continue
  Do
    l$=Inkey$
    k% = Asc(l$)
    If k%=27 Then Exit Do
  Loop
End Sub

' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
' TEMPLE OF APSHAI, INNKEEPER - TRS80, TRSDOS, COPYRIGHT 1979, AUTOMATED SIMULATIONS, REV 2
' Maximite 2 Conversion - Craig S. Buchanan 2021-Dec-27
' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

' Original Machine Language USR Routine Stored in MC$="". At location 27323 in testing. 19 bytes.
'
' CD,7F,0A        Call 0A7F      GetPar. Puts argument in HL.
' 06,F9           LD B, F9       Levels are 9 strings of F9 (249) bytes.
' 11,99,B9        LD DE, B999    Overwritten by A$ address before being called!
' 1A        LOOP: LD A,(DE)      Get byte at (DE)
' D6,3A           SUB 3A         Subtracts 3A (58) from A
' 77              LD (HL),A      Store byte at (HL)
' 23              INC HL
' 13              INC DE
' 10,F8           DJNZ F8 (LOOP) Decrement B. If B<>0 Goto LOOP
' 58              Filler
' 58              Filler
'
' Levels are stored as 9 Strings of length 249 for a total of 2,250 bytes.
'
' Original USR Handling Code
'5 CLEAR800:DEFINTD,I-Y:MC$="<code>":MH=PEEK(VARPTR(MC$)+2):ML=PEEK(VARPTR(MC$)+1):MC=MH*256+ML:ONERRORGOTO6:DEFUSR0=MC:MC#=1:GOTO10
'6 IFERR=44THENPOKE16526,ML:POKE16527,MH:RESUME10ELSESTOP
'

DisplayWelcome

IN:
  TrsClear
  taMode = 1
  Restore
'1 REM TEMPLE OF APSHAI, INNKEEPER - TRS80, TRSDOS, COPYRIGHT 1979, AUTOMATED SIMULATIONS, REV 2  
'5 CLEAR800:DEFINTD,I-Y:MC$="<CODE!!>":MH=PEEK(VARPTR(MC$)+2):ML=PEEK(VARPTR(MC$)+1):MC=MH*256+ML:ONERRORGOTO6:DEFUSR0=MC:MC#=1:GOTO10
'6 IFERR=44THENPOKE16526,ML:POKE16527,MH:RESUME10ELSESTOP
'10 DIMNA$(5),W(5),P(5),NC$(6),CH(6),SD(2),SP(2),SW(2),NW$(6),WW(5),WP(5),WD(5),WR(5),DS(1,1),DW(5,1),R$(6),RR(6),NS$(2)
'20 C%=15360:KA=-1*(65536-46864):Q=60:KB=KA+2251'***DISK
  CS=15360
  KA=-1*(65536-46864)
  Q=60
  KB=KA+2251'***DISK
'30 J8=248:J0=59:J7=249:JQ=1
  J8=248
  J0=59
  J7=249
  JQ=1
'50 DATA"NONE",0,0,"LEATHER",150,30,"RING MAIL",350,100,"CHAIN MAIL",500,150,"PARTIAL PLATE",750,250,"FULL PLATE",1000,1000,"INTELLIGENCE","INTUITION","EGO","STRENGTH","CONSTITUTION","DEXTERITY"
  DATA "NONE",0,0,"LEATHER",150,30,"RING MAIL",350,100,"CHAIN MAIL",500,150,"PARTIAL PLATE",750,250
  DATA "FULL PLATE",1000,1000
  DATA "INTELLIGENCE","INTUITION","EGO","STRENGTH","CONSTITUTION","DEXTERITY"
'60 FORI=0TO5:READNA$(I),W(I),P(I):W(I)=INT(W(I)/16+.5):NEXTI
  FOR I= 0 TO 5
    READ NA$(I),W(I),P(I) 
    W(I)=INT(W(I)/16+.5)
  NEXT I
'70 W(0)=0:FORI=1TO6:READNC$(I):NEXTI
  W(0)=0
  FOR I=1TO 6
    READ NC$(I)
  NEXT I
'80 DATA"DAGGER",1,5,5,3,7,12,"SHORTSWORD",2,14,6,8,9,12,"BROADSWORD",3,18,7,10,9,13,"HAND-AND-A-HALF SWORD",6,35,8,16,9,14,"GREAT SWORD",9,70,10,15,7,15,"MAGIC SWORD"
  DATA "DAGGER",1,5,5,3,7,12,"SHORTSWORD",2,14,6,8,9,12,"BROADSWORD",3,18,7,10,9,13
  DATA "HAND-AND-A-HALF SWORD",6,35,8,16,9,14,"GREAT SWORD",9,70,10,15,7,15
  DATA "MAGIC SWORD"
'85 FORI=1TO5:READNW$(I),WW(I),WP(I),WD(I),WR(I):FORJ=0TO1:READDW(I,J):NEXTJ:NEXTI:READNW$(6)
  FOR I=1TO 5
    READ NW$(I),WW(I),WP(I),WD(I),WR(I)
    FOR J=0TO 1
      READ DW(I,J)
    NEXT J
  NEXT I
  READ NW$(6)
'90 DATA6,3,10,8,15,12,5,15,6,17
  DATA 6,3,10,8,15
  DATA 12,5,15,6,17
'95 FORI=1TO2:READSW(I),SD(I),SP(I):FORJ=0TO1:READDS(I-1,J):NEXTJ:NEXTI
  FOR I=1TO 2
    READ SW(I),SD(I),SP(I)
    FOR J=0TO 1
      READ DS(I-1,J)
    NEXT J
  NEXT I
'99 LS=0:SM=0
  LS=0:SM=0
'100 DATA"I'd not part with these fine goods for that pittance!    Mayhap for",5,"Not so cheap my friend! But for thee just",10,"Blackheart! Thou takest the food from my children's mouths! I'll not settle for less than",5
  DATA "I'd not part with these fine goods for that pittance!    Mayhap for",5
  DATA "Not so cheap my friend! But for thee just",10
  DATA "Blackheart! Thou takest the food from my children's mouths! I'll not settle for less than",5
'110 DATA"Well, life is short and thy arse long! Whay say thee to",2,"Hmm...but such fine workmanshup.  I could not part with this for less than",10,"A pox on thee! But I'd take",3
  DATA "Well, life is short and thy arse long! Whay say thee to",2
  DATA "Hmm...but such fine workmanship.  I could not part with this for less than",10
  DATA "A pox on thee! But I'd take",3
'130 RN=6:RT=0:FORI=1TORN:READR$(I),RR(I):RT=RT+RR(I):NEXTI
  RN2=6:RT=0
  FOR I=1TO RN2
    READ R$(I),RR(I)
    RT=RT+RR(I)
  NEXT I
'140 DATA"NONE","SMALL","LARGE":FORI=0TO2:READNS$(I):NEXTI
  DATA "NONE","SMALL","LARGE"
  FOR I=0 TO 2
    READ NS$(I)
  NEXT I
'790 IFMEM>8000THENCLS:PRINT"YOU CAN'T PLAY UNLESS YOU PROTECT MEMORY SEE SPECIAL INSTRUCTIONS":STOP
'800 GOTO1000
IN800:
  GOTO IN1000

'900 RANDOM:CLS:PRINTCHR$(23):PRINT@276,"DUNJONQUEST":PRINT@140,"THE TEMPLE OF APSHAI"  
IN900:
  Mode32x16
'  MODE 9,8
'  FONT 3,2
'  CLS
  TrsPrintD 276,"DUNJONQUEST"
  TrsPrintD 140,"THE TEMPLE OF APSHAI"
'910 PRINT@594,"COPYRIGHT 1979":PRINT@714,"AUTOMATED  SIMULATIONS"
  TrsPrintD 594,"COPYRIGHT 1979"
  TrsPrintD 714,"AUTOMATED  SIMULATIONS"
'915 J=191:FORI=0TO63STEP2:POKEC%+I,J:NEXTI:FORI=960TO1023STEP2:POKEC%+I,J:NEXTI:FORI=64TO896STEP64:POKEC%+I,J:NEXTI:FORI=126TO958STEP64:POKEC%+I,J:NEXTI
  J=191
  FOR I=0TO 63 STEP 2
    TrsPokeScreen CS+I,J
  NEXT I
  FOR I=960TO 1023 STEP 2
    TrsPokeScreen CS+I,J
  NEXT I
  FOR I=64TO 896 STEP 64
    TrsPokeScreen CS+I,J
  NEXT I
  FOR I=126TO 958 STEP 64
    TrsPokeScreen CS+I,J
  NEXT I
'917 K=140:FORI=834TO892STEP2:POKEC%+I,K:NEXTI
  K=140
  FOR I=834TO 892 STEP 2
    TrsPokeScreen CS+I,K
  NEXT I
'920 PRINT@904,"HIT ANY KEY TO CONTINUE";
  TrsPrintD 904,"HIT ANY KEY TO CONTINUE"
'930 POKEKB,0:I=RND(5):A$=INKEY$:IFLEN(A$)=0GOTO930
IN930:
  TrsPoke KB+0,0
  DO WHILE INKEY$="":I=TrsRnd(5):LOOP
'990 RETURN
  RETURN
 
'1000 IFPEEK(KB-12)<>123THENGOSUB900
IN1000:
  IF TrsPeek(KB-12)<>123 THEN GOSUB IN900
'1001 CLS:WC=5:PRINT"THUS QUOTH THE INNKEEPER:"
  Mode64x16
  WC=5
  TrsPrint "THUS QUOTH THE INNKEEPER:"
'1003 IFPEEK(KB-12)=123THENINPUT"Wouldst thou know what treasures thou hast";A$:IFLEFT$(A$,1)="Y"THENGOSUB9920
IN1003:
  IF TrsPeek(KB-12)=123 THEN
    A$ = TrsInput("Wouldst thou know what treasures thou hast")
    IF LEFT$(A$,1)="Y" THEN
      GOSUB IN9920
    END IF
  END IF
'1004 IFPEEK(KB-12)=123THENGOSUB4500
  IF TrsPeek(KB-12)=123 THEN GOSUB IN4500
'1005 IFPEEK(KB-12)=123THENPRINT"I trust thine was a pleasant sojourn within the temple.":PRINT"Art but passing through the inn and wish to return";:INPUTA$:IFLEFT$(A$,1)="Y"THENGOSUB9900:INPUT"How many silver pieces hast thou";MO:OM=MO:GOSUB2600:GOTO1205
  IF TrsPeek(KB-12)=123 THEN
    TrsPrint "I trust thine was a pleasant sojourn within the temple."
    PRINT "Art but passing through the inn and wish to return";
    A$ = TrsInput("")
    IF LEFT$(A$,1)="Y" THEN 
      GOSUB IN9900
'      INPUT "How many silver pieces hast thou";MO
      MO = TrsInputD("How many silver pieces hast thou")
      OM=MO
      GOSUB IN2600
      GOTO IN1205
    END IF
  END IF
'1009 POKEKB-12,0
  TrsPoke KB-12,0
'1010 PRINT"Hail and well met! Shall I find the a goodly character, or hastthou brought one with thee?  Prithee, say YEA if I should.":INPUTC$:IFLEFT$(C$,1)="Y"THENGOSUB2200:GOTO1200
  TrsPrint "Hail and well met! Shall I find the a goodly character, or hastthou brought one with thee?  Prithee, say YEA if I should."
  A$ = TrsInput("")
  IF LEFT$(A$,1)="Y" THEN
    GOSUB IN2200
    GOTO IN1200
  END IF
'1020 INPUT"HAST THOU A CHARACTER ON DISK ";C$:IF LEFT$(C$,1)<>"Y"THENGOSUB2400:GOTO1200
  A$ = TrsInput("HAST THOU A CHARACTER ON DISK ")
  IF LEFT$(A$,1)<>"Y" THEN
    GOSUB IN2400
    GOTO IN1200
  END IF

'1030 INPUT"WHAT BE THE FELLOW'S NAME ";C$:PRINT"READING IN CHARACTERISTICS FOR ";C$:ONERRORGOTO1032:OPEN"I",1,C$:GOTO1040
IN1030:
  A$ = TrsInput("WHAT BE THE FELLOW'S NAME ")
  TrsPrint "READING IN CHARACTERISTICS FOR "+A$
  ON ERROR SKIP
  OPEN path$ + "Characters/" + A$+".CHR" FOR INPUT AS #1
  IF MM.ERRNO<>0 THEN GOTO IN1032
  GOTO IN1040

'1032 IFERR=106THENPRINT"NO SUCH FELLOW AS ";C$;" HERE.":RESUME1030
IN1032:
  TrsPrint "NO SUCH FELLOW AS "+A$+" HERE."
  GOTO IN1030

'1040 FORI=61TO96:INPUT#1,J:POKEKA-I,J:NEXTI:INPUT#1,J:PRINT"LAST ON LEVEL ";J
IN1040:
  FOR I=61 TO 96
    INPUT #1,J
  TrsPoke KA-I,J
  NEXT I
  INPUT #1,J
  TrsPrint "LAST ON LEVEL "+STR$(J)
'1050 FORI=0TO37:INPUT#1,J:POKEKB-12+I,J:NEXTI:CLOSE:POKEKB-12,123:GOTO1003
  FOR I=0 TO 37
    INPUT #1,J
  TrsPoke KB-12+I,J
  NEXT I
  CLOSE 1
  TrsPoke KB-12,123
  GOTO IN1003
 
'1200 OM=MO:INPUT"CHARACTER NAME";NM$:NM%=0:FORI=1TOLEN(NM$):IFMID$(NM$,I,1)<"A"ORMID$(NM$,I,1)>"Z"THENNM%=-1
IN1200:
  OM=MO
  NM$ = TrsInput("CHARACTER NAME")
  NM2=0
  FOR I=1 TO LEN(NM$)
    IF MID$(NM$,I,1)<"A" OR MID$(NM$,I,1)>"Z" THEN NM2=-1
'1201 NEXTI:IFNM%=-1THENPRINT"KNAVE ! GIVE ME AN ADVENTURER'S NAME.":GOTO1200ELSEIFMO>0THENGOSUB2600
  NEXT I
  IF NM2=-1 THEN
    TrsPrint "KNAVE ! GIVE ME AN ADVENTURER'S NAME."
    GOTO IN1200
  ELSE
    IF MO>0 THEN GOSUB IN2600
  END IF
'1205 IFMO>0THENGOSUB2800
IN1205:
  IF MO>0 THEN GOSUB IN2800
'1206 IFPS=0THENWP=WP+SP:SP=0
  IF PS=0 THEN
    WP2=WP2+SP2
    SP2=0
  END IF
'1210 PB=PB-WP-INT(PEEK(KB+15)/2):IFWP>0THENPA=PA+WP-1ELSEIFWP<0THENPA=PA+WP+1
  PB=PB-WP2-INT(TrsPeek(KB+15)/2)
  IF WP2>0 THEN 
    PA=PA+WP2-1
  ELSE 
    IF WP2<0 THEN PA=PA+WP2+1
  END IF
'1211 IFMO=0GOTO1215ELSEGOSUB2000
  IF MO=0 GOTO IN1215 ELSE GOSUB IN2000
'1212 IFMO=0GOTO1215ELSEGOSUB3000
  IF MO=0 GOTO IN1215 ELSE GOSUB IN3000
'1213 IFMO=0GOTO1215ELSEGOSUB3200
  IF MO=0 GOTO IN1215 ELSE GOSUB IN3200
'1215 SE=100:IFPB<0THENPB=0
IN1215:
  SE=100
  IF PB<0 THEN PB=0
'1218 INPUT"MONSTER SPEED (SLOW,MEDIUM, OR FAST)";C$:IFLEFT$(C$,1)="S"THEN
'SE=250ELSEIFLEFT$(C$,1)="F"THENSE=60ELSESE=120
  C$ = TrsInput("MONSTER SPEED (SLOW,MEDIUM, OR FAST)")
  IF LEFT$(C$,1)="S" THEN
    SE=250
  ELSE
    IF LEFT$(C$,1)="F" THEN
      SE=60 
    ELSE
      SE=120
    END IF
  END IF
'1219 POKEKB+9,SE:POKEKB+11,HS:POKEKB+13,RS:POKEKB+14,RM:POKEKB+12,WC:POKEKB+7,WM
  TrsPoke KB+9,SE
  TrsPoke KB+11,HS
  TrsPoke KB+13,RS
  TrsPoke KB+14,RM
  TrsPoke KB+12,WC
  TrsPoke KB+7,WM
'1220 POKEKB+24,CA:POKEKB+6,HN:POKEKB+11,HS
  TrsPoke KB+24,CA
  TrsPoke KB+6,HN
  TrsPoke KB+11,HS
'1221 POKEKB+23,AS:POKEKB+7,WM:POKEKB+18,PB:POKEKB+19,PA:POKEKB+21,IT:POKEKB+22,EG:POKEKB+4,SP:
'POKEKB+25,DX:J=INT(EX#/65536):POKEKB+3,J:JJ=INT(EX#/256-J*256):
'POKEKB+2,JJ:POKEKB+1,EX#-256*JJ-65536*J
  TrsPoke KB+23,AS2
  TrsPoke KB+7,WM
  TrsPoke KB+18,PB
  TrsPoke KB+19,PA
  TrsPoke KB+21,IT
  TrsPoke KB+22,EG
  TrsPoke KB+4,SP2
  TrsPoke KB+25,DX
  J=INT(EX/65536)
  TrsPoke KB+3,J
  JJ=INT(EX/256-J*256)
  TrsPoke KB+2,JJ
  TrsPoke KB+1,EX-256*JJ-65536*J
'1223 WC=5+WW(PEEK(KB+17))+W(AA)+SW(INT(PEEK(KB)/2))+1+(RS+RM)/10
  WC=5+WW(TrsPeek(KB+17))+W(AA)+SW(INT(TrsPeek(KB)/2))+1+(RS+RM)/10
'1225 POKEKB+13,RS:POKEKB+14,RM:POKEKB+12,WC:POKEKB+20,CH(1)
  TrsPoke KB+13,RS
  TrsPoke KB+14,RM
  TrsPoke KB+12,WC
  TrsPoke KB+20,CH(1)
'1230 POKEKB+10,SM
  TrsPoke KB+10,SM
'1260 POKEKB,PS
  TrsPoke KB,PS
'1270 POKEKB+8,AA:IFPEEK(KB-12)=123GOTO1400
  TrsPoke KB+8,AA
  IF TrsPeek(KB-12)=123 GOTO IN1400
'1315 J=LEN(NM$):IFJ>11THENJ=11
  J=LEN(NM$)
  IF J>11 THEN J=11
'1320 FORI=1TOJ:POKEKB-12+I,ASC(MID$(NM$,I,1)):NEXTI
  FOR I=1 TO J
    TrsPoke KB-12+I,ASC(MID$(NM$,I,1))
  NEXT I
'1330 IFJ<11THENFORI=J+1TO11:POKEKB-12+I,ASC(" "):NEXTI
  IF J<11 THEN
    FOR I=J+1 TO 11
      TrsPoke KB-12+I,ASC(" ")
    NEXT I
  END IF
'1400 GOSUB4500
IN1400:
  GOSUB IN4500
'1500 POKEKB-12,123
  TrsPoke KB-12,123
'1600 INPUT"ENTER DUNGEON LEVEL";L$:LV=VAL(L$)'***DISK
  LL$ = TrsInput("ENTER DUNGEON LEVEL")
  LV=VAL(LL$)'***DISK
'1605 IFPEEK(KA)=LVGOTO1670
  IF TrsPeek(KA)=LV GOTO IN1670
'1607 N=PEEK(KB-11)
  N=TrsPeek(KB-11)
'1610 AN$="LEVEL"+L$:OPEN"I",1,AN$'***DISK
  AN$=Path$+"Levels/" + "LEVEL"+LL$
' OPEN "I",1,AN$'***DISK
  OPEN AN$ FOR INPUT AS #1
'1620 INPUT#1,A$:LQ=ASC(LEFT$(A$,1))-J0:PRINT"LEVEL:"LQ'***DISK
'  INPUT #1,A$
  A$=INPUT$(250,#1)
  LQ=ASC(LEFT$(A$,1))-J0
  TrsPrint "LEVEL: "+STR$(LQ) '***DISK
'1625 K=0
  K=0
'1630 IFLV=LQTHENGOSUB1950
  IF LV=LQ THEN GOSUB IN1950
'1640 FORI=2TO9
  FOR I=2 TO 9
'1642 IFLEN(A$)=249GOTO1650
    IF LEN(A$)=250 GOTO IN1650
'1644 PRINT"*** POSSIBLE DATA READ ERROR - BLOCK LENGTH ="LEN(A$);" ***"
      TrsPrint "*** POSSIBLE DATA READ ERROR - BLOCK LENGTH ="+STR$(LEN(A$))+" ***"
      CLOSE #1
    END
'1650 INPUT#1,A$:IFLV=LQTHENGOSUB1950'***DISK
IN1650:
    A$=INPUT$(250,#1)
    IF LV=LQ THEN GOSUB IN1950 '***DISK
'1665 NEXTI
  NEXT I
  CLOSE #1
'1666 REM ***DISK
  REM ***DISK
'1667 POKEKB-11,N
  TrsPoke KB-11,N
'1670 POKEKB-12,123:PRINT"LEVEL COMPLETE - LOADING DUNJONMASTER":RUN"DM"'***DISK
IN1670:
  TrsPoke KB-12,123
  TrsPrint "LEVEL COMPLETE - LOADING DUNJONMASTER"
  Goto DM
'1900 STOP

'1950 REM
IN1950:
'1955 POKEMC+6,PEEK(VARPTR(A$)+1):POKEMC+7,PEEK(VARPTR(A$)+2)
'1960 IFMC#=1THENMC!=USR0(KA+K)ELSEMC!=USR(KA+K)
'  IF MC!=1 THEN MC!=USR0(KA+K) ELSE MC!=USR(KA+K)
  TrsLevelCopy KA+K, A$
'1965 REM
'1970 K=K+J7:RETURN
  K=K+J7
  RETURN
 
'2000 INPUT"WILT THOU BUY NEW ARMOR";A$:IFLEFT$(A$,1)="Y"THENFORI=1TO900:NEXTI:CLS:PRINT"TYPE"TAB(25);"WEIGHT"TAB(35);"OFFERED PRICE":ELSERETURN
IN2000:
  A$ = TrsInput("WILT THOU BUY NEW ARMOR")
  IF LEFT$(A$,1)="Y" THEN
    Pause 1000
    TrsCls
    PRINT "TYPE" TAB(25); "WEIGHT" TAB(35); "OFFERED PRICE";
    TrsPrint ""
  ELSE 
    RETURN
  END IF
'2010 FORI=1TO5:PRINTNA$(I);TAB(25);W(I);TAB(35);P(I):NEXTI
  FOR I=1 TO 5
    PRINT NA$(I);TAB(25);W(I);TAB(35);P(I);
    TrsPrint ""
  NEXT I
'2020 INPUT"WHAT SORT OF ARMOR WOULDST THOU WEAR";A$:IFLEFT$(A$,1)="N"THENAM=0:AA=0:RETURN
IN2020:
  A$ = TrsInput("WHAT SORT OF ARMOR WOULDST THOU WEAR")
  IF LEFT$(A$,1)="N" THEN
    AM=0
    AA=0
    RETURN
  END IF
'2021 FORI=1TO5:IFLEFT$(A$,2)=LEFT$(NA$(I),2)THENN=I:GOTO2026
  FOR I=1 TO 5
    IF LEFT$(A$,2)=LEFT$(NA$(I),2) THEN
      N=I
      GOTO IN2026
    END IF
'2022 NEXTI:PRINT"I HAVE NOT "A$;" FOR SALE":GOTO2020
  NEXT I
  TrsPrint "I HAVE NOT "+A$+" FOR SALE"
  GOTO 2020
'2024 IFPEEK(KB-12)=123THENWC=WC-W(AA)
  IF TrsPeek(KB-12)=123 THEN WC=WC-W(AA)
'2026 LO=.3*P(N):AK=P(N):A1=AK:GOSUB4000:IFOO=0GOTO2020
IN2026:
  LO=.3*P(N):AK=P(N)
  A1=AK
  GOSUB IN4000
  IF OO=0 THEN GOTO IN2020
'2030 AA=N:MO=MO-OO:WC=WC+W(N):POKEKB+16,0
IN2030:
  AA=N
  MO=MO-OO
  WC=WC+W(N)
  TrsPoke KB+16,0
'2040 PRINT"THOU HAST"MO;"SILVER PIECES LEFT IN THY PURSE":RETURN
  TrsPrint "THOU HAST "+STR$(MO)+" SILVER PIECES LEFT IN THY PURSE"
  RETURN

'2190 J=RND(6)+RND(6)+RND(6):RETURN
IN2190:
  J=TrsRnd(6)+TrsRnd(6)+TrsRnd(6)
  RETURN

'2200 HB=0:SC=0:FORI=1TO6:GOSUB2190:CH(I)=J:SC=SC+J:NEXTI:IFSC<60ORCH(4)<8ORCH(5)<7GOTO2200
IN2200:
  HB=0
  SC=0
  FOR I=1 TO 6
    GOSUB IN2190
    CH(I)=J
    SC=SC+J
  NEXT I
  IF SC<60 OR CH(4)<8 OR CH(5)<7 THEN GOTO IN2200
'2205 EX#=0:RS=0:RM=0:AA=0:PS=0:POKEKB+17,0:SP=0:WP=1:AM=0:SM=0:HS=0:HN=0:POKEKB+15,1
  EX=0
  RS=0
  RM=0
  AA=0
  PS=0
  TrsPoke KB+17,0
  SP2=0
  WP2=1
  AM=0
  SM=0
  HS=0
  HN=0
  TrsPoke KB+15,1
'2210 CLS:PRINT"THY QUALITIES:":FORI=1TO6:PRINTNC$(I);TAB(25);CH(I):NEXTI
  TrsCls
  TrsPrint "THY QUALITIES:"
  FOR I=1 TO 6
    PRINT NC$(I);TAB(25);CH(I);
    TrsPrint ""
  NEXT I
'2215 FORI=1TO6:POKEKA-97+I,CH(I):NEXTI
  FOR I=1 TO 6
    TrsPoke KA-97+I,CH(I)
  NEXT I
'2220 PRINT" ":GOSUB2190:MO=J*10:PRINT"THOU HAST"MO;" PIECES OF SILVER"
  TrsPrint " "
  GOSUB IN2190
  MO=J*10
  TrsPrint "THOU HAST "+STR$(MO)+" PIECES OF SILVER"
'2225 GOSUB2250:L=1:POKEKB+16,0
  GOSUB IN2250
  L=1
  TrsPoke KB+16,0
'2230 EG=CH(3):IN=CH(1):AS=CH(4):CA=CH(5):IT=CH(2):DX=CH(6):PA=11:PB=11
IN2230:
  EG=CH(3)
  IN=CH(1)
  AS2=CH(4)
  CA=CH(5)
  IT=CH(2)
  DX=CH(6)
  PA=11
  PB=11
'2240 RETURN
  RETURN
 
'2250 FORI=KA-90TOKA-81:POKEI,0:NEXTI:RETURN
IN2250:
  FOR I=KA-90 TO KA-81
    TrsPoke I,0
  NEXT I
  RETURN

'2400 POKEKB+15,0:FORI=1TO6:PRINT"ENTER "NC$(I);:INPUTCH(I):POKEKA-97+I,CH(I):
'IFCH(I)>18THENPRINTCH(I);"BE TOO HIGH. NO MORE THAN 18 CAN IT BE.":I=I-1
IN2400:
  TrsPoke KB+15,0
  FOR I=1 TO 6
    CH(I) = TrsInputD("ENTER "+NC$(I))
    TrsPoke KA-97+I,CH(I)
    IF CH(I)>18 THEN
      TrsPrint STR$(CH(I))+" BE TOO HIGH. NO MORE THAN 18 CAN IT BE."
      I=I-1
    END IF
'2403 NEXTI
  NEXT I
'2405 INPUT"THY CHARACTER'S EXPERIENCE IS";EX#:IFEX#>16000000THEN
'PRINT"THEY CHARACTER IS TOO WORLDWISE; FIND ANOTHER MY FRIEND":GOTO2400
'  INPUT "THY CHARACTER'S EXPERIENCE IS";EX
  EX = TrsInputD("THY CHARACTER'S EXPERIENCE IS")
  IF EX>16000000 THEN
    TrsPrint "THEY CHARACTER IS TOO WORLDWISE; FIND ANOTHER MY FRIEND"
    GOTO IN2400
  END IF
'2410 E=EX#/1000:FORL=1TO20:IF2[L>ETHENGOTO2420:ELSENEXTL
  E=EX/1000
  FOR L= 1 TO 20
    IF 2^L>E THEN
      GOTO IN2420
    ELSE
      NEXT L
    END IF
'2420 INPUT"HOW MUCH MONEY HAST THOU TO SPEND";MO
IN2420:
'  INPUT "HOW MUCH MONEY HAST THOU TO SPEND";MO
  MO = TrsInputD("HOW MUCH MONEY HAST THOU TO SPEND")
'2425 GOSUB2500 'PRESUMES A STRANGER TO THE QUEST SYSTEM
  GOSUB IN2500 'PRESUMES A STRANGER TO THE QUEST SYSTEM
'2427 INPUT"WHAT KIND OF SWORD HAST THOU";A$:FORI=1TO5:IFLEFT$(A$,1)=LEFT$(NW$(I),1)THEN
'IFCH(4)>=WR(I)THENN=I:OO=0:GOSUB2690:GOTO2429
  A$ = TrsInput("WHAT KIND OF SWORD HAST THOU")
  FOR I=1 TO 5
    IF LEFT$(A$,1)=LEFT$(NW$(I),1) THEN
      IF CH(4)>=WR(I) THEN
        N=I
        OO=0
        GOSUB IN2690
        GOTO IN2429
      END IF
    END IF
'2428 NEXTI:PRINT"THOU CANNOT TAKE A "A$;" TO THE DUNJON. THOU MUST BUY ANOTHER"
  NEXT I
  TrsPrint "THOU CANNOT TAKE A "+A$+" TO THE DUNJON. THOU MUST BUY ANOTHER"
'2429 INPUT"WHAT SORT OF ARMOR DOST THOU WEAR";A$:FORI=0TO5:
'IFLEFT$(A$,2)=LEFT$(NA$(I),2)THENN=I:OO=0:GOSUB2030:GOTO2435
IN2429:
  A$ = TrsInput("WHAT SORT OF ARMOR DOST THOU WEAR")
  FOR I=0 TO 5
    IF LEFT$(A$,2)=LEFT$(NA$(I),2) THEN
      N=I
      OO=0
      GOSUB IN2030
      GOTO IN2435
    END IF
'2432 NEXTI:PRINT"THOU CANNOT WEAR "A$;" IN THE DUNJON":GOTO2429
  NEXT I
  TrsPrint "THOU CANNOT WEAR "+A$+" IN THE DUNJON"
  GOTO IN2429

'2435 N=0:IFW2<>1THENINPUT"HAST THOU A SHIELD";A$:IFLEFT$(A$,1)="Y"THEN
'INPUT"BE IT LARGE OR SMALL";A$:N=1:IFLEFT$(A$,1)="L"THENN=2
IN2435:
  N=0
  IF W2<>1 THEN
    A$ = TrsInput("HAST THOU A SHIELD")
    IF LEFT$(A$,1)="Y" THEN
      A$ = TrsInput("BE IT LARGE OR SMALL")
      N=1
      IF LEFT$(A$,1)="L" THEN N=2
    END IF
  END IF
'2437 IFN>0THENGOSUB2865
  IF N>0 THEN GOSUB IN2865
'2439 INPUT"HAST THOU A BOW";A$:IFLEFT$(A$,1)="Y"THENHB=1ELSEHB=0
  A$ = TrsInput("HAST THOU A BOW")
  IF LEFT$(A$,1)="Y" THEN HB=1 ELSE HB=0
'2440 INPUT"HOW MANY ARROWS HAST THOU";RS:INPUT"HOW MANY MAGIC ARROWS HAST THOU";RM
'  INPUT "HOW MANY ARROWS HAST THOU";RS
  RS = TrsInputD("HOW MANY ARROWS HAST THOU")
'  INPUT "HOW MANY MAGIC ARROWS HAST THOU";RM
  RM = TrsInputD("HOW MANY MAGIC ARROWS HAST THOU")
'2450 INPUT"HOW MANY HEALING POTIONS HAST THOU NOW";HN:
'INPUT"HOW MANY HEALING SALVES HAST THOU";HS:IFHS>10THENHS=10
'  INPUT "HOW MANY HEALING POTIONS HAST THOU NOW";HN
  HN = TrsInputD("HOW MANY HEALING POTIONS HAST THOU NOW")
  'INPUT "HOW MANY HEALING SALVES HAST THOU";HS
  HS = TrsInputD("HOW MANY HEALING SALVES HAST THOU")
  IF HS>10 THEN HS=10
'2480 INPUT"IS THY SWORD MAGICAL";A$:IFLEFT$(A$,1)="Y"THENINPUT"WHAT BE THE PLUS";SM
  A$ = TrsInput("IS THY SWORD MAGICAL")
  IF LEFT$(A$,1)="Y" THEN
    'INPUT "WHAT BE THE PLUS";SM
    SM = TrsInputD("WHAT BE THE PLUS")
  END IF
'2485 POKEKB+16,0:INPUT"IS THY ARMOR MAGICAL";A$:IFLEFT$(A$,1)="Y"THEN
'INPUT"WHAT BE THE PLUS";AM:POKEKB+16,AM
  TrsPoke KB+16,0
  A$ = TrsInput("IS THY ARMOR MAGICAL")
  IF LEFT$(A$,1)="Y" THEN
'    INPUT "WHAT BE THE PLUS";AM
    AM = TrsInputD("WHAT BE THE PLUS")
    TrsPoke KB+16,AM
  END IF
'2490 GOSUB2250:GOTO2230
  GOSUB IN2250
  GOTO IN2230
 
'2500 IFW2=1THENWP=L:SP=0:ELSEWP=INT((L+1)/2):SP=L-WP
IN2500:
  IF W2=1 THEN
    WP2=L
    SP2=0
  ELSE
    WP2=INT((L+1)/2)
    SP2=L-WP2
  END IF
'2503 IFPEEK(KB+15)=LTHENRETURNELSEPOKEKB+15,L
  IF TrsPeek(KB+15)=L THEN RETURN ELSE TrsPoke KB+15,L
'2505 IFPEEK(KB-12)=123THENJ=L-1:L=2:ONJGOTO2510,2520,2530,2540,2550,2510,2520,2530,2540,2550
  IF TrsPeek(KB-12)=123 THEN
    J=L-1
    L=2
    ON J GOTO IN2510,IN2520,IN2530,IN2540,IN2550,IN2510,IN2520,IN2530,IN2540,IN2550
  END IF
'2510 L=L-1:IFL=0GOTO2565
IN2510:
  L=L-1
  IF L=0 GOTO IN2565
'2515 IFCH(5)<9THENCH(5)=CH(5)+1ELSECH(4)=CH(4)+1
  IF CH(5)<9 THEN CH(5)=CH(5)+1 ELSE CH(4)=CH(4)+1
'2520 L=L-1:IFL=0GOTO2565
IN2520:
  L=L-1
  IF L=0 GOTO IN2565
'2525 IFCH(6)<9THENCH(6)=CH(6)+1ELSECH(5)=CH(5)+1
  IF CH(6)<9 THEN CH(6)=CH(6)+1 ELSE CH(5)=CH(5)+1
'2530 L=L-1:IFL=0GOTO2565
IN2530:
  L=L-1
  IF L=0 GOTO IN2565
'2535 CH(5)=CH(5)+1
  CH(5)=CH(5)+1
'2540 L=L-1:IFL=0GOTO2565
IN2540:
  L=L-1
  IF L=0 GOTO IN2565
'2545 IFCH(6)<9THENCH(6)=CH(6)+1:ELSEIFCH(4)<CH(5)THENCH(4)=CH(4)+1:ELSECH(5)=CH(5)+1
  IF CH(6)<9 THEN
    CH(6)=CH(6)+1
  ELSE 
    IF CH(4)<CH(5) THEN
      CH(4)=CH(4)+1
    ELSE
      CH(5)=CH(5)+1
    END IF
  END IF
'2550 L=L-1:IFL=0GOTO2565
IN2550:
  L=L-1
  IF L=0 GOTO IN2565
'2555 IFCH(2)<CH(3)THENCH(2)=CH(2)+1ELSECH(3)=CH(3)+1
  IF CH(2)<CH(3) THEN CH(2)=CH(2)+1 ELSE CH(3)=CH(3)+1
'2560 GOTO2510
  GOTO IN2510

'2565 FORI=1TO6:M=CH(I)-18:IFM<=0GOTO2580
IN2565:
  FOR I=1 TO 6
    M2=CH(I)-18
    IF M2<=0 GOTO IN2580
'2570 CH(I)=18:FORJ=1TOM:IFCH(4)<18THENCH(4)=CH(4)+1:GOTO2575
    CH(I)=18
    FOR J=1 TO M2
      IF CH(4)<18 THEN CH(4)=CH(4)+1:GOTO IN2575
'2571 IFCH(5)<18THENCH(5)=CH(5)+1:GOTO2575
      IF CH(5)<18 THEN CH(5)=CH(5)+1:GOTO IN2575
'2573 IFCH(6)<18THENCH(6)=CH(6)+1:GOTO2575
      IF CH(6)<18 THEN CH(6)=CH(6)+1:GOTO IN2575
'2574 R=RND(3):CH(R)=CH(R)+1
      R2=TrsRnd(3):CH(R2)=CH(R2)+1
'2575 NEXTJ
IN2575:
    NEXT J
'2580 NEXTI:CA=CH(5):AS=CH(4):DX=CH(6):EG=CH(3):IT=CH(2):RETURN
IN2580:
  NEXT I
  CA=CH(5)
  AS2=CH(4)
  DX=CH(6)
  EG=CH(3)
  IT=CH(2)
  RETURN
 
'2600 IFMO>0THENPRINT"WILT THOU BUY ONE OF OUR FINE SWORDS":INPUTA$:
'IFLEFT$(A$,1)="N"THENN=PEEK(KB+17):GOSUB2694:RETURN:ELSEW2=0ELSERETURN
IN2600:
  IF MO>0 THEN
    TrsPrint "WILT THOU BUY ONE OF OUR FINE SWORDS"
    A$ = TrsInput("")
    IF LEFT$(A$,1)="N" THEN
      N=TrsPeek(KB+17)
      GOSUB IN2694
      RETURN
    ELSE
      W2=0
    END IF
  ELSE
    RETURN
  END IF
'2650 PRINT"WEAPON"TAB(30);"WEIGHT"TAB(40);"ASKING PRICE"
  PRINT "WEAPON" TAB(30);"WEIGHT"TAB(40);"ASKING PRICE";
  TrsPrint ""
'2660 FORI=1TO5:PRINTNW$(I);TAB(30);WW(I);TAB(40);WP(I):NEXTI
  FOR I= 1 TO 5
    PRINT NW$(I);TAB(30);WW(I);TAB(40);WP(I);
    TrsPrint ""
  NEXT I
'2670 INPUT"WHAT WEAPON WILT THOU PURCHASE";A$:FORI=1TO5:IFLEFT$(A$,1)=LEFT$(NW$(I),1)THEN
'N=I:GOTO2675:ELSEIFLEFT$(A$,1)="N"THENN=PEEK(KB+17):GOSUB2694:RETURN
IN2670:
  A$ = TrsInput("WHAT WEAPON WILT THOU PURCHASE")
  FOR I=1 TO 5
    IF LEFT$(A$,1)=LEFT$(NW$(I),1) THEN
      N=I
      GOTO IN2675
    ELSE
      IF LEFT$(A$,1)="N" THEN
        N=TrsPeek(KB+17)
        GOSUB IN2694
        RETURN
      END IF
    END IF
'2672 NEXTI:PRINT"I HAVE NOT SUCH A WEAPON AS A "A$:GOTO2670
  NEXT I
  TrsPrint "I HAVE NOT SUCH A WEAPON AS A "+A$
  GOTO IN2670

'2675 IFWR(N)>ASTHENPRINT"THOU CANNOT WIELD SUCH A GREAT WEAPON":GOTO2670
IN2675:
  IF WR(N)>AS2 THEN 
    TrsPrint "THOU CANNOT WIELD SUCH A GREAT WEAPON"
    GOTO IN2670
  END IF
'2677 PRINT"FEAST THY EYES 'PON THIS FINE "NW$(N):IFRND(3)>2THEN
'PRINT"'TIS SURE TO ALWAYS DRINK THY FOE'S BLOOD":ELSEPRINT"'TIS WELL FORGED IRON"
  TrsPrint "FEAST THY EYES 'PON THIS FINE "+NW$(N)
  IF TrsRnd(3)>2 THEN
    TrsPrint "'TIS SURE TO ALWAYS DRINK THY FOE'S BLOOD"
  ELSE
    TrsPrint "'TIS WELL FORGED IRON"
  END IF
'2678 LO=.3*WP(N):AK=WP(N):A1=AK:GOSUB4000:IFOO=0GOTO2670
  LO=.3*WP(N)
  AK=WP(N)
  A1=AK
  GOSUB IN4000
  IF OO=0 GOTO IN2670
'2680 MO=MO-OO:PRINT"THOU HAST"MO;" SILVER PIECES LEFT"
  MO=MO-OO
  TrsPrint "THOU HAST "+STR$(MO)+" SILVER PIECES LEFT"
'2690 WM=INT(WD(N)*AS/10+.5):SM=0:POKEKB+10,0:POKEKB+17,N:WC=WC+WW(N):IFN=5THENW2=1
IN2690:
  WM=INT(WD(N)*AS2/10+.5)
  SM=0
  TrsPoke KB+10,0
  TrsPoke KB+17,N
  WC=WC+WW(N)
  IF N=5 THEN W2=1
'2694 IFDW(N,0)>CH(6)THENWP=WP+CH(6)-DW(N,0)ELSEIFDW(N,1)<CH(6)THENWP=WP+CH(6)-DW(N,1)
IN2694:
  IF DW(N,0)>CH(6) THEN
    WP2=WP2+CH(6)-DW(N,0)
  ELSE
    IF DW(N,1)<CH(6) THEN WP2=WP2+CH(6)-DW(N,1)
  END IF
'2696 IFWP>0THENWP=INT(1.3*LOG(WP)+1)
  IF WP2>0 THEN WP2=INT(1.3*LOG(WP2)+1)
'2700 RETURN
  RETURN
 
'2800 IFW2=1THENPS=0:POKEKB,0:SP=0:RETURN
IN2800:
  IF W2=1 THEN PS=0:TrsPoke KB+0,0:SP2=0
  RETURN

'2840 INPUT"WILT THOU BUY A SHIELD";A$:IFLEFT$(A$,1)="N"THEN
'GOSUB2885:RETURN:ELSEPRINT"SHIELD     WEIGHT     ASK":
'PRINT"SMALL"TAB(11);SW(1);TAB(22);SP(1):PRINT"LARGE"TAB(11);SW(2);TAB(22);SP(2):
'INPUT"WHAT SORT";C$:IFLEFT$(C$,1)="L"THENN=2ELSEN=1
IN2840:
  A$ = TrsInput("WILT THOU BUY A SHIELD")
  IF LEFT$(A$,1)="N" THEN
    GOSUB IN2885
    RETURN
  ELSE
    TrsPrint "SHIELD     WEIGHT     ASK"
    PRINT "SMALL" TAB(11);SW(1);TAB(22);SP(1);
    TrsPrint ""
    PRINT "LARGE" TAB(11);SW(2);TAB(22);SP(2);
    TrsPrint ""
'    INPUT "WHAT SORT";C$
    C$ = TrsInput("WHAT SORT")
    IF LEFT$(C$,1)="L" THEN N=2 ELSE N=1
  END IF
'2842 IFLEFT$(C$,1)="L"THENN=2ELSEIFLEFT$(C$,1)="S"THENN=1ELSEN=0:POKEKB,0:PS=0:RETURN
  IF LEFT$(C$,1)="L" THEN
    N=2
  ELSE
    IF LEFT$(C$,1)="S" THEN
      N=1 
    ELSE
      N=0:TrsPoke KB,0:PS=0:RETURN
    END IF
  END IF
'2850 LO=.3*SP(N):AK=SP(N):A1=AK:GOSUB4000:IFOO=0THENGOTO2840
  LO=.3*SP(N)
  AK=SP(N)
  A1=AK
  GOSUB 4000
  IF OO=0 THEN GOTO IN2840
'2865 IFDS(N-1,0)>DXTHENSP=SP+DX-DS(N-1,0)ELSEIFDS(N-1,1)<DXTHENSP=SP+DX-DS(N-1,1)
IN2865:
  IF DS(N-1,0)>DX THEN
    SP2=SP2+DX-DS(N-1,0)
  ELSE
    IF DS(N-1,1)<DX THEN SP2=SP2+DX-DS(N-1,1)
  END IF
'2870 MO=MO-OO:PRINT"THOU HAST"MO;"SILVER PIECES LEFT":
'WC=WC+SW(N):PS=SD(N):POKEKB,PS:GOSUB2885:RETURN
  MO=MO-OO
  TrsPrint "THOU HAST "+STR$(MO)+" SILVER PIECES LEFT"
  WC=WC+SW(N)
  PS=SD(N)
  TrsPoke KB,PS
  GOSUB IN2885
  RETURN
'2885 IFPS=5THENSP=INT(SP/2)
IN2885:
  IF PS=5 THEN SP2=INT(SP2/2)
'2890 IFSP>0THENSP=INT(1.3*LOG(SP)+1)
  IF SP2>0 THEN SP2=INT(1.3*LOG(SP2)+1)
'2895 SP=2*PS+SP:RETURN
  SP2=2*PS+SP2
  RETURN
 
'3000 IFHB>0GOTO3030
IN3000:
  IF HB>0 GOTO IN3030
'3010 INPUT"WILT THOU BUY A BOW";C$:IFLEFT$(C$,1)="N"THENRETURN
  C$ = TrsInput("WILT THOU BUY A BOW")
  IF LEFT$(C$,1)= "N" THEN RETURN
'3020 PRINT"I'VE A  FINE BOW, YEW AND NEARLY NEW, FOR 12 SILVER PIECES":LO=4:AK=12:A1=AK:GOSUB4000:IFOO=0THENRETURN
  TrsPrint "I'VE A  FINE BOW, YEW AND NEARLY NEW, FOR 12 SILVER PIECES"
  LO=4
  AK=12
  A1=AK
  GOSUB IN4000
  IF OO=0 THEN RETURN
'3025 MO=MO-OO:PRINT"THOU HAST"MO;"REMAINING"
  MO=MO-OO
  TrsPrint "THOU HAST "+STR$(MO)+" REMAINING"
'3030 IFRS+RM>60THENRS=60-RM:GOTO3050
IN3030:
  IF RS+RM>60 THEN
    RS=60-RM
    GOTO IN3050
  END IF
'3040 N=0:INPUT"HOW MANY ARROWS WILT THOU BUY (AT 5 COPPERS EACH)";N:
'IFINT((N+1)/2)>MOTHENPRINT"NO CREDIT":GOTO3040:ELSEIFRS+RM+N>60THEN
'PRINT"THOU CANST CARRY BUT 60 ARROWS. BUY FEWER.":GOTO3040
IN3040:
  N=0
  'INPUT "HOW MANY ARROWS WILT THOU BUY (AT 5 COPPERS EACH)";N
  N = TrsInputD("HOW MANY ARROWS WILT THOU BUY (AT 5 COPPERS EACH)")
  IF INT((N+1)/2)>MO THEN
    TrsPrint "NO CREDIT"
    GOTO 3040
  ELSE
    IF RS+RM+N>60 THEN
      TrsPrint "THOU CANST CARRY BUT 60 ARROWS. BUY FEWER."
      GOTO IN3040
    END IF
  END IF
'3045 RS=RS+N:MO=MO-INT((N+1)/2)
  RS=RS+N
  MO=MO-INT((N+1)/2)
'3050 RETURN
IN3050:
  RETURN

'3200 REM
IN3200:
'3210 INPUT"HOW MANY SALVES WILT THOU BUY? THEY COST THEE 10 SILVER PIECES  EACH";N:
'IF10*N>MOTHENPRINT"NO CREDIT":GOTO3210:ELSEMO=MO-10*N:HS=HS+N:IFHS>10THENN=HS-10:MO=MO+10*N:HS=10:PRINT"MORE THAN 10 WILL DO THEE NO GOOD"
IN3210:
  'INPUT "HOW MANY SALVES WILT THOU BUY? THEY COST THEE 10 SILVER PIECES  EACH";N
  TrsPrint "HOW MANY SALVES WILT THOU BUY? THEY COST THEE 10 SILVER PIECES"
  N = TrsInputD("EACH")
  IF 10*N>MO THEN
    TrsPrint "NO CREDIT"
    GOTO IN3210
  ELSE
    MO=MO-10*N
    HS=HS+N
    IF HS>10 THEN
      N=HS-10
      MO=MO+10*N
      HS=10
      TrsPrint "MORE THAN 10 WILL DO THEE NO GOOD"
    END IF
  END IF
'3220 IFMO<.35*OMTHENPRINTNM$;"! THOU SPENDTHRIFT!":PRINT"THOU HAST BUT"MO;
'"SILVER PIECES LEFT TO THEE"ELSEPRINTNM$;", THOU ART FRUGAL. THOU HAST"MO;"SILVER PIECES LEFT."
  IF MO<.35*OM THEN
    TrsPrint NM$+"! THOU SPENDTHRIFT!"
    TrsPrint "THOU HAST BUT "+STR$(MO)+" SILVER PIECES LEFT TO THEE"
  ELSE
    TrsPrint NM$+", THOU ART FRUGAL. THOU HAST "+STR$(MO)+" SILVER PIECES LEFT."
  END IF
'3230 RETURN
  RETURN
 
'4000 IFRND(3)>2THENPRINT"WHAT BE THY OFFER "NM$;"?":ELSEPRINT"WHAT OFFEREST THOU?"
IN4000:
  IF TrsRnd(3)>2 THEN
    TrsPrint "WHAT BE THY OFFER "+NM$+"?"
  ELSE 
    TrsPrint "WHAT OFFEREST THOU?"
  END IF
'4010 INPUTOO:IFOO>MOTHENPRINT"LIAR, THOU HAST BUT"MO:GOTO4000
  'INPUT OO
  OO = TrsInputD("")
  IF OO>MO THEN
    TrsPrint "LIAR, THOU HAST BUT"+STR$(MO)
    GOTO IN4000
  END IF
'4020 IFOO=0THENLS=0:RETURN
  IF OO=0 THEN
    LS=0
    RETURN
  END IF
'4030 IFOO<=LOTHENGOSUB4800:GOTO4000
  IF OO<=LO THEN
    GOSUB IN4800
    GOTO IN4000
  END IF
'4035 IFOO<=LSTHENGOSUB4700:GOTO4000
  IF OO<=LS THEN
    GOSUB IN4700
    GOTO IN4000
  END IF
'4040 IFOO>=AKTHENPRINT"DONE":LS=0:RETURN
  IF OO>=AK THEN 
    TrsPrint "DONE"
    LS=0
    RETURN
  END IF
'4050 IFOO>AK-.3*RND(0)[2*AKTHENIFOO<.6*A1THENPRINT"THOU ART A HARD BARGAINER "NM$:
'LS=0:RETURN:ELSEPRINT"DONE":LS=0:RETURN
  IF OO>AK-.3*TrsRnd(0)^2*AK THEN
    IF OO<.6*A1 THEN
      TrsPrint "THOU ART A HARD BARGAINER "+NM$
      LS=0
      RETURN
    ELSE
      TrsPrint "DONE"
      LS=0
      RETURN
    END IF
  END IF
'4055 IFRND(200)=200THENPRINT"i SEE THE GODS LOOK WITH FAVOR ON THEE, SO TAKE IT FOR THAT":
'LS=0:RETURN
  IF TrsRnd(200)=200 THEN
    TrsPrint "I SEE THE GODS LOOK WITH FAVOR ON THEE, SO TAKE IT FOR THAT"
    LS=0
    RETURN
  END IF
'4058 B=20/(CH(1)+CH(3)):IFCH(3)>12ANDCH(1)<10THENB=20/(CH(1)+20-CH(3))
  B=20/(CH(1)+CH(3))
  IF CH(3)>12 AND CH(1)<10 THEN B=20/(CH(1)+20-CH(3))
'4060 AK=OO+(AK-OO)*SQR(1-RND(0))*B
  AK=OO+(AK-OO)*SQR(1-TrsRnd(0))*B
'4070 LS=OO
  LS=OO
'4080 GOSUB4900:GOTO4000
  GOSUB IN4900
  GOTO IN4000
 
'4500 CH(1)=PEEK(KB+20):CH(2)=PEEK(KB+21):CH(3)=PEEK(KB+22):CH(4)=PEEK(KB+23):CH(5)=PEEK(KB+24):
'CH(6)=PEEK(KB+25):SM=PEEK(KB+10):RM=PEEK(KB+14):RS=PEEK(KB+13):AA=PEEK(KB+8):
'WM=PEEK(KB+7)-SM:WN=PEEK(KB+17)
IN4500:
  CH(1)=TrsPeek(KB+20)
  CH(2)=TrsPeek(KB+21)
  CH(3)=TrsPeek(KB+22)
  CH(4)=TrsPeek(KB+23)
  CH(5)=TrsPeek(KB+24)
  CH(6)=TrsPeek(KB+25)
  SM=TrsPeek(KB+10)
  RM=TrsPeek(KB+14)
  RS=TrsPeek(KB+13)
  AA=TrsPeek(KB+8)
  WM=TrsPeek(KB+7)-SM
  WN=TrsPeek(KB+17)
'4505 IFSM<>0THENWN=6
  IF SM<>0 THEN WN=6
'4510 GOSUB9800:CLS:PRINT"CHARACTER SUMMARY FOR "NM$
  GOSUB IN9800
  TrsCls
  PRINT "CHARACTER SUMMARY FOR " NM$
'4520 PRINT"":FORI=1TO6:PRINTNC$(I);TAB(20);PEEK(KA-97+I):NEXTI
  PRINT ""
  FOR I=1 TO 6
    PRINT NC$(I);TAB(20);TrsPeek(KA-97+I);
    TrsPrint ""
  NEXT I
'4525 J=0:IFPEEK(KB)=3THENJ=1ELSEIFPEEK(KB)=5THENJ=2
  J=0
  IF TrsPeek(KB)=3 THEN
    J=1
  ELSE
    IF TrsPeek(KB+0)=5 THEN J=2
  END IF
'4530 PRINT"WEAPON: "NW$(WN);TAB(32);"ARMOR: "NA$(AA)
  PRINT "WEAPON: " NW$(WN);TAB(32);"ARMOR: " NA$(AA);
  TrsPrint ""
'4532 IFWN=6THENPRINT"PLUS:"PEEK(KB+10);
  IF WN=6 THEN PRINT "PLUS:" TrsPeek(KB+10);
'4533 IFPEEK(KB+16)>0THENPRINTTAB(32);"PLUS:"PEEK(KB+16):ELSEPRINT" "
  IF TrsPeek(KB+16)>0 THEN
    PRINT TAB(32);"PLUS:" TrsPeek(KB+16);
  ELSE
    PRINT " ";
  END IF
  TrsPrint ""
'4535 PRINT"ARROWS:"RS;TAB(22);"MAGIC ARROWS:"RM;TAB(45);"SHIELD: "NS$(J)
  PRINT "ARROWS:" RS;TAB(22);"MAGIC ARROWS:"RM;TAB(45);"SHIELD: " NS$(J);
  TrsPrint ""
'4537 PRINT"SALVES:"PEEK(KB+11);TAB(22);"ELIXIRS:"PEEK(KB+6);TAB(45);"SILVER:"MO
  PRINT "SALVES:" TrsPeek(KB+11);TAB(22);"ELIXIRS:" TrsPeek(KB+6);TAB(45);"SILVER:" MO;
  TrsPrint ""
'4540 PRINT"EXPERIENCE:"PEEK(KB+1)+256*PEEK(KB+2)+65536*PEEK(KB+3);TAB(22);"WEIGHT CARRIED:"PEEK(KB+12)
  PRINT"EXPERIENCE:" TrsPeek(KB+1)+256*TrsPeek(KB+2)+65536*TrsPeek(KB+3);TAB(22);"WEIGHT CARRIED:" TrsPeek(KB+12);
  TrsPrint ""
'4550 RETURN
  RETURN

'4700 R=RND(3):R=RND(3):ONRND(3)GOTO4710,4720,4730
IN4700:
  R2=TrsRnd(3)
  R2=TrsRnd(3)
  ON TrsRnd(3) GOTO IN4710,IN4720,IN4730
'4710 PRINT"DOST THOU TAKE ME FOR A DOLT?":RETURN
IN4710:
  TrsPrint "DOST THOU TAKE ME FOR A DOLT?"
  RETURN
'4720 PRINT"FOOL OR KNAVE "NM$;"! MAKE AN OFFER HIGHER THAN THY LAST":RETURN
IN4720:
  TrsPrint "FOOL OR KNAVE "+NM$+"! MAKE AN OFFER HIGHER THAN THY LAST"
  RETURN
'4730 PRINT"PERCHANCE THOU WOULDST NOT HAVE THIS AT ALL?":RETURN
IN4730:
  TrsPrint "PERCHANCE THOU WOULDST NOT HAVE THIS AT ALL?"
  RETURN

'4800 IFRND(100)>50THENPRINT"HA! 'TIS LESS THAN I PAID FOR IT"ELSEPRINT"I SPIT ON THY PALTRY OFFER"
IN4800:
  IF TrsRnd(100)>50 THEN
    TrsPrint "HA! 'TIS LESS THAN I PAID FOR IT"
  ELSE
    TrsPrint "I SPIT ON THY PALTRY OFFER"
  END IF
'4810 RETURN
  RETURN
 
'4900 FORI1=1TO10:R=RND(RT):NEXTI1:RQ=0:R=RND(RT)-1:FORI1=1TORN:RQ=RQ+RR(I1):
'IFRQ<RTHENNEXTI1:ELSEPRINTR$(I1);INT(AK+.999):RETURN
IN4900:
  FOR I1= 1 TO 10
    R2=TrsRnd(RT)
  NEXT I1
  RQ=0
  R2=TrsRnd(RT)-1
  FOR I1=1 TO RN2
    RQ=RQ+RR(I1)
    IF RQ<R2 THEN
      NEXT I1
    ELSE
      TrsPrint R$(I1)+" "+STR$(INT(AK+.999))
      RETURN
    END IF

'9800 NM$="":FORI=1TO11:NM$=NM$+CHR$(PEEK(KB-12+I)):NEXTI
IN9800:
  NM$=""
  FOR I=1 TO 11
    NM$=NM$+CHR$(TrsPeek(KB-12+I))
  NEXT I
'9810 FORI=1TO10:IFMID$(NM$,12-I,1)<>" "GOTO9830
  FOR I= 1 TO 10
    IF MID$(NM$,12-I,1)<>" " THEN GOTO IN9830
'9820 NEXTI
  NEXT I
'9830 NM$=LEFT$(NM$,12-I):RETURN
IN9830:
  NM$=LEFT$(NM$,12-I)
  RETURN

'9900 SM=PEEK(KB+10):HS=PEEK(KB+11):HN=PEEK(KB+6):RS=PEEK(KB+13):RM=PEEK(KB+14):FORI=1TO6:CH(I)=PEEK(KB+19+I):NEXTI:AA=PEEK(KB+8):WM=PEEK(KB+7):PS=PEEK(KB):WC=PEEK(KB+12):IFPEEK(KB+17)=5THENW2=1ELSEW2=0
IN9900:
  SM=TrsPeek(KB+10)
  HS=TrsPeek(KB+11)
  HN=TrsPeek(KB+6)
  RS=TrsPeek(KB+13)
  RM=TrsPeek(KB+14)
  FOR I=1 TO 6
    CH(I)=TrsPeek(KB+19+I)
  NEXT I
  AA=TrsPeek(KB+8)
  WM=TrsPeek(KB+7)
  PS=TrsPeek(KB+0)
  WC=TrsPeek(KB+12)
  IF TrsPeek(KB+17)=5 THEN W2=1 ELSE W2=0
'9907 EX#=PEEK(KB+1)+256*PEEK(KB+2)+65536*PEEK(KB+3):IFEX#>1999THENL=LOG(EX#/1000)/LOG(2)+1ELSEL=1
  EX=TrsPeek(KB+1)+256*TrsPeek(KB+2)+65536*TrsPeek(KB+3)
  IF EX>1999 THEN L=LOG(EX/1000)/LOG(2)+1 ELSE L=1
'9908 HB=1:GOSUB2500:GOSUB2230
  HB=1
  GOSUB IN2500
  GOSUB IN2230
'9910 RETURN
  RETURN
 
'9920 GOSUB9800:CLS:PRINT"TREASURES FOR "NM$:PRINT"TREASURE"TAB(12);"#"TAB(24);"TREASURE"TAB(36);"#"
IN9920:
  GOSUB IN9800
  TrsCls
  TrsPrint "TREASURES FOR "+NM$
  PRINT "TREASURE" TAB(12);"#" TAB(24);"TREASURE" TAB(36);"#";
  TrsPrint ""
'9930 FORI=1TO20STEP2:PRINTI;TAB(12);PEEK(KA-81+I);TAB(24);I+1;TAB(36);PEEK(KA-80+I):NEXTI:INPUT"ART THOU READY FOR MORE";A$:RETURN
IN9930:
  FOR I=1 TO 20 STEP 2
    PRINT I;TAB(12);TrsPeek(KA-81+I);TAB(24);I+1;TAB(36);TrsPeek(KA-80+I);
    TrsPrint ""
  NEXT I
  A$ = TrsInput("ART THOU READY FOR MORE")
  RETURN

' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
' TEMPLE OF APSHAI, DUNJONMASTER - TRS80, TRSDOS, COPYRIGHT 1979, AUTOMATED SIMULATIONS, REV 3 7/3/80
' Maximite 2 Conversion - Craig S. Buchanan 2021-Dec-27
' +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

DM:
  TrsClear
  taMode = 2
'5 REMCOPYRIGHT 1979,AUTOMATED SIMULATIONS REV 3 7/3/80  
'10 CLEAR600:DEFINTD,H-Z
'12 J=0:IK=0:II=0:NX=0:NY=0:L1=0:L2=0:L4=0:YY=0:KK=0:I=0:IJ=0:K=0:Q=60:
'DIMP(4),S(4),ZA(3),ZD(5),TX(3,1),TY(3,1),TM(5),H(3),T$(9),RF(3),RN(3)
DM12:
  J=0:IK=0:II=0:NX=0:NY=0:L1=0:L2=0:L4=0:YY=0:KK=0:I=0:IJ=0:K=0:Q=60
'13 Q1=12:DIMM$(Q1):C%=15360:KM=0:B1$="      "
  Q1=12
  C2=15360
  KM=0
  B1$="      "
'15 Q2=20:CLS:DATA0,2,-1,2,-2,6,2,1,4,1,12,-2,1,-2,2,18,-2,-1,-4,-1,10,13,2,1,1:
'FORI=0TO3:READH(I):FORJ=0TO1:READTX(I,J),TY(I,J):NEXTJ:NEXTI:FORI=1TO5:READTM(I):NEXTI
  Q2=20
  TrsCls
  DATA 0,2,-1,2,-2,6,2,1,4,1,12,-2,1,-2,2,18,-2,-1,-4,-1,10,13,2,1,1
  FOR I=0 TO 3
    READ H(I)
    FOR J=0 TO 1
      READ TX(I,J),TY(I,J)
    NEXT J
  NEXT I
  FOR I=1 TO 5
    READ TM(I)
  NEXT I
'16 KA=-1*(65536-46864):NO=KA:NT=4*Q+KA:D1=8*Q+KA:D2=12*Q+KA:MT=16*Q+KA:MN=MT+Q:NP=MN+Q:
'XP=NP+Q:YP=XP+Q:TR=YP+Q:XR=TR+Q:YR=XR+Q:KB=KA+2251:FORI=1TO60:POKEKA-I,0:NEXTI'***DISK
  KA=-1*(65536-46864)
  NO=KA
  NT=4*Q+KA
  D1=8*Q+KA
  D2=12*Q+KA
  MT=16*Q+KA
  MN=MT+Q
  NP=MN+Q
  XP=NP+Q
  YP=XP+Q
  TR=YP+Q
  XR=TR+Q 
  YR=XR+Q
  KB=KA+2251
  FOR I=1 TO 60
    TrsPoke KA-I,0
  NEXT I
'17 X1=YR+Q:X2=X1+2*Q:Y1=X2+2*Q:Y2=Y1+2*Q:TN=Y2+2*Q:TV=TN+10:TP=TV+10:TG=TP+10:TW=TG+10:
'TS=TW+Q2:UL=TS+Q2:UA=UL+Q1:UP=UA+Q1:US=UP+Q1:UD=US+Q1:UC=UD+Q1:UR=UC+Q1:UI=UR+Q1:UW=UI+Q1:UV=UW+Q1
  X1=YR+Q
  X2=X1+2*Q
  Y1=X2+2*Q
  Y2=Y1+2*Q
  TN=Y2+2*Q
  TV=TN+10
  TP=TV+10
  TG=TP+10
  TW=TG+10
  TS=TW+Q2
  UL=TS+Q2
  UA=UL+Q1
  UP=UA+Q1
  US=UP+Q1
  UD=US+Q1
  UC=UD+Q1
  UR=UC+Q1
  UI=UR+Q1
  UW=UI+Q1
  UV=UW+Q1
'20 C%=15360:BL$=STRING$(15," "):ZD(0)=3:ZA(1)=0:ZD(1)=0:ZA(2)=3:ZD(2)=3:ZA(3)=-6:
'ZD(3)=-2:ZD(4)=5:ZD(5)=5:SE=100
  C2=15360
  BL$=STRING$(15," ")
  ZD(0)=3
  ZA(1)=0
  ZD(1)=0
  ZA(2)=3
  ZD(2)=3
  ZA(3)=-6
  ZD(3)=-2
  ZD(4)=5
  ZD(5)=5
  SE=100
'25 DATA"FLAME","DUST","MOLD","PIT","","SPEAR","NEEDLE","XBOW","CAVEIN","CEILING"
  'DATA "FLAME","DUST","MOLD","PIT","","SPEAR","NEEDLE","XBOW","CAVEIN","CEILING"
  'Dagger is missing so adding it here.
  DATA "FLAME","DUST","MOLD","PIT","DAGGER","SPEAR","NEEDLE","XBOW","CAVEIN","CEILING"
'45 FORI=0TO9:READT$(I):NEXTI:J9=191:J6=64:GOTO5000
  FOR I=0 TO 9
    READ T$(I)
  NEXT I
  J9=191
  J6=64
  GOTO DM5000
  
'50 PC=PC-1:POKEKB+24,PEEK(KB+24)-1:POKEKA-92,PEEK(KA-92)-1:RETURN
DM50:
  PC=PC-1
  TrsPoke KB+24,TrsPeek(KB+24)-1
  TrsPoke KA-92,TrsPeek(KA-92)-1
  RETURN
  
'55 YY=W1-W2-5:IFYM>YYTHENYM=YYELSEIFYM<5THENYM=5  
DM55:
  YY=W1-W2-5
  IF YM>YY THEN
    YM=YY
  ELSE
    IF YM<5 THEN YM=5
  END IF
'56 XX=V2-V1-5:IFXM>XXTHENXM=XXELSEIFXM<5THENXM=5
  XX=V2-V1-5
  IF XM>XX THEN
    XM=XX
  ELSE
    IF XM<5 THEN XM=5
  END IF
'57 RETURN
  RETURN
  
'60 PRINT@945,J;"ARROWS";:RETURN
DM60: 
  TrsPrintFS 945,J,"ARROWS"
  RETURN

'65 PRINTCHR$(23);:FORI=1TO30:NEXTI:PRINTCHR$(28);:RETURN
DM65:
  PRINT CHR$(23);
  Pause 250
'  FOR I=1 TO 30:NEXT I
  PRINT CHR$(28);
  RETURN

'70 GOSUB500:IFL=0GOTO70ELSEJ=ASC(C$)-48:IFJ<0ORJ>9GOTO70ELSERETURN
DM70:
  GOSUB DM500
  IF L=0 THEN
    GOTO DM70
  ELSE
    J=ASC(C$)-48
    IF J<0 OR J>9 THEN
      GOTO DM70
    ELSE 
      RETURN
    END IF
  END IF
'72 PRINT@503,B1$;:PRINT@503,PEEK(KB+13);:RETURN
DM72:
  TrsPrintS 503,B1$:
  TrsPrintF 503,TrsPeek(KB+13)
  RETURN

'74 PRINT@569,B1$;:PRINT@569,PEEK(KB+14);:RETURN  
DM74:
  TrsPrintS 569,B1$
  TrsPrintF 569,TrsPeek(KB+14)
  RETURN

'80 FORI=1TO60:POKEKA-I,0:NEXTI:RETURN  
DM80:
  FOR I=1 TO 60
    TrsPoke KA-I,0
  NEXT I
  RETURN
  
'82 FORI=0TO3:RN(I)=0:IFRF(I)>0THENPOKENT+KR+I*Q,3:POKEKA-61+KR,0:RN(I)=1:RF(I)=0
DM82:
  FOR I=0 TO 3
    RN(I)=0
    IF RF(I)>0 THEN
       TrsPoke NT+KR+I*Q,3
       TrsPoke KA-61+KR,0
       RN(I)=1
       RF(I)=0
    END IF
'84 NEXTI:I=KF+1:IFI>3THENI=I-4
  NEXT I
  I=KF+1
  IF I>3 THEN I=I-4
'85 IFRN(KF-1)>0THENRF(I)=1
  IF RN(KF-1)>0 THEN RF(I)=1
'86 RETURN
  RETURN

'90 POKEN,PEEK(IA+NN):NN=NN+1:RETURN  
DM90:
  TrsPoke N,TrsPeek(IA+NN)
  NN=NN+1
  RETURN

'100 L1=YY:L2=YY+L-1:IFL1<0THENL1=0:IFL2<0THENRETURN  
DM100:
  L1=YY
  L2=YY+L-1
  IF L1<0 THEN 
    L1=0
    IF L2<0 THEN
      RETURN
    END IF
  END IF
'101 IFL2>47THENL2=47:IFL1>47THENRETURN
  IF L2>47 THEN
    L2=47
    IF L1>47 THEN
      RETURN
    END IF
  END IF
'102 L3=INT((L1+3)/3):L4=INT((L2-3)/3):LM=3*L4+3:LX=3*L3-1:IFLM<L1THENLM=L1
  L3=INT((L1+3)/3)
  L4=INT((L2-3)/3)
  LM=3*L4+3
  LX=3*L3-1
  IF LM<L1 THEN LM=L1
'103 IFLX>L2THENLX=L2
  IF LX>L2 THEN LX=L2
'104 FORII=XXTOXX+1:IFII<0ORII>47THENRETURN
  FOR II=XX TO XX+1
    IF II<0 OR II>47 THEN
      RETURN
    END IF
'105 I=2*II:FORIK=L1TOLX:GOSUB400:NEXT:FORIK=LMTOL2:GOSUB400:NEXT
    I=2*II
    FOR IK=L1 TO LX
      GOSUB DM400
    NEXT
    FOR IK=LM TO L2
      GOSUB DM400
    NEXT
'110 IFL4>=L3THENFORNY=L3TOL4:POKEC%+II+64*NY,J9:NEXT
    IF L4>=L3 THEN
      FOR NY=L3 TO L4
        TrsPokeScreen C2+II+64*NY,J9
      NEXT
    END IF
'115 NEXT:RETURN
  NEXT
  RETURN

'120 L1=YY:L2=YY+L-1:IFL1<0THENL1=0:IFL2<0THENRETURN  
DM120:
  L1=YY
  L2=YY+L-1
  IF L1<0 THEN
    L1=0
    IF L2<0 THEN RETURN
  END IF
'121 IFL2>47THENL2=47:IFL1>47THENRETURN
  IF L2>47 THEN
    L2=47
    IF L1>47 THEN RETURN
  END IF
'122 FORI=0TOI1:NX=2*XX+I:IFNX<0ORNX>96THENRETURN
  FOR I=0 TO I1
    NX=2*XX+I
    IF NX<0 OR NX>96 THEN RETURN
'125 FORJ=L1TOL2:RESET(NX,J):NEXT:NEXT:RETURN
    FOR J=L1 TO L2
      TRSRESET(NX,J)
    NEXT
  NEXT
  RETURN

'140 L1=2*XX:L2=L1+L-1:IFL1<0THENL1=0:IFL2<0THENRETURN  
DM140:
  L1=2*XX
  L2=L1+L-1
  IF L1<0 THEN 
    L1=0
    IF L2<0 THEN RETURN
  END IF
'141 IFL2>95THENL2=95:IFL1>95THENRETURN
  IF L2>95 THEN
    L2=95
    IF L1>95 THEN RETURN
  END IF
'142 FORI=0TOI1:NY=YY+I:IFNY<0ORNY>47THENRETURN
  FOR I=0 TO I1
    NY=YY+I
    IF NY<0 OR NY>47 THEN RETURN
'145 FORJ=L1TOL2:SET(J,NY):NEXT:NEXT:RETURN
    FOR J=L1 TO L2
      TRSSET(J,NY)
    NEXT
  NEXT
  RETURN

'150 L1=2*XX:L2=L1+L-1:IFL1<0THENL1=0:IFL2<0THENRETURN
DM150:
  L1=2*XX
  L2=L1+L-1
  IF L1<0 THEN
    L1=0
    IF L2<0 THEN RETURN
  END IF
'151 IFL2>95THENL2=95:IFL1>95THENRETURN
  IF L2>95 THEN
    L2=95
    IF L1>95 THEN RETURN
  END IF
'152 FORI=0TOI1:NY=YY+I:IFNY<0ORNY>47THENRETURN
  FOR I=0 TO I1
    NY=YY+I
    IF NY<0 OR NY>47 THEN RETURN
'155 FORJ=L1TOL2:RESET(J,NY):NEXT:NEXT:RETURN
    FOR J=L1 TO L2
      TRSRESET(J,NY)
    NEXT
  NEXT
  RETURN

'200 I1=1
DM200:
  I1=1
'205 XX=2*(XA-XB)-1:YY=YB-YA:IFKF=2ORKF=4GOTO230
DM205:
  XX=2*(XA-XB)-1
  YY=YB-YA
  IF KF=2 OR KF=4 GOTO DM230
'210 FORI=XX-3TOXX+1STEP4:IK=YY-(KF-1)/2:ONI1GOSUB400,410:IK=IK+1:ONI1GOSUB400,410:NEXTI:I=XX-1:IK=YY-2+KF:ONI1GOSUB400,410:RETURN
  FOR I=XX-3 TO XX+1 STEP 4
    IK=YY-(KF-1)/2
    ON I1 GOSUB DM400,DM410
    IK=IK+1
    ON I1 GOSUB DM400,DM410
  NEXT I
  I=XX-1
  IK=YY-2+KF
  ON I1 GOSUB DM400,DM410
  RETURN
  
'230 FORIK=YY-1TOYY+1STEP2:I=XX-5+KF:ONI1GOSUB400,410:I=I+2:ONI1GOSUB400,410:NEXTIK:IK=YY:I=XX+5-2*KF:ONI1GOSUB400,410:RETURN
DM230:
  FOR IK=YY-1 TO YY+1 STEP 2
    I=XX-5+KF
    ON I1 GOSUB DM400,DM410
    I=I+2
    ON I1 GOSUB DM400,DM410
  NEXT IK
  IK=YY
  I=XX+5-2*KF
  ON I1 GOSUB DM400,DM410
  RETURN

'250 I1=2:GOTO205  
DM250:
  I1=2
  GOTO DM205

' Draw Character?
'300 I1=1
DM300:
  I1=1
'305 XX=2*(XM-XB+V1)-1:YY=YB-YM-W2:I=XX:IK=YY-1:ONI1GOSUB400,410:IK=YY+1:ONI1GOSUB400,410:
'IK=YY:FORI=XX-2TOXX+2STEP4:ONI1GOSUB400,410:NEXTI:RETURN  
DM305:
  XX=2*(XM-XB+V1)-1
  YY=YB-YM-W2
  I=XX
  IK=YY-1
  ON I1 GOSUB DM400,DM410
  IK=YY+1
  ON I1 GOSUB DM400,DM410
  IK=YY
  FOR I=XX-2 TO XX+2 STEP 4
    ON I1 GOSUB DM400,DM410
  NEXT I
  RETURN

'345 IFNB=0THENRETURN  
DM345:
  IF NB=0 THEN RETURN
'350 I1=2:GOTO305
DM350:
  I1=2
  GOTO DM305

'400 IFI<0ORI>94ORIK<0ORIK>47THENRETURNELSEFORJ=0TO1:SET(I+J,IK):NEXT:RETURN  
DM400:
  IF I<0 OR I>94 OR IK<0 OR IK>47 THEN
    RETURN
  ELSE
    FOR J=0 TO 1
      TRSSET(I+J,IK)
    NEXT
    RETURN
  END IF

'410 IFI<0ORI>94ORIK<0ORIK>47THENRETURNELSEFORJ=0TO1:RESET(I+J,IK):NEXT:RETURN  
DM410:
  IF I<0 OR I>94 OR IK<0 OR IK>47 THEN
    RETURN
  ELSE
    FOR J=0 TO 1
      TRSRESET(I+J,IK)
    NEXT
    RETURN
  END IF

'440 IFK=0THENSET(I,YY-J):SET(I,YY+J):ELSERESET(I,YY-J):RESET(I,YY+J)  
DM440:
  IF K=0 THEN
    TRSSET(I,YY-J)
    TRSSET(I,YY+J)
  ELSE
    TRSRESET(I,YY-J)
    TRSRESET(I,YY+J)
  END IF
  PAUSE 75
'445 RETURN
  RETURN

' Draw Monster Slain?
'450 FORL=0TO2:FORK=0TO1:I=XX+1:J=1:GOSUB440:I=XX-1:GOSUB440:I=XX+2:J=0:GOSUB440:I=XX-2:GOSUB440
DM450:
  FOR L=0 TO 2
    FOR K=0 TO 1
      I=XX+1
      J=1
      GOSUB DM440
      I=XX-1
      GOSUB DM440
      I=XX+2
      J=0
      GOSUB DM440
      I=XX-2
      GOSUB DM440
'460 I=XX+2:J=2:GOSUB440:I=XX-2:GOSUB440:I=XX+4:J=0:GOSUB440:I=XX-4:GOSUB440:NEXTK:NEXTL:RETURN
      I=XX+2
      J=2
      GOSUB DM440
      I=XX-2
      GOSUB DM440
      I=XX+4
      J=0
      GOSUB DM440
      I=XX-4
      GOSUB DM440
'      PAUSE 500
    NEXT K
  NEXT L
  RETURN

'500 LE=SE/(NB+1):FORIC=1TOLE
DM500:
  LE=SE/(NB+1)
  pause 1000
  FOR IC=1 TO LE
'510 C$=INKEY$:IFLEN(C$)>0THENL=1:RETURN
    C$=TrsInkey()
    IF LEN(C$)>0 THEN
      L=1
      RETURN
    END IF
'520 NEXTIC:L=0:RETURN
  NEXT IC
  L=0
  RETURN

'600 II=2*(XL-XB+V1-1):IJ=YB-YL-W2:FORL=0TO1:I=II+TX(KF-1,L):IK=IJ+TY(KF-1,L):IFK=0THENGOSUB400:ELSEGOSUB410  
DM600:
  II=2*(XL-XB+V1-1)
  IJ=YB-YL-W2:
  FOR L=0 TO 1
    I=II+TX(KF-1,L)
    IK=IJ+TY(KF-1,L)
    IF K=0 THEN
      GOSUB DM400
    ELSE
      GOSUB DM410
    END IF
'610 NEXTL:RETURN
  NEXT L
  RETURN

'650 PH=PEEK(KB+24):TA=TA-(ABS(M)/MM*(100/PH+5-5*PC/PH)*(1+WC/WT*3)/2)+11:IFTA>100THENTA=100  
DM650:
  PH=TrsPeek(KB+24)
  TA=TA-(ABS(M2)/MM*(100/PH+5-5*PC/PH)*(1+WC/WT*3)/2)+11
  IF TA>100 THEN TA=100
'660 PRINT@184,B1$;:PRINT@184,TA;"%";:RETURN
  TrsPrintS 184,B1$
  TrsPrintFS 184,TA,"%"
  RETURN

'670 Y=YB-PEEK(YP+KR)-W2:X=PEEK(XP+KR)+V1-XB:FORL=1TO15:I=2*X:IK=Y:GOSUB400:GOSUB410:NEXTL:RETURN  
DM670:
  Y=YB-TrsPeek(YP+KR)-W2
  X=TrsPeek(XP+KR)+V1-XB
  FOR L=1 TO 15
    I=2*X
    IK=Y
    GOSUB DM400
    PAUSE 75
    GOSUB DM410
    PAUSE 75
  NEXT L
  RETURN

'679 IFPEEK(TR+KR)=0THENRETURN
DM679:
  IF TrsPeek(TR+KR)=0 THEN RETURN
'680 I=2*(V1+PEEK(XR+KR)-XB):YY=YB-W2-PEEK(YR+KR):FORIK=YY-1TOYY:IFPEEK(TR+KR)>0THEN
'GOSUB400ELSEGOSUB410
DM680:
  I=2*(V1+TrsPeek(XR+KR)-XB)
  YY=YB-W2-TrsPeek(YR+KR)
  FOR IK=YY-1 TO YY
    IF TrsPeek(TR+KR)>0 THEN
      GOSUB DM400
    ELSE
      GOSUB DM410
    END IF
'690 NEXTIK:RETURN
  NEXT IK
  RETURN

'695 N=PEEK(NP+KR):L=0:IFNB>0ORN=0ORABS(XL-PEEK(XP+KR))>3ORABS(YL-PEEK(YP+KR))>3OR
'RND(100)>PEEK(TP+N)THENRETURN  
DM695:
  N=TrsPeek(NP+KR)
  L=0
  IF NB>0 OR N=0 OR ABS(XL-TrsPeek(XP+KR))>3 OR ABS(YL-TrsPeek(YP+KR))>3 OR TrsRnd(100)>TrsPeek(TP+N) THEN
    RETURN
  END IF
'700 PRINT@624,BL$;:PRINT@624,T$(PEEK(TG+N));" TRAP";:GOSUB65:GOSUB65:IFPEEK(TN+N)>0THENI=PEEK(TN+N):NB=1:GOSUB4830:XM=PEEK(XP+KR):YM=PEEK(YP+KR):PRINT@624,A$;:GOTO720
  TrsPrintS 624,BL$
  LastTrap = TrsPeek(TG+N)
  TrsPrintSS 624,T$(LastTrap)," TRAP"
  DisplayTrap TrsPeek(KA), LastTrap
'  Pause 500
  GOSUB DM65
  GOSUB DM65
  IF TrsPeek(TN+N)>0 THEN
    I=TrsPeek(TN+N)
    NB=1
    GOSUB DM4830
    XM=TrsPeek(XP+KR)
    YM=TrsPeek(YP+KR)
    TrsPrintS 624,A$
    GOTO DM720
  END IF
'710 IFPEEK(TV+N)>0THENML=PEEK(TV+N):L=1:MD=2*ML
  IF TrsPeek(TV+N)>0 THEN
    ML=TrsPeek(TV+N)
    L=1
    MD=2*ML
  END IF
'720 POKENP+KR,0:RETURN  
DM720:
  TrsPoke NP+KR,0
  RETURN
  
'800 LL=KR:GOTO850  
DM800:
  LL2=KR
  GOTO DM850
'840 LL=LR  
DM840:
  LL2=LR
'850 V3=PEEK(X1+LL)+128*PEEK(X1+Q+LL):V4=PEEK(X2+LL)+128*PEEK(X2+Q+LL):W3=PEEK(Y1+LL)+128*PEEK(Y1+Q+LL):W4=PEEK(Y2+LL)+128*PEEK(Y2+Q+LL):IFLL<>KRTHENRETURN
DM850:
  V3=TrsPeek(X1+LL2)+128*TrsPeek(X1+Q+LL2)
  V4=TrsPeek(X2+LL2)+128*TrsPeek(X2+Q+LL2)
  W3=TrsPeek(Y1+LL2)+128*TrsPeek(Y1+Q+LL2)
  W4=TrsPeek(Y2+LL2)+128*TrsPeek(Y2+Q+LL2)
  IF LL2<>KR THEN RETURN
'860 V1=V3:V2=V4:W1=W3:W2=W4:RETURN
  V1=V3
  V2=V4
  W1=W3
  W2=W4
  RETURN

'880 K=0:MO=KA+2121:FORLL=1TOMQ:L=PEEK(K+MO):K=K+L+1:NEXTLL:K=K-L-1:A$="":
'FORLL=1TOL:A$=A$+CHR$(PEEK(K+MO+LL)):NEXTLL:RETURN  
DM880:
  K=0
  MO=KA+2121
  FOR LL2=1 TO MQ
    L=TrsPeek(K+MO)
    K=K+L+1
  NEXT LL2
  K=K-L-1
  A$=""
  FOR LL2=1 TO L
    A$=A$+CHR$(TrsPeek(K+MO+LL2))
  NEXT LL2
  RETURN

'1200 XA=PEEK(KA+2236):YA=PEEK(KA+2237):KF=PEEK(KA+2238):PW=PEEK(KA+2235):
'EX#=PEEK(KB+3)*65536+PEEK(KB+2)*256+PEEK(KB+1):FORI=61TO80:POKEKA-I,0:NEXTI
DM1200:
  XA=TrsPeek(KA+2236)
  YA=TrsPeek(KA+2237)
  KF=TrsPeek(KA+2238)
  PW=TrsPeek(KA+2235)
  EX=TrsPeek(KB+3)*65536+TrsPeek(KB+2)*256+TrsPeek(KB+1)
  FOR I=61 TO 80
    TrsPoke KA-I,0
  NEXT I
'1210 MM=4:PH=PEEK(KB+24):PC=PH:PB=PEEK(KB+18):PA=PEEK(KB+19):PS=PEEK(KB):AA=PEEK(KB+8):TA=100:
'AS=PEEK(KB+23)/10:WM=PEEK(KB+7):SE=PEEK(KB+9):WT=AS*AS*100:WC=PEEK(KB+12):SM=PEEK(KB+10):RETURN
  MM=4
  PH=TrsPeek(KB+24)
  PC=PH:PB=TrsPeek(KB+18)
  PA=TrsPeek(KB+19)
  PS=TrsPeek(KB)
  AA=TrsPeek(KB+8)
  TA=100
  AS2=TrsPeek(KB+23)/10
  WM=TrsPeek(KB+7)
  SE=TrsPeek(KB+9)
  WT=AS2*AS2*100
  WC=TrsPeek(KB+12)
  SM=TrsPeek(KB+10)
  RETURN

'3000 IFPEEK(KA+LR-61)=1THENRETURNELSEGOSUB840:XX=V4-XB-2:
'IFV3-XB>-1ANDV4-XB<49ANDYB-W3>-1ANDYB-W4<49THENPOKEKA+LR-61,1
DM3000:
  IF TrsPeek(KA+LR-61)=1 THEN
    RETURN
  ELSE
    GOSUB DM840
    XX=V4-XB-2
    IF V3-XB>-1 AND V4-XB<49 AND YB-W3>-1 AND YB-W4<49 THEN TrsPoke KA+LR-61,1
  END IF
'3011 FORK=1TO3STEP2:NN=LR+K*Q:YY=YB-W3:IFNS=KGOTO3014ELSEI1=3:L=W3-W4:
'IFPEEK(D2+NN)-PEEK(D1+NN)+4=LGOTO3014ELSEGOSUB100
  FOR K=1 TO 3 STEP 2
    NN=LR+K*Q
    YY=YB-W3
    IF NS2=K THEN 
      GOTO DM3014 
    ELSE 
      I1=3
      L=W3-W4
      IF TrsPeek(D2+NN)-TrsPeek(D1+NN)+4=L THEN
        GOTO DM3014
      ELSE
        GOSUB DM100
      END IF
    END IF
'3012 I1=5-2*PEEK(NT+NN):IFI1>=0ANDI1<5THENYY=YB-W4-PEEK(D2+NN):
'L=PEEK(D2+NN)-PEEK(D1+NN):GOSUB120
    I1=5-2*TrsPeek(NT+NN)
    IF I1>=0 AND I1<5 THEN
      YY=YB-W4-TrsPeek(D2+NN)
      L=TrsPeek(D2+NN)-TrsPeek(D1+NN)
      GOSUB DM120
    END IF
'3014 XX=V3-XB:NEXTK
DM3014:
    XX=V3-XB
  NEXT K
'3032 YY=YB-W3:FORK=0TO2STEP2:NN=LR+K*Q:IFNS=KGOTO3034ELSEI1=1:XX=V3-XB:L=2*(V4-V3):
'IFPEEK(D2+NN)-PEEK(D1+NN)+4=L/2GOTO3034ELSEGOSUB140:I1=2-PEEK(NT+NN):
'IFI1>=0ANDI1<2THENXX=V3-XB+PEEK(D1+NN):L=2*(PEEK(D2+NN)-PEEK(D1+NN)):GOSUB150
  YY=YB-W3
  FOR K=0 TO 2 STEP 2
    NN=LR+K*Q
    IF NS2=K THEN
      GOTO DM3034
    ELSE 
      I1=1
      XX=V3-XB
      L=2*(V4-V3)
      IF TrsPeek(D2+NN)-TrsPeek(D1+NN)+4=L/2 THEN
        GOTO DM3034
      ELSE 
        GOSUB DM140
        I1=2-TrsPeek(NT+NN)
        IF I1>=0 AND I1<2 THEN
          XX=V3-XB+TrsPeek(D1+NN)
          L=2*(TrsPeek(D2+NN)-TrsPeek(D1+NN))
          GOSUB DM150
        END IF
      END IF
    END IF
'3034 YY=YB-W4-2:NEXTK:RETURN
DM3034:
    YY=YB-W4-2
  NEXT K
  RETURN

'4000 LR=KR:GOSUB800:IFPEEK(KA+KR-61)=1GOTO4005ELSECLS:GOSUB80
DM4000:
  LR=KR
  GOSUB DM800
  IF TrsPeek(KA+KR-61)=1 THEN
    GOTO DM4005
  ELSE
    TrsCls
    GOSUB DM80
  END IF
'4002 XB=V1:YB=W1:IFPEEK(NT+LR+3*Q)=1THENXB=XB-(48-V2+V1)/2:IFPEEK(NT+LR+Q)=1THENXB=XB+(V1-XB)/2
  XB=V1:YB=W1
  IF TrsPeek(NT+LR+3*Q)=1 THEN 
    XB=XB-(48-V2+V1)/2
    IF TrsPeek(NT+LR+Q)=1 THEN XB=XB+(V1-XB)/2
  END IF
'4003 IFPEEK(NT+LR)=1THENYB=YB+48-W1+W2:IFPEEK(NT+LR+2*Q)=1THENYB=YB-(YB-W1)/2
  IF TrsPeek(NT+LR)=1 THEN
    YB=YB+48-W1+W2
    IF TrsPeek(NT+LR+2*Q)=1 THEN YB=YB-(YB-W1)/2
  END IF
'4004 PRINT@624,BL$;:XL=XA-V1:YL=YA-W2:IFPEEK(MT+KR)>0ANDPEEK(MN+KR)>0THEN
DM4004:
'GOSUB4500:NB=1:ELSEGOSUB4800
  TrsPrintS 624,BL$
  XL=XA-V1
  YL=YA-W2
  IF TrsPeek(MT+KR)>0 AND TrsPeek(MN+KR)>0 THEN
    GOSUB DM4500
    NB=1
  ELSE
    GOSUB DM4800
  END IF
'4005 PRINT@48,"ROOM NO.:"KR;:PRINT@112,"WOUNDS:"INT(100*PC/PH+.5);"%";:PRINT@176,"FATIGUE:"TA;"%";:PRINT@240,"WGT:"WC;" LBS";:PRINT@624,A$;:PRINT@496,"ARROWS:"PEEK(KB+13);:PRINT@560,"MAGIC AR:"PEEK(KB+14);:PRINT@752,"TOTAL SLAIN:";:PRINT@816,KC;  
DM4005:
  TrsPrintSF 48,"ROOM NO.:",KR
  ' Temp Fix!
  If PH <> 0 THEN TrsPrintSFS 112,"WOUNDS:",INT(100*PC/PH+.5),"%"
  TrsPrintSFS 176,"FATIGUE:",TA,"%"
  TrsPrintSFS 240,"WGT:",WC," LBS"
  TrsPrintS 624,A$
  IF A$<>"" THEN PAUSE 200
  TrsPrintSF 496,"ARROWS:",TrsPeek(KB+13)
  TrsPrintSF 560,"MAGIC AR:",TrsPeek(KB+14)
  TrsPrintS 752,"TOTAL SLAIN:"
  TrsPrintF 816,KC
'  Colour RGB(BLUE)
'  TrsPrintS 1024, "[F1] MOVE-[#RLV] SPECIAL-[OESGDQ!HY] ATTACK-[ATPFM] [END]"
'  Colour RGB(WHITE)
'4010 IFNB>0THENGOSUB300
  IF NB>0 THEN GOSUB DM300
'4020 NS=5:IFIB=3GOTO4050
  NS2=5
  IF IB=3 THEN GOTO DM4050
'4030 FORIR=0TO3:I=KR+IR*Q:IFPEEK(NT+I)=1ANDPEEK(NO+I)>0THENLR=PEEK(NO+I):GOSUB3000
  FOR IR2=0 TO 3
    I=KR+IR2*Q
    IF TrsPeek(NT+I)=1 AND TrsPeek(NO+I)>0 THEN
      LR=TrsPeek(NO+I)
      GOSUB DM3000
    END IF
'4040 NEXTIR
  NEXT IR2
'4050 LR=KR:GOSUB3000:GOSUB200:GOSUB679:RETURN
DM4050:
  LR=KR
  GOSUB DM3000
  GOSUB DM200
  GOSUB DM679
  If LastKr <> KR THEN
    DisplayRoomByMode TrsPeek(KA),KR
    LastKr = KR
    If A$<>"" Then
      LastMonster$ = A$
      DisplayMonsterByMode A$
    End If
  End If
  RETURN

'4500 MQ=PEEK(MT+KR):MW=0:ML=PEEK(UL+MQ):MA=PEEK(UA+MQ):MP=PEEK(UP+MQ):MS=PEEK(UV+MQ):
'MD=PEEK(UD+MQ):MH=PEEK(UC+MQ):MR=0:GOSUB880:IH=0
DM4500:
  MQ=TrsPeek(MT+KR)
  MW=0
  ML=TrsPeek(UL+MQ)
  MA=TrsPeek(UA+MQ)
  MP=TrsPeek(UP+MQ)
  MS=TrsPeek(UV+MQ)
  MD=TrsPeek(UD+MQ)
  MH=TrsPeek(UC+MQ)
  MR=0
  GOSUB DM880
  IH=0
'4520 XM=RND(V2-V1-8)+4:YM=RND(W1-W2-8)+4
DM4520:
  XM=TrsRnd(V2-V1-8)+4
  YM=TrsRnd(W1-W2-8)+4
'4550 MF=RND(4):RETURN
  MF=TrsRnd(4)
  RETURN

'4800 L=RND(100):IFL>PWTHENNB=0:A$="":RETURN  
DM4800:
  L=TrsRnd(100)
  IF L>PW THEN
    NB=0
    A$=""
    RETURN
  END IF
'4810 L=RND(100):LS=0:NB=1:FORI=1TOQ1:LS=LS+PEEK(UW+I):IFL<=LSGOTO4830
DM4810:
  L=TrsRnd(100)
  LS=0
  NB=1
  FOR I=1 TO Q1
    LS=LS+TrsPeek(UW+I)
    IF L<=LS GOTO DM4830
'4820 NEXTI
  NEXT I
'4830 POKEMT+KR,I:POKEMN+KR,1:MQ=I:ML=PEEK(UL+I):MA=PEEK(UA+I):MP=PEEK(UP+I):MS=PEEK(UV+I):MD=PEEK(UD+I):MH=PEEK(UC+I):GOSUB880:IH=0:GOSUB4520:RETURN
DM4830:
  TrsPoke MT+KR,I
  TrsPoke MN+KR,1
  MQ=I
  ML=TrsPeek(UL+I)
  MA=TrsPeek(UA+I)
  MP=TrsPeek(UP+I)
  MS=TrsPeek(UV+I)
  MD=TrsPeek(UD+I)
  MH=TrsPeek(UC+I)
  GOSUB DM880
  IH=0
  GOSUB DM4520
  RETURN  

'4850 L=RND(100):IFL<PW/6GOTO4810ELSERETURN
DM4850: ' Monster Generator?
  L=TrsRnd(100)
  IF L<PW/6 THEN
    GOTO DM4810
  ELSE
    RETURN
  END IF
  
'5000 KR=1:O$="RLATPFMGEV!HQSYDOI":GOSUB1200
DM5000:
  KR=1
  LastKR = -1
  O$="RLATPFMGEV!HQSYDOIC" ' Added C (I never mentioned in manual.)
  GOSUB DM1200
'5030 GOSUB4000:S(1)=W1-W2:S(2)=V2-V1:S(3)=0:S(4)=0'BUILD INITIAL ROOM
DM5030:
  GOSUB DM4000
  S(1)=W1-W2
  S(2)=V2-V1
  S(3)=0
  S(4)=0'BUILD INITIAL ROOM
'5040 P(1)=XA-V1:P(2)=YA-W2:P(3)=P(1):P(4)=P(2):P(0)=P(2)
  P(1)=XA-V1
  P(2)=YA-W2
  P(3)=P(1)
  P(4)=P(2)
  P(0)=P(2)
'5044 X0=XL:Y0=YL:IA=0:GOSUB500:M=0:PRINT@304,BL$;:PRINT@368,BL$;:PRINT@432,BL$;:IFL=0GOTO7000
DM5044:
  X0=XL
  Y0=YL
  IA=0
  GOSUB DM500
  M2=0
  TrsPrintS 304,BL$
  TrsPrintS 368,BL$
  TrsPrintS 432,BL$
  IF L=0 THEN GOTO DM7000
'5045 IA=0:IFASC(C$)>47ANDASC(C$)<58GOTO5100ELSEFORI=1TO18:IFC$=MID$(O$,I,1)THENONIGOTO
'5300,5350,5390,5390,5390,5390,5390,5800,5700,5370,5900,6100,6200,6300,6140,5850,5600,8000
  IA=0
  IF ASC(C$)>47 AND ASC(C$)<58 THEN
    GOTO DM5100
  ELSE
    FOR I=1 TO 19 ' 19 for 'C'
      IF C$=MID$(O$,I,1) THEN
        ON I GOTO DM5300,DM5350,DM5390,DM5390,DM5390,DM5390,DM5390,DM5800,DM5700,DM5370,DM5900,DM6100,DM6200,DM6300,DM6140,DM5850,DM5600,DM8000,DM8100
      END IF
'5050 NEXTI:GOTO5044
    NEXT I
    GOTO DM5044
  END IF

'5100 IFTA<1GOTO5390:ELSEM=ASC(C$)-48
DM5100: ' Command 1-9
  IF TA<1 THEN
    GOTO DM5390
  ELSE
    M2=ASC(C$)-48
  END IF
'5110 M1=M:N=PEEK(NP+KR):IB=0:ONKFGOTO5120,5210,5164,5250
  M1=M2
  N=TrsPeek(NP+KR)
  IB=0
  ON KF GOTO DM5120,DM5210,DM5164,DM5250
'5120 IFYA+M>W1-3THENM=W1-3-YA:IB=1
DM5120:
  IF YA+M2>W1-3 THEN M2=W1-3-YA:IB=1
'5150 GOTO5281
  GOTO DM5281

'5164 M=-M:M1=M:IFYA+M<W2+4THENM=W2+4-YA:IB=1
DM5164:
  M2=-M2
  M1=M2
  IF YA+M2<W2+4 THEN M2=W2+4-YA:IB=1
'5180 GOTO5281
  GOTO DM5281

'5210 IFXA+M>V2-3THENM=V2-3-XA:IB=1  
DM5210:
  IF XA+M2>V2-3 THEN M2=V2-3-XA:IB=1
'5240 GOTO5281
  GOTO DM5281

'5250 M=-M:M1=M:IFXA+M<V1+4THENM=V1+4-XA:IB=1  
DM5250:
  M2 =-M2
  M1=M2
  IF XA+M2<V1+4 THEN M2=V1+4-XA:IB=1
'5270 GOTO5281
  GOTO DM5281

'5280 IFNB>0GOTO7000ELSEGOTO7020
DM5280:
  IF NB>0 THEN 
    GOTO DM7000
  ELSE
    GOTO DM7020
  END IF

'5281 IFIB=0GOTO5290:ELSEXL=XA-V1:YL=YA-W2:IFPEEK(NT+KR+KF*Q-Q)<>1GOTO5290  
DM5281:
  IF IB=0 THEN
    GOTO DM5290
  ELSE
    XL=XA-V1
    YL=YA-W2
    IF TrsPeek(NT+KR+KF*Q-Q)<>1 THEN GOTO DM5290
  END IF
'5282 L=KR+KF*Q-Q:IFP(KF)>PEEK(D1+L)ANDP(KF)<PEEK(D2+L)THENGOSUB345:GOSUB82:NB=0:IN=0:KR=PEEK(NO+L):GOSUB800:IB=2:M=ABS(M)+4:IFKR=0GOTO10000
  L=KR+KF*Q-Q
  IF P(KF)>TrsPeek(D1+L) AND P(KF)<TrsPeek(D2+L) THEN
    GOSUB DM345
    GOSUB DM82
    NB=0
    IN=0
    KR=TrsPeek(NO+L)
    GOSUB DM800
    IB=2
    M2=ABS(M2)+4
    IF KR=0 THEN GOTO DM10000
  END IF
'5284 IFIB=1GOTO5290
  IF IB=1 THEN GOTO DM5290
'5285 IFV1-XB>-1ANDV2-XB<49ANDYB-W1>-1ANDYB-W2<49THENIB=3:S(1)=W1-W2:S(2)=V2-V1:M=M1:GOTO5290
  IF V1-XB>-1 AND V2-XB<49 AND YB-W1>-1 AND YB-W2<49 THEN
    IB=3
    S(1)=W1-W2
    S(2)=V2-V1
    M2=M1
    GOTO DM5290
  END IF
'5286 IFKF=1THENYA=W2+4:GOTO5030
  IF KF=1 THEN YA=W2+4:GOTO DM5030
'5287 IFKF=3THENYA=W1-4:GOTO5030
  IF KF=3 THEN YA=W1-4:GOTO DM5030
'5288 IFKF=2THENXA=V1+4:ELSEXA=V2-4
  IF KF=2 THEN
    XA=V1+4 
  ELSE 
    XA=V2-4
  END IF
'5289 GOTO5030
  GOTO DM5030

'5290 GOSUB250:GOSUB679:IFKF=1ORKF=3THENYA=YA+M:ELSEXA=XA+M  
DM5290:
  GOSUB DM250
  GOSUB DM679
  IF KF=1 OR KF=3 THEN 
    YA=YA+M2
  ELSE
    XA=XA+M2
  END IF
'5291 IFPEEK(KA-86)>0THENM=M/2
  IF TrsPeek(KA-86)>0 THEN M2=M2/2
'5292 GOSUB200:P(1)=XA-V1:P(3)=P(1):P(2)=YA-W2:P(4)=P(2):P(0)=P(2):IFIB=3THENLR=KR:GOSUB4004:IB=0:GOTO5044
  GOSUB DM200
  P(1)=XA-V1:P(3)=P(1):P(2)=YA-W2:P(4)=P(2):P(0)=P(2)
  IF IB=3 THEN
    LR=KR
    GOSUB DM4004
    IB=0
    GOTO DM5044
  END IF
'5295 XL=XA-V1:YL=YA-W2:GOSUB695:IFL>0THENGOSUB650:GOTO7020ELSEGOTO7000
  XL=XA-V1:YL=YA-W2
  GOSUB DM695
  IF L>0 THEN
    GOSUB DM650
    GOTO DM7020
  ELSE
    GOTO DM7000
  END IF

'5300 GOSUB250:KF=KF+1:IFKF>4THENKF=1
DM5300: ' Command R
  GOSUB DM250
  KF=KF+1
  IF KF>4 THEN KF=1
'5310 GOSUB200:GOTO5044
  GOSUB DM200
  GOTO DM5044

'5350 GOSUB250:KF=KF-1:IFKF<1THENKF=4
DM5350: ' Command L
  GOSUB DM250
  KF=KF-1
  IF KF<1 THEN KF=4
'5360 GOSUB200:GOTO5044
  GOSUB DM200
  GOTO DM5044

'5370 GOSUB250:KF=KF-2:IFKF<1THENKF=KF+4
DM5370: ' Command V
  GOSUB DM250
  KF=KF-2
  IF KF<1 THEN KF=KF+4
'5380 GOSUB200:GOTO5044
  GOSUB DM200
  GOTO DM5044

'5390 IFTA<1THENPRINT@368,BL$;:PRINT@368,"TOO TIRED";:GOTO7000:
'ELSEIA=I-2:IN=0:KM=0:ONIAGOTO5400,5400,5400,5500,5490
DM5390: ' Commands A,T,P,F,M
  IF TA<1 THEN
    TrsPrintS 368,BL$
    TrsPrintS 368,"TOO TIRED"
    GOTO DM7000
  ELSE
    IA=I-2
    IN=0
    KM=0
    ON IA GOTO DM5400,DM5400,DM5400,DM5500,DM5490
  END IF

'5400 HI=0:IFABS(XL-XM)>5ORABS(YL-YM)>5THENPRINT@368,"TOO FAR TO HIT";:GOTO7000
DM5400:
  HI=0
  IF ABS(XL-XM)>5 OR ABS(YL-YM)>5 THEN
    TrsPrintS 368,"TOO FAR TO HIT"
    GOTO DM7000
  END IF
'5430 M=TM(IA):K=0:GOSUB600:P=PB-(PEEK(KB+22)-9)/3*EXP(-2*PC/PH)+ML/3-ZA( IA):R=RND(20):IFR<PTHENPRINT@368,"SWISH!";:GOTO6990
  M2=TM(IA)
  K=0
  GOSUB DM600
  P2=PB-(TrsPeek(KB+22)-9)/3*EXP(-2*PC/PH)+ML/3-ZA(IA)
  R2=TrsRnd(20)
  IF R2<P2 THEN 
    TrsPrintS 368,"SWISH!"
    GOTO DM6990
  END IF
'5435 AK=AS*(R-P+1):PRINT@368,"CRUNCH!";:IFAK>WMTHENAK=WM
  AK=AS2*(R2-P2+1)
  TrsPrintS 368,"CRUNCH!"
  IF AK>WM THEN AK=WM
'5437 IFAK<MHTHENAK=MH
DM5437:
  IF AK<MH THEN AK=MH
'5440 IFPEEK(US+MQ)<>2OR(PEEK(KB+10)>0ANDIA<3)ORIA=5THENMP=MP-AK+MH:IN=0
  IF TrsPeek(US+MQ)<>2 OR (TrsPeek(KB+10)>0 AND IA<3) OR IA=5 THEN
    MP=MP-AK+MH
    IN=0
  END IF
'5450 GOTO6990
  GOTO DM6990

'5490 I1=5:KM=5:IFPEEK(KB+14)<=0GOTO7000:ELSEPOKEKB+14,PEEK(KB+14)-1:GOSUB74:GOTO5505
DM5490:
  I1=5:KM=5
  IF TrsPeek(KB+14)<=0 THEN 
    GOTO DM7000
  ELSE
    TrsPoke KB+14,TrsPeek(KB+14)-1
    GOSUB DM74
    GOTO DM5505
  END IF

'5500 I1=3:IFPEEK(KB+13)<=0GOTO7000:ELSEPOKEKB+13,PEEK(KB+13)-1:GOSUB72
DM5500:
  I1=3
  IF TrsPeek(KB+13)<=0 THEN
    GOTO DM7000
  ELSE
    TrsPoke KB+13,TrsPeek(KB+13)-1
    GOSUB DM72
  END IF
'5505 ONKFGOTO5510,5550,5515,5555
DM5505:
  ON KF GOTO DM5510,DM5550,DM5515,DM5555

'5510 LY=YL+2:UY=YM-3:S=1:GOTO5520
DM5510:
  LY=YL+2
  UY=YM-3
  S3=1
  GOTO DM5520
'5515 LY=YL-2:UY=YM+3:S=-1
DM5515:
  LY=YL-2
  UY=YM+3
  S3=-1
'5520 X=XL-1:FORY=LYTOUYSTEPS:I=2*(X+V1-XB):J=YB-Y-W2:SET(I,J):RESET(I,J):X=X*1:NEXTY:GOTO5580
DM5520:
  X=XL-1
  FOR Y=LY TO UY STEP S3
    I=2*(X+V1-XB)
    J=YB-Y-W2
    TRSSET(I,J)
    pause 100 'WAIT!!!
    TRSRESET(I,J)
    X=X*1
  NEXT Y
  GOTO DM5580

'5550 LX=XL+1:UX=XM-3:S=1:GOTO5560
DM5550:
  LX=XL+1
  UX=XM-3
  S3=1
  GOTO DM5560

'5555 LX=XL-3:UX=XM+3:S=-1
DM5555:
  LX=XL-3
  UX=XM+3
  S3=-1
'5560 Y=YL:FORX=LXTOUXSTEPS:I=2*(X+V1-XB):J=YB-Y-W2:SET(I,J):RESET(I,J):Y=Y*1:NEXTX
DM5560:
  Y=YL
  FOR X=LX TO UX STEP S3
    I=2*(X+V1-XB)
    J=YB-Y-W2
    TRSSET(I,J)
    pause 100 ' WAIT!!!
    TRSRESET(I,J)
    Y=Y*1
  NEXT X
'5580 IFABS(X-XM)<I1ANDABS(Y-YM)<I1THENPRINT@368,"THWUNK!";:AK=RND(7)+KM:GOTO5437:ELSEPRINT@368,"SSWHT!";:GOTO7000
DM5580:
  IF ABS(X-XM)<I1 AND ABS(Y-YM)<I1 THEN 
    TrsPrintS 368,"THWUNK!"
    AK=TrsRnd(7)+KM
    GOTO DM5437
  ELSE
    TrsPrintS 368,"SSWHT!"
    GOTO DM7000
  END IF

'5600 K=KR+KF*Q-Q:IFPEEK(NT+K)=2THENIFP(KF)>PEEK(D1+K)ANDP(KF)<PEEK(D2+K)THENIFABS(P(KF-1)-S(KF))<6THENPOKENT+K,1:IJ=0:ONKFGOSUB5660,5620,5680,5640:GOTO5605
DM5600: ' Command O
  K=KR+KF*Q-Q
  IF TrsPeek(NT+K)=2 THEN
    IF P(KF)>TrsPeek(D1+K) AND P(KF)<TrsPeek(D2+K) THEN 
      IF ABS(P(KF-1)-S(KF))<6 THEN
        TrsPoke NT+K,1
        IJ=0
        ON KF GOSUB DM5660,DM5620,DM5680,DM5640
        GOTO DM5605
      END IF
    END IF
  END IF
'5603 GOTO7000
  GOTO DM7000

'5605 LR=PEEK(NO+K):NS=KF+1:IFNS>3THENNS=NS-4
DM5605:
  LR=TrsPeek(NO+K)
  NS2=KF+1
  IF NS2>3 THEN NS2=NS2-4
'5607 POKENT+LR+NS*Q,1:NS=5:GOSUB3000:GOTO7000
  TrsPoke NT+LR+NS2*Q,1
  NS2=5
  GOSUB DM3000
  GOTO DM7000
  
'5620 XX=V2-XB-2DM5620:
DM5620:
  XX=V2-XB-2
'5630 I1=3+IJ:YY=YB-W2-PEEK(D2+K):L=PEEK(D2+K)-PEEK(D1+K):GOSUB120:RETURN
DM5630:
  I1=3+IJ
  YY=YB-W2-TrsPeek(D2+K)
  L=TrsPeek(D2+K)-TrsPeek(D1+K)
  GOSUB DM120
  RETURN
  
'5640 XX=V1-XB:GOTO5630
DM5640:
  XX=V1-XB
  GOTO DM5630

'5660 YY=YB-W1
DM5660:
  YY=YB-W1
'5670 I1=1+IJ/2:XX=V1-XB+PEEK(D1+K):L=2*(PEEK(D2+K)-PEEK(D1+K)):GOSUB150:RETURN
DM5670:
  I1=1+IJ/2
  XX=V1-XB+TrsPeek(D1+K)
  L=2*(TrsPeek(D2+K)-TrsPeek(D1+K))
  GOSUB DM150
  RETURN

'5680 YY=YB-W2-2:GOTO5670
DM5680:
  YY=YB-W2-2
  GOTO DM5670

'5700 K=KR+(KF-1)*Q:IFPEEK(K+NT)=3ANDRND(40)<20+PEEK(KB+21)THENPOKEK+NT,2:RF(KF-1)=1:PRINT@304,BL$;:PRINT@304,"A SECRET DOOR!";:IJ=-2:LR=KR:ONKFGOSUB5660,5620,5680,5640:ELSEPRINT@304,"NOTHING";
DM5700: ' Command E
  K=KR+(KF-1)*Q
  IF TrsPeek(K+NT)=3 AND TrsRnd(40)<20+TrsPeek(KB+21) THEN
    TrsPoke K+NT,2
    RF(KF-1)=1
    TrsPrintS 304,BL$
    TrsPrintS 304,"A SECRET DOOR!"
    Pause 500
    IJ=-2:LR=KR
    ON KF GOSUB DM5660,DM5620,DM5680,DM5640
  ELSE
    TrsPrintS 304,"NOTHING"
  END IF
'5710 GOTO7000
  GOTO DM7000

'5800 N=PEEK(TR+KR):PRINT@304,BL$;:IFN=0ORXL-PEEK(XR+KR)>3ORYL-PEEK(YR+KR)>3THENPRINT@304,"YOU CAN'T";:GOTO7000:ELSEPOKETR+KR,0:PRINT@304,"TREASURE #"N;:POKEKA+N-81,PEEK(KA+N-81)+1:WC=WC+PEEK(TW+N):PRINT@240,"WGT:"WC;:IN=0
DM5800: ' Command G
  N=TrsPeek(TR+KR)
  TrsPrintS 304,BL$
  IF N=0 OR XL-TrsPeek(XR+KR)>3 OR YL-TrsPeek(YR+KR)>3 THEN 
    TrsPrintS 304,"YOU CAN'T"
    GOTO DM7000
  ELSE
    TrsPoke TR+KR,0
    TrsPrintSF 304,"TREASURE #",N
    TrsPoke KA+N-81,TrsPeek(KA+N-81)+1
    WC=WC+TrsPeek(TW+N)
    TrsPrintSF 240,"WGT:",WC
    IN=0
    LastTreasure = N
    DisplayTreasureByMode TrsPeek(KA), N
  END IF
'5803 GOSUB680:I=PEEK(TS+N):IFI>100THENJ=KB+I-81:POKEJ,PEEK(J)+1:J=KA+I-197:POKEJ,PEEK(J)+1:GOTO7000:ELSEONI+1GOTO7000,5810,5815,5820,5825,5830,5835,5805,5840,5845,5846,12000
  GOSUB DM680
  I=TrsPeek(TS+N)
  IF I>100 THEN
    J=KB+I-81
    TrsPoke J,TrsPeek(J)+1
    J=KA+I-197
    TrsPoke J,TrsPeek(J)+1
    GOTO DM7000
  ELSE
    ON I+1 GOTO DM7000,DM5810,DM5815,DM5820,DM5825,DM5830,DM5835,DM5805,DM5840,DM5845,DM5846,DM12000
  END IF

'5805 POKEKA-89,1:GOTO7000
DM5805:
  TrsPoke KA-89,1
  GOTO DM7000
  
'5810 J=RND(6):PRINT@945,J;"ITEMS";:POKEKB+6,PEEK(KB+6)+J:GOTO7000
DM5810:
  J=TrsRnd(6)
  TrsPrintFS 945,J,"ITEMS"
  TrsPoke KB+6,TrsPeek(KB+6)+J
  GOTO DM7000

'5815 PS=4:SP=SP+1:GOTO7000
DM5815:
  PS=4:SP=SP+1
  GOTO DM7000
'5820 PRINT@944,"DOST USE SWORD?";:GOSUB500:IFL=0GOTO5820ELSEPRINT@944,BL$;:IFC$="Y"THENPB=PB+SM:SM=RND(4)+RND(4)-4:PB=PB-SM:WM=AS*(7+SM)+.5:IFSM>0THENSM=SM-INT(2-PEEK(KA)/2):IFSM>1THENPRINT@944,"THE SWORD GLOWS";
DM5820:
  TrsPrintS 944,"DOST USE SWORD?"
  GOSUB DM500
  IF L=0 THEN 
    GOTO DM5820
  ELSE
    TrsPrintS 944,BL$
    IF C$="Y" THEN
      PB=PB+SM:SM=TrsRnd(4)+TrsRnd(4)-4:PB=PB-SM:WM=AS2*(7+SM)+.5
      IF SM>0 THEN
        SM=SM-INT(2-TrsPeek(KA)/2)
        IF SM>1 THEN TrsPrintS 944,"THE SWORD GLOWS"
      END IF
    END IF
  END IF
'5821 IFC$<>"Y"THENPOKEKA+N-81,PEEK(KA+N-81)-1:WC=WC-PEEK(TW+N):PRINT@944,BL$;
  IF C$<>"Y" THEN
    TrsPoke KA+N-81,TrsPeek(KA+N-81)-1
    WC=WC-TrsPeek(TW+N)
    TrsPrintS 944,BL$
  END IF
'5822 GOTO7000
  GOTO DM7000
  
'5825 J=RND(20):GOSUB60:POKEKB+13,PEEK(KB+13)+J:GOSUB72:GOTO7000
DM5825:
  J=TrsRnd(20)
  GOSUB DM60
  TrsPoke KB+13,TrsPeek(KB+13)+J
  GOSUB DM72
  GOTO DM7000
  
'5830 J=RND(10):GOSUB60:POKEKB+14,PEEK(KB+14)+J:GOSUB74:GOTO7000
DM5830:
  J=TrsRnd(10)
  GOSUB DM60:
  TrsPoke KB+14,TrsPeek(KB+14)+J
  GOSUB DM74
  GOTO DM7000
  
'5835 POKEKA-90,1:PA=PA+1:GOTO7000
DM5835:
  TrsPoke KA-90,1
  PA=PA+1
  GOTO DM7000
  
'5840 POKEKA-88,1:GOTO7000
DM5840:
  TrsPoke KA-88,1
  GOTO DM7000
  
'5845 POKEKA-87,1:GOTO7000
DM5845:
  TrsPoke KA-87,1
  GOTO DM7000
  
'5846 PW=75:POKEKA+2235,PW:GOTO7000
DM5846:
  PW=75
  TrsPoke KA+2235,PW
  GOTO DM7000
  
'5850 PRINT@944,"DROP SOME?";
DM5850: ' Command D
  TrsPrintS 944,"DROP SOME?"
'5860 GOSUB70:JJ=10*J:GOSUB70:JJ=JJ+J:IFJJ<21THENI=PEEK(KA+JJ-81):IFI>0THENPOKEKA+JJ-81,I-1:WC=WC-PEEK(TW+JJ):PRINT@944,BL$;:PRINT@240,"WGT:"WC;:IFPEEK(TR+KR)=0ANDPEEK(TS+JJ)=0THENPOKETR+KR,JJ:POKEXR+KR,XL:POKEYR+KR,YL:GOSUB680
  GOSUB DM70
  JJ=10*J
  GOSUB DM70
  JJ=JJ+J
  IF JJ<21 THEN
    I=TrsPeek(KA+JJ-81)
    IF I>0 THEN 
      TrsPoke KA+JJ-81,I-1
      WC=WC-TrsPeek(TW+JJ)
      TrsPrintS 944,BL$
      TrsPrintSF 240,"WGT:",WC
      IF TrsPeek(TR+KR)=0 AND TrsPeek(TS+JJ)=0 THEN
        TrsPoke TR+KR,JJ
        TrsPoke XR+KR,XL
        TrsPoke YR+KR,YL
        GOSUB DM680
      END IF
    END IF
  END IF
'5870 PRINT@944,BL$;:GOTO7000
  TrsPrintS 944,BL$
  GOTO DM7000

'5900 IFRND(100)<.3*(PEEK(KB+20)+PEEK(KB+22))*PEEK(UI+MQ)THENIN=1ELSEGOTO7000
DM5900: ' Command !
  IF TrsRnd(100)<.3*(TrsPeek(KB+20)+TrsPeek(KB+22))*TrsPeek(UI+MQ) THEN
    IN=1
  ELSE
    GOTO DM7000
  END IF
'5910 PRINT@304,BL$;:PRINT@304,"PASS BY";:GOTO7000
  TrsPrintS 304,BL$
  TrsPrintS 304,"PASS BY"
  GOTO DM7000
  
'6100 IFPEEK(KB+11)>0THENPOKEKB+11,PEEK(KB+11)-1:J=0ELSEGOTO6150
DM6100: ' Command H
  IF TrsPeek(KB+11)>0 THEN
    TrsPoke KB+11,TrsPeek(KB+11)-1
    J=0
  ELSE
    GOTO DM6150
  END IF
'6110 PC=PC+1+J:IFPC>PHTHENPC=PH
  PC=PC+1+J
  IF PC>PH THEN PC=PH
'6130 PRINT@945,BL$;:PRINT@119,"     ";:PRINT@119,INT(100*PC/PH+.5);"%";:GOTO7000
  TrsPrintS 945,BL$
  TrsPrintS 119,"     "
  TrsPrintFS 119,INT(100*PC/PH+.5),"%"
  GOTO DM7000
  
'6140 I=KB+6:IFPEEK(I)>0THENPOKEI,PEEK(I)-1:J=RND(6)+1:GOTO6110
DM6140: ' Command Y
  I=KB+6
  IF TrsPeek(I)>0 THEN
    TrsPoke I,TrsPeek(I)-1
    J=TrsRnd(6)+1
    GOTO DM6110
  END IF
'6150 PRINT@945,"NONE LEFT";:GOTO5044
DM6150:
  TrsPrintS 945,"NONE LEFT"
  GOTO DM5044
  
'6200 JJ=PEEK(KB+21):PRINT@304,BL$;:I=PEEK(NO+KR+Q*KF-Q):MQ=PEEK(MT+I):IFI=0ORPEEK(MN+I)=0ORRND(1000)>JJ*JJ+PEEK(KA-89)*700THENPRINT@304,"NOTHING";:GOTO7000
DM6200: ' Command Q
  JJ=TrsPeek(KB+21)
  TrsPrintS 304,BL$
  I=TrsPeek(NO+KR+Q*KF-Q)
  MQ=TrsPeek(MT+I)
  IF I=0 OR TrsPeek(MN+I)=0 OR TrsRnd(1000)>JJ*JJ+TrsPeek(KA-89)*700 THEN
    TrsPrintS 304,"NOTHING"
    GOTO DM7000
  END IF
'6210 GOSUB880:PRINT@304,A$;:GOTO7000
  GOSUB DM880
  TrsPrintS 304,A$
  GOTO DM7000
  
'6300 IFPEEK(NP+KR)>0ANDRND(20)<PEEK(KB+21)THENGOSUB670:ELSEPRINT@304,BL$;:PRINT@304,"NOTHING";
DM6300: ' Command S
  IF TrsPeek(NP+KR)>0 AND TrsRnd(20)<TrsPeek(KB+21) THEN
    GOSUB DM670
  ELSE
    TrsPrintS 304,BL$
    TrsPrintS 304,"NOTHING"
  END IF
'6310 GOTO7000
  GOTO DM7000
  
'6990 K=1:GOSUB600
DM6990:
  K=1
  GOSUB DM600
'7000 GOSUB650:IFNB>0GOTO7005ELSEGOSUB4850:IFNB=0GOTO5044
DM7000:
  GOSUB DM650
  IF NB>0 THEN
    GOTO DM7005
  ELSE
    GOSUB DM4850
    IF NB=0 THEN GOTO DM5044
  END IF
'7002 PRINT@688,"APPEARS";:PRINT@624,BL$;:PRINT@624,A$;:GOSUB300:GOTO5044
  TrsPrintS 688,"APPEARS"
  TrsPrintS 624,BL$
  TrsPrintS 624,A$
  GOSUB DM300
  LastMonster$ = a$
  DisplayMonsterByMode a$
  GOTO DM5044
  
'7005 PRINT@432,BL$;:IFIN>0ORHI>0GOTO7500
DM7005:
  TrsPrintS 432,BL$
  IF IN>0 OR HI>0 THEN GOTO DM7500
'7010 IFABS(X0-XM)>5ORABS(Y0-YM)>5GOTO7300
  IF ABS(X0-XM)>5 OR ABS(Y0-YM)>5 THEN GOTO DM7300
'7015 IM=MA
  IM=MA
'7017 IM=IM-1:IFIM<0GOTO7250
DM7017:
  IM=IM-1
  IF IM<0 THEN GOTO DM7250
'7020 P=PA-ZD(IA):R=RND(20)+ML:IFR<PTHENPRINT@432,BL$;:IM=IM*1:PRINT@432,"IT MISSED!";:GOTO7017
DM7020:
  P2=PA-ZD(IA)
  R2=TrsRnd(20)+ML
  IF R2<P2 THEN 
    TrsPrintS 432,BL$
    IM=IM*1
    TrsPrintS 432,"IT MISSED!"
    GOTO DM7017
  END IF
'7030 GOSUB65:IFRND(20)-1<PEEK(KB+4)THENPRINT@432,BL$;:PRINT@432,"SHIELD HIT!";:K=-PS:ELSEPRINT@432,BL$;:PRINT@432,"STRUCK THEE!";:K=0:IFPEEK(US+MQ)=2ANDRND(20)>PEEK(KB+21)/2+PEEK(KA-81)ANDPEEK(KA-88)=0THENGOSUB50:PRINT@432,BL$;:PRINT@432,"A CHILL";
  GOSUB DM65
  IF TrsRnd(20)-1<TrsPeek(KB+4) THEN
    TrsPrintS 432,BL$
    TrsPrintS 432,"SHIELD HIT!"
    K=-PS
  ELSE
    TrsPrintS 432,BL$
    TrsPrintS 432,"STRUCK THEE!"
    K=0
    IF TrsPeek(US+MQ)=2 AND TrsRnd(20)>TrsPeek(KB+21)/2+TrsPeek(KA-81) AND TrsPeek(KA-88)=0 THEN
      GOSUB DM50
      TrsPrintS 432,BL$
      TrsPrintS 432,"A CHILL"
    END IF
  END IF
'7040 K=K+(MD*(R-P))/10-AA-PEEK(KB+16):IFK<0THENK=0
  K=K+(MD*(R2-P2))/10-AA-TrsPeek(KB+16)
  IF K<0 THEN K=0
'7050 PC=PC-K:IFPC<1THENFORI=1TO1000:NEXTI:GOTO11000
  PC=PC-K
  IF PC<1 THEN
    Pause 1000
    GOTO DM11000
  END IF
'7060 PRINT@119,B1$;:PRINT@119,INT(100*PC/PH+.5);"%";:IFNB>0GOTO7017ELSEGOTO5044
  TrsPrintS 119,B1$
  TrsPrintFS 119,INT(100*PC/PH+.5),"%"
  IF NB>0 THEN 
    GOTO DM7017
  ELSE
    GOTO DM5044
  END IF

'7250 IFNB=0GOTO5044ELSEGOSUB350:L=2:ONRND(4)GOTO7260,7270,7280,7290
DM7250:
  IF NB=0 THEN
    GOTO DM5044
  ELSE
    GOSUB DM350
    L=2
    ON TrsRnd(4) GOTO DM7260,DM7270,DM7280,DM7290
  END IF
  
'7260 YM=YM+L:GOTO7490
DM7260:
  YM=YM+L
  GOTO DM7490
  
'7270 XM=XM+L:GOTO7490
DM7270:
  XM=XM+L
  GOTO DM7490
  
'7280 YM=YM-L:GOTO7490
DM7280:
  YM=YM-L
  GOTO DM7490
  
'7290 XM=XM-L:GOTO7490
DM7290:
  XM=XM-L
  GOTO DM7490

'7300 GOSUB350:GOSUB679:XX=XL-XM:YY=YL-YM:IFABS(XX)<ABS(YY)GOTO7306:ELSEIFXX>0THENMF=2ELSEMF=4
DM7300:
  GOSUB DM350
  GOSUB DM679
  XX=XL-XM
  YY=YL-YM
  IF ABS(XX)<ABS(YY)THEN
    GOTO DM7306
  ELSE
    IF XX>0 THEN
      MF=2
    ELSE
      MF=4
    END IF
  END IF
'7304 GOTO7310
  GOTO DM7310
  
'7306 IFYY>0THENMF=1ELSEMF=3
DM7306:
  IF YY>0 THEN
    MF=1
  ELSE
    MF=3
  END IF
'7310 L=MS:ONMFGOTO7320,7360,7400,7440
DM7310:
  L=MS
  ON MF GOTO DM7320,DM7360,DM7400,DM7440
  
'7320 IFYM+L>YL-3THENYM=YL-3:ELSEYM=YM+L
DM7320:
  IF YM+L>YL-3 THEN
    YM=YL-3
  ELSE
    YM=YM+L
  END IF
'7330 GOTO7490
  GOTO DM7490
  
'7360 IFXM+L>XL-3THENXM=XL-3:ELSEXM=XM+L
DM7360:
  IF XM+L>XL-3 THEN
    XM=XL-3
  ELSE
    XM=XM+L
  END IF
'7370 GOTO7490
  GOTO DM7490

'7400 IFYM-L<YL+3THENYM=YL+3:ELSEYM=YM-L
DM7400:
  IF YM-L<YL+3 THEN
    YM=YL+3
  ELSE
    YM=YM-L
  END IF
'7410 GOTO7490
  GOTO DM7490

'7440 IFXM-L<XL+3THENXM=XL+3:ELSEXM=XM-L
DM7440:
  IF XM-L<XL+3 THEN
    XM=XL+3
  ELSE
    XM=XM-L
  END IF
'7490 GOSUB55:GOSUB300
DM7490:
  GOSUB DM55
  GOSUB DM300

'7500 IFMP<1ANDNB>0THENGOSUB350:GOSUB450:EX#=EX#+20*ML*ML+15:KC=KC+1:PRINT@816,KC;:PRINT@304,BL$;:
'PRINT@304,"MONSTER SLAIN!";:M=PEEK(MN+KR):IFM>0THENPOKEMN+KR,M-1:IFM>1THENGOSUB4500:
'PRINT@304,"ANOTHER APPEARS";:GOSUB300
DM7500:
  IF MP<1 AND NB>0 THEN
    GOSUB DM350
    GOSUB DM450
    EX=EX+20*ML*ML+15
    KC=KC+1
    TrsPrintF 816,KC
    TrsPrintS 304,BL$
    TrsPrintS 304,"MONSTER SLAIN!"
    M2=TrsPeek(MN+KR)
    IF M2>0 THEN
      TrsPoke MN+KR,M2-1
      IF M2>1 THEN
        GOSUB DM4500
        TrsPrintS 304,"ANOTHER APPEARS"
        GOSUB DM300
      END IF
    END IF
  END IF
'7510 IFMP<1THENNB=0:PRINT@368,BL$;:PRINT@432,BL$;:PRINT@624,BL$;:PRINT@688,BL$;
  IF MP<1 THEN
    NB=0
    TrsPrintS 368,BL$
    TrsPrintS 432,BL$
    TrsPrintS 624,BL$
    TrsPrintS 688,BL$
  END IF
'7520 GOTO5044
  GOTO DM5044
  
'8000 PRINT@880,CHR$(30);"TREASURE    #";:FORL5=1TO20:
'PRINT@946,CHR$(30);:PRINTL5;TAB(59);PEEK(KA-81+L5);
DM8000: ' Command I
  DisplayTreasures
  GOTO DM7000
'  TrsPrintS 880, "TREASURE    #"
'  FOR L5=1 TO 20
'    TrsPrintF 946,L5
'    TrsPrintF 955, TrsPeek(KA-81+L5)
'    Pause 500
    'TrsPrint ""
'8010 FORL6=1TO30:IFPEEK(14400)<>0L6=1
'    FOR L6=1 TO 30
'      IF TrsPeek(14400)<>0 THEN L6=1
'8020 NEXTL6,L5:PRINT@880,CHR$(30)CHR$(26)CHR$(30);:GOTO7000
'    NEXT L6
'  NEXT L5
'  TrsPrintS 880, "             "
'  TrsPrintS 946, "             "
  GOTO DM7000

DM8100: ' Command C ' New Code
  DisplayCharacter
  GOTO DM7000

'10000 CLS:PRINT"THOU LEAVEST THE DUNJON"
DM10000:
  TrsCls
  PRINT "THOU LEAVEST THE DUNJON"
'10010 PRINT"EXPERIENCE:"EX#:INPUT"DOST WISH TO REENTER";C$:IFLEFT$(C$,1)="Y"THENWC=PEEK(KB+12):
'KR=1:PC=PH:XA=PEEK(KA+2236):YA=PEEK(KA+2237):KF=PEEK(KA+2238):TA=100:GOSUB80:GOTO5030
DM10010:
  PRINT "EXPERIENCE:" EX
  C$ = TrsInput("DOST WISH TO REENTER ")
  IF LEFT$(C$,1)="Y" THEN
    WC=TrsPeek(KB+12)
    KR=1
    PC=PH
    XA=TrsPeek(KA+2236)
    YA=TrsPeek(KA+2237)
    KF=TrsPeek(KA+2238)
    TA=100
    GOSUB DM80
    GOTO DM5030
  END IF  
'10011 INPUT"DOST WISH TO SAVE THE DUNJON ";A$:IFLEFT$(A$,1)<>"Y"THENGOTO10015ELSE
'INPUT"WHAT LEVEL WOULDST CALL IT ";X$:AN$="LEVEL"+X$:ONERRORGOTO10017:OPEN"I",1,AN$:CLOSE
DM10011:
  A$ = TrsInput("DOST WISH TO SAVE THE DUNJON ")
  IF LEFT$(A$,1)<>"Y" THEN
    GOTO DM10015
  ELSE
    A$ = TrsInput("WHAT LEVEL WOULDST CALL IT ")
    AN$=Path$+"levels/" + "LEVEL"+A$+".DJL"
    ON ERROR SKIP
    OPEN AN$ FOR INPUT AS #1
    IF MM.ERRNO = 0 THEN
      CLOSE #1
    ELSE
      GOTO DM10013
    END IF
  END IF
'10012 PRINTAN$" ALREADY EXISTS, ARE YOU SURE YOU WANT TO CHANGE IT ";:INPUTX$:
'IFLEFT$(X$,1)<>"Y"THEN10011
  PRINT AN$ " ALREADY EXISTS, ARE YOU SURE YOU WANT TO CHANGE IT "
  A$ = TrsInput("")
  IF LEFT$(A$,1)<>"Y" THEN GOTO DM10011
'10013 PRINT"SAVING ";AN$:OPEN"O",1,AN$:K=0:FORI=1TO9:A$="":FORJ=0TO248:
'A$=A$+CHR$(PEEK(KA+K+J)+59):NEXTJ:PRINT#1,A$:K=K+249:NEXTI:CLOSE1
DM10013:
  PRINT "SAVING " AN$
  OPEN AN$ FOR OUTPUT AS #1
  K=0
  FOR I=1 TO 9
    A$=""
    FOR J=0 TO 248
      A$=A$+CHR$(TrsPeek(KA+K+J)+59)
    NEXT J
    PRINT #1,A$
    K=K+249
  NEXT I
  CLOSE #1
'10014 PRINT"LEVEL SAVED"
  PRINT "LEVEL SAVED"
'10015 J#=INT(EX#/256):POKEKB+1,EX#-J#*256:JJ=J#/256:POKEKB+2,J#-JJ*256:POKEKB+3,JJ:
'IFSM<0THENSM=0
DM10015:
  J=INT(EX/256)
  TrsPoke KB+1,EX-J*256
  JJ=J/256
  TrsPoke KB+2,J-JJ*256
  TrsPoke KB+3,JJ
  IF SM<0 THEN SM=0
'10016 GOTO20000
  GOTO DM20000
'10017 IFERR=106THENCLOSE:RESUME10013ELSESTOP

'10020 POKEKB+10,SM:PRINT"LOADING THE INNKEEPER":RUN"INN"'***DISK
DM10020:
  TrsPoke KB+10,SM
  PRINT "LOADING THE INNKEEPER"
  Pause 2000
'RUN "INN"
  GOTO IN

'11000 CLS:PRINT@401,CHR$(23);"THOU ART SLAIN!":FORI=1TO2500:NEXTI:CLS:I=SQR(RND(16))+.7:ONIGOTO11010,11030,11040,11020
DM11000:
  CLS
  TrsPrintS 401,CHR$(23)+"THOU ART SLAIN!"
  Pause 3000
  CLS
  I=SQR(TrsRnd(16))+.7
  ON I GOTO DM11010,DM11030,DM11040,DM11020
  
'11010 PRINT"THOU ART EATEN":POKEKB-12,0:GOSUB20005:ONERRORGOTO20050:KILL S$
DM11010:
  PRINT"THOU ART EATEN"
  TrsPoke KB-12,0
  GOSUB DM20005
  ON ERROR SKIP
  KILL S2$+".CHR"
  
'11012 RUN"INN"
  Pause 5000
  GOTO IN
  
'11020 PRINT"BENEDIC THE CLERIC FOUND THEE":GOTO10010
DM11020:
  PRINT "BENEDIC THE CLERIC FOUND THEE"
  GOTO DM10010
  
'11030 PRINT"LOWENTHAL THE MAGE FOUND THEE":FORI=81TO90:POKEKA-I,0:NEXTI:GOTO10010
DM11030:
  PRINT "LOWENTHAL THE MAGE FOUND THEE"
  FOR I=81 TO 90
    TrsPoke KA-I,0
  NEXT I
  GOTO DM10010

'11040 PRINT"OLIAS THE DWARF FOUND THEE":FORI=61TO90:POKEKA-I,0:NEXTI:SM=0:POKEKB+10,0:POKEKB+16,0:POKEKB+14,0:POKEKB+6,0:GOTO10010
DM11040:
  PRINT "OLIAS THE DWARF FOUND THEE"
  FOR I=61 TO 90
    TrsPoke KA-I,0
  NEXT I
  SM=0
  TrsPoke KB+10,0
  TrsPoke KB+16,0
  TrsPoke KB+14,0
  TrsPoke KB+6,0
  GOTO DM10010

'20000 INPUT"WOULDST THOU SAVE THY CHARACTER BEFORE RETURNING TO THE INN ";C$
DM20000:
  C$ = TrsInput("WOULDST THOU SAVE THY CHARACTER BEFORE RETURNING TO THE INN ")
'20002 IFLEFT$(C$,1)<>"Y"THENGOTO10020ELSEGOSUB20005:GOTO20010
  IF LEFT$(C$,1)<>"Y" THEN
    GOTO DM10020
  ELSE
    GOSUB DM20005
    GOTO DM20010
  END IF

'20005 S$="":FORI=0TO10:CH=PEEK(KB-11+I):IFCH>64 AND CH<91 THEN S$=S$+CHR$(CH) ELSE S$=S$+" "
DM20005:
  S2$="":
  FOR I=0 TO 10
    CH2=TrsPeek(KB-11+I)
    IF CH2>64 AND CH2<91 THEN
      S2$=S2$+CHR$(CH2)
    END IF
'20008 NEXTI:RETURN
  NEXT I
  RETURN
  
'20010 PRINT"STORING THE ADVENTURER HIGHT ";S$:OPEN"O",1,S$:FORI=61TO96:J=PEEK(KA-I):PRINT#1,J:NEXTI:J=PEEK(KA):PRINT#1,J:FORI=0TO37:PRINT#1,PEEK(KB-12+I):NEXTI:CLOSE:GOTO10020
DM20010:
  PRINT "STORING THE ADVENTURER HIGHT ";S2$
  OPEN Path$+"Characters/" + S2$+".CHR" FOR OUTPUT AS #1
  FOR I=61 TO 96
    J=TrsPeek(KA-I)
  PRINT #1,J
  NEXT I
  J=TrsPeek(KA)
  PRINT #1,J
  FOR I=0 TO 37
    PRINT #1,TrsPeek(KB-12+I)
  NEXT I
  CLOSE #1
  Pause 3000
  GOTO DM10020

'20050 RESUMENEXT




