;===============================+
; YATL - Yet Another Todo List	|
;-------------------------------|
; Version:  1.11                |
; Date:     12/2/2008           |
; Author:   rulfzid             |
;===============================+


;==============+
; AUTO-EXECUTE |
;===============================================================================
#SingleInstance, Force
#NoEnv

; Listbox message constants
;---------------------------------------
LB_INSERTSTRING := 0x181
LB_GETCURSEL := 0x188
LB_GETTEXT := 0x189
LB_GETTEXTLEN := 0x18A
LB_SETITEMDATA := 0x19A
LB_DELETESTRING := 0x182
LB_SETCURSEL := 0x186
LB_GETCOUNT := 0x18B
LB_GETITEMRECT := 0x198

; SendMessageProc - for faster DllCalls
;---------------------------------------
SendMessageProc := GetProcAddress("user32", "SendMessageA")

; Gui options
;---------------------------------------
yatl_font = Trebuchet MS
yatl_font_size = 10
yatl_font_color = White
yatl_tabstop = 12
yatl_margin = 10
yatl_bgcolor = 333333
yatl_control_bgcolor = 444444
yatl_guiW = 400
yatl_listboxH = 300
yatl_x := ( A_ScreenWidth - 420 )
add_task_points := 5
points      = 0
pointsSent  = 0
pointsTimer = 0

; Set the hotkeys
;---------------------------------------
Hotkey, IfWinActive, YATL ahk_class AutoHotkeyGUI
Hotkey, ~Enter,		_AddTask
Hotkey, ~e,		_EditTask
Hotkey, ~i,		Inc
Hotkey, ~x,		CheckOffAndRemove
Hotkey, ~a,		Add5
Hotkey, ~f,		Flush
Hotkey, ~r,		Reload
Hotkey, ~t,		Timer
Hotkey, ~NumpadDel,	CheckOffAndRemove
Hotkey, ~Del,	CheckOffAndRemove
Hotkey, ~s,	Archive
Hotkey, ~.,	CheckOffAndRemove
Hotkey, ~Right, Indent
Hotkey, ~Left,	Dedent
Hotkey, +Up,	MoveUp
Hotkey, +Down,	MoveDown
Hotkey, #q,		Quit
Hotkey, IfWinExist, YATL ahk_class AutoHotkeyGUI
Hotkey, #h,		ToggleHelpGui
Hotkey, Escape, yatlEscape
Hotkey, IfWinActive
Hotkey, f11, ToggleMainGUI

; Create the Gui's
;---------------------------------------
createGui()
createHelpText()
createHelpGui()

; Load the saved todo lists
;---------------------------------------
LoadTodos(hList)


OnExit, SaveAndExit
return
;===============================================================================


;========================+
; AUTO-EXECUTE FUNCTIONS |
;===============================================================================
createGui() {
	global
	Gui, +lastfound -caption +alwaysontop
	gid := WinExist()
	Gui, Margin, %yatl_margin%, %yatl_margin%
	Gui, Color, %yatl_bgcolor%, %yatl_control_bgcolor%
	Gui, Font, s%yatl_font_size% c%yatl_font_color%, %yatl_font%
	Gui, Add, Text, vtext, YET ANOTHER TODO LIST................
	Gui, Font, s%yatl_font_size%
	Gui, Add, ListBox, h%yatl_listboxH% w%yatl_guiW% t%yatl_tabstop% vyatl_list HwndhList,
	Gui, Add, Edit, xp yp wp hp vyatl_help -vscroll +readonly Hidden, Help stuff
	Gui, Add, Edit, r1 w%yatl_guiW% -WantReturn Hidden vyatl_edit, 
	Gui, Add, Button, Default Hidden gAddOrEditTask, 
	Gui, Show, AutoSize X%yatl_x% Y0, YATL
	yatl_shown = 1
        Winset, Transparent, 170
	WinActivate
}

createHelpGui() {
	global
	Gui, 2: +lastfound -caption +alwaysontop +owner1
	hid := WinExist()
	Gui, 2: Margin, %yatl_margin%, %yatl_margin%
	Gui, 2: Color, 555555
	2fsize := yatl_font_size - 1
	Gui, 2: Font, s%2fsize% c%yatl_font_color%, %yatl_font%
	Gui, 2: Add, Text, , YATL - Command List
	Gui, 2: Add, Text, -vscroll +readonly, %yatl_helptext%
	Gui, 2: Show, Hide AutoSize, Help
	yatl_help_shown = 0
}

createHelpText() {
	global
	yatl_helptext = 
	(LTrim
	Win-t`t`tHide/show YATL
	n`t`tAdd a new task
	e`t`tEdit a task
	x`t`tCheck off a task
	Delete`t`tDelete selected task
	Right`t`tIndent selected task
	Left`t`tDedent selected task
	Win-Up`t`tMove task up
	Win-Down`tMove task down
	Win-q`t`tQuit YATL
	Win-h`t`tToggle command list
	Escape`t`tEscape out of things
	)
}
;===============================================================================

;=====================+
; SHOW/HIDE THE GUI'S |
;===============================================================================
ToggleMainGui:
	If yatl_help_shown
		ToggleGui( hid, yatl_help_shown )
	ToggleGui( gid, yatl_shown )
return

ToggleHelpGui:
	GuiControlGet, focused_control, focusv
	If ( focused_control = "yatl_edit" )
		return
	ToggleGui( hid, yatl_help_shown )
return

ToggleGui( hWnd, ByRef isshown ) {
	If isshown
	{	WinHide, ahk_id %hWnd%
		isshown = 0
	} Else {
		WinShow, ahk_id %hWnd%
		isshown = 1
		WinActivate, ahk_id %hWnd%
	}
}
;===============================================================================


;=========================+
; HOTKEYS AND SUBROUTINES |
;===============================================================================
CheckOffAndRemove:
	GuiControlGet, focused_control, focusv
	If ( focused_control != "yatl_list" )
		return	
	i := LB_GetCurrentSelection( hList )
	If (i < 0)
		return
	Gui, Submit, NoHide
	
	replaceStr := RegExReplace( yatl_list, "(\s*\[) (\])", "$1x$2" )
	LB_ReplaceString( hList, i, replaceStr )
	
	; Get the x,y,w,h of current list item
	VarSetCapacity( yatl_rect, 16 )
	DllCall( "SendMessage", UInt, hList, UInt, LB_GETITEMRECT, UInt, i, UInt, &yatl_rect )
	yatl_rect_x := NumGet( yatl_rect, 0, Int )
	yatl_rect_y := NumGet( yatl_rect, 4, Int )
	yatl_rect_w := NumGet( yatl_rect, 8, Int ) - yatl_rect_x
	yatl_rect_h := NumGet( yatl_rect, 12, Int ) - yatl_rect_y

	; Get the coordinates for the YATL GUI and the listbox within it
	WinGetPos, gX,gY, , , ahk_id %gid%	
	ControlGetpos, lX, lY, , , , ahk_id %hList%

	; Using info above, set coordinate so new GUI will be positioned
	; exactly over the current list item
	; (the extra "2" is to account for the with of the border of the
	; listbox - so far it's been 2 pixels on Vista and XP)
	itemX := gX + lx + yatl_rect_x + 2
	itemY := gY + ly + yatl_rect_y + 2

	; Create second, temporary GUI 
	Gui, 99: +toolwindow -caption +alwaysontop +lastfound
	99gid := WinExist()
	Gui, 99: color, green, green

	; Position it over the selected list item (the "99" accounts for the 
	; 99 pixels of WS_EXCLIENTEDGE for the listbox - on XP and Vista, at least
	; Show the temporary GUI, then fade it out
	Gui, 99: Show, x%itemX% y%itemY% w%yatl_rect_w% h%yatl_rect_h%,
	AnimateWindow( 99gid, 500, 0x90000 )
	Gui, 99: destroy

	; Delete the list item
	LB_DeleteString( hList, i )


;MsgBox %replaceStr%
RegExMatch(replaceStr, "\s*\[([0-9]*)\]", val)
;MsgBox %val1%

if val1
  AddPoints(val1)
else
  AddPoints(5)

FileAppend, %A_YYYY%%A_MM%%A_DD%:%A_Hour%:%A_Min%%replaceStr%`r`n, yatl.log


return

Archive:
	GuiControlGet, focused_control, focusv
	If ( focused_control != "yatl_list" )
		return	
	i := LB_GetCurrentSelection( hList )
	If (i < 0)
		return
	Gui, Submit, NoHide
	
	replaceStr := RegExReplace( yatl_list, "(\s*\[) (\])", "$1x$2" )
	LB_ReplaceString( hList, i, replaceStr )
	
	; Get the x,y,w,h of current list item
	VarSetCapacity( yatl_rect, 16 )
	DllCall( "SendMessage", UInt, hList, UInt, LB_GETITEMRECT, UInt, i, UInt, &yatl_rect )
	yatl_rect_x := NumGet( yatl_rect, 0, Int )
	yatl_rect_y := NumGet( yatl_rect, 4, Int )
	yatl_rect_w := NumGet( yatl_rect, 8, Int ) - yatl_rect_x
	yatl_rect_h := NumGet( yatl_rect, 12, Int ) - yatl_rect_y

	; Get the coordinates for the YATL GUI and the listbox within it
	WinGetPos, gX,gY, , , ahk_id %gid%	
	ControlGetpos, lX, lY, , , , ahk_id %hList%

	; Using info above, set coordinate so new GUI will be positioned
	; exactly over the current list item
	; (the extra "2" is to account for the with of the border of the
	; listbox - so far it's been 2 pixels on Vista and XP)
	itemX := gX + lx + yatl_rect_x + 2
	itemY := gY + ly + yatl_rect_y + 2

	; Create second, temporary GUI 
	Gui, 99: +toolwindow -caption +alwaysontop +lastfound
	99gid := WinExist()
	Gui, 99: color, green, green

	; Position it over the selected list item (the "99" accounts for the 
	; 99 pixels of WS_EXCLIENTEDGE for the listbox - on XP and Vista, at least
	; Show the temporary GUI, then fade it out
	Gui, 99: Show, x%itemX% y%itemY% w%yatl_rect_w% h%yatl_rect_h%,
	AnimateWindow( 99gid, 500, 0x90000 )
	Gui, 99: destroy

	; Delete the list item
	LB_DeleteString( hList, i )

FileAppend, %replaceStr%`r`n, todo.txt


return

_AddTask:
	GuiControlGet, focused_control, focusv
	If (focused_control != "yatl_list")
		return
	yatl_action := "add"
	GuiControl, Show, yatl_edit	
	Gui, Show, Autosize
	GuiControl, Focus, yatl_edit
AddPoints(5)
return

_EditTask:
	GuiControlGet, focused_control, focusv
	If (focused_control != "yatl_list")
		return
	yatl_action := "edit"
	Gui, Submit, NoHide
	RegExMatch(yatl_list, "(\s*\[[0-9]*\] )(.*)", yedit)
	
	; This is so trying to "edit" tasks when the list is empty behaves properly
	yedit1 := yedit1 ? yedit1 : "[ ] "
	
	Guicontrol, , yatl_edit, %yedit2%
	GuiControl, Show, yatl_edit	
	Gui, Show, Autosize
	GuiControl, Focus, yatl_edit
return

Inc:
	GuiControlGet, focused_control, focusv
	If (focused_control != "yatl_list")
		return
	i := LB_GetCurrentSelection(hList)
	If (i < 0)
		return
	Gui, Submit, NoHide


RegExMatch(yatl_list, "\s*\[([0-9]*)\]", val)

;MsgBox %val1%
new := ( val1 + 5 )
;MsgBox %new%
yatl_list := RegExReplace(yatl_list, "[0-9]+", new)

;	replaceStr := "`t" . yatl_list
;xMsgBox %yatl_list%
	LB_ReplaceString(hList, i, yatl_list)
return

AddOrEditTask:
	Gui, Submit, NoHide
	If not yatl_edit
		return
	If (yatl_action = "add")
	{	yatl_edit := "[" . add_task_points . "] " . yatl_edit
		i := LB_GetCurrentSelection(hList)
		i := (i >= 0) ? i + 1 : i
		LB_InsertString(hList, i, yatl_edit)
	}
	If (yatl_action = "edit")
	{	i := LB_GetCurrentSelection(hList)
		yatl_edit := yedit1 . yatl_edit
		LB_ReplaceString(hList, i, yatl_edit)
	}
	GuiControl, Hide, yatl_edit
	GuiControl, , yatl_edit, 
	Gui, Show, Autosize
return

DeleteSelected:
	GuiControlGet, focused_control, focusv
	If (focused_control != "yatl_list")
		return	
	i := LB_GetCurrentSelection(hList)
	If (i < 0)
		return
	LB_DeleteString(hList, i)
AddPoints(5)
return

Indent:
Dedent:
	GuiControlGet, focused_control, focusv
	If (focused_control != "yatl_list")
		return
	i := LB_GetCurrentSelection(hList)
	If (i < 0)
		return
	Gui, Submit, NoHide
	If (A_ThisLabel = "Indent")
		replaceStr := "`t" . yatl_list
	If (A_ThisLabel = "Dedent")
		replaceStr := (Substr(yatl_list, 1, 1) = "`t") ? Substr(yatl_list, 2) : yatl_list
	LB_ReplaceString(hList, i, replaceStr)
return

MoveUp:
MoveDown:
	GuiControlGet, focused_control, focusv
	If (focused_control != "yatl_list")
		return
	i := LB_GetCurrentSelection(hList)
	If (i < 0)
		return	
	If ( A_ThisLabel = "MoveUp") and ( (j:=i-1) < 0 )
		return
	If ( A_ThisLabel = "MoveDown") and ( (j:=i+1) >= LB_GetCount( hList ) )
		return
	LB_SwapMove(hList, i, j)
return

~Up::
~Down::
	IfWinActive, YATL
		ControlSend, , {%A_ThisHotkey%}, ahk_id %hList%
return

yatlEscape:
	GuiControlGet, editisvisible, Visible, yatl_edit
	If editisvisible
	{	GuiControl, , yatl_edit, 
		GuiControl, Hide, yatl_edit
		Gui, Show, Autosize
	} 
	Else If yatl_help_shown
		Gosub, ToggleHelpGui
	Else 
	{	WinHide, ahk_id %gid%
		WinActivate, ahk_id %curractive%
        shown := 0
	}
return

GuiClose:
Quit:
SaveAndExit:
	SaveToDos(hList)
        FlushQueue()
        ExitApp
Return
;===============================================================================


;=====================+
; SAVE/LOAD FUNCTIONS |
;===============================================================================
SaveToDos(hWnd) {
	cnt := LB_GetCount(hWnd)
	Loop, % cnt
		todos .= LB_GetString(hWnd, A_Index - 1) . "`n"
	StringTrimRight, todos, todos, 1
	Loop, Read, %A_ScriptFullPath%
	{	thisscript .= A_LoopReadLine . "`n"
		If InStr(A_LoopReadLine, "Saved YATL List") and not Instr(A_LoopReadLine, "InStr")
			break
	}
	FileDelete, %A_ScriptFullPath%
	FileAppend, %thisscript%%todos%, %A_ScriptFullPath%	
}

LoadToDos(hWnd) {
	istodo = 0
	Loop, Read, %A_ScriptFullPath%
	{	If istodo
		{	If A_LoopReadline
				todos .= A_LoopReadLine . "`n"
			continue
		}
		If InStr(A_LoopReadLine, "Saved YATL List") and not Instr(A_LoopReadLine, "InStr")
			istodo = 1
	}
	StringTrimRight, todos, todos, 1
	Loop, Parse, Todos, `n
		LB_InsertString(hWnd, -1, A_LoopField)		
}
;===============================================================================


;===========================+
; DLLCALL WRAPPER FUNCTIONS |
;===============================================================================
AnimateWindow( hWnd, Duration, Flag) {
	Return DllCall( "AnimateWindow", UInt, hWnd, Int, Duration, UInt, Flag )
}



GetProcAddress(dll, funcname) {
	return DllCall("GetProcAddress", UInt, DllCall("GetModuleHandle", Str, dll) ,Str, funcname)
}

LB_GetCurrentSelection(hWnd) {
	global SendMessageProc
	global LB_GETCURSEL
	Return DllCall(SendMessageProc, UInt, hWnd, UInt, LB_GETCURSEL, UInt, 0, UInt, 0)
}

LB_SwapMove(hWnd, from_index, to_index) {
	global SendMessageProc
	global LB_GETTEXTLEN, LB_GETTEXT, LB_DELETESTRING, LB_INSERTSTRING, LB_SETCURSEL
	If (DllCall(SendMessageProc, UInt, hWnd, UInt, LB_GETTEXTLEN, UInt, j, UInt, 0) < 0)
		return
	lo := (from_index < to_index) ? from_index : to_index
	len := DllCall(SendMessageProc, UInt, hWnd, UInt, LB_GETTEXTLEN, UInt, lo, UInt, 0)
	VarSetCapacity(tmp, len)
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_GETTEXT, UInt, lo, UInt, &tmp)
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_DELETESTRING, UInt, lo, UInt, 0)
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_INSERTSTRING, UInt, lo + 1, UInt, &tmp)
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_SETCURSEL, UInt, to_index, UInt, 0)
}

LB_InsertString(hWnd, index, string) {
	global SendMessageProc
	global LB_INSERTSTRING, LB_SETCURSEL	
	i := DllCall(SendMessageProc, UInt, hWnd, UInt, LB_INSERTSTRING, UInt, index, UInt, &string)
	Sleep, -1
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_SETCURSEL, UInt, i, UInt, 0)
	Return i
}

LB_ReplaceString(hWnd, index, string) {
	global SendMessageProc
	global LB_DELETESTRING
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_DELETESTRING, UInt, index, UInt, 0)
	LB_InsertString(hWnd, index, string)
}

LB_DeleteString(hWnd, index) {
	global SendMessageProc
	global LB_DELETESTRING
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_DELETESTRING, UInt, index, UInt, 0)
	cnt := LB_GetCount( hWnd )
	LB_SelectThis( hWnd, (index >= cnt) ? (index - 1) : (index) )
}

LB_GetString(hWnd, index) {
	global SendMessageProc
	global LB_GETTEXTLEN, LB_GETTEXT
	len := DllCall(SendMessageProc, UInt, hWnd, UInt, LB_GETTEXTLEN, UInt, index, UInt, 0)
	VarSetCapacity(tmp, len)
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_GETTEXT, UInt, index, UInt, &tmp)
	return tmp
}

LB_GetCount(hWnd) {
	global SendMessageProc
	global LB_GETCOUNT
	return DllCall(SendMessageProc, UInt, hWnd, UInt, LB_GETCOUNT, UInt, 0, UInt, 0)
}

LB_SelectThis(hWnd, index) {
	global SendMessageProc
	global LB_SETCURSEL
	DllCall(SendMessageProc, UInt, hWnd, UInt, LB_SETCURSEL, UInt, index, UInt, 0)
}

AddToQueue(p) {
  global points
  points := (p + points)
}

Timer:
GuiControlGet, focused_control, focusv
If ( focused_control != "yatl_list" )
	return	
i := LB_GetCurrentSelection( hList )
If (i < 0)
	return

  pointsTimer := (points + pointsSent)
  ;MsgBox ,,, %points%, 2
  SoundBeep, 4800, 50
  SetTimer, EndTimer, 60000
return

EndTimer:
  SoundBeep, 4800, 50
  val := (points + pointsSent) - pointsTimer
  MsgBox ,,, %val%, 2
  SetTimer, EndTimer, Off 
return

Flush:
	GuiControlGet, focused_control, focusv
	If ( focused_control != "yatl_list" )
		return	
	i := LB_GetCurrentSelection( hList )
	If (i < 0)
		return
        FlushQueue()
return

FlushQueue() {
  global points
  URLDownloadToFile, http://points4.me/addpoints.php?u=1&points=%points% , points.txt
  points := 0
  pointsSent := 0

  SoundBeep, 4800, 50
}

AddPoints(p) {

  global points
  global pointsSent
  global hList

  AddToQueue(p)

  if ( points >= 25 ) 
  {
    pointsSent := (pointsSent + points)
    FlushQueue()
  }
  
  FileRead, j, points.txt
;MsgBox %j%
  total := json(j, "score")
;MsgBox %total%
  total := ( total + points )
;MsgBox %total%
  count := LB_GetCount(hList)
;MsgBox %hList%
  title = points: %points% Total is %total% tasks: %count% 
;MsgBox %title%
  GuiControl,, text, %title%

}


Add5:
	GuiControlGet, focused_control, focusv
	If ( focused_control != "yatl_list" )
		return	
	i := LB_GetCurrentSelection( hList )
	If (i < 0)
		return
        AddPoints(5)
return


Reload:
	GuiControlGet, focused_control, focusv
	If ( focused_control != "yatl_list" )
		return	
	i := LB_GetCurrentSelection( hList )
	If (i < 0)
		return
	FlushQueue()
	Reload
return


/*

	Function: JSON
	
	Parameters:
		js - source
		s - path to element
		v - (optional) value to overwrite
	
	Returns:
		Value of element (prior to change).
	
	License:
		- Version 2.0 by Titan <http://www.autohotkey.net/~Titan/#json>
		- New BSD license terms <http://www.autohotkey.net/~Titan/license.txt>

*/

json(ByRef js, s, v = "") {
	j = %js%
	Loop, Parse, s, .
	{
		p = 2
		RegExMatch(A_LoopField, "([+\-]?)([^[]+)((?:\[\d+\])*)", q)
		Loop {
			If (!p := RegExMatch(j, "(?<!\\)(""|')([^\1]+?)(?<!\\)(?-1)\s*:\s*((\{(?:[^{}]++|(?-1))*\})|(\[(?:[^[\]]++|(?-1))*\])|"
				. "(?<!\\)(""|')[^\7]*?(?<!\\)(?-1)|[+\-]?\d+(?:\.\d*)?|true|false|null?)\s*(?:,|$|\})", x, p))
				Return
			Else If (x2 == q2 or q2 == "*") {
				j = %x3%
				z += p + StrLen(x2) - 2
				If (q3 != "" and InStr(j, "[") == 1) {
					StringTrimRight, q3, q3, 1
					Loop, Parse, q3, ], [
					{
						z += 1 + RegExMatch(SubStr(j, 2, -1), "^(?:\s*((\[(?:[^[\]]++|(?-1))*\])|(\{(?:[^{\}]++|(?-1))*\})|[^,]*?)\s*(?:,|$)){" . SubStr(A_LoopField, 1) + 1 . "}", x)
						j = %x1%
					}
				}
				Break
			}
			Else p += StrLen(x)
		}
	}
	If v !=
	{
		vs = "
		If (RegExMatch(v, "^\s*(?:""|')*\s*([+\-]?\d+(?:\.\d*)?|true|false|null?)\s*(?:""|')*\s*$", vx)
			and (vx1 + 0 or vx1 == 0 or vx1 == "true" or vx1 == "false" or vx1 == "null" or vx1 == "nul"))
			vs := "", v := vx1
		StringReplace, v, v, ", \", All
		js := SubStr(js, 1, z := RegExMatch(js, ":\s*", zx, z) + StrLen(zx) - 1) . vs . v . vs . SubStr(js, z + StrLen(x3) + 1)
	}
	Return, j == "false" ? 0 : j == "true" ? 1 : j == "null" or j == "nul"
		? "" : SubStr(j, 1, 1) == """" ? SubStr(j, 2, -1) : j
}

;===============================================================================


/*******************************************************************************
/*************** DO NOT MODIFY BELOW THIS LINE: Saved YATL List ****************
