'-----------------------------------------------------------------------------
' 由 VisualFreeBasic 5.8.11 生成的源代码
' 生成时间：2025年03月15日 17时47分16秒
' 更多信息请访问 www.yfvb.com 
'-----------------------------------------------------------------------------


Function gFLY_GetFontHandles(mFont As String) As HFONT '由字体字符，返回字体句柄
   Dim As Long i,u=UBound(gFLY_FontNames),aa =-1
   If u > -1 Then
      For i = 0 To u
         If gFLY_FontNames(i) = mFont Then
            Return gFLY_FontHandles(i)
         End If
      Next
   End If
   aa = u + 1
   ReDim Preserve gFLY_FontNames(aa), gFLY_FontHandles(aa)
   gFLY_FontNames(aa) = mFont
   Dim pvv(5) As String, ff as Long, zz as String = mFont
   for i = 0 to 5
      ff = instr(zz, ",")
      if ff = 0 then
         pvv(i) = Trim(zz)
         exit for
      end if
      pvv(i) = Trim(.left(zz, ff -1))
      zz = Mid(zz, ff + 1)
   next
   if valint(pvv(1)) = 0 then pvv(1) = "9"
   if len(pvv(0)) = 0 then pvv(0) = "SimSun" '宋体
   gFLY_FontHandles(aa) = AfxCreateFont(pvv(0), ValInt(pvv(1)), -1, IIf(ValInt(pvv(2)) = 0, FW_NORMAL, FW_BOLD), ValInt(pvv(3)), ValInt(pvv(4)), ValInt(pvv(5)))

   IF gFLY_FontHandles(aa) = 0 then  '创建失败
      aa -= 1
      ReDim Preserve gFLY_FontNames(aa), gFLY_FontHandles(aa)
      Return 0
   End If
   Function = gFLY_FontHandles(aa)
End Function
Function GetCodeColorGDI(coColor As Long, defaultColor As Long = -1) As Long  '把控件特殊颜色值，转换为 GDI 色  ,返回-1 为不使用或默认
   '格式：&H01020304  02.03.04为7F时，01值0到30是系统色（25为不使用）其它时候是A通道，GDI+需要 02 04 里值对调
  If (&H00FFFFFF And coColor) = &H7F7F7F Then
      Dim f As Long = Cast(UInteger, (&HFF000000 And coColor)) Shr 24
      If f = 25 Then Return defaultColor   '不使用或默认值 
      If f < 31 Then 
          Return GetSysColor(f)  
      End If  
  End If
  Function = (&H00FFFFFF And coColor) '去掉 A 通道
End Function
Function GetCodeColorGDIplue(coColor As Long, defaultColor As Long = 0) As Long  '把控件特殊颜色值，转换为 GDI+ 色  ,返回0 为不使用或默认
 '格式：&H01020304  02.03.04为7F时，01值0到30是系统色（25为不使用）其它时候是A通道，GDI+需要 02 04 里值对调  
  Dim tColor As Long = coColor 
  If (&H00FFFFFF And coColor) = &H7F7F7F Then
      Dim f As Long = Cast(UInteger, (&HFF000000 And coColor)) Shr 24
      If f = 25 Then Return defaultColor  ' 不使用或默认值 
      If f < 31 Then 
          tColor = GetSysColor(f) Or &HFF000000 '增加 A通道，不透明，不然是全透明  
      End If  
  End If 
  '因为保存的是GDI 的颜色，GDI+ 需要调换
  Dim As UInteger c1 =(&H00FF0000 And tColor),c2 = (&H000000FF And tColor) ,c3 =(&HFF00FF00 And tColor)
  c1 Shr= 16
  c2 Shl= 16 
  Function = c1 Or c2 Or c3  
End Function
Function FLY_ResourceToIcon(ByVal ResImg As String) As HICON '资源获取图标句柄
   Dim nIcon As HICON
   Dim ffi   As Long = InStr(ResImg ,"|")
   if ffi > 0 Then ResImg = Mid(ResImg ,ffi + 1)
   if Left(ResImg ,7) = "BITMAP_" Then
      Dim nBmp As HBITMAP = LoadImageA(app.hInstance ,ResImg ,IMAGE_BITMAP ,0 ,0 ,LR_DEFAULTCOLOR)
      Dim po   As ICONINFO
      po.fIcon    = TRUE
      po.hbmColor = nBmp
      po.hbmMask  = nBmp
      nIcon       = CreateIconIndirect(@po)
      DeleteObject nBmp
   Elseif Left(ResImg ,5) = "ICON_" Or ResImg = "AAAAA_APPICON" Then
      nIcon = LoadImageA(app.hInstance ,ResImg ,IMAGE_ICON ,0 ,0 ,LR_DEFAULTCOLOR) '从资源里加载图标
   Else 
      nIcon = AfxGdipIconFromRes(App.hInstance ,ResImg)
   End if
   Function = nIcon
End Function
' =====================================================================================
' 根据Jose Roca的代码
' 为窗口的整个客户区域创建标准工具提示。
' 参数:
' - hwnd = 窗口句柄
' - strTooltipText = 工具提示文本
' - bBalloon = 气球提示 (TRUE or FALSE)
' 返回值:
'   工具提示控件的句柄
' =====================================================================================
Function FF_AddTooltip(hWndForm AS HWND, strTooltipText AS wString, bBalloon AS Long, X as Long = 0, Y As Long = 0, W As Long = 0, H As Long = 0) As HWND
   
   IF hWndForm = 0 Then Exit Function
   
   Dim hwndTT AS HWND
   Dim dwStyle As Long
   
   dwStyle = WS_POPUP OR TTS_NOPREFIX OR TTS_ALWAYSTIP
   IF bBalloon THEN dwStyle = dwStyle OR TTS_BALLOON
   hwndTT = CreateWindowExW(WS_EX_TOPMOST, "tooltips_class32", "", dwStyle, 0, 0, 0, 0, 0, Cast(HMENU, Null), 0, ByVal Cast(LPVOID, Null))
   
   IF hwndTT = 0 THEN Exit Function
   'SetWindowPos(hwndTT, null, 100, 100, 0, 0,  SWP_NOSIZE Or SWP_NOZORDER Or SWP_NOACTIVATE)
   
   Dim tti AS TTTOOLINFOW
   tti.cbSize = SIZEOF(tti)
   tti.uFlags = TTF_SUBCLASS
   tti.hwnd = hWndForm
   tti.hinst = GetModuleHandle(BYVAL NULL)
   
   GetClientRect(hWndForm, Varptr(tti.rect))
   tti.rect.Left = x
   tti.rect.top = y
   if w > 0 then tti.rect.Right = x + w
   if h > 0 then tti.rect.bottom = y + h
   '// 字符串的长度不能超过80个字符，包括终止的空值
   tti.uId = 0
   Dim ff As Long = InStr(strTooltipText, WChr(13, 10) )
   if ff Then
      tti.lpszText =Cast(LPWSTR, Cast(UInteger, @strTooltipText) + ff * 2 + 2) 
      SendMessageW hwndTT, TTM_ADDTOOLW, 0, Cast(LPARAM, Varptr(tti))
      Dim nw As WString * 100 = Left(strTooltipText,ff-1)
      SendMessageW hwndTT, TTM_SETTITLEW, 0, Cast(LPARAM, @nw)
   Else
      tti.lpszText = @strTooltipText
      SendMessageW hwndTT, TTM_ADDTOOLW, 0, Cast(LPARAM, Varptr(tti))
   End if

   
   Function = hwndTT
   
End Function
Function FLY_DoMessagePump(pWindow AS CWindow Ptr, ByVal ShowModalFlag As Long, ByVal hWndForm As HWND, ByVal hWndParent As HWND, ByVal nFormShowState As Long, ByVal IsMDIForm As Long) As HWND
   '如果这是一个MDI子窗体，那么它不能显示为模态。
   If (GetWindowLongPtr(hWndForm, GWL_EXSTYLE) And WS_EX_MDICHILD) = WS_EX_MDICHILD Then ShowModalFlag = False
   If (GetWindowLongPtr(hWndForm, GWL_EXSTYLE) And WS_EX_NOACTIVATE) = WS_EX_NOACTIVATE And nFormShowState = SW_SHOWNORMAL Then nFormShowState = SW_SHOWNOACTIVATE
   If ShowModalFlag = True Then '模式窗口，进入消息循环处理
      '确定活动控件的顶层窗口
      While (GetWindowLongPtr(hWndParent, GWL_STYLE) And WS_CHILD) <> 0
         hWndParent = GetParent(hWndParent)
         If IsWindow(hWndParent) = 0 Then Exit While
         If (GetWindowLongPtr(hWndParent, GWL_EXSTYLE) And WS_EX_MDICHILD) <> 0 Then Exit While
      Wend
      
      '为父窗体禁用鼠标和键盘输入
      If IsWindow(hWndParent) Then EnableWindow(hWndParent, False)
      ShowWindow(hWndForm, nFormShowState)
      UpdateWindow(hWndForm)
      '主消息循环：
      Dim uMsg        As MSG
      Dim zTempString As zString * MAX_PATH
      Dim hWndP       As HWND = pWindow->hWindow
      Dim HACCEL      As HACCEL
      '获取系统版本号
      Dim vi As OSVERSIONINFO, bil As Long
      vi.dwOsVersionInfoSize = SizeOf(OSVERSIONINFO)
      GetVersionEx @vi
      If vi.dwPlatformId = VER_PLATFORM_WIN32_NT Then
         bil = vi.dwMajorVersion * 10 + vi.dwMinorVersion
      End If
      Dim moua As Point, mouh As hWnd
      
      '消息循环开始
      Do While GetMessage(@uMsg, NULL, 0, 0)
         If FF_PUMPHOOK(uMsg) = 0 Then
            HACCEL = pWindow->AccelHandle
            '处理菜单命令的快捷键
            If (hAccel = 0) OrElse (TranslateAcceleratorW(hWndP, hAccel, @uMsg)) = 0 Then '
               If IsMDIForm = True Then
                  If TranslateMDISysAccel(hWndP, @uMsg) <> 0 Then Continue Do
               End If
               If bil < 62 AndAlso uMsg.message = WM_MOUSEWHEEL Then
                  'WIn7 鼠标滚轮只在当前焦点窗口，WIn10系统是鼠标在什么窗口上滚轮在什么窗口
                  GetCursorPos(@moua)
                  mouh = WindowFromPoint(moua)
                  If mouh <> uMsg.hWnd Then
                     PostMessage(mouh, WM_MOUSEWHEEL, uMsg.wParam, uMsg.lParam)
                     Continue Do
                  End If
               End If
               If IsDialogMessageW(hWndP, @uMsg) = 0 Then
                  TranslateMessage @uMsg
                  DispatchMessage @uMsg
               End If
            End If
         End If
         If IsWindow(hWndForm) = False Then Exit Do ' 如果窗口被销毁，则退出模态消息循环（重要）。
      Loop
      Function        = Cast(hWnd, Cast(LONG_PTR, App.ReturnValue))
      App.ReturnValue = 0
   Else
      ShowWindow hWndForm, nFormShowState
      Function = hWndForm
   End If
End Function

Sub FLY_VFB_Layout_hWndForm(hWndForm As HWND) '处理控件布局
   DIM rcParent AS RECT ,rcChild AS RECT
   DIM x        AS LONG ,y       AS LONG ,xWidth AS LONG ,yHeight AS LONG
   GetClientRect(hWndForm ,@rcParent)
   rcParent.Right  = AfxUnscaleX(rcParent.Right)  ' 为自动响应DPI，全部调整为 100%DPI 时的数值
   rcParent.Bottom = AfxUnscaleY(rcParent.Bottom)
   '真实控件
   Dim zWnd  As HWND = GetWindow(hWndForm ,GW_CHILD)
   Dim nHDWP As HDWP = BeginDeferWindowPos(1) '同时更新控件位置
   While zWnd
      Dim fp As FormControlsPro_TYPE ptr = vfb_Get_Control_Ptr(zWnd)
      If fp <> 0 AndAlso fp->anchor > 0 Then
         GetWindowRect(zWnd ,@rcChild)
         FLY_VFB_Layout_Handle(fp ,rcParent.Right ,rcParent.Bottom ,AfxUnscaleX(rcChild.Right - rcChild.Left) ,AfxUnscaleY(rcChild.Bottom - rcChild.Top) ,x ,y ,xWidth ,yHeight)
         fp->nLeft   = x
         fp->nTop    = y
         fp->nWidth  = xWidth
         fp->nHeight = yHeight
         nHDWP       = DeferWindowPos(nHDWP ,zWnd ,0 ,AfxscaleX(x) ,AfxscaleY(y) ,AfxscaleX(xWidth) ,AfxscaleY(yHeight) ,SWP_NOZORDER Or SWP_NOACTIVATE)
      End If
      zWnd = GetWindow(zWnd ,GW_HWNDNEXT)
   Wend
   
   '虚拟控件
   Dim fp As FormControlsPro_TYPE ptr = vfb_Get_Control_Ptr(hWndForm)
   While fp    '
      if fp->anchor > 0 Then
         if fp->CtrlFocus Then '非 CW创建的控件，因为不 fp 指针不内置入窗口，真实控件查不到
            GetWindowRect(fp->CtrlFocus ,@rcChild)
            FLY_VFB_Layout_Handle(fp ,rcParent.Right ,rcParent.Bottom ,AfxUnscaleX(rcChild.Right - rcChild.Left) ,AfxUnscaleY(rcChild.Bottom - rcChild.Top) ,x ,y ,xWidth ,yHeight)
            nHDWP = DeferWindowPos(nHDWP ,fp->CtrlFocus ,0 ,AfxscaleX(x) ,AfxscaleY(y) ,AfxscaleX(xWidth) ,AfxscaleY(yHeight) ,SWP_NOZORDER Or SWP_NOACTIVATE)
         Else
            FLY_VFB_Layout_Handle(fp ,rcParent.Right ,rcParent.Bottom ,fp->nWidth ,fp->nHeight ,x ,y ,xWidth ,yHeight)
         End if
         fp->nLeft   = x
         fp->nTop    = y
         fp->nWidth  = xWidth
         fp->nHeight = yHeight
      End if
      Dim sfp As FormControlsPro_TYPE ptr = fp->VrControls
      fp = sfp
   Wend
   EndDeferWindowPos nHDWP
End Sub

Sub FLY_VFB_Layout_Handle(fp As FormControlsPro_TYPE ptr, pWidth AS LONG, pHeight AS LONG, nWidth AS LONG, nHeight AS LONG, ByRef x AS LONG, ByRef y AS LONG, ByRef xWidth AS LONG, ByRef yHeight AS LONG)
  '处理控件布局 
   x = 0 : y = 0 : xWidth = 0 : yHeight = 0
   SELECT CASE fp->anchor
      CASE 0 'AFX_ANCHOR_NONE
         x = fp->nLeft
         y = fp->nTop
         xWidth = MAX(pWidth - fp->nLeft - fp->nRight, 0)
         yHeight = nHeight
      CASE 1 'AFX_ANCHOR_WIDTH
         x = fp->nLeft
         y = fp->nTop
         xWidth = MAX(pWidth - fp->nLeft - fp->nRight, 0)
         yHeight = nHeight
      CASE 2 'AFX_ANCHOR_RIGHT
         x = pWidth - nWidth - fp->nRight
         y = fp->nTop
         xWidth = nWidth
         yHeight = nHeight
      CASE 3 'AFX_ANCHOR_CENTER_HORZ
         x = (pWidth \ 2) + fp->centerX
         y = fp->nTop
         xWidth = nWidth
         yHeight = nHeight
      CASE 4 'AFX_ANCHOR_HEIGHT
         x = fp->nLeft
         y = fp->nTop
         xWidth = nWidth
         yHeight = MAX(pHeight - fp->nTop - fp->nBottom, 0)
      CASE 5 'AFX_ANCHOR_HEIGHT_WIDTH
         x = fp->nLeft
         y = fp->nTop
         xWidth = MAX(pWidth - fp->nLeft - fp->nRight, 0)
         yHeight = MAX(pHeight - fp->nTop - fp->nBottom, 0)
      CASE 6 'AFX_ANCHOR_HEIGHT_RIGHT
         x = pWidth - nWidth - fp->nRight
         y = fp->nTop
         xWidth = nWidth
         yHeight = MAX(pHeight - fp->nTop - fp->nBottom, 0)
      CASE 7 'AFX_ANCHOR_BOTTOM
         x = fp->nLeft
         y = pHeight - fp->nBottom - nHeight
         xWidth = nWidth
         yHeight = nHeight
      CASE 8 'AFX_ANCHOR_BOTTOM_WIDTH
         x = fp->nLeft
         y = pHeight - fp->nBottom - nHeight
         xWidth = MAX(pWidth - fp->nLeft - fp->nRight, 0)
         yHeight = nHeight
      CASE 9 'AFX_ANCHOR_BOTTOM_RIGHT
         x = pWidth - nWidth - fp->nRight
         y = pHeight - fp->nBottom - nHeight
         xWidth = nWidth
         yHeight = nHeight
      CASE 10 'AFX_ANCHOR_CENTER_HORZ_BOTTOM
         x = (pWidth \ 2) + fp->centerX
         y = pHeight - fp->nBottom - nHeight
         xWidth = nWidth
         yHeight = nHeight
      CASE 11 'AFX_ANCHOR_CENTER_VERT
         x = fp->nLeft
         y = (pHeight - nHeight) \ 2
         xWidth = nWidth
         yHeight = nHeight
      CASE 12 'AFX_ANCHOR_CENTER_VERT_RIGHT
         x = pWidth - nWidth - fp->nRight
         y = (pHeight - nHeight) \ 2
         xWidth = nWidth
         yHeight = nHeight
      CASE 13 'AFX_ANCHOR_CENTER
         x = (pWidth \ 2) + fp->centerX
         y = (pHeight \ 2) + fp->centerY
         xWidth = nWidth
         yHeight = nHeight
   END SELECT
End Sub





'[SourceDB:0-171072]
'画线 DrawLine(ByVal hDc As Long , ByVal X1 As Long ,ByVal Y1 As Long ,ByVal X2 As Long ,ByVal Y2 As Long ,_
'             ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long)
'需要支持高DPI，请代码中加 AfxScaleX(X), AfxScaleY(Y)  
'nPenStyle ------ Long，指定画笔样式，可以是下述常数之一
'PS_SOLID        画笔画出的是实线
'PS_DASH         画笔画出的是虚线（nWidth必须不大于1）
'PS_DOT          画笔画出的是点线（nWidth必须不大于1）
'PS_DASHDOT      画笔画出的是点划线（nWidth必须不大于1）
'PS_DASHDOTDOT   画笔画出的是点-点-划线（nWidth必须不大于1）
'PS_NULL         画笔不能画图
'PS_INSIDEFRAME  由椭圆、矩形、圆角矩形、饼图以及弦等生成的封闭对象框时，画线宽度向内扩展。如指定的准确RGB颜色不存在，就进行抖动处理
'颜色用 BGR(0, 0, 255) 创建  不能用 RGB
Sub DrawLine(ByVal hDC as hDC, ByVal X1 as Integer, ByVal Y1 as Integer, ByVal X2 as Integer, ByVal Y2 as Integer, _
      ByVal nPenStyle as Integer, ByVal nWidth as Integer, ByVal crColor as Long) '画线
   '线条类型 nPenStyle
   '实线 = 0
   '虚线 = 1
   '点线 = 2
   '点划线 = 3
   '双点划线 = 4
   '无线 = 5
   '内收实线 = 6
   Dim hOldPen as HGDIOBJ, hPen as HGDIOBJ, lpPoint as Point
   hPen = CreatePen(nPenStyle, nWidth, crColor) '创建新画笔
   hOldPen = SelectObject(hDC, hPen)            '画笔存入DC
   MoveToEx hDC, X1, Y1, @lpPoint             '设置起点
   LineTo hDC, X2, Y2                      '画到终点
   SelectObject hDC, hOldPen            '销毁
   DeleteObject hPen
   
End Sub
'[SourceDB:0-171071]
'画框 DrawFrame(DC,X1,Y1,X2,Y2,填充色彩,线条类型,线条色彩,线条宽)
'需要支持高DPI，请代码中加 AfxScaleX(X), AfxScaleY(Y)  
'nPenStyle ------ Long，指定画笔样式，可以是下述常数之一
'PS_SOLID        画笔画出的是实线
'PS_DASH         画笔画出的是虚线（nWidth必须不大于1）
'PS_DOT          画笔画出的是点线（nWidth必须不大于1）
'PS_DASHDOT      画笔画出的是点划线（nWidth必须不大于1）
'PS_DASHDOTDOT   画笔画出的是点-点-划线（nWidth必须不大于1）
'PS_NULL         画笔不能画图
'PS_INSIDEFRAME  由椭圆、矩形、圆角矩形、饼图以及弦等生成的封闭对象框时，画线宽度向内扩展。如指定的准确RGB颜色不存在，就进行抖动处理
'颜色用 BGR(0, 0, 255) 创建  不能用 RGB
Sub DrawFrame(DC as hDC, X1 as Long, Y1 as Long, X2 as Long, Y2 as Long, C as Long, L as Long = PS_NULL, cL as Long = 0, wL as Long = 1)
   '画框                                                                   填充色彩  线条类型     线条色彩
   
   Dim hBrush as HBRUSH, hPen as HPEN
   Dim hOldBrush as HGDIOBJ, hOldPen as HGDIOBJ
   
   If c <> -1 Then
      hBrush = CreateSolidBrush(c)
   Else
      hBrush = GetStockObject(HOLLOW_BRUSH) '创建空画笔，才能画出空心来
   End If
   hOldBrush = SelectObject(DC, hBrush)
   
   hPen = CreatePen(L, wl, cl)
   
   hOldPen = SelectObject(DC, hPen)
   
   Rectangle DC, X1, Y1, X2, Y2
   
   SelectObject DC, hOldPen
   
   DeleteObject hPen
   
   SelectObject DC, hOldBrush
   DeleteObject hBrush
   
   
End Sub
'[SourceDB:0-171069]
'圆角矩形 
' DrawCircleFrame(DC,X,Y,W,H,x圆角,y圆角,框线粗细,线条类型 ,边框色,填充色)
'需要支持高DPI，请代码中加 AfxScaleX(X), AfxScaleY(Y)  
'nPenStyle ------ Long，指定画笔样式，可以是下述常数之一
'PS_SOLID        画笔画出的是实线
'PS_DASH         画笔画出的是虚线（nWidth必须不大于1）
'PS_DOT          画笔画出的是点线（nWidth必须不大于1）
'PS_DASHDOT      画笔画出的是点划线（nWidth必须不大于1）
'PS_DASHDOTDOT   画笔画出的是点-点-划线（nWidth必须不大于1）
'PS_NULL         画笔不能画图
'PS_INSIDEFRAME  由椭圆、矩形、圆角矩形、饼图以及弦等生成的封闭对象框时，画线宽度向内扩展。如指定的准确RGB颜色不存在，就进行抖动处理
'颜色用 BGR(0, 0, 255) 创建  不能用 RGB
Sub DrawCircleFrame(ByVal sDC as hDC, ByVal X as Long, ByVal Y as Long, ByVal hWidth as Long, ByVal hHeight as Long, ByVal xCircle as Long, ByVal yCircle as Long, _
      ByVal LineWidth as Long, ByVal LineType as Long , ByVal bColor as Long, ByVal tColor as Long)'圆角矩形
   
   '(ByVal sDC As Long, ByVal X As Long, ByVal Y As Long, ByVal 宽度 As Long, ByVal 高度 As Long, X圆角 As Long, Y圆角 As Long, _
   'ByVal 框线粗细 As Long, ByVal 线条类型 As 线条类型_m, ByVal 边框色 As Long, ByVal 填充色 As Long) '圆角矩形
   '以支持高DPI，以DPI100%设置坐标就可以了
   
   Dim hBrush as HBRUSH, hPen as HPEN
   Dim hOldBrush as HGDIOBJ, hOldPen as HGDIOBJ
   
   If tColor <> -1 Then
      hBrush = CreateSolidBrush(tColor)
   Else
      hBrush = GetStockObject(HOLLOW_BRUSH) '创建空画笔，才能画出空心来
   End If
   hOldBrush = SelectObject(sDC, hBrush)
   
   hPen = CreatePen(LineType, LineWidth, bColor)
   
   hOldPen = SelectObject(sDC, hPen)
   
   RoundRect sDC, X, Y, X + hWidth, Y + hHeight, xCircle, yCircle
   
   SelectObject sDC, hOldPen
   
   DeleteObject hPen
   
   SelectObject sDC, hOldBrush
   DeleteObject hBrush
   
   '参数表
   '参数 类型及说明
   'sDC Long，用于绘图的设备场景
   'X1,Y1 Long，对矩形左上角位置进行说明的X，Y坐标
   'X2,Y2 Long，对矩形右下角位置进行说明的X，Y坐标
   'X3 Long，用于生成圆角效果的一个椭圆的宽度。取值范围从零（表示不加圆角），一直到矩形的宽度（全圆）
   'Y3 Long，用于生成圆角效果的一个椭圆的高度。取值范围从零（表示不加圆角），一直到矩形的高度（全圆）
   
   
End Sub
'[SourceDB:0-171070]
'描绘一个椭圆 
'DrawEllipse(Dc,X,Y,宽度 ,高度,线条类型,框线粗细,边框色,填充色)
'需要支持高DPI，请代码中加 AfxScaleX(X), AfxScaleY(Y)  
'nPenStyle ------ Long，指定画笔样式，可以是下述常数之一
'PS_SOLID        画笔画出的是实线
'PS_DASH         画笔画出的是虚线（nWidth必须不大于1）
'PS_DOT          画笔画出的是点线（nWidth必须不大于1）
'PS_DASHDOT      画笔画出的是点划线（nWidth必须不大于1）
'PS_DASHDOTDOT   画笔画出的是点-点-划线（nWidth必须不大于1）
'PS_NULL         画笔不能画图
'PS_INSIDEFRAME  由椭圆、矩形、圆角矩形、饼图以及弦等生成的封闭对象框时，画线宽度向内扩展。如指定的准确RGB颜色不存在，就进行抖动处理
'颜色用 BGR(0, 0, 255) 创建  不能用 RGB
Sub DrawEllipse(ByVal hDC as hDC, ByVal X as Integer, ByVal Y as Integer, ByVal hWidth as Integer, ByVal hHeight as Integer, _
      ByVal LineType as Integer ,ByVal LineWidth as Integer, ByVal bColor as  Integer , ByVal tColor as  Integer )'描绘一个椭圆
   
   '(ByVal hDc As Long, ByVal X As Long, ByVal Y As Long, ByVal 宽度 As Long, ByVal 高度 As Long, _
   'ByVal 框线粗细 As Long, ByVal 线条类型 As 线条类型_m, ByVal 边框色 As Long, ByVal 填充色 As Long) '圆角矩形
   
   
   Dim hBrush as HGDIOBJ, hPen as HGDIOBJ
   Dim hOldBrush as HGDIOBJ, hOldPen as HGDIOBJ
   
   If tColor <> -1 Then
      hBrush = CreateSolidBrush(tColor)
   Else
      hBrush = GetStockObject(HOLLOW_BRUSH)
   End If
   hOldBrush = SelectObject(hDC, hBrush)
   
   
   'SetBkMode hdc,TRANSPARENT
   '设置这个后，画上的字是透明的
   
   hPen = CreatePen(LineType, LineWidth, bColor)
   
   hOldPen = SelectObject(hDC, hPen)
   
   Ellipse hDC, X, Y, X + hWidth, Y + hHeight
   
   SelectObject hDC, hOldPen
   
   DeleteObject hPen
   
   SelectObject hDC, hOldBrush
   DeleteObject hBrush
   
   '参数表
   
   'X1，Y1 Long，约束矩形采用逻辑坐标的左上角位置
   'X2，Y2 Long，约束矩形采用逻辑坐标的右下角位置
   
   
End Sub

'[SourceDB:0-171078]
'获取文本显示宽度
Function GetTextWidth(ByVal DC as hDC, nText as String) as Long '获取文本显示宽度
   Dim s as SIZE
   GetTextExtentPoint32A DC, StrPtr(nText), Len(nText), @s
   Function = s.cx
End Function
'[SourceDB:0-171076]
'获取文本显示高度
Function GetTextHeight(ByVal DC as hDC, nText as String) as Long '获取文本显示高度
   Dim s as SIZE
   GetTextExtentPoint32A DC, StrPtr(nText), Len(nText), @s
   Function = s.cy
End Function