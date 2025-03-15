'-----------------------------------------------------------------------------
' 由 VisualFreeBasic 5.8.11 生成的源代码
' 生成时间：2025年03月15日 16时32分17秒
' 更多信息请访问 www.yfvb.com 
'-----------------------------------------------------------------------------


     
Dim Shared vfb_Control_Ptr_Save As Any Ptr '控件数据指针保存处
Function vfb_Get_Control_Ptr(hWndControl As Any Ptr) As Any Ptr '获取控件数据指针
   if vfb_Control_Ptr_Save = 0 Then Return 0
   Dim tsave As Any Ptr Ptr = vfb_Control_Ptr_Save
   Dim ii    As Long
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = hWndControl Then Return tsave[ii + 1]
      Next
      if tsave[201] = 0 Then Return 0  '没有下一个指针组
      tsave = tsave[201]
   Loop
End Function
Sub vfb_Set_Control_Ptr(hWndControl As Any Ptr ,pp As Any Ptr)   ' 设置控件数据指针
   '1个数据为：窗口句柄+数据指针  每组数据 100个，第101个是，前1个数据组指针+后1个数据组指针。
   '数据只增加不减少，可以应对多线程操作安全。用完后需要回收，可以保存其它窗口。每个控件只保存唯一指针，重复保存只是更新指针
   '参考 API GetProp SetProp RemoveProp 自己写函数保存比API快而安全。
   if vfb_Control_Ptr_Save = 0 Then vfb_Control_Ptr_Save = CAllocate(SizeOf(Integer) * 202)
   Dim tsave As Any Ptr Ptr = vfb_Control_Ptr_Save
   Dim ii    As Long
   '先寻找是不是重复窗口，有重复就更新
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = hWndControl Then
            tsave[ii + 1] = pp
            Return
         End if
      Next
      if tsave[201] = 0 Then Exit Do '没有下一个指针组
      tsave = tsave[201]
   Loop
   '寻找空位置来保存窗口数据指针
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = 0 Then
            tsave[ii] = hWndControl
            tsave[ii + 1] = pp
            Return
         End if
      Next
      if tsave[201] = 0 Then  '没有下一组，就创建一个
         tsave[200] = tsave
         tsave[201] = CAllocate(SizeOf(Integer) * 202)
      End if
      tsave = tsave[201]
   Loop
End Sub
Sub vfb_Remove_Control_Ptr(hWndControl As Any Ptr) '删除数据指针
   if vfb_Control_Ptr_Save = 0 Then Return 
   Dim tsave As Any Ptr Ptr = vfb_Control_Ptr_Save
   Dim ii    As Long
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = hWndControl Then  '清除数据，为别的控件可以保存数据
            tsave[ii] = 0
            tsave[ii + 1] = 0
            Return
         End if
      Next
      if tsave[201] = 0 Then Return  '没有下一个指针组
      tsave = tsave[201]
   Loop
End Sub

'[START_APPSTART]
'************ 应用程序起始模块 ************
' 这里是打开软件后最初被执行代码的地方，此时软件内部还未初始化。（注：一般情况EXE包含DLL的，DLL先于EXE执行DLL自己的起始代码）
' 不管是EXE还是DLL，都从这里开始执行，然后到【程序入口函数】执行，整个软件结束或DLL被卸载就执行【程序出口】过程。(这里的EXE、DLL表示自己程序)
' 一般情况在这里写 DLL 声明、自定义声明、常量和#Include的包含文件。由于很多初始化代码未执行，这里不建议写用户代码。

#define UNICODE                 '表示WIN的API默认使用 W系列，宽字符处理，如果删除（使用ASCII字符）会造成控件和API使用的代码编写方式，影响深远。
#lang "FB"                      '表示为标准FB格式
#include Once "windows.bi"      'WIN系统基础库，主要是WIN系统常用的API，不使用API可以不包含它。
#include Once "win/shlobj.bi"   'WIN系统对象库，shell32.dll的头文件，主要涉及shell及通用对话框等。
#include Once "afx/CWindow.inc" 'WinFBX 库，是WIN系统增强基础库，使用窗口和控件必须要用到它。
#include Once "vbcompat.bi"     '一些兼容VB的语句和常数，不包含就会无法使用它们了。
#include Once "fbthread.bi"     'VisualFreeBasic线程语句支持库，要用线程语句，就必须包含。

#include Once "crt/mem.bi" 'ln 9  
#include Once "inc/md5.bi"
#include Once "dir.bi" 'ln 7  
'以上 包含文件 会影响最终编译生成文件的大小，而不会影响运行效率，对于文件大小比较敏感的，可以根据需要调整。

'当你打开这个模块的同时，证明你已经对 VisualFreeBasic 有所了解，即将进入高手阶段。
'此时你需要了解VFB的工作过程。
'VFB 代码执行流程：
'系统加载EXE -->【起始模块】-->【入口函数】-->【启动窗口】-->关闭启动窗口后-->【出口函数】--> 进程退出 
'
'为了简化编写代码，VisualFreeBasic 对代码进行加工，但不包括【起始模块】，就是说【起始模块】不处理任何东西直接去编译的。
'因此本模块不要写用户代码，一般写定义和引用。
'
'代码进行加工过程如下：
'1，将窗口设计，转换为WIn系统SDK模式的纯代码，包括事件和消息流程处理等等非常复杂的操作。
'2，把中文代码替换成英文代码后编译，让FreeBasic支持中文代码（前提：VFB选项中选择了这个功能，默认选择的）
'3，提取函数名，类、全局变量等的定义设置放到单独文件中引用。让你写代码无需考虑函数先后顺序问题，以及自己写定义。
'4，自动检查代码调用了【源码库】和【我的代码库】，把相关代码全部放入单独文件中引用。让我们可以直接使用源码库，而无需自己去复制源码。
'5，当你工程属性选择多国语言，将支持多国语言函数。多国语言用法见相关例题。
'6，当工程中包含其它模块时，自动帮你引用。
'加工过程产生临时文件，在工程属性里设置是否自动删除。

'[END_APPSTART]

#include Once "win/shlobj.bi"   'WIN系统对象库，shell32.dll的头文件，主要涉及shell及通用对话框等。
#include Once "Afx/CWStr.inc"   

' 程序员可以通过共享APP变量访问的公共信息。
Type APP_TYPE
   Comments        As  CWSTR      ' 注释
   CompanyName     As  CWSTR       ' 公司名 
   EXEName         As  CWSTR      ' 程序的EXE名称 
   FileDescription As  CWSTR       ' 文件描述 
   hInstance       As  HINSTANCE                ' 程序的实例句柄
   Path            As  CWSTR      ' EXE的当前路径
   ProductName     As  CWSTR      ' 产品名称 
   LegalCopyright  As  CWSTR       ' 版权所有 
   LegalTrademarks As  CWSTR     ' 商标
   ProductMajor    As Long                    ' 产品主要编号 
   ProductMinor    As Long                    ' 产品次要编号   
   ProductRevision As Long                    ' 产品修订号
   ProductBuild    As Long                    ' 产品内部编号   
   FileMajor       As Long                    ' 文件主要编号     
   FileMinor       As Long                    ' 文件次要编号     
   FileRevision    As Long                    ' 文件修订号  
   FileBuild       As Long                    ' 文件内部编号     
   ReturnValue     As Integer                 ' 返回的用户值
End Type
Dim Shared App As APP_TYPE
Sub Setting_up_Application_Common_Information()
   '设置共享应用程序变量的值
   #if __FB_OUT_EXE__
   App.hInstance = GetModuleHandle(null)
   #else
   Dim mbi as MEMORY_BASIC_INFORMATION
   VirtualQuery(@Setting_up_Application_Common_Information, @mbi, SizeOf(mbi))
   App.hInstance = mbi.AllocationBase
   #endif
   Dim zTemp As WString * MAX_PATH
   Dim x As Long
   App.CompanyName = ""
   App.FileDescription = ""
   App.ProductName = ""
   App.LegalCopyright = ""
   App.LegalTrademarks = ""
   App.Comments = ""
   
   App.ProductMajor = 1
   App.ProductMinor = 0
   App.ProductRevision = 0
   App.ProductBuild = 15
   
   App.FileMajor = 1
   App.FileMinor = 0
   App.FileRevision = 0
   App.FileBuild = 890
   
   'App.hInstance 在WinMain / LibMain中设置
   
   '检索程序完整路径和 EXE/DLL 名称
   GetModuleFileNameW App.hInstance, zTemp, MAX_PATH
   x = InStrRev(zTemp, Any ":/\")
   If x Then
      App.Path = Left(zTemp, x)
      App.EXEname = Mid(zTemp, x + 1)
   Else
      App.Path = ""
      App.EXEname = zTemp
   End If
End Sub
Setting_up_Application_Common_Information
' 声明/等同 项目中的所有函数，表单和控件
#Include Once "CODEGEN_XzV_DECLARES.inc"
#Include Once "CODEGEN_XzV_UTILITY.inc"
#Include Once "CODEGEN_XzV_CUg_MODULE.inc"
#Include Once "CODEGEN_XzV_hbV_MODULE.inc"
#Include Once "CODEGEN_XzV_XzV_MODULE.inc"
#Include Once "CODEGEN_XzV_fe_MODULE.inc"
#Include Once "CODEGEN_XzV_CFRt_MODULE.inc"
#Include Once "CODEGEN_XzV_r_MODULE.inc"
#Include Once "CODEGEN_XzV_txzb_MODULE.inc"
#Include Once "CODEGEN_XzV_BaZYS5V_MODULE.inc"
#Include Once "CODEGEN_XzV_Form1_FORM.inc"
    

'[START_WINMAIN]
Function FF_WINMAIN(ByVal hInstance As HINSTANCE) As Long '程序入口函数
   'hInstance EXE或DLL的模块句柄，就是在内存中的地址，EXE 通常固定为 &H400000  DLL 一般不固定 
   '编译为 LIB静态库时，这里是无任何用处 
   ' -------------------------------------------------------------------------------------------
   '  DLL 例题 ********  函数无需返回值
   '  DLL被加载到内存时，不要执行太耗时间的代码，若需要耗时就用多线程。
   '        AfxMsg "DLL被加载到内存时"
   ' -------------------------------------------------------------------------------------------
   '  EXE 例题 ********   
   '        AfxMsg "EXE刚启动"
   ' 如果这个函数返回TRUE（非零），将会结束该软件。如果没有启动窗口，那么此函数过后，也会终止软件。
   ' 您可以在此函数做程序初始化。
   ' -------------------------------------------------------------------------------------------
   ' (这里的EXE、DLL表示自己程序，无法获取其它EXE、DLL入口和出口)
   CH_E7BC96E8AF91E599A8_.CH_E590AFE58AA8_()
   
   Function = False   
End Function

Sub FF_WINEND(ByVal hInstance As HINSTANCE) '程序出口，程序终止后的最后代码。
   'hInstance EXE或DLL的模块句柄，就是在内存中的地址，EXE 通常固定为 &H400000  DLL 一般不固定 
   '编译为 LIB静态库时，这里是无任何用处 
   ' -------------------------------------------------------------------------------------------
   '  DLL 例题 ********    
   '    卸载DLL，DLL被卸载，需要快速完成，不能用进程锁。
   '    AfxMsg "DLL被卸载时" 
   ' -------------------------------------------------------------------------------------------
   '  EXE 例题 ********   
   '   程序即将结束，这里是最后要执行的代码，（：无法停止被退出的命运。
   '      AfxMsg "EXE退出"
   ' -------------------------------------------------------------------------------------------
   ' (这里的EXE、DLL表示自己程序，无法获取其它EXE、DLL入口和出口)

End Sub



'[END_WINMAIN]


'[START_PUMPHOOK]
Function FF_PUMPHOOK( uMsg As Msg ) As Long '消息钩子
   '所有窗口消息都经过这里，你可以在这里拦截消息。

   ' 如果这个函数返回 FALSE（零），那么 VisualFreeBasic 消息泵将继续进行。
   ' 返回 TRUE（非零）将绕过消息泵（屏蔽消息），就是吃掉这消息不给窗口和控件处理。
   ' 

   Function = False    '如果你需要屏蔽消息，返回 TRUE 。

End Function



'[END_PUMPHOOK]


Function FLY_Win_Main(ByVal hInstance As HINSTANCE) As Long

      Dim gdipToken As ULONG_PTR
   Dim gdipsi As GdiplusStartupInput
   gdipsi.GdiplusVersion = 1
   GdiplusStartup( @gdipToken, @gdipsi, Null )

   ' 调用 FLY_WinMain()函数。 如果该函数返回True，则停止执行该程序。
   If FF_WINMAIN(hInstance) Then Return True
   ' 创建启动窗体。
   #if __FB_OUT_EXE__ 
   
   GdiplusShutdown( gdipToken )
   #endif 
   Function = 0
End Function
Public Sub WinMainsexit() Destructor
   FF_WINEND(App.hInstance)
End Sub
FLY_Win_Main( App.hInstance )




