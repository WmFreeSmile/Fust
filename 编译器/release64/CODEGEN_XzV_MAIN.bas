'-----------------------------------------------------------------------------
' �� VisualFreeBasic 5.8.11 ���ɵ�Դ����
' ����ʱ�䣺2025��03��15�� 16ʱ32��17��
' ������Ϣ����� www.yfvb.com 
'-----------------------------------------------------------------------------


     
Dim Shared vfb_Control_Ptr_Save As Any Ptr '�ؼ�����ָ�뱣�洦
Function vfb_Get_Control_Ptr(hWndControl As Any Ptr) As Any Ptr '��ȡ�ؼ�����ָ��
   if vfb_Control_Ptr_Save = 0 Then Return 0
   Dim tsave As Any Ptr Ptr = vfb_Control_Ptr_Save
   Dim ii    As Long
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = hWndControl Then Return tsave[ii + 1]
      Next
      if tsave[201] = 0 Then Return 0  'û����һ��ָ����
      tsave = tsave[201]
   Loop
End Function
Sub vfb_Set_Control_Ptr(hWndControl As Any Ptr ,pp As Any Ptr)   ' ���ÿؼ�����ָ��
   '1������Ϊ�����ھ��+����ָ��  ÿ������ 100������101���ǣ�ǰ1��������ָ��+��1��������ָ�롣
   '����ֻ���Ӳ����٣�����Ӧ�Զ��̲߳�����ȫ���������Ҫ���գ����Ա����������ڡ�ÿ���ؼ�ֻ����Ψһָ�룬�ظ�����ֻ�Ǹ���ָ��
   '�ο� API GetProp SetProp RemoveProp �Լ�д���������API�����ȫ��
   if vfb_Control_Ptr_Save = 0 Then vfb_Control_Ptr_Save = CAllocate(SizeOf(Integer) * 202)
   Dim tsave As Any Ptr Ptr = vfb_Control_Ptr_Save
   Dim ii    As Long
   '��Ѱ���ǲ����ظ����ڣ����ظ��͸���
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = hWndControl Then
            tsave[ii + 1] = pp
            Return
         End if
      Next
      if tsave[201] = 0 Then Exit Do 'û����һ��ָ����
      tsave = tsave[201]
   Loop
   'Ѱ�ҿ�λ�������洰������ָ��
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = 0 Then
            tsave[ii] = hWndControl
            tsave[ii + 1] = pp
            Return
         End if
      Next
      if tsave[201] = 0 Then  'û����һ�飬�ʹ���һ��
         tsave[200] = tsave
         tsave[201] = CAllocate(SizeOf(Integer) * 202)
      End if
      tsave = tsave[201]
   Loop
End Sub
Sub vfb_Remove_Control_Ptr(hWndControl As Any Ptr) 'ɾ������ָ��
   if vfb_Control_Ptr_Save = 0 Then Return 
   Dim tsave As Any Ptr Ptr = vfb_Control_Ptr_Save
   Dim ii    As Long
   Do
      For ii = 0 To 198 Step 2
         if tsave[ii] = hWndControl Then  '������ݣ�Ϊ��Ŀؼ����Ա�������
            tsave[ii] = 0
            tsave[ii + 1] = 0
            Return
         End if
      Next
      if tsave[201] = 0 Then Return  'û����һ��ָ����
      tsave = tsave[201]
   Loop
End Sub

'[START_APPSTART]
'************ Ӧ�ó�����ʼģ�� ************
' �����Ǵ�����������ִ�д���ĵط�����ʱ����ڲ���δ��ʼ������ע��һ�����EXE����DLL�ģ�DLL����EXEִ��DLL�Լ�����ʼ���룩
' ������EXE����DLL���������￪ʼִ�У�Ȼ�󵽡�������ں�����ִ�У��������������DLL��ж�ؾ�ִ�С�������ڡ����̡�(�����EXE��DLL��ʾ�Լ�����)
' һ�����������д DLL �������Զ���������������#Include�İ����ļ������ںܶ��ʼ������δִ�У����ﲻ����д�û����롣

#define UNICODE                 '��ʾWIN��APIĬ��ʹ�� Wϵ�У����ַ��������ɾ����ʹ��ASCII�ַ�������ɿؼ���APIʹ�õĴ����д��ʽ��Ӱ����Զ��
#lang "FB"                      '��ʾΪ��׼FB��ʽ
#include Once "windows.bi"      'WINϵͳ�����⣬��Ҫ��WINϵͳ���õ�API����ʹ��API���Բ���������
#include Once "win/shlobj.bi"   'WINϵͳ����⣬shell32.dll��ͷ�ļ�����Ҫ�漰shell��ͨ�öԻ���ȡ�
#include Once "afx/CWindow.inc" 'WinFBX �⣬��WINϵͳ��ǿ�����⣬ʹ�ô��ںͿؼ�����Ҫ�õ�����
#include Once "vbcompat.bi"     'һЩ����VB�����ͳ������������ͻ��޷�ʹ�������ˡ�
#include Once "fbthread.bi"     'VisualFreeBasic�߳����֧�ֿ⣬Ҫ���߳���䣬�ͱ��������

#include Once "crt/mem.bi" 'ln 9  
#include Once "inc/md5.bi"
#include Once "dir.bi" 'ln 7  
'���� �����ļ� ��Ӱ�����ձ��������ļ��Ĵ�С��������Ӱ������Ч�ʣ������ļ���С�Ƚ����еģ����Ը�����Ҫ������

'��������ģ���ͬʱ��֤�����Ѿ��� VisualFreeBasic �����˽⣬����������ֽ׶Ρ�
'��ʱ����Ҫ�˽�VFB�Ĺ������̡�
'VFB ����ִ�����̣�
'ϵͳ����EXE -->����ʼģ�顿-->����ں�����-->���������ڡ�-->�ر��������ں�-->�����ں�����--> �����˳� 
'
'Ϊ�˼򻯱�д���룬VisualFreeBasic �Դ�����мӹ���������������ʼģ�顿������˵����ʼģ�顿�������κζ���ֱ��ȥ����ġ�
'��˱�ģ�鲻Ҫд�û����룬һ��д��������á�
'
'������мӹ��������£�
'1����������ƣ�ת��ΪWInϵͳSDKģʽ�Ĵ����룬�����¼�����Ϣ���̴���ȵȷǳ����ӵĲ�����
'2�������Ĵ����滻��Ӣ�Ĵ������룬��FreeBasic֧�����Ĵ��루ǰ�᣺VFBѡ����ѡ����������ܣ�Ĭ��ѡ��ģ�
'3����ȡ���������ࡢȫ�ֱ����ȵĶ������÷ŵ������ļ������á�����д�������迼�Ǻ����Ⱥ�˳�����⣬�Լ��Լ�д���塣
'4���Զ�����������ˡ�Դ��⡿�͡��ҵĴ���⡿������ش���ȫ�����뵥���ļ������á������ǿ���ֱ��ʹ��Դ��⣬�������Լ�ȥ����Դ�롣
'5�����㹤������ѡ�������ԣ���֧�ֶ�����Ժ�������������÷���������⡣
'6���������а�������ģ��ʱ���Զ��������á�
'�ӹ����̲�����ʱ�ļ����ڹ��������������Ƿ��Զ�ɾ����

'[END_APPSTART]

#include Once "win/shlobj.bi"   'WINϵͳ����⣬shell32.dll��ͷ�ļ�����Ҫ�漰shell��ͨ�öԻ���ȡ�
#include Once "Afx/CWStr.inc"   

' ����Ա����ͨ������APP�������ʵĹ�����Ϣ��
Type APP_TYPE
   Comments        As  CWSTR      ' ע��
   CompanyName     As  CWSTR       ' ��˾�� 
   EXEName         As  CWSTR      ' �����EXE���� 
   FileDescription As  CWSTR       ' �ļ����� 
   hInstance       As  HINSTANCE                ' �����ʵ�����
   Path            As  CWSTR      ' EXE�ĵ�ǰ·��
   ProductName     As  CWSTR      ' ��Ʒ���� 
   LegalCopyright  As  CWSTR       ' ��Ȩ���� 
   LegalTrademarks As  CWSTR     ' �̱�
   ProductMajor    As Long                    ' ��Ʒ��Ҫ��� 
   ProductMinor    As Long                    ' ��Ʒ��Ҫ���   
   ProductRevision As Long                    ' ��Ʒ�޶���
   ProductBuild    As Long                    ' ��Ʒ�ڲ����   
   FileMajor       As Long                    ' �ļ���Ҫ���     
   FileMinor       As Long                    ' �ļ���Ҫ���     
   FileRevision    As Long                    ' �ļ��޶���  
   FileBuild       As Long                    ' �ļ��ڲ����     
   ReturnValue     As Integer                 ' ���ص��û�ֵ
End Type
Dim Shared App As APP_TYPE
Sub Setting_up_Application_Common_Information()
   '���ù���Ӧ�ó��������ֵ
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
   
   'App.hInstance ��WinMain / LibMain������
   
   '������������·���� EXE/DLL ����
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
' ����/��ͬ ��Ŀ�е����к��������Ϳؼ�
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
Function FF_WINMAIN(ByVal hInstance As HINSTANCE) As Long '������ں���
   'hInstance EXE��DLL��ģ�������������ڴ��еĵ�ַ��EXE ͨ���̶�Ϊ &H400000  DLL һ�㲻�̶� 
   '����Ϊ LIB��̬��ʱ�����������κ��ô� 
   ' -------------------------------------------------------------------------------------------
   '  DLL ���� ********  �������践��ֵ
   '  DLL�����ص��ڴ�ʱ����Ҫִ��̫��ʱ��Ĵ��룬����Ҫ��ʱ���ö��̡߳�
   '        AfxMsg "DLL�����ص��ڴ�ʱ"
   ' -------------------------------------------------------------------------------------------
   '  EXE ���� ********   
   '        AfxMsg "EXE������"
   ' ��������������TRUE�����㣩�������������������û���������ڣ���ô�˺�������Ҳ����ֹ�����
   ' �������ڴ˺����������ʼ����
   ' -------------------------------------------------------------------------------------------
   ' (�����EXE��DLL��ʾ�Լ������޷���ȡ����EXE��DLL��ںͳ���)
   CH_E7BC96E8AF91E599A8_.CH_E590AFE58AA8_()
   
   Function = False   
End Function

Sub FF_WINEND(ByVal hInstance As HINSTANCE) '������ڣ�������ֹ��������롣
   'hInstance EXE��DLL��ģ�������������ڴ��еĵ�ַ��EXE ͨ���̶�Ϊ &H400000  DLL һ�㲻�̶� 
   '����Ϊ LIB��̬��ʱ�����������κ��ô� 
   ' -------------------------------------------------------------------------------------------
   '  DLL ���� ********    
   '    ж��DLL��DLL��ж�أ���Ҫ������ɣ������ý�������
   '    AfxMsg "DLL��ж��ʱ" 
   ' -------------------------------------------------------------------------------------------
   '  EXE ���� ********   
   '   ���򼴽����������������Ҫִ�еĴ��룬�����޷�ֹͣ���˳������ˡ�
   '      AfxMsg "EXE�˳�"
   ' -------------------------------------------------------------------------------------------
   ' (�����EXE��DLL��ʾ�Լ������޷���ȡ����EXE��DLL��ںͳ���)

End Sub



'[END_WINMAIN]


'[START_PUMPHOOK]
Function FF_PUMPHOOK( uMsg As Msg ) As Long '��Ϣ����
   '���д�����Ϣ��������������������������Ϣ��

   ' �������������� FALSE���㣩����ô VisualFreeBasic ��Ϣ�ý��������С�
   ' ���� TRUE�����㣩���ƹ���Ϣ�ã�������Ϣ�������ǳԵ�����Ϣ�������ںͿؼ�����
   ' 

   Function = False    '�������Ҫ������Ϣ������ TRUE ��

End Function



'[END_PUMPHOOK]


Function FLY_Win_Main(ByVal hInstance As HINSTANCE) As Long

      Dim gdipToken As ULONG_PTR
   Dim gdipsi As GdiplusStartupInput
   gdipsi.GdiplusVersion = 1
   GdiplusStartup( @gdipToken, @gdipsi, Null )

   ' ���� FLY_WinMain()������ ����ú�������True����ִֹͣ�иó���
   If FF_WINMAIN(hInstance) Then Return True
   ' �����������塣
   #if __FB_OUT_EXE__ 
   
   GdiplusShutdown( gdipToken )
   #endif 
   Function = 0
End Function
Public Sub WinMainsexit() Destructor
   FF_WINEND(App.hInstance)
End Sub
FLY_Win_Main( App.hInstance )




