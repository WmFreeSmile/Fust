'[FILE:C:\Users\WmFreeSmile\Desktop\fust\编译器\modules\对应表.Inc
'-----------------------------------------------------------------------------
' 由 VisualFreeBasic 5.8.11 生成的源代码
' 生成时间：2025年03月07日 13时56分49秒
' 更多信息请访问 www.yfvb.com 
'-----------------------------------------------------------------------------


Namespace CH_E698A0E5B084E7AEA1E79086_


' Type 映射项 '已经搬到定义文件处
'    起始 As Any Ptr '已经搬到定义文件处
'    目标 As Any Ptr '已经搬到定义文件处
' End Type '已经搬到定义文件处

' Type 映射表 '已经搬到定义文件处
'    表(Any) As 映射项 '已经搬到定义文件处
'     '已经搬到定义文件处
'    Declare Sub 加入(起始 As Any Ptr, 目标 As Any Ptr) '已经搬到定义文件处
'    Declare Function 寻找(起始 As Any Ptr) As Any Ptr '已经搬到定义文件处
' End Type '已经搬到定义文件处

Sub CH_E698A0E5B084E8A1A8_.CH_E58AA0E585A5_(CH_E8B5B7E5A78B_ As Any Ptr, CH_E79BAEE6A087_ As Any Ptr)
   Dim CH_E7B4A2E5BC95_ As Integer
   CH_E7B4A2E5BC95_ = UBound(This.CH_E8A1A8_) + 1
   ReDim Preserve This.CH_E8A1A8_(CH_E7B4A2E5BC95_)
   This.CH_E8A1A8_(CH_E7B4A2E5BC95_).CH_E8B5B7E5A78B_ = CH_E8B5B7E5A78B_
   This.CH_E8A1A8_(CH_E7B4A2E5BC95_).CH_E79BAEE6A087_ = CH_E79BAEE6A087_
End Sub

Function CH_E698A0E5B084E8A1A8_.CH_E5AFBBE689BE_(CH_E8B5B7E5A78B_ As Any Ptr) As Any Ptr
   For i As Integer = LBound(This.CH_E8A1A8_) To UBound(This.CH_E8A1A8_)
      If This.CH_E8A1A8_(i).CH_E8B5B7E5A78B_ = CH_E8B5B7E5A78B_ Then
         Return This.CH_E8A1A8_(i).CH_E79BAEE6A087_
      End If
   Next
   Function = 0
End Function


End Namespace