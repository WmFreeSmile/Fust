'[FILE:C:\Users\WmFreeSmile\Desktop\fust\编译器\modules\对象管理.Inc
'-----------------------------------------------------------------------------
' 由 VisualFreeBasic 5.8.11 生成的源代码
' 生成时间：2025年03月15日 17时43分47秒
' 更多信息请访问 www.yfvb.com 
'-----------------------------------------------------------------------------


Namespace CH_E5AFB9E8B1A1E7AEA1E79086_

Dim CH_E5AFB9E8B1A1E8A1A8_(Any) As Any Ptr

Function CH_E58AA0E585A5_(CH_E4B8B4E697B6E5AFB9E8B1A1_ As Any Ptr) As Any Ptr
   Dim CH_E7B4A2E5BC95_ As Integer
   CH_E7B4A2E5BC95_ = UBound(CH_E5AFB9E8B1A1E8A1A8_) + 1
   ReDim Preserve CH_E5AFB9E8B1A1E8A1A8_(CH_E7B4A2E5BC95_)
   CH_E5AFB9E8B1A1E8A1A8_(CH_E7B4A2E5BC95_) = CH_E4B8B4E697B6E5AFB9E8B1A1_
   Function = CH_E4B8B4E697B6E5AFB9E8B1A1_
End Function

Sub CH_E9878AE694BE_()
   For i As Integer = LBound(CH_E5AFB9E8B1A1E8A1A8_) To UBound(CH_E5AFB9E8B1A1E8A1A8_) 
      Delete CH_E5AFB9E8B1A1E8A1A8_(i)
   Next
   Erase CH_E5AFB9E8B1A1E8A1A8_
End Sub


End Namespace
