'[FILE:C:\Users\WmFreeSmile\Desktop\fust\编译器\modules\文件.Inc
'-----------------------------------------------------------------------------
' 由 VisualFreeBasic 5.8.11 生成的源代码
' 生成时间：2025年03月15日 17时43分47秒
' 更多信息请访问 www.yfvb.com 
'-----------------------------------------------------------------------------


Namespace CH_E69687E4BBB6_


Sub KillDir(CH_E79BAEE5BD95_ As String)
   If Right(CH_E79BAEE5BD95_, 1) = "\" OrElse Right(CH_E79BAEE5BD95_, 1) = "/" Then'目录标准化
      CH_E79BAEE5BD95_ = Left(CH_E79BAEE5BD95_, Len(CH_E79BAEE5BD95_) -1)
   End If
   
#ifdef __FB_WIN32__
      CH_E79BAEE5BD95_=CH_E79BAEE5BD95_+"\"
#else
      CH_E79BAEE5BD95_=CH_E79BAEE5BD95_+ "/"
#endif
   Dim CH_E69E9AE4B8BEE8B7AFE5BE84_ As String
   Dim filename As String
   
   Dim CH_E7B4A2E5BC95_ As Integer
   ReDim CH_E79BAEE5BD95E58897E8A1A8_() As String
   
   filename = Dir(CH_E79BAEE5BD95_ + "*", fbDirectory)
   Do While Len(filename) > 0
      If filename <> "." AndAlso filename <> ".." Then
         CH_E7B4A2E5BC95_ = UBound(CH_E79BAEE5BD95E58897E8A1A8_) + 1
         ReDim Preserve CH_E79BAEE5BD95E58897E8A1A8_(CH_E7B4A2E5BC95_)
#ifdef __FB_WIN32__
         CH_E79BAEE5BD95E58897E8A1A8_(CH_E7B4A2E5BC95_) = CH_E79BAEE5BD95_ + filename + "\"
#else
         CH_E79BAEE5BD95E58897E8A1A8_(CH_E7B4A2E5BC95_) = CH_E79BAEE5BD95_ + filename + "/"
#endif
      End If
      filename = Dir(fbDirectory)
   Loop
   
   For i As Integer = 0 To UBound(CH_E79BAEE5BD95E58897E8A1A8_)
      KillDir(CH_E79BAEE5BD95E58897E8A1A8_(i))
   Next

   CH_E69E9AE4B8BEE8B7AFE5BE84_ = CH_E79BAEE5BD95_ + "*"
   filename = Dir(CH_E69E9AE4B8BEE8B7AFE5BE84_)
   Do While Len(filename) > 0
      Kill(CH_E79BAEE5BD95_ + filename)
      filename = Dir()
   Loop
   
   For i As Integer = LBound(CH_E79BAEE5BD95E58897E8A1A8_) To UBound(CH_E79BAEE5BD95E58897E8A1A8_)
      RmDir(CH_E79BAEE5BD95E58897E8A1A8_(i))
   Next
   
   RmDir(CH_E79BAEE5BD95_)
End Sub




Function LoadFile(ByRef filename As String) As String
    Dim h As Integer
    Dim txt As String
    h = FreeFile
    If Open( filename For Binary Access Read As #h ) <> 0 Then Return ""
    If LOF(h) > 0 Then
        txt = String(LOF(h), 0)
        If Get( #h, ,txt ) <> 0 Then txt = ""
    End If
    Close #h
    Return txt
End Function

Sub SaveFile(filename As String, text As String)
    Dim h As Integer
    h = FreeFile
    If Open(filename For Binary As #h) <> 0 Then Return
    
    Put #h,, text
    Close #h
End Sub

End Namespace
