Namespace 文件


Sub KillDir(目录 As String)
   If Right(目录, 1) = "\" OrElse Right(目录, 1) = "/" Then'目录标准化
      目录 = Left(目录, Len(目录) -1)
   End If
   
#ifdef __FB_WIN32__
      目录=目录+"\"
#else
      目录=目录+ "/"
#endif
   Dim 枚举路径 As String
   Dim filename As String
   
   Dim 索引 As Integer
   ReDim 目录列表() As String
   
   filename = Dir(目录 + "*", fbDirectory)
   Do While Len(filename) > 0
      If filename <> "." AndAlso filename <> ".." Then
         索引 = UBound(目录列表) + 1
         ReDim Preserve 目录列表(索引)
#ifdef __FB_WIN32__
         目录列表(索引) = 目录 + filename + "\"
#else
         目录列表(索引) = 目录 + filename + "/"
#endif
      End If
      filename = Dir(fbDirectory)
   Loop
   
   For i As Integer = 0 To UBound(目录列表)
      KillDir(目录列表(i))
   Next

   枚举路径 = 目录 + "*"
   filename = Dir(枚举路径)
   Do While Len(filename) > 0
      Kill(目录 + filename)
      filename = Dir()
   Loop
   
   For i As Integer = LBound(目录列表) To UBound(目录列表)
      RmDir(目录列表(i))
   Next
   
   RmDir(目录)
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
