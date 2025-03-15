'[FILE:C:\Users\WmFreeSmile\Desktop\fust\编译器\modules\文件.Inc
'-----------------------------------------------------------------------------
' 由 VisualFreeBasic 5.8.11 生成的源代码
' 生成时间：2025年03月07日 13时56分49秒
' 更多信息请访问 www.yfvb.com 
'-----------------------------------------------------------------------------


Namespace CH_E69687E4BBB6_


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
