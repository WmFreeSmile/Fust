
Function Dump_命名空间路径(当前命名空间 As 命名空间信息 Ptr) As String
   Dim 临时 As 命名空间信息 Ptr
   Dim 返回路径 As String
   临时 = 当前命名空间
   
   While True
      If 临时->超空间 Then
        Exit While
      End If
      
      返回路径 = 临时->名称 + "::" + 返回路径
      临时 = 临时->父空间
   Wend
   Return 返回路径
End Function

Function Dump_类路径(临时类路径 As 类路径) As String
   Dim temp As String
   
   For i As Integer = 0 To UBound(临时类路径.命名空间路径)
      temp = temp + 临时类路径.命名空间路径(i) + "::"
   Next
   temp = temp + 临时类路径.类名
   Return temp
End Function


Function 寻找默认构造(临时类信息 As 类信息 Ptr) As 方法指向
   Dim 构造方法 As 方法指向 = -1
   For i As Integer = 0 To UBound(临时类信息->函数表)
      If 临时类信息->函数表(i)->构造方法 Then
         构造方法 = 临时类信息->函数表(i)
         Exit For
      End If
   Next
   Function = 构造方法
End Function


Function 取类深度(临时类信息 As 类信息 Ptr) As Integer
   Dim Temp As 类信息 Ptr = 临时类信息
   Dim 深度 As Integer = 1
   While Temp->超类 = False
      Temp = Temp->基类
      深度 = 深度 + 1
   Wend
   Function = 深度
End Function

Function 取成员深度(临时类信息 As 类信息 Ptr) As Integer
   Dim Temp as 类信息 ptr = 临时类信息
   Dim 深度 As Integer = ARRAYSIZE(临时类信息->成员表)
   
   While Temp->超类 = False
      深度 = 深度 + ARRAYSIZE(Temp->成员表)
      Temp = Temp->基类
   Wend
   
   Function = 深度
End Function

#ifdef __FB_WIN32__
Function MyUtf8toStr(Utf8str As String, CodePage As UINT = CP_ACP) As String
   Dim eLen As Integer = Len(Utf8str)
   If eLen = 0 Then Return ""
   Dim wsStr() As UByte ' = CAllocate(eLen * 2 + 2)  '+2 是为了最后空出2个00 为宽字符结尾
   ReDim wsStr(eLen * 2 + 2)
   UTFToWChar(UTF_ENCOD_UTF8, StrPtr(Utf8str), Cast(WString Ptr, @wsStr(0)), @eLen)
   Dim ZStr as String
   ZStr = String(eLen * 2 + 2, 0)
   Dim ZStrLen As Integer = WideCharToMultiByte(CodePage, 0, Cast(Wstring Ptr, @wsStr(0)), eLen, StrPtr(ZStr), eLen * 2 + 2, NULL, NULL)
   If ZStrLen Then Function = Left(ZStr, ZStrLen)
End Function
'[SourceDB:0-171141]
'普通字符 换成 UTF8字符
'返回包含UTF8字符格式的普通字符串（String 可以包含任何数据）
Function MyStrToUtf8(sStr As String, CodePage As UINT = CP_ACP) As String 'CodePage=语言代码页 简体中文=936 CP_ACP是默认系统语言，不同语言系统用CP_ACP转换会发生乱码
   Dim LenStr As Integer = Len(sStr)
   If LenStr = 0 Then Return ""
   Dim bLen As Integer = LenStr * 2
   '转换为宽字符，这是必须的，大家都这么做，直接转换就不一样了
   Dim wsStr As String  = String(bLen + 2, 0)
   Dim eLen  As Integer = MultiByteToWideChar(CodePage, 0, StrPtr(sStr), LenStr, Cast(Wstring Ptr, StrPtr(wsStr)), bLen)
   '由宽字符转换为 UTF8
   Dim dst     As String  = String(bLen, 0) '为输出Utf8预留空间
   Dim UTF8Len_ As Integer = WideCharToMultiByte(CP_UTF8, 0, Cast(Wstring Ptr, StrPtr(wsStr)), eLen, StrPtr(dst), bLen, NULL, NULL)
   If UTF8Len_ Then Function = Left(dst, UTF8Len_)
End Function

#endif

Sub SaveFile(filename As String, text As String)
    Dim h As Integer
    h = FreeFile
    If Open(filename For Binary As #h) <> 0 Then Return
    
    Put #h,, text
    Close #h
End Sub