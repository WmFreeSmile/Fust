

Namespace 虚拟机

Enum 错误枚举
    
    找不到指定类
    
End Enum


Dim 最后一次错误 As 错误枚举
Dim 界目录表() As String
Dim 类文件路径表() As String

Sub 加入界目录(界目录 As String)
    Dim 索引 As Integer = ubound(界目录表) + 1
    ReDim Preserve 界目录表(索引)
    界目录表(索引) = 界目录
    文件.EnumFile(界目录, 类文件路径表())
End Sub


Type 命名空间路径
    路径(Any) As String
End Type

Type 类路径
    命名空间 As 命名空间路径
    类名 As String
End Type


Function 文本转命名空间路径(文本 As String) As 命名空间路径
    Dim result As 命名空间路径
    vbSplit(文本, "::", result.路径())
    Function = result
End Function

Function 文本转类路径(文本 As String) As 类路径
    Dim result As 类路径
    
    Dim 文本组() As String
    vbSplit(文本, "::", 文本组())
    ReDim result.命名空间.路径(ubound(文本组) -1)
    For i As Integer = LBound(文本组) To UBound(文本组) -1
        result.命名空间.路径(i) = 文本组(i)
    Next
    result.类名 = 文本组(UBound(文本组))
    Function = result
End Function

Function 读文本(f As Integer) As String
    Dim 长度 As Long
    Dim 文本字节组() As UByte
    
    Get #f,, 长度
    长度 = 长度 + 1
    ReDim 文本字节组(长度 -1)
    For i As Integer = LBound(文本字节组) To UBound(文本字节组)
        Get #f,, 文本字节组(i)
    Next
    
    Function = *Cast(ZString Ptr, @文本字节组(0))
End Function

Sub 读类路径(f As Integer, ByRef 类 As 类路径)
    Dim temp_long   As Long
    ReDim temp(Any) As UByte
    Dim temp_string As String
    Get #f,, temp_long
    Dim 索引 As Integer
    Erase 类.命名空间.路径
    For i As Integer = 1 To temp_long
        索引 = UBound(类.命名空间.路径) + 1
        ReDim Preserve 类.命名空间.路径(索引)
        类.命名空间.路径(索引) = 读文本(f)
    Next
    类.类名 = 读文本(f)
End Sub


Function 取类路径(类文件路径 As String) As 类路径
    Dim fhandle As Integer = FreeFile
    Dim result    As 类路径
    Open 类文件路径 For Binary As #fhandle
    
    读类路径(fhandle, result)
    
    Close fhandle
    Function = result
End Function

Function 类路径是否相同(类1 As 类路径,类2 As 类路径) As BOOL
    If 类1.类名 <> 类2.类名 Then Return False
    If LBound(类1.命名空间.路径) <> LBound(类2.命名空间.路径) OrElse UBound(类1.命名空间.路径) <> UBound(类2.命名空间.路径) Then Return False
    For i As Integer = LBound(类1.命名空间.路径) To UBound(类1.命名空间.路径)
        If 类1.命名空间.路径(i) <> 类2.命名空间.路径(i) Then Return False
    Next
    Function = True
End Function


Function 寻找类文件(类 As 类路径, ByRef 返回值 As String) As BOOL
    For i As Integer = LBound(类文件路径表) To UBound(类文件路径表)
        If 类路径是否相同(取类路径(类文件路径表(i)),类) Then
            返回值 = 类文件路径表(i)
            Return True
        End If
    Next
    
    最后一次错误 = 错误枚举.找不到指定类
    Function = False
End Function

Function 引入类(类 As 类路径) As BOOL
    Dim 类文件路径 As String
    If 寻找类文件(类, 类文件路径) = False Then Return False
    Function = 加载类(类文件路径)
End Function

Function 加载类(类文件路径 As String) As BOOL
    Dim fhandle As Integer = FreeFile
    Open 类文件路径 For Binary As #fhandle
    
    
    Dim 本类路径 As 类路径
    
    
    读类路径(fhandle, 本类路径)
    
    
    
    
    Close fhandle
End Function

Sub 启动()
    加入界目录("C:\Users\WmFreeSmile\Desktop\fust\编译器\release64\code")
    引入类(文本转类路径("Root::User::Program"))
    
End Sub





End Namespace
