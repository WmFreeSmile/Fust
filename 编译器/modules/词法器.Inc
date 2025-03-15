Namespace 词法器


Dim 源 As String
Dim 位置 As Integer




Function 是否代码尾() As BOOL
   Function = 位置 >= Len(源)
End Function



Enum 单元类型
   终止 = 0
   标识符 = 1
   数字 = 2
   字符串 = 3
   符号 = 4
   关键字 = 5
   跃点 = 6
End Enum

Type 单元信息
   Dim 行 As Integer
   
   Dim 类型 As 单元类型
End Type


Type 单元信息_标识符 Extends 单元信息
   Dim 名称 As String
   Declare Constructor(名称 As String)
End Type

Constructor 单元信息_标识符(名称 As String)
   Base.类型=单元类型.标识符
   
   This.名称 = 名称
End Constructor

Type 单元信息_数字 Extends 单元信息
   Dim 是否小数 As BOOL
   Dim 值 As String
   Declare Constructor(是否小数 As BOOL,值 As String)
End Type

Constructor 单元信息_数字(是否小数 As BOOL, 值 As String)
   Base.类型=单元类型.数字
   
   This.是否小数 = 是否小数
   This.值 = 值 
End Constructor




Type 单元信息_字符串 Extends 单元信息
   Dim 值 As String
   Declare Constructor(值 As String)
End Type

Constructor 单元信息_字符串(值 As String)
   Base.类型 = 单元类型.字符串
   
   this.值=值
End Constructor


Type 单元信息_符号 Extends 单元信息
   Dim 值 As String
   Declare Constructor(值 As String)
End Type

Constructor 单元信息_符号(值 As String)
   Base.类型 = 单元类型.符号
   
   This.值=值
End Constructor



Enum 关键字类型

key_namespace'0
key_class'1
key_enum'2
key_dim'3
key_function'4
key_sub'5
key_return'6
key_print'7
key_load'8
key_import'9
key_use'10
key_if'11
key_for'12
key_while'13
key_do'14
key_break'15
key_continue'16

key_as'17

'class
key_abstract
key_extends

key_static
key_virtual
key_override

key_operator
key_native

key_self

'if
key_else

'other
key_scope


key_true
key_false
key_new

End Enum


Type 单元信息_关键字 Extends 单元信息
   Dim 类型 As 关键字类型
   Declare Constructor(类型 As 关键字类型)
End Type

Constructor 单元信息_关键字(类型 As 关键字类型)
   Base.类型 = 单元类型.关键字
   This.类型 = 类型
End Constructor
 


Function 是否符号(临时文本 As String) As BOOL
   Function = (Mid(源, 位置, Len(临时文本)) = 临时文本)
End Function
Sub 符号(临时值 As String)
   If (Mid(源, 位置, 1) = 临时值) Then
      位置 = 位置 + 1
   Else
      '错误信息("应输入符号:'" + 临时值 + "'")
   End If
End Sub

Sub 跳过空格()
   Dim 临时值1 As String
   Dim 临时值2 As String
   Dim 局部记录位置 As Integer
   局部记录位置 = 位置
   
   临时值2 = Mid(源, 位置, 2)
   临时值1 = Left(临时值2, 1)
   
   While (临时值1 = " " OrElse 临时值1 = !"\13" OrElse 临时值1 = !"\10" OrElse 临时值1 = !"\9" OrElse 临时值1 = "#" OrElse 临时值2 = "/#")
      If (临时值1 = "#") Then
         While (Mid(源, 位置, 1) <> !"\n")
            位置 = 位置 + 1
            If (位置 >= Len(源)) Then
               Return
            End If
         Wend
      ElseIf (临时值2 = "/#") Then
         While (Mid(源, 位置, 2) <> "#/")
            位置 = 位置 + 1
            If (位置 >= Len(源)) Then
               Return
            End If
         Wend
         位置 = 位置 + 2
      End If
      位置 = 位置 + 1
      临时值2 = Mid(源, 位置, 2)
      临时值1 = Left(临时值2, 1)
   Wend
End Sub

Function 是否数字(临时文本 As String) As BOOL
   Function = Asc(临时文本) >= 48 AndAlso Asc(临时文本) <= 57   
End Function

Function 是否数字表达式() As BOOL
   Dim 临时文本 As String
   跳过空格()
   临时文本 = Mid(源, 位置, 1)
   If 是否数字(临时文本) Then
      Return True
   End If
   Return False
End Function

Function 数字表达式(ByRef 是否小数 As BOOL) As String
   Dim 临时文本 As String
   Dim 返回值 As String
   Dim 小数 As BOOL
   
   是否小数 = False
   
   临时文本 = Mid(源, 位置, 1)
   While 是否数字(临时文本)
      返回值 = 返回值 + 临时文本
      位置 = 位置 + 1
      临时文本 = Mid(源, 位置, 1)
      If (临时文本 = ".") Then
         If (小数) then
            Return "" 
         Else
            小数 = True
            返回值 = 返回值 + "."
            位置 = 位置 + 1
            临时文本 = Mid(源, 位置, 1)
            是否小数 = True
         End If
      End If
   Wend
   
   Return 返回值
End Function

Function 是否字符串表达式() As BOOL
   Function = (Mid(源, 位置, 1) = !"\34")
End Function

Function 字符串表达式() As String
   Dim 临时文本 As String
   Dim 临时值 As String
   Dim 临时位置 As Integer
   Dim 开始位置 As Integer
   Dim 转义字符 As BOOL 
   
   开始位置 = 位置
   
   符号(!"\34")
   While (Mid(源, 位置, 1) <> !"\34")
      临时位置 = 位置
      临时值 = Mid(源, 位置, 1)
      位置 = 位置 + 1
      
      If 临时值 = !"\10" Then
         '错误信息("字符串中有换行符")
         Return ""
      End If
      If 位置 > Len(源) Then
         Return ""
      End If
      
      If 临时值 = !"\10" Then
         临时文本 = 临时文本 + "\10a"
      ElseIf 临时值 = !"\13" Then
         临时文本 = 临时文本 + "\13a"
      ElseIf 临时值 = !"\32" Then
         临时文本 = 临时文本 + "\32a"
      Else
         If 转义字符 Then
            If 临时值 = "r" Then
               临时文本 = 临时文本 + !"\13"
            ElseIf 临时值 = "n" Then
               临时文本 = 临时文本 +  !"\10"
            ElseIf 临时值 = "t" Then
               临时文本 = 临时文本 + !"\9"
            ElseIf 临时值 = "q" Then
               临时文本 = 临时文本 + !"\34"
            ElseIf 临时值 = "\" Then
               临时文本 = 临时文本 + !"\92"
            End If
            转义字符 = False
         Else
            If 临时值 = "\" Then
               转义字符 = True
               开始位置 = 临时位置
               Continue While
            Else
               临时文本 = 临时文本 + 临时值
            End If
         End If
      End If
   Wend
   符号(!"\34")
   Function = 临时文本
End Function

Function 是否不是标识符首(临时值 As String) As BOOL
   Dim Code As UInteger
   Code = Asc(临时值)
   
   If Code >= 0 AndAlso Code <= 64 Then
      Return True
   ElseIf Code = 91 OrElse Code = 92 OrElse Code = 93 OrElse Code = 94 OrElse Code = 96 Then
      Return True
   ElseIf Code >= 123 AndAlso Code <= 127 Then
      Return True
   End If
   Return False
End Function

Function 是否不是标识符(临时值 As String) As BOOL
   Dim Code As UInteger
   Code = Asc(临时值)
   
   If Code >= 0 AndAlso Code <= 47 Then
      Return True
   ElseIf Code >= 58 AndAlso Code <= 64 Then
      Return True
   ElseIf Code = 91 OrElse Code = 92 OrElse Code = 93 OrElse Code = 94 OrElse Code = 96 Then
      Return True
   ElseIf Code >= 123 AndAlso Code <= 127 Then
      Return True
   End If
   Return False
End Function

Function 标识符() As String
   Dim 临时值 As String
   Dim 临时文本 As String
   临时值 = Mid(源, 位置, 1)
   If 是否不是标识符首(临时值) Then
      Return ""
   End If
   While (是否不是标识符(临时值) = False)
      临时文本 = 临时文本 + 临时值
      位置 = 位置 + 1
      临时值 = Mid(源, 位置, 1)
   Wend
   Function = 临时文本
End Function



Function 获取行号(当前位置 As Integer) As Integer
   Function = 取换行符数量(Left(源, 当前位置)) + 1
End Function

Function 取换行符数量(文本 As String) As Integer
   Dim 位置_ As Integer
   Dim 临时线 as integer
   位置_ = InStr(文本, !"\n")
   
   While (位置_ <> 0)
      临时线 = 临时线 + 1
      位置_ = InStr(位置_ + 2, 文本, !"\n")
   Wend
   Function = 临时线
End Function

Function 窥视() As 单元信息 Ptr
   Dim 记录位置 As Integer = 位置
   Function = 取下一个单元()
   位置 = 记录位置
End Function

Sub 连续窥视(ByRef 下一个单元 As 单元信息 Ptr,ByRef 再下一个单元 As 单元信息 Ptr)
   Dim 记录位置 As Integer = 位置
   下一个单元 = 取下一个单元()
   再下一个单元 = 取下一个单元()
   位置 = 记录位置
End Sub

Sub 三连窥视(ByRef 第一个单元 As 单元信息 Ptr, ByRef 第二个单元 As 单元信息 Ptr, ByRef 第三个单元 As 单元信息 Ptr)
   Dim 记录位置 As Integer = 位置
   第一个单元 = 取下一个单元()
   第二个单元 = 取下一个单元()
   第三个单元 = 取下一个单元()
   位置 = 记录位置
End Sub

Function 取下一个单元() As 单元信息 Ptr
   Dim result As 单元信息 Ptr 
   
   If 是否代码尾() Then
      result = 对象管理.加入(New 单元信息)
      result->类型 = 单元类型.终止
      Return result
   End If
   
   跳过空格()
   If 是否数字表达式() Then
      Dim 是否小数 As BOOL
      Dim 值 As String
      值 = 数字表达式(是否小数)
      
      result = 对象管理.加入(New 单元信息_数字(是否小数, 值))
   ElseIf 是否字符串表达式() Then
      Dim 值 As String
      值 = 字符串表达式()
      
      result = 对象管理.加入(New 单元信息_字符串(值))
   ElseIf 是否不是标识符首(Mid(源, 位置, 1)) Then
      If Mid(源, 位置, 1) = "@" Then
         result = 对象管理.加入(New 单元信息)
         result->类型 = 单元类型.跃点
      Else
         result = 对象管理.加入(New 单元信息_符号(Mid(源, 位置, 1)))
      End If
      
      位置 = 位置 + 1
   Else
      Dim temp As String
      temp = 标识符()
      Select Case temp
         Case "namespace"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_namespace))
         Case "class"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_class))
         Case "enum"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_enum))
         Case "dim"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_dim))
         Case "function"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_function))
         Case "sub"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_sub))
         Case "return"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_return))
         Case "print"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_print))
         Case "load"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_load))
         Case "import"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_import))
         Case "use"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_use))
         Case "if"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_if))
         Case "for"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_for))
         Case "while"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_while))
         Case "do"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_do))
         Case "break"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_break))
         Case "continue"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_continue))
         Case "as"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_as))
            'class
         Case "abstract"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_abstract))
         Case "extends"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_extends))
         Case "static"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_static))
         Case "virtual"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_virtual))
         Case "operator"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_operator))
         Case "native"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_native))
         Case "self"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_self))
            'if
         Case "else"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_else))
            'other
         Case "scope"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_scope))
         Case "true"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_true))
         Case "false"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_false))
         Case "new"
            result = 对象管理.加入(New 单元信息_关键字(关键字类型.key_new))
         Case Else
            result = 对象管理.加入(New 单元信息_标识符(temp))
      End Select
   End If
   跳过空格()
   result->行 = 获取行号(位置)
   
   Function = result
End Function


End Namespace

