


Sub Read_Line(f As Integer,存储容器() As UByte)
   Dim temp As UByte
   Dim 索引 As Integer
   Erase 存储容器
   While True
      Get #f,, temp
      if temp = 0 then
         Exit While
      End If
      索引 = UBound(存储容器) + 1
      ReDim Preserve 存储容器(索引)
      存储容器(索引) = temp
   Wend
End Sub

Function Read_Str(f As Integer) As String
   ReDim temp(Any) As UByte
   Dim temp_string As String
   Read_Line(f, temp())
   temp_string = Space(ARRAYSIZE(temp))
   memcpy(StrPtr(temp_string), @temp(0), ARRAYSIZE(temp))
   Function = temp_string
End Function


Sub Read_Class(f As Integer, ByRef 临时类路径 As 类路径)
   Dim temp_long As Long
   ReDim temp(Any) As UByte
   Dim temp_string As String
   Get #f,, temp_long
   Dim 索引 As Integer
   Erase 临时类路径.命名空间路径
   For i As Integer = 1 To temp_long
      索引 = UBound(临时类路径.命名空间路径) + 1
      ReDim Preserve 临时类路径.命名空间路径(索引)
      临时类路径.命名空间路径(索引) = Read_Str(f)
   Next
   临时类路径.类名 = Read_Str(f)
End Sub

Sub Read_ClassMember(f As Integer,ByRef 临时成员路径 As 成员路径)
   Read_Class(f, 临时成员路径.类)
   临时成员路径.成员名 = Read_Str(f)
End Sub

Sub Read_ClassMethod(f As Integer, ByRef 临时方法路径 As 方法路径)
   Read_Class(f, 临时方法路径.类)
   临时方法路径.方法签名 = Read_ULI(f)
End Sub


Function Read_yesORno(f As Integer) As BOOL
   Dim temp As UByte
   Get #f,, temp
   
   Function = (temp = 1)
End Function

Function Read_LI(f As Integer) As LongInt
   Dim temp As LongInt
   Get #f,, temp
   
   Function = temp
End Function

Function Read_L(f As Integer) As Long
   Dim temp As Long
   Get #f,, temp
   
   Function = temp
End Function

Function Read_S(f As Integer) As Short
   Dim temp As Short
   Get #f,, temp
   
   Function = temp
End Function

Function Read_SI(f As Integer) As Single
   Dim temp As Single
   Get #f,, temp
   
   Function = temp
End Function


Function Read_ULI(f As Integer) As ULongInt
   Dim temp As ULongInt
   Get #f,, temp
   
   Function = temp
End Function
Function Read_UB(f As Integer) As UByte
   Dim temp As UByte
   Get #f,, temp
   
   Function = temp
End Function

Sub Read_UBs(f As Integer,存储容器() As UByte)
   Dim size_len As LongInt
   Dim 索引 As Integer
   Erase 存储容器
   size_len = Read_LI(f)
   For i As Integer = 1 To size_len
      索引 = UBound(存储容器) + 1
      ReDim Preserve 存储容器(索引)
      存储容器(索引) = Read_UB(f)
   Next
End Sub
Function Read_D(f As Integer) As Double
   Dim temp As Double
   Get #f,, temp
   
   Function = temp
End Function

Function 取类路径(文件路径 As String) As 类路径
   Dim fhandle As Integer
   
   fhandle = FreeFile
   
   Open 文件路径 For Binary As #fhandle
   
   Dim 本类 As 类路径
   
   Read_Class(fhandle, 本类)
   
   Close fhandle
   
   Function = 本类
End Function

Sub 遍历文件(目录 As String, 文件列表() As String)
   Dim 枚举路径 As String
   Dim index As Integer
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
      遍历文件(目录列表(i), 文件列表())
   Next

   枚举路径 = 目录 + "*"
   filename = Dir(枚举路径)
   Do While Len(filename) > 0
      index = UBound(文件列表) + 1
      ReDim Preserve 文件列表(index)
      文件列表(index) = 目录 + filename
      filename = Dir()
   Loop
End Sub



Function 寻找类(临时类路径 As 类路径) As String
   ReDim 文件列表(Any) As String
   
   Dim 枚举路径 As String
   
   For i As Integer = LBound(引用目录) To UBound(引用目录)
      遍历文件(引用目录(i), 文件列表())
   Next

   For i As Integer = LBound(文件列表) To UBound(文件列表)
      Dim 临时 As 类路径
      临时 = 取类路径(文件列表(i))
      
      If 临时.类名 <> 临时类路径.类名 Then
         Continue For
      End If
      If UBound(临时.命名空间路径) <> UBound(临时类路径.命名空间路径) Then
         Continue For
      End If
      
      Dim IsOk As BOOL
      IsOk = True
      For j As Integer = LBound(临时.命名空间路径) To UBound(临时.命名空间路径)
         If 临时.命名空间路径(j) <> 临时类路径.命名空间路径(j) Then
            IsOk = False
            Exit For
         End If
      Next
      
      If IsOk = True Then
         Return 文件列表(i)
      End If
   Next
   
   Return ""
End Function

Sub 引入类(临时类路径 As 类路径)
   Dim 类文件 As String
   类文件 = 寻找类(临时类路径)
   If 类文件 <> "" Then
      加载类(类文件)
   End If
End Sub
Sub 加载类(路径 As String)
    
    Dim fhandle As Integer
    
    Dim temp_long   As Long
    ReDim temp(Any) As UByte
    fhandle = FreeFile
    
    Open 路径 For Binary As #fhandle
    
    Dim yesORno As BOOL
    
    Dim 本类对象 As 类信息 Ptr
    
    Dim 本类     As 类路径
    Dim 父类     As 类路径
    Dim 析构方法 As LongInt
    
    Dim 超类   As BOOL
    Dim 空析构 As BOOL
    
    Dim 前置数 As Long
    
    Dim 指令数 As LongInt
    
    Dim 方法数     As Long
    Dim 成员数     As Long
    Dim 静态成员数 As Long
    
    Dim 基类 As 类指向
    
    Read_Class(fhandle, 本类)
    
    yesORno = Read_yesORno(fhandle)
    超类    = (yesORno = False)
    
    If 超类 = False Then '父类
        Read_Class(fhandle, 父类)
        基类 = Find_Class(父类)
    End If
    
    yesORno = Read_yesORno(fhandle)
    空析构  = (yesORno = False)
    
    If 空析构 = False Then '析构方法
        析构方法 = Read_LI(fhandle)
    End If
    
    Dim 临时类路径 As 类路径
    前置数 = Read_L(fhandle)
    For i As Integer = 1 To 前置数
        Read_Class(fhandle, 临时类路径)
        Find_Class(临时类路径)
    Next
    
    本类对象 = Create_Class(本类)
    
    本类对象->超类 = 超类
    本类对象->基类 = 基类
    
    本类对象->深度 = 取类深度(本类对象)
    
    'If 本类对象->标志 = 类标志_byte Then
    '   类_byte = 本类对象
    'ElseIf 本类对象->标志 = 类标志_short Then
    '   类_short = 本类对象
    'ElseIf 本类对象->标志 = 类标志_int Then
    '   类_int = 本类对象
    'ElseIf 本类对象->标志 = 类标志_long Then
    '   类_long = 本类对象
    'ElseIf 本类对象->标志 = 类标志_float Then
    '   类_float = 本类对象
    'ElseIf 本类对象->标志 = 类标志_double Then
    '   类_double = 本类对象
    'ElseIf 本类对象->标志 = 类标志_bool Then
    '   类_bool = 本类对象
    'ElseIf 本类对象->标志 = 类标志_string Then
    '   类_string = 本类对象
    'End If
    
    yesORno = Read_yesORno(fhandle)
    本类对象->是否抽象 = yesORno
    
    'Print "Load", 路径, 本类对象->标志
    
    指令数 = Read_LI(fhandle)
    
    方法数 = Read_L(fhandle)
    
    Dim 静态构造方法 As 方法指向 = -1
    
    For i As Integer = 1 To 方法数
        Dim 索引 As Integer
        
        索引 = UBound(本类对象->函数表) + 1
        ReDim Preserve 本类对象->函数表(索引)
        
        本类对象->函数表(索引) = New 函数信息
        
        本类对象->函数表(索引)->方法签名 = Read_ULI(fhandle)
        
        本类对象->函数表(索引)->静态方法         = Read_yesORno(fhandle)
        本类对象->函数表(索引)->方法名           = Read_Str(fhandle)
        本类对象->函数表(索引)->地址             = Read_LI(fhandle)
        本类对象->函数表(索引)->实例化方式       = Read_UB(fhandle)
        本类对象->函数表(索引)->返回值类型表达式 = Read_Str(fhandle)
        本类对象->函数表(索引)->形参类型表达式   = Read_Str(fhandle)
        
        yesORno = Read_yesORno(fhandle)
        本类对象->函数表(索引)->重载运算符 = yesORno
        If 本类对象->函数表(索引)->重载运算符 Then
            本类对象->函数表(索引)->运算符 = Read_UB(fhandle)
        End If
        
        yesORno = Read_yesORno(fhandle)
        本类对象->函数表(索引)->静态构造方法 = yesORno
        If 本类对象->函数表(索引)->静态构造方法 Then
            静态构造方法 = 本类对象->函数表(索引)
        End If
        
        yesORno = Read_yesORno(fhandle)
        本类对象->函数表(索引)->本地方法     = yesORno
        本类对象->函数表(索引)->本地方法实体 = @DefaultNative
        
        yesORno = Read_yesORno(fhandle)
        本类对象->函数表(索引)->构造方法 = yesORno
        
        yesORno = Read_yesORno(fhandle)
        本类对象->函数表(索引)->析构方法 = yesORno
        If 本类对象->函数表(索引)->析构方法 Then
            本类对象->有析构方法 = True
            本类对象->析构方法   = 本类对象->函数表(索引)
        End If
        
        本类对象->函数表(索引)->最大容器数量 = Read_L(fhandle)
        本类对象->函数表(索引)->指令数       = Read_L(fhandle)
        
        本类对象->函数表(索引)->类 = 本类对象
    Next
    
    成员数 = Read_L(fhandle)
    
    If 本类对象->超类 = False Then
        本类对象->成员偏移计数器 = 本类对象->基类->成员偏移计数器
    End If
    
    For i As Integer = 1 To 成员数
        Dim 索引 As Integer
        
        索引 = UBound(本类对象->成员表) + 1
        
        ReDim Preserve 本类对象->成员表(索引)
        本类对象->成员表(索引).名称       = Read_Str(fhandle)
        本类对象->成员表(索引).序号       = Read_L(fhandle)
        本类对象->成员表(索引).类型表达式 = Read_Str(fhandle)
        本类对象->成员表(索引).偏移       = 本类对象->成员偏移计数器
        本类对象->成员偏移计数器          = 本类对象->成员偏移计数器 + 1
    Next
    
    静态成员数 = Read_L(fhandle)
    
    For i As Integer = 1 To 静态成员数
        Dim 索引 As Integer
        
        索引 = UBound(本类对象->静态成员表) + 1
        
        ReDim Preserve 本类对象->静态成员表(索引)
        本类对象->静态成员表(索引).名称           = Read_Str(fhandle)
        本类对象->静态成员表(索引).类型表达式     = Read_Str(fhandle)
        本类对象->静态成员表(索引).容器           = New 容器信息
        本类对象->静态成员表(索引).容器->关键容器 = True
        容器池.Join(本类对象->静态成员表(索引).容器)
    Next
    
    For i As Integer = 1 To 指令数
        Dim 索引 As Integer
        索引 = UBound(本类对象->指令表) + 1
        ReDim Preserve 本类对象->指令表(索引)
        
        Dim 操作码 As UByte
        
        操作码 = Read_UB(fhandle)
        
        本类对象->指令表(索引).释放函数 = @free_Default
        
        本类对象->指令表(索引).操作码 = 操作码
        'Print "IS",操作码,本类.类名
        Select Case 操作码
            Case is_code_nop
                本类对象->指令表(索引).控制码 = control_code.label_is_nop
                
            Case is_code_push_box
                Dim 容器序号 As Long
                容器序号 = Read_L(fhandle)
                本类对象->指令表(索引).参数   = Cast(Integer, 容器序号)
                本类对象->指令表(索引).控制码 = control_code.label_is_push_box
            Case is_code_push_box_ref
                Dim 容器序号 As Long
                容器序号 = Read_L(fhandle)
                本类对象->指令表(索引).参数   = Cast(Integer, 容器序号)
                本类对象->指令表(索引).控制码 = control_code.label_is_push_box_ref
            Case is_code_push_method
                Dim 临时方法路径 As 方法路径
                Read_ClassMethod(fhandle, 临时方法路径)
                本类对象->指令表(索引).参数 = Find_ClassMethod(临时方法路径)
                If Cast(方法指向, 本类对象->指令表(索引).参数)->静态方法 Then
                    本类对象->指令表(索引).控制码 = control_code.label_is_push_method_static
                Else
                    本类对象->指令表(索引).控制码 = control_code.label_is_push_method_instance
                End If
            Case is_code_push_byte
                Dim 值 As Byte
                值 = Read_UB(fhandle)
                本类对象->指令表(索引).参数 = New Byte
                 *Cast(Byte Ptr, 本类对象->指令表(索引).参数) = 值
                本类对象->指令表(索引).控制码   = control_code.label_is_push_byte
                本类对象->指令表(索引).释放函数 = @free_is_push_byte
            Case is_code_push_short
                Dim 值 As Short
                值 = Read_S(fhandle)
                本类对象->指令表(索引).参数 = New Short
                 *Cast(Short Ptr, 本类对象->指令表(索引).参数) = 值
                本类对象->指令表(索引).控制码   = control_code.label_is_push_short
                本类对象->指令表(索引).释放函数 = @free_is_push_short
            Case is_code_push_int
                Dim 值 As Long
                值 = Read_L(fhandle)
                本类对象->指令表(索引).参数 = New Long
                 *Cast(Long Ptr, 本类对象->指令表(索引).参数) = 值
                本类对象->指令表(索引).控制码   = control_code.label_is_push_int
                本类对象->指令表(索引).释放函数 = @free_is_push_integer
            Case is_code_push_long
                Dim 值 As LongInt
                值 = Read_LI(fhandle)
                本类对象->指令表(索引).参数 = New LongInt
                 *Cast(LongInt Ptr, 本类对象->指令表(索引).参数) = 值
                本类对象->指令表(索引).控制码   = control_code.label_is_push_long
                本类对象->指令表(索引).释放函数 = @free_is_push_long
            Case is_code_push_float
                Dim 值 As Single
                值 = Read_SI(fhandle)
                本类对象->指令表(索引).参数 = New Single
                 *Cast(Single Ptr, 本类对象->指令表(索引).参数) = 值
                本类对象->指令表(索引).控制码   = control_code.label_is_push_float
                本类对象->指令表(索引).释放函数 = @free_is_push_float
            Case is_code_push_double
                Dim 值 As Double
                值 = Read_D(fhandle)
                本类对象->指令表(索引).参数 = New Double
                 *Cast(Double Ptr, 本类对象->指令表(索引).参数) = 值
                本类对象->指令表(索引).控制码   = control_code.label_is_push_double
                本类对象->指令表(索引).释放函数 = @free_is_push_double
            Case is_code_push_bool
                Dim 值 As UByte
                值 = Read_UB(fhandle)
                本类对象->指令表(索引).参数 = New BOOL
                 *Cast(BOOL Ptr, 本类对象->指令表(索引).参数) = IIf(值, -1, 0)
                本类对象->指令表(索引).控制码   = control_code.label_is_push_bool
                本类对象->指令表(索引).释放函数 = @free_is_push_bool
            Case is_code_push_string
                Read_UBs(fhandle, temp())
                本类对象->指令表(索引).参数 = Allocate(ARRAYSIZE(temp))
                memcpy(本类对象->指令表(索引).参数, @temp(0), ARRAYSIZE(temp))
                本类对象->指令表(索引).控制码   = control_code.label_is_push_string
                本类对象->指令表(索引).释放函数 = @free_is_push_string
                'Case is_code_push
                '   Dim 类型 As UByte
                '
                '   类型 = Read_UB(fhandle)
                '   Select Case 类型
                '      Case push_type.Push_压入容器
                '         Dim 容器序号 As Long
                '
                '         容器序号 = Read_L(fhandle)
                '         本类对象->指令表(索引).参数 = Cast(Integer, 容器序号)
                '         本类对象->指令表(索引).解释函数 = @is_push_local
                '      Case push_type.Push_压入容器引用
                '         Dim 容器序号 As Long
                '
                '         容器序号 = Read_L(fhandle)
                '         本类对象->指令表(索引).参数 = Cast(Integer, 容器序号)
                '         本类对象->指令表(索引).解释函数 = @is_push_local_refpass
                '      Case push_type.Push_压入方法
                '         Dim 临时方法路径 As 方法路径
                '
                '         Read_ClassMethod(fhandle, 临时方法路径)
                '
                '         本类对象->指令表(索引).参数 = Find_ClassMethod(临时方法路径)
                '         If Cast(方法指向, 本类对象->指令表(索引).参数)->静态方法 Then
                '            本类对象->指令表(索引).解释函数 = @is_push_method_static
                '         Else
                '            本类对象->指令表(索引).解释函数 = @is_push_method_instance
                '         End If
                '      Case push_type.push_压入字节
                '         Dim 值 As Byte
                '
                '         值 = Read_UB(fhandle)
                '
                '         本类对象->指令表(索引).参数 = New Byte
                '         *Cast(Byte Ptr, 本类对象->指令表(索引).参数) = 值
                '         本类对象->指令表(索引).解释函数 = @is_push_byte
                '         本类对象->指令表(索引).释放函数 = @free_is_push_byte
                '      Case push_type.push_压入短整数
                '         Dim 值 As Short
                '
                '         值 = Read_S(fhandle)
                '
                '         本类对象->指令表(索引).参数 = New short
                '         *Cast(short Ptr, 本类对象->指令表(索引).参数) = 值
                '         本类对象->指令表(索引).解释函数 = @is_push_short
                '         本类对象->指令表(索引).释放函数 = @free_is_push_short
                '      Case push_type.Push_压入整数
                '         Dim 值 As Long
                '
                '         值 = Read_L(fhandle)
                '
                '         本类对象->指令表(索引).参数 = New Long
                '         *Cast(Long Ptr, 本类对象->指令表(索引).参数) = 值
                '         本类对象->指令表(索引).解释函数 = @is_push_integer
                '         本类对象->指令表(索引).释放函数 = @free_is_push_integer
                '      Case push_type.push_压入长整数
                '         Dim 值 As LongInt
                '
                '         值 = Read_LI(fhandle)
                '
                '         本类对象->指令表(索引).参数 = New LongInt
                '         *Cast(LongInt Ptr, 本类对象->指令表(索引).参数) = 值
                '         本类对象->指令表(索引).解释函数 = @is_push_long
                '         本类对象->指令表(索引).释放函数 = @free_is_push_long
                '      Case push_type.push_压入小数
                '         Dim 值 As Single
                '
                '         值 = Read_SI(fhandle)
                '
                '         本类对象->指令表(索引).参数 = New Single
                '         *Cast(Single Ptr, 本类对象->指令表(索引).参数) = 值
                '         本类对象->指令表(索引).解释函数 = @is_push_float
                '         本类对象->指令表(索引).释放函数 = @free_is_push_float
                '      Case push_type.push_压入双精度小数
                '         Dim 值 As Double
                '
                '         值 = Read_D(fhandle)
                '
                '         本类对象->指令表(索引).参数 = New Double
                '         *Cast(Double Ptr, 本类对象->指令表(索引).参数) = 值
                '         本类对象->指令表(索引).解释函数 = @is_push_double
                '         本类对象->指令表(索引).释放函数 = @free_is_push_double
                '      Case push_type.push_压入逻辑
                '         Dim 值 As UByte
                '
                '         值 = Read_UB(fhandle)
                '         本类对象->指令表(索引).参数 = New BOOL
                '         *Cast(BOOL Ptr, 本类对象->指令表(索引).参数) = IIf(值, -1, 0)
                '         本类对象->指令表(索引).解释函数 = @is_push_bool
                '         本类对象->指令表(索引).释放函数 = @free_is_push_bool
                '      Case push_type.Push_压入文本
                '         Read_UBs(fhandle, temp())
                '         本类对象->指令表(索引).参数 = Allocate(ARRAYSIZE(temp))
                '         memcpy(本类对象->指令表(索引).参数, @temp(0), ARRAYSIZE(temp))
                '
                '         本类对象->指令表(索引).解释函数 = @is_push_string
                '         本类对象->指令表(索引).释放函数 = @free_is_push_string
                '
                '   End Select
            Case is_code_push_global
                Dim 临时成员路径 As 成员路径
                Read_ClassMember(fhandle, 临时成员路径)
                
                本类对象->指令表(索引).参数   = Find_ClassStatics(临时成员路径)
                本类对象->指令表(索引).控制码 = control_code.label_is_push_global
            Case is_code_popn
                本类对象->指令表(索引).控制码 = control_code.label_is_popn
            Case is_code_new
                Dim 临时类路径   As 类路径
                Dim 临时类指向   As 类指向
                Dim 临时方法路径 As 方法路径
                Dim 临时类信息   As 类信息 Ptr
                Read_Class(fhandle, 临时类路径)
                
                临时类指向 = Find_Class(临时类路径)
                临时类信息 = 临时类指向
                
                Read_ClassMethod(fhandle, 临时方法路径)
                
                Dim FindOk As BOOL
                
                FindOk = False
                For j As Integer = LBound(临时类信息->函数表) To UBound(临时类信息->函数表)
                    If 临时类信息->函数表(j)->方法签名 = 临时方法路径.方法签名 Then
                        本类对象->指令表(索引).参数 = 临时类信息->函数表(j)
                        FindOk = True
                        Exit For
                    End If
                Next
                
                If FindOk = False Then
                    ErrorBox("不能找到指定构造函数")
                End If
                
                本类对象->指令表(索引).控制码 = control_code.label_is_new
            Case is_code_next
                Dim 临时成员路径 As 成员路径
                Read_ClassMember(fhandle, 临时成员路径)
                
                本类对象->指令表(索引).参数   = Find_ClassMember(临时成员路径)->偏移
                本类对象->指令表(索引).控制码 = control_code.label_is_next
            Case is_code_change
                本类对象->指令表(索引).控制码 = control_code.label_is_change
            Case is_code_create
                Dim 容器序号 As Long
                容器序号 = Read_L(fhandle)
                本类对象->指令表(索引).参数   = Cast(Integer, 容器序号)
                本类对象->指令表(索引).控制码 = control_code.label_is_create
            Case is_code_close
                Dim 容器序号 As Long
                容器序号 = Read_L(fhandle)
                本类对象->指令表(索引).参数   = Cast(Integer, 容器序号)
                本类对象->指令表(索引).控制码 = control_code.label_is_close
            Case is_code_return
                本类对象->指令表(索引).控制码 = control_code.label_is_return
            Case is_code_call_path
                Dim 临时方法路径 As 方法路径
                Dim 临时方法指向 As 方法指向
                
                Read_ClassMethod(fhandle, 临时方法路径)
                
                临时方法指向 = Find_ClassMethod(临时方法路径)
                
                If 临时方法指向->本地方法 Then
                    本类对象->指令表(索引).参数   = @临时方法指向->本地方法实体
                    本类对象->指令表(索引).控制码 = control_code.label_is_call_path_native
                Else
                    本类对象->指令表(索引).参数   = 临时方法指向
                    本类对象->指令表(索引).控制码 = control_code.label_is_call_path
                End If
                
            Case is_code_call_virtual
                本类对象->指令表(索引).参数 = New 调用虚方法信息
                Cast(调用虚方法信息 Ptr, 本类对象->指令表(索引).参数)->虚方法实例索引 = Read_L(fhandle)
                Cast(调用虚方法信息 Ptr, 本类对象->指令表(索引).参数)->方法签名       = Read_ULI(fhandle)
                本类对象->指令表(索引).控制码   = control_code.label_is_call_virtual
                本类对象->指令表(索引).释放函数 = @free_is_call_virtual
            Case is_code_jmp
                本类对象->指令表(索引).参数 = New LongInt
                 *Cast(LongInt Ptr, 本类对象->指令表(索引).参数) = Read_LI(fhandle)
                本类对象->指令表(索引).控制码   = control_code.label_is_jmp
                本类对象->指令表(索引).释放函数 = @free_is_jmp
            Case is_code_locate
                本类对象->指令表(索引).控制码 = control_code.label_is_locate
                
                
            Case is_code_new_array
                本类对象->指令表(索引).控制码 = control_code.label_is_new_array
            Case is_code_set
                本类对象->指令表(索引).参数   = Cast(Integer, Read_L(fhandle))
                本类对象->指令表(索引).控制码 = control_code.label_is_set
            Case is_code_ref
                本类对象->指令表(索引).控制码 = control_code.label_is_ref
            Case is_code_call_callback_set
                本类对象->指令表(索引).控制码 = control_code.label_is_call_callback_set
            Case is_code_call_callback
                本类对象->指令表(索引).控制码 = control_code.label_is_call_callback
            Case is_code_print
                本类对象->指令表(索引).控制码 = control_code.label_is_print
            Case is_code_load
                Read_UBs(fhandle, temp())
                本类对象->指令表(索引).参数 = Allocate(ARRAYSIZE(temp))
                memcpy(本类对象->指令表(索引).参数, @temp(0), ARRAYSIZE(temp))
                
                本类对象->指令表(索引).控制码   = control_code.label_is_load
                本类对象->指令表(索引).释放函数 = @free_is_load
                
                
            Case is_code_assign_box_box_ref
                本类对象->指令表(索引).控制码 = control_code.label_is_assign_box_box_ref
                
            Case is_code_assign_box_obj
                本类对象->指令表(索引).控制码 = control_code.label_is_assign_box_obj
                
                
            Case is_code_pop_box_box_ref
                本类对象->指令表(索引).控制码 = control_code.label_is_pop_box_box_ref
                
            Case is_code_pop_box_obj
                本类对象->指令表(索引).控制码 = control_code.label_is_pop_box_obj
                
                
            Case is_code_jz
                本类对象->指令表(索引).参数 = New LongInt
                 *Cast(LongInt Ptr, 本类对象->指令表(索引).参数) = Read_LI(fhandle)
                本类对象->指令表(索引).控制码   = control_code.label_is_jz
                本类对象->指令表(索引).释放函数 = @free_is_jz
                
            Case is_code_size
                本类对象->指令表(索引).控制码 = control_code.label_is_size
                
            Case is_code_clear
                本类对象->指令表(索引).控制码 = control_code.label_is_clear
            Case is_code_state
                本类对象->指令表(索引).控制码 = control_code.label_is_state
            Case is_code_type
                Dim 临时类路径 As 类路径
                Dim 临时类指向 As 类指向
                Read_Class(fhandle, 临时类路径)
                
                临时类指向 = Find_Class(临时类路径)
                本类对象->指令表(索引).参数   = 临时类指向
                本类对象->指令表(索引).控制码 = control_code.label_is_type
            Case is_code_lock
                本类对象->指令表(索引).控制码 = control_code.label_is_lock
            Case is_code_unlock
                本类对象->指令表(索引).控制码 = control_code.label_is_unlock
            Case Else
                
        End Select
    Next
    
    本类对象->成员深度 = 取成员深度(本类对象)
    
    If 静态构造方法 <> -1 Then
        Dim 索引 As integer
        索引 = UBound(静态构造方法表) + 1
        ReDim Preserve 静态构造方法表(索引)
        
        静态构造方法表(索引) = 静态构造方法
    End If
End Sub

Function Create_Namespace(命名空间路径() As String) As 命名空间信息 Ptr
   Dim this_space As 命名空间信息 Ptr
   
   If 命名空间路径(0) = "ROOT" Then
      this_space = @命名空间
      
      For i As Integer = 1 To UBound(命名空间路径)
         Dim FindOk As BOOL
         FindOk = False
         For j As Integer = 0 To UBound(this_space->子空间)
            if this_space->子空间(j)->名称 = 命名空间路径(i) then
               this_space = this_space->子空间(j)
               FindOk = True
               Exit For
            End If
         Next
         
         If FindOk = False Then
            Dim 索引 as integer
            
            索引 = UBound(this_space->子空间) + 1
            ReDim Preserve this_space->子空间(索引)
            
            this_space->子空间(索引) = New 命名空间信息
            this_space->子空间(索引)->名称 = 命名空间路径(i)
            this_space->子空间(索引)->父空间 = this_space
            this_space = this_space->子空间(索引)
         End If
      Next
   Else
      ErrorBox("命名空间不是以ROOT开始")
   End If
   Function = this_space
End Function

Function Create_Class(临时类路径 As 类路径) As 类信息 Ptr
   Dim this_space As 命名空间信息 Ptr
   this_space = Create_Namespace(临时类路径.命名空间路径())
   
   Dim 索引 As Integer
   索引 = UBound(this_space->类表) + 1
   ReDim Preserve this_space->类表(索引)
   this_space->类表(索引) = New 类信息
   
   this_space->类表(索引)->名称 = 临时类路径.类名
   this_space->类表(索引)->命名空间 = this_space
   Function = this_space->类表(索引)
End Function

'尝试模式会尝试寻找这个类并加载
Function Find_Class(临时类路径 As 类路径, 尝试模式 As BOOL = True) As 类信息 Ptr
   Dim this_space As 命名空间信息 Ptr
   Dim this_class As 类信息 Ptr
   Dim FindOk As BOOL
   this_space = Create_Namespace(临时类路径.命名空间路径())
   FindOk = FALSE
   For i As Integer = LBound(this_space->类表) To UBound(this_space->类表)
      If this_space->类表(i)->名称 = 临时类路径.类名 Then
         this_class= this_space->类表(i)
         FindOk = True
         Exit For
      End If
   Next
   If FindOk = FALSE Then
      'Print "Error ";Dump_类路径(临时类路径)
      If 尝试模式 Then
         引入类(临时类路径)
         Return Find_Class(临时类路径, False)
      Else
         ErrorBox("不能找到指定的类"+":"+临时类路径.类名)
      End If
   End If

   Return this_class
End Function



Function Find_ClassStatics(临时成员路径 As 成员路径) As 容器信息 Ptr
   Dim 临时类 As 类信息 Ptr
   临时类 = Find_Class(临时成员路径.类)
   
   For i As Integer = LBound(临时类->静态成员表) To UBound(临时类->静态成员表)
      If 临时类->静态成员表(i).名称 = 临时成员路径.成员名 Then
         Return 临时类->静态成员表(i).容器
      End If
   Next
   ErrorBox("不能找到指定的类静态成员")
   Return 0
End Function

Function Find_ClassMember(临时成员路径 As 成员路径) As 成员信息 Ptr
   Dim 临时类 As 类信息 Ptr
   临时类 = Find_Class(临时成员路径.类)
   
   For i As Integer = LBound(临时类->成员表) To UBound(临时类->成员表)
      If 临时类->成员表(i).名称 = 临时成员路径.成员名 Then
         Return @临时类->成员表(i)
      End If
   Next
   ErrorBox("不能找到指定的类静态成员")
   Return 0
End Function

Function Find_ClassMethod(临时方法路径 As 方法路径) As 函数信息 Ptr
   Dim 临时类信息 As 类信息 Ptr
   临时类信息 = Find_Class(临时方法路径.类)
   
   Dim result As 函数信息 Ptr
   
   Dim FindOk As BOOL
   FindOk = False
   
   For i As Integer = LBound(临时类信息->函数表) To UBound(临时类信息->函数表)
      If 临时类信息->函数表(i)->方法签名 = 临时方法路径.方法签名 Then
         result = 临时类信息->函数表(i)
         FindOk = True
         Exit For
      End If
   Next
   
   Function = result
End Function
