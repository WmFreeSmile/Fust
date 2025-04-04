
Namespace 虚拟机


const 最大栈尺寸 as integer = 8196



Sub 内存复制(目标 As Any Ptr, 源 As Any Ptr, 长度 As Integer)
    memcpy(目标, 源, 长度)
End Sub



Type 别名_命名空间信息 As 命名空间信息
Type 别名_类信息 As 类信息
Type 别名_方法信息 As 方法信息

Type 别名_静态变量信息 As 静态变量信息


MMapTemplate(ULongInt, Integer)


Enum 错误枚举
    
    找不到指定类
    引入父类失败
    未找到静态变量
    未找到方法
End Enum

Enum 操作码枚举
   空操作
   如果
   返回
   加载参数
   丢弃
   读取
   静态读取
   调用
   新建
   保存
   静态保存
   成员保存
   成员读取
   加载整数
   加载文本
   加载逻辑
   创建变量
   销毁变量
End Enum

Enum 字节码枚举
   如果
   返回
   加载参数
   丢弃
   读取
   静态读取
   调用
   保存
   静态保存
   成员保存
   成员读取
   加载整数
   加载文本
   加载逻辑
   创建变量
   销毁变量
End Enum


Dim 最后一次错误 As 错误枚举
Dim 界目录表() As String
Dim 类文件路径表() As String

Dim 根命名空间 As 命名空间信息 Ptr

Dim 根引用() As 对象信息 Ptr

Dim 入口方法表() As 方法信息 Ptr

Dim 常量表() As 常量信息 Ptr

Sub 加入界目录(界目录 As String)
    Dim 索引 As Integer = UBound(界目录表) + 1
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

Type 静态成员路径
    类 As 类路径
    成员名 As String
End Type

Type 方法路径
    类 As 类路径
    方法签名摘要 As ULongInt
End Type


Type 对象信息
    成员表 As 对象信息 Ptr Ptr
    虚方法表 As TMAPULONGINTINTEGER Ptr
End Type

Type 方法实体 Extends Object
    Declare Virtual Sub 调用(方法 As 别名_方法信息 Ptr,栈指针 As 对象信息 Ptr Ptr, 参数表 As 对象信息 Ptr Ptr)
End Type

Sub 方法实体.调用(方法 As 别名_方法信息 Ptr, 栈指针 As 对象信息 Ptr Ptr, 参数表 As 对象信息 Ptr Ptr)
    Print "调用空方法:";方法->名称
End Sub

Type 常量信息 Extends Object
    对象 As 对象信息 Ptr
End Type

Type 常量信息_整数 Extends 常量信息
    值 As Long
End Type

Type 常量信息_文本 Extends 常量信息
    值 As String
End Type

Type 常量信息_逻辑 Extends 常量信息
    值 As BOOL
End Type

Type 指令信息
    操作码 As UByte
End Type

Type 指令信息_如果 Extends 指令信息
    真目标 As Long
    假目标 As Long
End Type

Type 指令信息_加载参数 Extends 指令信息
    序列 As Long
End Type

Type 指令信息_读取 Extends 指令信息
    序列 As Long
End Type

Type 指令信息_静态读取 Extends 指令信息
    引用 As 别名_静态变量信息 Ptr
End Type

Type 指令信息_调用 Extends 指令信息
    引用 As 别名_方法信息 Ptr
End Type

Type 指令信息_新建 Extends 指令信息
    方法 As 别名_方法信息 Ptr
    成员表尺寸 As Integer
End Type


Type 指令信息_保存 Extends 指令信息
    序列 As Long
End Type

Type 指令信息_静态保存 Extends 指令信息
    引用 As 别名_静态变量信息 Ptr
End Type

Type 指令信息_成员保存 Extends 指令信息
    实例定位序列 As Long
End Type

Type 指令信息_成员读取 Extends 指令信息
    实例定位序列 As Long
End Type

Type 指令信息_加载整数 Extends 指令信息
    值 As 常量信息 Ptr
End Type

Type 指令信息_加载文本 extends 指令信息
    值 As 常量信息 Ptr
End Type

Type 指令信息_加载逻辑 Extends 指令信息
    值 As 常量信息 Ptr
End Type

Type 指令信息_创建变量 Extends 指令信息
    序列 As Long
End Type

Type 指令信息_销毁变量 Extends 指令信息
    序列 As Long
End Type


Type 方法实体_字节码 Extends 方法实体
    指令表(Any) As 指令信息 Ptr
    
    Declare Sub 调用(方法 As 别名_方法信息 Ptr, 栈指针 As 对象信息 Ptr Ptr, 参数表 As 对象信息 Ptr Ptr) Override
End Type



Function 创建对象(方法 As 方法信息 Ptr, 栈指针 As 对象信息 Ptr Ptr, 成员表尺寸 As Integer) As 对象信息 Ptr
    Dim result As 对象信息 Ptr = New 对象信息
    
    result->成员表 = Allocate(SizeOf(对象信息 Ptr) *成员表尺寸)
    result->虚方法表 = New TMAPULONGINTINTEGER
    
    Dim 参数表 As 对象信息 Ptr Ptr = Allocate(SizeOf(对象信息 Ptr) * (ARRAYSIZE(方法->参数表) + 1))
    *参数表 = result
    
    内存复制(*参数表 + 1, *栈指针 + ARRAYSIZE(方法->参数表), SizeOf(对象信息 Ptr) *ARRAYSIZE(方法->参数表))
    *栈指针 = *栈指针 - ARRAYSIZE(方法->参数表)
    
    方法->实体->调用(方法, 栈指针, 参数表)
    Deallocate(参数表)
    
    Function = result
End Function


Sub 方法实体_字节码.调用(方法 As 别名_方法信息 Ptr, 栈指针 As 对象信息 Ptr Ptr, 参数表 As 对象信息 Ptr Ptr)
    Dim 指令指针 As 指令信息 Ptr Ptr = @This.指令表(0)
    
    Goto start
    
    指令_如果:_
    
    '( *栈指针 -0)->
    
    *栈指针 = *栈指针 + 2
    
    Goto skip
    
    指令_返回:_
    Goto quit
    Goto skip
    
    
    指令_加载参数 : _
    
    Goto skip
    
    指令_丢弃 : _
    
    Goto skip
    
    指令_读取 : _
    
    Goto skip
    
    
    指令_静态读取 : _
    
    Goto skip
    
    
    指令_调用 : _
    
    Goto skip
    
    指令_新建:_
    创建对象(Cast(指令信息_新建 Ptr, *指令指针)->方法, 栈指针, Cast(指令信息_新建 Ptr, *指令指针)->成员表尺寸)
    
    Goto skip
    
    指令_保存 : _
    
    Goto skip
    
    指令_静态保存 : _
    
    Goto skip
    
    指令_成员保存 : _
    
    Goto skip
    
    指令_成员读取 : _
    
    Goto skip
    
    指令_加载整数 : _
    
    Goto skip
    
    指令_加载文本 : _
    
    Goto skip
    
    指令_加载逻辑 : _
    
    Goto skip
    
    指令_创建变量 : _
    
    Goto skip
    
    指令_销毁变量 : _
    
    Goto skip
    
    
    
    skip : _
    指令指针 = 指令指针 + 1
    
    start : _
    Print "code", ( *指令指针)->操作码
    On (*指令指针)->操作码 Goto _
    指令_如果, _
    指令_返回,_
    指令_加载参数,_
    指令_丢弃,_
    指令_读取,_
    指令_静态读取,_
    指令_调用, _
    指令_新建,_
    指令_保存,_
    指令_静态保存,_
    指令_成员保存,_
    指令_成员读取,_
    指令_加载整数,_
    指令_加载文本,_
    指令_加载逻辑,_
    指令_创建变量, _
    指令_销毁变量
    
    Goto skip
    
    quit : _
    
End Sub

Enum 方法类型
    实例方法
    静态方法
    构造方法
    析构方法
End Enum


Enum 运算符类型
   赋值
   
   等于
   不等于
   大于等于
   小于等于
   大于
   小于
   无符号大于等于
   无符号小于等于
   无符号大于
   无符号小于
   
   加
   减
   
   乘
   除
   余
   幂
   整除
   无符号乘
   无符号除
   无符号余
   无符号整除
   
   与
   或
   左移
   右移
   异或
   End Enum
   
Type 参数信息
    名称 As String
    类型 As String
End Type

Type 方法信息
    绑定类 As 别名_类信息 Ptr
    
    是否本地 As BOOL
    
    类型 As 方法类型
    
    是否函数 As BOOL
    返回值 As String
    
    是否运算符 As BOOL
    运算符 As 运算符类型
    
    名称 As String
    参数表(Any) As 参数信息
    
    方法签名摘要 As ULongInt
    
    是否入口 As BOOL
    
    实体 As 方法实体 Ptr
End Type

Type 变量信息
    名称 As String
    类型 As String
End Type

Type 成员变量信息 Extends 变量信息
    绑定类 As 别名_类信息 Ptr
End Type

Type 静态变量信息 Extends 变量信息
    绑定类 As 别名_类信息 Ptr
    
    引用 As 对象信息 Ptr
End Type

Type 类信息
    绑定命名空间 As 别名_命名空间信息 Ptr
    
    父类 As 类信息 Ptr
    
    名称 As String
    方法表(Any) As 方法信息 Ptr
    成员表(Any) As 成员变量信息 Ptr
    静态变量表(Any) As 静态变量信息 Ptr
End Type


Type 命名空间信息
    上一级 As 命名空间信息 Ptr
    
    名称 As String
    类表(Any) As 类信息 Ptr
    命名空间表(Any) As 命名空间信息 Ptr
End Type

Sub Dump_命名空间路径(命名空间 As 命名空间路径)
    For i As Integer = LBound(命名空间.路径) To UBound(命名空间.路径)
        Print 命名空间.路径(i);
        If i <> UBound(命名空间.路径) Then
            Print "::";
        End If
    Next
    Print ""
End Sub

Sub Dump_类路径(类 As 类路径)
    For i As Integer = LBound(类.命名空间.路径) To UBound(类.命名空间.路径)
        Print 类.命名空间.路径(i);"::";
    Next
    Print 类.类名
End Sub

Sub Dump_方法路径(方法 As 方法路径)
    For i As Integer = LBound(方法.类.命名空间.路径) To UBound(方法.类.命名空间.路径)
        Print 方法.类.命名空间.路径(i);"::";
    Next
    Print 方法.类.类名;"::";方法.方法签名摘要
End Sub

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

Function 文本转方法路径(文本 As String) As 方法路径
    Dim result As 方法路径
    
    Dim 文本组() As String
    vbSplit(文本, "::", 文本组())
    ReDim result.类.命名空间.路径(UBound(文本组) -2)
    For i As Integer = LBound(文本组) To UBound(文本组) -2
        result.类.命名空间.路径(i) = 文本组(i)
    Next
    result.类.类名 = 文本组(UBound(文本组) -1)
    result.方法签名摘要 = ValULng(文本组(UBound(文本组)))
    
    Function = result
End Function

Function 读字节(f As Integer) As UByte
    Dim result As UByte
    Get #f,, result
    Function = result
End Function
Function 读整数(f As Integer) As Long
    Dim result As Long
    Get #f,, result
    Function = result
End Function

Function 读长整数(f As Integer) As LongInt
    Dim result As LongInt
    Get #f,, result
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

Sub 读静态成员路径(f As Integer, ByRef 静态成员 As 静态成员路径)
    读类路径(f, 静态成员.类)
    静态成员.成员名 = 读文本(f)
End Sub

Sub 读方法路径(f As Integer, ByRef 方法 As 方法路径)
    读类路径(f, 方法.类)
    方法.方法签名摘要 = 读长整数(f)
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


Function 寻找类文件(类 As 类路径, ByRef 返回值 As String="") As BOOL
    For i As Integer = LBound(类文件路径表) To UBound(类文件路径表)
        If 类路径是否相同(取类路径(类文件路径表(i)),类) Then
            返回值 = 类文件路径表(i)
            Return True
        End If
    Next
    
    最后一次错误 = 错误枚举.找不到指定类
    Function = False
End Function

Function 引入类(类 As 类路径, ByRef 返回值 As 类信息 Ptr = 0) As BOOL
    If 寻找类(类, 返回值) Then Return True
    
    Dim 类文件路径 As String
    If 寻找类文件(类, 类文件路径) = False Then 
        Return False
    End If
    Function = 加载类(类文件路径,返回值)
End Function

Function 架构命名空间(命名空间 As 命名空间路径) As 命名空间信息 Ptr
    Dim 节点索引 As 命名空间信息 Ptr = 根命名空间
    Dim 临时节点 As 命名空间信息 Ptr
    For i As Integer = LBound(命名空间.路径) To UBound(命名空间.路径)
        临时节点 = 0
        For j As Integer = LBound(节点索引->命名空间表) To UBound(节点索引->命名空间表)
            If 节点索引->命名空间表(j)->名称 = 命名空间.路径(i) Then
                临时节点 = 节点索引->命名空间表(j)
                Exit For
            End If
        Next
        If 临时节点 = 0 Then
            临时节点 = New 命名空间信息
            Dim 索引 As Integer
            索引 = UBound(节点索引->命名空间表) + 1
            ReDim Preserve 节点索引->命名空间表(索引)
            节点索引->命名空间表(索引) = 临时节点
            
            临时节点->名称 = 命名空间.路径(i)
        End If
        节点索引 = 临时节点
    Next
    Function = 临时节点
End Function

Function 寻找命名空间(命名空间 As 命名空间路径, ByRef 返回值 As 命名空间信息 Ptr = 0) As BOOL
    Dim 节点索引 As 命名空间信息 Ptr = 根命名空间
    Dim 临时节点 As 命名空间信息 Ptr
    For i As Integer = LBound(命名空间.路径) To UBound(命名空间.路径)
        临时节点 = 0
        For j As Integer = LBound(节点索引->命名空间表) To UBound(节点索引->命名空间表)
            If 节点索引->命名空间表(j)->名称 = 命名空间.路径(i) Then
                临时节点 = 节点索引->命名空间表(j)
                Exit For
            End If
        Next
        If 临时节点 = 0 Then
            Return False
        End If
        节点索引 = 临时节点
    Next
    返回值 = 临时节点
    Function = True
End Function

Function 寻找类(类 As 类路径,ByRef 返回值 As 类信息 Ptr=0) As BOOL
    Dim 命名空间 As 命名空间信息 Ptr
    If 寻找命名空间(类.命名空间, 命名空间) = False Then Return False
    
    For i As Integer = LBound(命名空间->类表) To UBound(命名空间->类表)
        If 命名空间->类表(i)->名称 = 类.类名 Then
            返回值 = 命名空间->类表(i) : Return True
        End If
    Next
    Function = False
End Function

Function 寻找静态变量(静态成员 As 静态成员路径,ByRef 返回值 As 静态变量信息 Ptr=0) As BOOL
    Dim 类 As 类信息 Ptr
    If 寻找类(静态成员.类, 类) = False Then Return False
    For i As Integer = LBound(类->静态变量表) To UBound(类->静态变量表)
        If 类->静态变量表(i)->名称 = 静态成员.成员名 Then
            返回值 = 类->静态变量表(i) : Return True
        End If
    Next
    Function = False
End Function

Function 引入静态变量(静态成员 As 静态成员路径,ByRef 返回值 As 静态变量信息 Ptr=0) As BOOL
    Dim 类 As 类信息 Ptr
    If 引入类(静态成员.类,类) = False Then Return False
    For i As Integer = LBound(类->静态变量表) To UBound(类->静态变量表)
        If 类->静态变量表(i)->名称 = 静态成员.成员名 Then
            返回值 = 类->静态变量表(i) : Return True
        End If
    Next
    Function = False
End Function



Function 寻找方法(方法 As 方法路径,ByRef 返回值 As 方法信息 Ptr=0) As BOOL
    Dim 类 As 类信息 Ptr
    If 寻找类(方法.类, 类) = False Then Return False
    For i As Integer = LBound(类->方法表) To UBound(类->方法表)
        If 类->方法表(i)->方法签名摘要 = 方法.方法签名摘要 Then
            返回值 = 类->方法表(i) : Return True
        End If
    Next
    Function = False
End Function


Function 引入方法(方法 As 方法路径,ByRef 返回值 As 方法信息 Ptr=0) As BOOL
    Dim 类 As 类信息 Ptr
    If 引入类(方法.类, 类) = False Then Return False
    For i As Integer = LBound(类->方法表) To UBound(类->方法表)
        If 类->方法表(i)->方法签名摘要 = 方法.方法签名摘要 Then
            返回值 = 类->方法表(i) : Return True
        End If
    Next
    Function = False
End Function

Function 加载类(类文件路径 As String, ByRef 返回值 As 类信息 Ptr = 0) As BOOL
    Dim fhandle As Integer = FreeFile
    Open 类文件路径 For Binary As #fhandle
    
    Print "load",类文件路径
    
    Dim 长度 As Long
    
    Dim 是否超类 As BOOL
    
    Dim 本类路径 As 类路径
    Dim 父类路径 As 类路径
    
    Dim 本类对象 As 类信息 Ptr
    Dim 父类对象 As 类信息 Ptr
    
    读类路径(fhandle, 本类路径)
    
    If 寻找类(本类路径, 返回值) Then Return True
    
    是否超类 = (读字节(fhandle) = 0)
    If 是否超类 = False Then
        读类路径(fhandle, 父类路径)
        If 引入类(父类路径,父类对象) = False Then
            最后一次错误 = 错误枚举.引入父类失败
            Return False
        End If
    End If
    
    本类对象 = New 类信息
    本类对象->绑定命名空间 = 架构命名空间(本类路径.命名空间)
    本类对象->父类 = 父类对象
    本类对象->名称 = 本类路径.类名
    
    Dim 索引 As Integer
    
    索引 = UBound(本类对象->绑定命名空间->类表) + 1
    ReDim Preserve 本类对象->绑定命名空间->类表(索引)
    本类对象->绑定命名空间->类表(索引) = 本类对象
    
    
    长度 = 读整数(fhandle)
    For i As Integer = 1 To 长度
        Dim 变量引用 As 成员变量信息 Ptr = New 成员变量信息
        
        索引 = UBound(本类对象->成员表) + 1
        ReDim Preserve 本类对象->成员表(索引)
        本类对象->成员表(索引) = 变量引用
        
        变量引用->绑定类 = 本类对象
        
        变量引用->名称 = 读文本(fhandle)
        变量引用->类型 = 读文本(fhandle)
    Next
    
    
    长度 = 读整数(fhandle)
    For i As Integer = 1 To 长度
        Dim 变量引用 As 静态变量信息 Ptr = New 静态变量信息
        
        索引 = UBound(本类对象->静态变量表) + 1
        ReDim Preserve 本类对象->静态变量表(索引)
        本类对象->静态变量表(索引) = 变量引用
        变量引用->绑定类 = 本类对象
        
        变量引用->名称 = 读文本(fhandle)
        变量引用->类型 = 读文本(fhandle)
    Next
    
    长度 = 读整数(fhandle)
    For i As Integer = 1 To 长度
        Dim 方法引用 As 方法信息 Ptr = New 方法信息
        Dim 长度 As Long
        
        索引 = UBound(本类对象->方法表) + 1
        ReDim Preserve 本类对象->方法表(索引)
        本类对象->方法表(索引) = 方法引用
        
        
        方法引用->绑定类 = 本类对象
        
        方法引用->是否本地 = (读字节(fhandle) = 1)
        方法引用->类型 = 读整数(fhandle)
        方法引用->是否函数 = (读字节(fhandle) = 1)
        
        If 方法引用->是否函数 Then
            方法引用->返回值 = 读文本(fhandle)
        End If
        
        方法引用->是否运算符 = (读字节(fhandle) = 1)
        If 方法引用->是否运算符 Then
            方法引用->运算符 = 读整数(fhandle)
        End If
        
        方法引用->是否入口 = (读字节(fhandle) = 1)
        If 方法引用->是否入口 Then
            Dim 索引 As Integer
            索引 = UBound(入口方法表) + 1
            ReDim Preserve 入口方法表(索引)
            入口方法表(索引) = 方法引用
        End If
        
        方法引用->名称 = 读文本(fhandle)
        
        长度 = 读整数(fhandle)
        ReDim 方法引用->参数表(长度 -1)
        For j As Integer = 0 To 长度-1
            方法引用->参数表(j).名称 = 读文本(fhandle)
            方法引用->参数表(j).类型 = 读文本(fhandle)
        Next
        方法引用->方法签名摘要 = 读长整数(fhandle)
        
        If 方法引用->是否本地 = False Then
            Dim 长度 As Long
            Dim 索引 As Integer
            
            Dim 实体 As 方法实体_字节码 Ptr = New 方法实体_字节码
            
            方法引用->实体 = 实体
            
            长度 = 读整数(fhandle)
            
            ReDim 实体->指令表(长度-1)
            
            For j As Integer = 0 To 长度-1
                Dim 字节码 As UByte = 读字节(fhandle)
                Dim 指令 As 指令信息 Ptr
                
                Select Case 字节码
                    Case 字节码枚举.如果
                        Dim 真目标 As Long = 读整数(fhandle)
                        Dim 假目标 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_如果
                        指令->操作码 = 操作码枚举.如果
                        Cast(指令信息_如果 Ptr, 指令)->真目标 = 真目标
                        Cast(指令信息_如果 Ptr, 指令)->假目标 = 假目标
                    Case 字节码枚举.返回
                        指令 = New 指令信息
                        指令->操作码 = 操作码枚举.返回
                        
                    Case 字节码枚举.加载参数
                        Dim 序列 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_加载参数
                        指令->操作码 = 操作码枚举.加载参数
                        Cast(指令信息_加载参数 Ptr, 指令)->序列 = 序列
                    Case 字节码枚举.丢弃
                        指令 = New 指令信息
                        指令->操作码 = 操作码枚举.丢弃
                    Case 字节码枚举.读取
                        Dim 序列 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_读取
                        指令->操作码 = 操作码枚举.读取
                        Cast(指令信息_读取 Ptr, 指令)->序列 = 序列
                    Case 字节码枚举.静态读取
                        Dim 静态成员 As 静态成员路径
                        读静态成员路径(fhandle, 静态成员)
                        
                        指令 = New 指令信息_静态读取
                        指令->操作码 = 操作码枚举.静态读取
                        If 引入静态变量(静态成员, Cast(指令信息_静态读取 Ptr, 指令)->引用) = False Then 
                            最后一次错误 = 错误枚举.未找到静态变量
                            Return False
                        End If
                    Case 字节码枚举.调用
                        Dim 方法 As 方法路径
                        Dim 方法引用 As 方法信息 Ptr
                        读方法路径(fhandle, 方法)
                        If 引入方法(方法, 方法引用) = False Then 
                            最后一次错误 = 错误枚举.未找到方法
                            Return False
                        End If
                        If 方法引用->类型 = 方法类型.构造方法 Then
                            指令 = New 指令信息_新建
                            指令->操作码 = 操作码枚举.新建
                            Cast(指令信息_新建 Ptr, 指令)->方法 = 方法引用
                            Cast(指令信息_新建 Ptr, 指令)->成员表尺寸 = ARRAYSIZE(方法引用->绑定类->成员表)
                        Else
                            指令 = New 指令信息_调用
                            指令->操作码 = 操作码枚举.调用
                            Cast(指令信息_调用 Ptr, 指令)->引用 = 方法引用
                        End If
                        
                    Case 字节码枚举.保存
                        Dim 序列 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_保存
                        指令->操作码 = 操作码枚举.保存
                        Cast(指令信息_保存 Ptr, 指令)->序列 = 序列
                    Case 字节码枚举.静态保存
                        Dim 静态成员 As 静态成员路径
                        读静态成员路径(fhandle, 静态成员)
                        
                        指令 = New 指令信息_静态保存
                        指令->操作码 = 操作码枚举.静态保存
                        If 引入静态变量(静态成员, Cast(指令信息_静态保存 Ptr, 指令)->引用) = False Then 
                            最后一次错误 = 错误枚举.未找到静态变量
                            Return False
                        End If
                    Case 字节码枚举.成员保存
                        Dim 实例定位序列 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_成员保存
                        指令->操作码 = 操作码枚举.成员保存
                        Cast(指令信息_成员保存 Ptr, 指令)->实例定位序列 = 实例定位序列
                    Case 字节码枚举.成员读取
                        Dim 实例定位序列 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_成员读取
                        指令->操作码 = 操作码枚举.成员读取
                        Cast(指令信息_成员读取 Ptr, 指令)->实例定位序列 = 实例定位序列
                    Case 字节码枚举.加载整数
                        Dim 值 As Long = 读整数(fhandle)
                        Dim 索引 As Integer
                        
                        指令 = New 指令信息_加载整数
                        指令->操作码 = 操作码枚举.加载整数
                        Cast(指令信息_加载整数 Ptr, 指令)->值 = New 常量信息_整数
                        
                        索引 = UBound(常量表) + 1
                        ReDim Preserve 常量表(索引)
                        常量表(索引) = Cast(指令信息_加载整数 Ptr, 指令)->值
                        
                    Case 字节码枚举.加载文本
                        Dim 值 As String = 读文本(fhandle)
                        
                        指令 = New 指令信息_加载文本
                        指令->操作码 = 操作码枚举.加载文本
                        Cast(指令信息_加载文本 Ptr, 指令)->值 = New 常量信息_文本
                        
                        索引 = UBound(常量表) + 1
                        ReDim Preserve 常量表(索引)
                        常量表(索引) = Cast(指令信息_加载文本 Ptr, 指令)->值
                    Case 字节码枚举.加载逻辑
                        Dim 值 As BOOL = 读整数(fhandle)
                        
                        指令 = New 指令信息_加载逻辑
                        指令->操作码 = 操作码枚举.加载逻辑
                        Cast(指令信息_加载逻辑 Ptr, 指令)->值 = New 常量信息_逻辑
                        
                        索引 = UBound(常量表) + 1
                        ReDim Preserve 常量表(索引)
                        常量表(索引) = Cast(指令信息_加载逻辑 Ptr, 指令)->值
                    Case 字节码枚举.创建变量
                        Dim 序列 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_创建变量
                        指令->操作码 = 操作码枚举.创建变量
                        Cast(指令信息_创建变量 Ptr, 指令)->序列 = 序列
                    Case 字节码枚举.销毁变量
                        Dim 序列 As Long = 读整数(fhandle)
                        
                        指令 = New 指令信息_销毁变量
                        指令->操作码 = 操作码枚举.销毁变量
                        Cast(指令信息_销毁变量 Ptr, 指令)->序列 = 序列
                End Select
                
                实体->指令表(j) = 指令
            Next
        Else
            方法引用->实体 = New 方法实体
        End If
    Next
    
    
    Close fhandle
    
    返回值 = 本类对象
    Function = True
End Function


Type 线程传入信息
    方法 As 方法信息 Ptr
    参数表 As 对象信息 Ptr Ptr
    
    条件 As Any Ptr
    条件互斥锁 As Any Ptr
End Type

Sub 工作线程(信息 As 线程传入信息 Ptr)
    Dim 方法 As 方法信息 Ptr = 信息->方法
    Dim 参数表 As 对象信息 Ptr Ptr = 信息->参数表
    
    MutexLock(信息->条件互斥锁)
    CondBroadcast 信息->条件
    MutexUnlock(信息->条件互斥锁)
    
    Dim 栈空间(最大栈尺寸 -1) As 对象信息 Ptr
    Dim 栈指针 As 对象信息 Ptr Ptr = @栈空间(UBound(栈空间))
    
    方法->实体->调用(方法, 栈指针, 参数表)
    
    'For i As Integer = UBound(栈空间) To UBound(栈空间) - 参数数量 + 1 Step -1
    '    栈空间(i) = 参数表[UBound(栈空间) - i]
    'Next
End Sub

Function 启动线程方法(方法 As 方法信息 Ptr, 参数表 As 对象信息 Ptr Ptr=0) As Any Ptr
    Dim 信息 As 线程传入信息 Ptr = New 线程传入信息
    信息->条件 = CondCreate()
    信息->条件互斥锁 = MutexCreate()
    信息->方法 = 方法
    信息->参数表 = 参数表
    
    Function = ThreadCreate(@工作线程, 信息)
    
    MutexLock(信息->条件互斥锁)
    CondWait(信息->条件, 信息->条件互斥锁)
    MutexUnlock(信息->条件互斥锁)
    
    CondDestroy(信息->条件)
    MutexDestroy(信息->条件互斥锁)
    Delete 信息
End Function


Sub 测试()
    加入界目录("C:\Users\WmFreeSmile\Desktop\fust\编译器\release64\code")
    
End Sub

Sub 测试2()
    Print "End"
    Sleep
End Sub


Sub 启动()
    根命名空间 = New 命名空间信息
    根命名空间->名称 = "Root"
    
    测试()
    
    '先引入Program类，触发类依赖链，建立类架构
    Print 引入类(文本转类路径("User::Program")), 最后一次错误
    
    '调用每个类的入口方法
    For i As Integer = LBound(入口方法表) To UBound(入口方法表)
        Print "start_enter:";入口方法表(i)
        ThreadWait 启动线程方法(入口方法表(i))
    Next
    
    
    
    Dim 方法 As 方法信息 Ptr
    If 引入方法(文本转方法路径("User::Program::14841474095010590698"), 方法) Then
        ThreadWait 启动线程方法(方法)
    End If
    
    测试2()
    
    Delete 根命名空间
End Sub





End Namespace
