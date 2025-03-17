

Namespace 编译器
    
Sub 启动()
   编译("C:\Users\WmFreeSmile\Desktop\fust\编译器\release64\code.txt", "C:\Users\WmFreeSmile\Desktop\fust\编译器\release64\code")
End Sub



Sub 检查(文本 As String, 信息 As String)
   Print 文本;!"\r\n";"-------->";信息
   Sleep
End Sub


Dim 是否出错 As BOOL



Dim 计数_方法 As Integer
Dim 计数_用户方法 As Integer

Dim 表达式结果 As 表达式格式 Ptr

Dim 方法映射 As 表.映射表

Dim 基本类型 As 基本类型类

Type 别名_类型格式 As 类型格式

Type 基本类型类
   单精度浮点型 As 别名_类型格式 Ptr
   双精度浮点型 As 别名_类型格式 Ptr
   字节型 As 别名_类型格式 Ptr
   短整数型 As 别名_类型格式 Ptr
   整数型 As 别名_类型格式 Ptr
   长整数型 As 别名_类型格式 Ptr
   文本型 As 别名_类型格式 Ptr
   逻辑型 As 别名_类型格式 Ptr
   
   对象型 As 别名_类型格式 Ptr
End Type

Type 别名_节点_方法 As 节点_方法
Type 别名_静态变量 As 静态变量

Type 单元 Extends Object
   
End Type

Type 单元_占位符 Extends 单元
   位置 As Integer
   标识 As Integer
End Type


Type 实单元 Extends 单元 '每个实单元都有一个位置 
   位置 As Integer
End Type


Type 单元_返回 Extends 实单元
   
End Type

Type 单元_加载参数 Extends 实单元
   序列 As Integer
End Type

Type 单元_丢弃 Extends 实单元
   
End Type

Type 单元_读取 Extends 实单元
   序列 As Integer
End Type
Type 单元_静态读取 Extends 实单元
   引用 As 别名_静态变量 Ptr
End Type
Type 单元_成员读取 Extends 实单元
    实例定位序列 As Integer
End Type

Type 单元_调用 Extends 实单元
   方法 As 别名_节点_方法 Ptr
End Type

Type 单元_保存 Extends 实单元
   序列 As Integer
End Type

Type 单元_静态保存 Extends 实单元
   引用 As 别名_静态变量 Ptr
End Type

Type 单元_成员保存 Extends 实单元
    实例定位序列 As Integer
End Type


Type 单元_加载整数 Extends 实单元
   值 As Long
End Type

Type 单元_加载文本 Extends 实单元
   值 As String
End Type

Type 单元_加载逻辑 Extends 实单元
   值 As BOOL
End Type

Type 单元_创建变量 Extends 实单元
   序列 As Integer
End Type

Type 单元_销毁变量 Extends 实单元
   序列 As Integer
End Type

Type 单元_如果 Extends 实单元
   真目标 As Integer
   假目标 As Integer
End Type


Enum 字节码
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
   加载整数
   加载文本
   加载逻辑
   创建变量
   销毁变量
End Enum



Type 方法信息
   流空间(Any) As 单元 Ptr
   计数器 As Integer
   计数器_标识 As Integer '标签标识
   
   字节码表(Any) As UByte
   
   Declare Sub 编码数据()
   
   Declare Function 取占位符标识() As Integer
   Declare Function 取占位符位置(标识 As Integer) As Integer
   
   Declare Sub 占位符(标识 As Integer)
   
   Declare Sub 指令_如果(真目标 As Integer, 假目标 As Integer)
   Declare Sub 指令_返回()
   Declare Sub 指令_加载参数(序列 As Integer)
   Declare Sub 指令_丢弃()
   Declare Sub 指令_读取(序列 As Integer)
   Declare Sub 指令_静态读取(引用 As 别名_静态变量 Ptr)
   Declare Sub 指令_成员读取(实例定位序列 As Integer)
   Declare Sub 指令_调用(方法 As 别名_节点_方法 Ptr)
   Declare Sub 指令_保存(序列 As Integer)
   Declare Sub 指令_静态保存(引用 As 别名_静态变量 Ptr)
   Declare Sub 指令_成员保存(实例定位序列 As Integer)
   Declare Sub 指令_加载整数(值 As Long)
   Declare Sub 指令_加载文本(值 As String)
   Declare Sub 指令_加载逻辑(值 As BOOL)
   Declare Sub 指令_创建变量(序列 As Integer)
   Declare Sub 指令_销毁变量(序列 As Integer)
End Type

Sub 方法信息.编码数据()
    Dim 索引 As Integer
    For j As Integer = LBound(This.流空间) To UBound(This.流空间)
        If *This.流空间(j) Is 单元_返回 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.返回
        ElseIf *This.流空间(j) Is 单元_加载参数 Then
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.加载参数
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
            
             *Cast(Long Ptr, @字节码表(索引)) = Cast(单元_加载参数 Ptr, This.流空间(j))->序列
        ElseIf *This.流空间(j) Is 单元_丢弃 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.丢弃
        ElseIf *This.流空间(j) Is 单元_读取 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.读取
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
            
             *Cast(Long Ptr, @字节码表(索引)) = Cast(单元_读取 Ptr, This.流空间(j))->序列
        ElseIf *This.流空间(j) Is 单元_静态读取 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.静态读取
            
            
            Dim 节点索引 As 节点_命名空间 Ptr = Cast(单元_静态读取 Ptr, This.流空间(j))->引用->绑定类->绑定命名空间
            Dim 组()     As String
            While True
                If 节点索引 = 0 Then
                    Exit While
                End If
                Dim 索引 As Integer
                索引 = UBound(组) + 1
                ReDim Preserve 组(索引)
                组(索引) = 节点索引->名称
                节点索引 = 节点索引->上一级
            Wend
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = ARRAYSIZE(组)
            
            For k As Integer = UBound(组) To LBound(组) Step -1
                索引 = UBound(字节码表) + 1
                ReDim Preserve 字节码表(UBound(字节码表) + 4)
                 *Cast(Long Ptr, @字节码表(索引)) = Len(组(k))
                
                索引 = UBound(字节码表) + 1
                ReDim Preserve 字节码表(UBound(字节码表) + Len(组(k)) + 1)
                 *Cast(ZString Ptr, @字节码表(索引)) = 组(k)
            Next
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Len(Cast(单元_静态读取 Ptr, This.流空间(j))->引用->绑定类->名称)
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + Len(Cast(单元_静态读取 Ptr, This.流空间(j))->引用->绑定类->名称) + 1)
             *Cast(ZString Ptr, @字节码表(索引)) = Cast(单元_静态读取 Ptr, This.流空间(j))->引用->绑定类->名称
            
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Len(Cast(单元_静态读取 Ptr, This.流空间(j))->引用->名称)
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + Len(Cast(单元_静态读取 Ptr, This.流空间(j))->引用->名称) + 1)
             *Cast(ZString Ptr, @字节码表(索引)) = Cast(单元_静态读取 Ptr, This.流空间(j))->引用->名称
            
        ElseIf *This.流空间(j) Is 单元_调用 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.调用
            
            
            Dim 节点索引 As 节点_命名空间 Ptr = Cast(单元_调用 Ptr, This.流空间(j))->方法->绑定类->绑定命名空间
            Dim 组()     As String
            While True
                If 节点索引 = 0 Then
                    Exit While
                End If
                Dim 索引 As Integer
                索引 = UBound(组) + 1
                ReDim Preserve 组(索引)
                组(索引) = 节点索引->名称
                节点索引 = 节点索引->上一级
            Wend
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = ARRAYSIZE(组)
            
            For k As Integer = UBound(组) To LBound(组) Step -1
                索引 = UBound(字节码表) + 1
                ReDim Preserve 字节码表(UBound(字节码表) + 4)
                 *Cast(Long Ptr, @字节码表(索引)) = Len(组(k))
                
                索引 = UBound(字节码表) + 1
                ReDim Preserve 字节码表(UBound(字节码表) + Len(组(k)) + 1)
                 *Cast(ZString Ptr, @字节码表(索引)) = 组(k)
            Next
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Len(Cast(单元_调用 Ptr, This.流空间(j))->方法->绑定类->名称)
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + Len(Cast(单元_调用 Ptr, This.流空间(j))->方法->绑定类->名称) + 1)
             *Cast(ZString Ptr, @字节码表(索引)) = Cast(单元_调用 Ptr, This.流空间(j))->方法->绑定类->名称
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 8)
             *Cast(ULongInt Ptr, @字节码表(索引)) = Cast(单元_调用 Ptr, This.流空间(j))->方法->方法签名摘要
            
        ElseIf *This.流空间(j) Is 单元_保存 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.保存
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
            
             *Cast(Long Ptr, @字节码表(索引)) = Cast(单元_保存 Ptr, This.流空间(j))->序列
            
        ElseIf *This.流空间(j) Is 单元_静态保存 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.静态保存
            
            
            Dim 节点索引 As 节点_命名空间 Ptr = Cast(单元_静态读取 Ptr, This.流空间(j))->引用->绑定类->绑定命名空间
            Dim 组()     As String
            While True
                If 节点索引 = 0 Then
                    Exit While
                End If
                Dim 索引 As Integer
                索引 = UBound(组) + 1
                ReDim Preserve 组(索引)
                组(索引) = 节点索引->名称
                节点索引 = 节点索引->上一级
            Wend
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = ARRAYSIZE(组)
            
            For k As Integer = UBound(组) To LBound(组) Step -1
                索引 = UBound(字节码表) + 1
                ReDim Preserve 字节码表(UBound(字节码表) + 4)
                 *Cast(Long Ptr, @字节码表(索引)) = Len(组(k))
                
                索引 = UBound(字节码表) + 1
                ReDim Preserve 字节码表(UBound(字节码表) + Len(组(k)) + 1)
                 *Cast(ZString Ptr, @字节码表(索引)) = 组(k)
            Next
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Len(Cast(单元_静态保存 Ptr, This.流空间(j))->引用->绑定类->名称)
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + Len(Cast(单元_静态保存 Ptr, This.流空间(j))->引用->绑定类->名称) + 1)
             *Cast(ZString Ptr, @字节码表(索引)) = Cast(单元_静态保存 Ptr, This.流空间(j))->引用->绑定类->名称
            
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Len(Cast(单元_静态保存 Ptr, This.流空间(j))->引用->名称)
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + Len(Cast(单元_静态保存 Ptr, This.流空间(j))->引用->名称) + 1)
             *Cast(ZString Ptr, @字节码表(索引)) = Cast(单元_静态保存 Ptr, This.流空间(j))->引用->名称
        ElseIf *This.流空间(j) Is 单元_成员保存 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.成员保存
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Cast(单元_成员保存 Ptr, This.流空间(j))->实例定位序列
        ElseIf *This.流空间(j) Is 单元_加载整数 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.加载整数
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Cast(单元_加载整数 Ptr, This.流空间(j))->值
        ElseIf *This.流空间(j) Is 单元_加载文本 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.加载文本
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = Len(Cast(单元_加载文本 Ptr, This.流空间(j))->值)
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + Len(Cast(单元_加载文本 Ptr, This.流空间(j))->值) + 1)
             *Cast(ZString Ptr, @字节码表(索引)) = Cast(单元_加载文本 Ptr, This.流空间(j))->值
        ElseIf *This.流空间(j) Is 单元_加载逻辑 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.加载逻辑
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + SizeOf(BOOL))
             *Cast(BOOL Ptr, @字节码表(索引)) = Cast(单元_加载逻辑 Ptr, This.流空间(j))->值
        ElseIf *This.流空间(j) Is 单元_创建变量 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.创建变量
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
            
             *Cast(Long Ptr, @字节码表(索引)) = Cast(单元_创建变量 Ptr, This.流空间(j))->序列
        ElseIf *This.流空间(j) Is 单元_销毁变量 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.销毁变量
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
            
             *Cast(Long Ptr, @字节码表(索引)) = Cast(单元_销毁变量 Ptr, This.流空间(j))->序列
        ElseIf *This.流空间(j) Is 单元_如果 Then
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 1)
            字节码表(索引) = 字节码.如果
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = 取占位符位置(Cast(单元_如果 Ptr, This.流空间(j))->真目标)
            
            索引 = UBound(字节码表) + 1
            ReDim Preserve 字节码表(UBound(字节码表) + 4)
             *Cast(Long Ptr, @字节码表(索引)) = 取占位符位置(Cast(单元_如果 Ptr, This.流空间(j))->假目标)
        End If
    Next
End Sub

Sub 方法信息.指令_如果(真目标 As Integer, 假目标 As Integer)
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_如果)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_如果 Ptr, This.流空间(索引))->真目标 = 真目标
   Cast(单元_如果 Ptr, This.流空间(索引))->假目标 = 假目标
End Sub

Sub 方法信息.指令_返回()
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_返回)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
End Sub

Sub 方法信息.指令_加载参数(序列 As Integer)
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_加载参数)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_加载参数 Ptr, This.流空间(索引))->序列 = 序列
End Sub

Sub 方法信息.指令_丢弃()'丢弃栈顶的引用
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_丢弃)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
End Sub

Sub 方法信息.指令_读取(序列 As Integer)'从某个局部变量里面读取引用放到栈里面
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_读取)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_读取 Ptr, This.流空间(索引))->序列 = 序列
End Sub

Sub 方法信息.指令_静态读取(引用 As 别名_静态变量 Ptr)
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_静态读取)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_静态读取 Ptr, This.流空间(索引))->引用 = 引用
End Sub
Sub 方法信息.指令_成员读取(实例定位序列 As Integer)
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_成员读取)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_成员读取 Ptr, This.流空间(索引))->实例定位序列 = 实例定位序列
End Sub


Sub 方法信息.指令_调用(方法 As 别名_节点_方法 Ptr)
   'Dim 临时文本 As String = ""
   '生成文式_类(方法->绑定类, 临时文本)
   '临时文本 = 临时文本 + "::" + Str(方法->方法签名摘要)
   '
   'This.中间语言指令 = This.中间语言指令+!"\t" + "call " + 临时文本 + !"\r\n"
   
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_调用)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_调用 Ptr, This.流空间(索引))->方法 = 方法
End Sub

Sub 方法信息.指令_保存(序列 As Integer)
   'This.中间语言指令 = This.中间语言指令+!"\t" + "save." + Str(序列) + !"\r\n"
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_保存)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_保存 Ptr, This.流空间(索引))->序列 = 序列
End Sub

Sub 方法信息.指令_静态保存(引用 As 别名_静态变量 Ptr)
    Dim 索引 As Integer = UBound(This.流空间) + 1
    ReDim Preserve This.流空间(索引)
    This.流空间(索引) = 对象管理.加入(New 单元_静态保存)
    Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
    计数器 = 计数器 + 1
    
    Cast(单元_静态保存 Ptr, This.流空间(索引))->引用 = 引用
End Sub

Sub 方法信息.指令_成员保存(实例定位序列 As Integer)
    Dim 索引 As Integer = UBound(This.流空间) + 1
    ReDim Preserve This.流空间(索引)
    This.流空间(索引) = 对象管理.加入(New 单元_成员保存)
    Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
    计数器 = 计数器 + 1
    
    Cast(单元_成员保存 Ptr, This.流空间(索引))->实例定位序列 = 实例定位序列
End Sub


Sub 方法信息.指令_加载整数(值 As Long)
   'This.中间语言指令 = This.中间语言指令+!"\t" + "load.i32." + Str(Cast(ULong,值)) + !"\r\n"
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_加载整数)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_加载整数 Ptr, This.流空间(索引))->值 = 值
End Sub

Sub 方法信息.指令_加载文本(值 As String)
   'This.中间语言指令 = This.中间语言指令+!"\t" + "load.string "+!"\34" + 值 +!"\34"+ !"\r\n"
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_加载文本)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_加载文本 Ptr, This.流空间(索引))->值 = 值
End Sub

Sub 方法信息.指令_加载逻辑(值 As BOOL)
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_加载逻辑)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_加载逻辑 Ptr, This.流空间(索引))->值 = 值
End Sub


Sub 方法信息.指令_创建变量(序列 As Integer)
   'This.中间语言指令 = This.中间语言指令 +!"\t"+ "create." + Str(序列) + !"\r\n"
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_创建变量)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_创建变量 Ptr, This.流空间(索引))->序列 = 序列
End Sub

Sub 方法信息.指令_销毁变量(序列 As Integer)
   'This.中间语言指令 = This.中间语言指令 +!"\t"+ "close." + Str(序列) + !"\r\n"
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_销毁变量)
   Cast(实单元 Ptr, This.流空间(索引))->位置 = 计数器
   计数器 = 计数器 + 1
   
   Cast(单元_销毁变量 Ptr, This.流空间(索引))->序列 = 序列
End Sub



Function 方法信息.取占位符标识() As Integer
   Function = 计数器_标识
   计数器_标识 = 计数器_标识 + 1
End Function

Function 方法信息.取占位符位置(标识 As Integer) As Integer
   For i As Integer = LBound(This.流空间) To UBound(This.流空间)
      If *This.流空间(i) Is 单元_占位符 AndAlso Cast(单元_占位符 Ptr,This.流空间(i))->标识=标识 Then
         Return Cast(单元_占位符 Ptr,This.流空间(i))->位置
      End If
   Next
End Function

Sub 方法信息.占位符(标识 As Integer)
   Dim 索引 As Integer = UBound(This.流空间) + 1
   ReDim Preserve This.流空间(索引)
   This.流空间(索引) = 对象管理.加入(New 单元_占位符)
   Cast(单元_占位符 Ptr, This.流空间(索引))->标识 = 标识
   Cast(单元_占位符 Ptr, This.流空间(索引))->位置 = 计数器
   
End Sub


Enum 解析模式
   声明模式
   定义模式
   流程模式
End Enum

Sub 错误信息(信息 As String)
   是否出错 = True
   
   Print "行:";Str(词法器.获取行号(词法器.位置));!"\t";"信息:'";信息;"'"
End Sub

Sub 语法_命名空间(模式 As 解析模式,编译信息指针 As 编译信息 Ptr)
   Dim 指针 As 词法器.单元信息 Ptr = 词法器.取下一个单元()
   
   If 指针->类型 <> 词法器.单元类型.标识符 Then
      错误信息("期待标识符"):代码块(模式,编译信息指针):Return
   End If
   
   If *编译信息指针->现行节点 Is 节点_根=False AndAlso *编译信息指针->现行节点 Is 节点_命名空间=False Then
      错误信息("在非'根域'和'命名空间域'创建'命名空间'"):代码块(模式,编译信息指针):Return
   End If
   
   Dim super As 节点 Ptr = 编译信息指针->现行节点
   Dim temp As 节点_命名空间 Ptr
   If *编译信息指针->现行节点 Is 节点_根 Then
      temp = Cast(节点_根 Ptr, 编译信息指针->现行节点)->根命名空间
   ElseIf *编译信息指针->现行节点 Is 节点_命名空间 Then
      temp = Cast(节点_命名空间 Ptr,编译信息指针->现行节点)
   End If
   
   Dim 引用 As 节点_命名空间 Ptr
   引用 = 0
   For i As Integer = LBound(temp->命名空间表) To UBound(temp->命名空间表)
      If temp->命名空间表(i)->名称 = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称 Then
         引用 = temp->命名空间表(i)
         Exit For
      End If
   Next
   If 引用 = 0 Then
      Dim 索引 As Integer
      索引 = UBound(temp->命名空间表) + 1
      ReDim Preserve temp->命名空间表(索引)
      temp->命名空间表(索引) = 对象管理.加入(New 节点_命名空间())
      temp->命名空间表(索引)->名称 = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
      引用 = temp->命名空间表(索引)
      引用->上一级 = temp
   End If
   
   Print "创建命名空间",引用->名称
   
   指针 = 词法器.取下一个单元()
   If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr,指针)->值<>"{" Then
      错误信息("期待符号'{'"):代码块(模式,编译信息指针):Return
   End If
   
   Dim 上一级引用 As 节点 Ptr
   Dim 上一级命名空间 As 节点_命名空间 Ptr
   上一级引用 = super
   上一级命名空间 = temp
   
   编译信息指针->现行节点 = 引用
   编译信息指针->所属命名空间节点 = 引用
   代码块(模式,编译信息指针)
   编译信息指针->现行节点 = 上一级引用
   编译信息指针->所属命名空间节点 = 上一级命名空间
   
   代码块(模式,编译信息指针)
End Sub

Sub 语法_类(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
    Dim 指针 As 词法器.单元信息 Ptr = 词法器.取下一个单元()
    
    If 指针->类型 <> 词法器.单元类型.标识符 Then
        错误信息("期待标识符") : 代码块(模式, 编译信息指针) : Return
    End If
    
    If *编译信息指针->现行节点 Is 节点_根 = False AndAlso *编译信息指针->现行节点 Is 节点_命名空间 = False Then
        错误信息("在非'根域'和'命名空间域'创建'类'") : 代码块(模式, 编译信息指针) : Return
    End If
    
    Dim super As 节点 Ptr = 编译信息指针->现行节点
    Dim temp  As 节点_命名空间 Ptr
    If *编译信息指针->现行节点 Is 节点_根 Then
        temp = Cast(节点_根 Ptr, 编译信息指针->现行节点)->根命名空间
    ElseIf *编译信息指针->现行节点 Is 节点_命名空间 Then
        temp = Cast(节点_命名空间 Ptr, 编译信息指针->现行节点)
    End If
    
    Dim 引用 As 节点_类 Ptr
    引用 = 0
    For i As Integer = LBound(temp->类表) To UBound(temp->类表)
        If temp->类表(i)->名称 = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称 Then
            引用 = temp->类表(i)
            Exit For
        End If
    Next
    If 引用 = 0 Then
        Dim 索引 As Integer
        索引 = UBound(temp->类表) + 1
        ReDim Preserve temp->类表(索引)
        temp->类表(索引)       = 对象管理.加入(New 节点_类())
        temp->类表(索引)->名称 = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
        引用 = temp->类表(索引)
        引用->绑定命名空间 = temp
        引用->用户类       = True
    End If
    
    Print "创建类", 引用->名称
    
    If 模式 = 解析模式.声明模式 OrElse 模式 = 解析模式.流程模式 Then
        检查_跳过()
    ElseIf 模式 = 解析模式.定义模式 Then
        指针 = 词法器.窥视()
        If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "<" Then
            词法器.取下一个单元()
            
            指针 = 词法器.取下一个单元()
            Dim 索引 As Integer
            While True
                If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = ">" Then
                    Exit While
                End If
                
                索引 = UBound(引用->泛型表) + 1
                ReDim Preserve 引用->泛型表(索引)
                引用->泛型表(索引) = 对象管理.加入(New 泛型信息)
                引用->泛型表(索引)->名称 = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
                引用->泛型表(索引)->引用类型表 = 对象管理.加入(New 表.线性表)
                
                指针 = 词法器.窥视()
                If 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_extends Then
                    词法器.取下一个单元()
                    
                    表达式(模式, 编译信息指针, 0)
                    If *表达式结果 Is 表达式格式_类型 = False OrElse *Cast(表达式格式_类型 Ptr, 表达式结果)->类型 Is 类型格式_类=False Then 错误信息("期待类") : 代码块(模式, 编译信息指针) : Return
                    引用->泛型表(索引)->继承 = Cast(类型格式_类 Ptr, Cast(表达式格式_类型 Ptr, 表达式结果)->类型)->引用
                    
                Else
                    引用->泛型表(索引)->继承 = Cast(类型格式_类 Ptr, 基本类型.对象型)->引用
                End If
                
                
                指针 = 词法器.取下一个单元()
                If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "," Then
                    指针 = 词法器.取下一个单元()
                End If
            Wend
        End If
        
        指针 = 词法器.窥视()
        If 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_extends Then
            词法器.取下一个单元()
            
            表达式(模式, 编译信息指针, 0)
            If *表达式结果 Is 表达式格式_类型 = False OrElse *Cast(表达式格式_类型 Ptr, 表达式结果)->类型 Is 类型格式_类=False Then 错误信息("期待类") : 代码块(模式, 编译信息指针) : Return
            引用->父类 = Cast(类型格式_类 Ptr, Cast(表达式格式_类型 Ptr, 表达式结果)->类型)->引用
        Else
            引用->父类 = Cast(类型格式_类 Ptr, 基本类型.对象型)->引用
        End If
        指针 = 词法器.取下一个单元()
        If 指针->类型 <> 词法器.单元类型.跃点 Then
            错误信息("期待符号'@'") : 代码块(模式, 编译信息指针) : Return
        End If
    End If
    
    Dim 临时文本 As String
    生成文式_类(引用, 临时文本)
    If 临时文本 = "Kernel::Lang::Float" Then
        基本类型.单精度浮点型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::Double" Then
        基本类型.双精度浮点型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::Byte" Then
        基本类型.字节型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::Short" Then
        基本类型.短整数型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::Int" Then
        基本类型.整数型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::Long" Then
        基本类型.长整数型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::String" Then
        基本类型.文本型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::Bool" Then
        基本类型.逻辑型 = 对象管理.加入(New 类型格式_类(引用))
    ElseIf 临时文本 = "Kernel::Lang::Object" Then
        引用->父类 = 0 'Object不允许有父类，擦除父类
        基本类型.对象型 = 对象管理.加入(New 类型格式_类(引用))
    End If
    
    指针 = 词法器.取下一个单元()
    If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> "{" Then
        错误信息("期待符号'{'") : 代码块(模式, 编译信息指针) : Return
    End If
    
    Dim 上一级引用 As 节点 Ptr
    上一级引用 = super
    Dim 上一级类节点 As 节点_类 Ptr = 编译信息指针->所属类节点
    编译信息指针->所属类节点 = 引用
    编译信息指针->现行节点 = 引用
    
    代码块(模式, 编译信息指针)
    
    编译信息指针->所属类节点 = 上一级类节点
    编译信息指针->现行节点 = 上一级引用
    
    代码块(模式, 编译信息指针)
End Sub


Sub 检查_跳过()
   Dim 指针 As 词法器.单元信息 Ptr
   指针 = 词法器.取下一个单元()
   While 指针->类型 <> 词法器.单元类型.跃点
      If 指针->类型 = 词法器.单元类型.终止 Then
         错误信息("找不到跃点")
      Else
         指针 = 词法器.取下一个单元()
      End If
   Wend
End Sub


Sub 表达式(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr,ByRef 项_返回 As 项 Ptr)
   分析赋值(模式, 编译信息指针, 项_返回)
End Sub

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


Function 是否赋值运算符() As BOOL
    Dim 下一个单元   As 词法器.单元信息 Ptr
    Dim 再下一个单元 As 词法器.单元信息 Ptr
    词法器.连续窥视(下一个单元, 再下一个单元)
    If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "=" Then
        If 再下一个单元->类型 = 词法器.单元类型.符号 Then
            If Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = ">" Then
                Function = False
            ElseIf Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = "=" Then
                Function = False
            Else
                Function = True
            End If
        Else
            Function = True
        End If
    Else
        Function = False
    End If
End Function

Sub 分析赋值(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr, ByRef 项_返回 As 项 Ptr)
    Dim 左项 As 项 Ptr, 右项 As 项 Ptr
    
    分析关系(模式, 编译信息指针, 左项)
    项_返回 = 左项
    
    While 是否赋值运算符()
        If *表达式结果 Is 表达式格式_变量 = False Then 错误信息("只能对变量进行赋值") : Return
        词法器.取下一个单元()
        分析关系(模式, 编译信息指针, 右项)
        
        Dim temp As 项_赋值 Ptr = 对象管理.加入(New 项_赋值)
        temp->变量 = Cast(项_变量 Ptr, 左项)
        temp->值   = 右项
        项_返回    = temp
    Wend
End Sub

Function 是否关系运算符(ByRef 类型 As 运算符类型) As BOOL
   Dim 第一个单元 As 词法器.单元信息 Ptr
   Dim 第二个单元 As 词法器.单元信息 Ptr
   Dim 第三个单元 As 词法器.单元信息 Ptr
   词法器.三连窥视(第一个单元, 第二个单元, 第三个单元)
   If 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "=" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = "=" Then
      Function = True:类型=运算符类型.等于
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "<" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = ">" Then
      Function = True:类型=运算符类型.不等于
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = ">" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = "=" Then
      Function = True:类型=运算符类型.大于等于
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "<" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = "=" Then
      Function = True:类型=运算符类型.小于等于
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = ">" Then
      If 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = ">" Then
         Function = False
      Else
         Function = True:类型=运算符类型.大于
      End If
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "<" Then
      If 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = "<" Then
         Function = False
      Else
         Function = True:类型=运算符类型.小于
      End If
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "?" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = ">" AndAlso 第三个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第三个单元)->值 = "=" Then
      Function = True:类型=运算符类型.无符号大于等于
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "?" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = "<" AndAlso 第三个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第三个单元)->值 = "=" Then
      Function = True:类型=运算符类型.无符号小于等于
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "?" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = ">" Then
      Function = True:类型=运算符类型.无符号大于
   ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "?" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = "<" Then
      Function = True:类型=运算符类型.无符号小于
   Else
      Function = False
   End If
End Function

Sub 分析关系(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr,ByRef 项_返回 As 项 Ptr)
   Left_Value(分析加减)
   While 是否关系运算符(类型)
      If 类型 = 运算符类型.等于 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.不等于 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.大于等于 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.小于等于 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.大于 Then
         词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.小于 Then
         词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.无符号大于等于 Then
         词法器.取下一个单元() : 词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.无符号小于等于 Then
         词法器.取下一个单元() : 词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.无符号大于 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      ElseIf 类型 = 运算符类型.无符号小于 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析加减)
      End If
   Wend
End Sub

Function 是否加减运算符(ByRef 类型 As 运算符类型) As BOOL
   Dim 指针 As 词法器.单元信息 Ptr = 词法器.窥视()
   If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "+" Then
      Function = True:类型=运算符类型.加
   ElseIf 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "-" Then
      Function = True:类型=运算符类型.减
   Else
      Function = False
   End If
End Function


Sub 分析加减(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr, ByRef 项_返回 As 项 Ptr)
   Left_Value(分析项)
   While 是否加减运算符(类型)
      If 类型 = 运算符类型.加 Then
         词法器.取下一个单元()
         Right_Value(分析项)
      ElseIf 类型 = 运算符类型.减 Then
         词法器.取下一个单元()
         Right_Value(分析项)
      End If
   Wend
End Sub


Function 是否项运算符(ByRef 类型 As 运算符类型) As BOOL
   Dim 下一个单元 As 词法器.单元信息 Ptr
   Dim 再下一个单元 As 词法器.单元信息 Ptr
   词法器.连续窥视(下一个单元, 再下一个单元)
   If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "*" Then
      Function = True:类型=运算符类型.乘
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "/" Then
      Function = True:类型=运算符类型.除
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "%" Then
      Function = True:类型=运算符类型.余
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "^" Then
      Function = True:类型=运算符类型.幂
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "\" Then
      Function = True:类型=运算符类型.整除
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "?" Then
      If 再下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = "*" Then
         Function = True:类型=运算符类型.无符号乘
      ElseIf 再下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = "/" Then
         Function = True:类型=运算符类型.无符号除
      ElseIf 再下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = "%" Then
         Function = True:类型=运算符类型.无符号余
      ElseIf 再下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = "\" Then
         Function = True:类型=运算符类型.无符号整除
      Else
         Function = False
      End If
   Else
      Function = False
   End If
End Function

Sub 分析项(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr,ByRef 项_返回 As 项 Ptr)
   Left_Value(分析位)
   While 是否项运算符(类型)
      If 类型 = 运算符类型.乘 Then
         词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.除 Then
         词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.余 Then
         词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.幂 then
         词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.整除 Then
         词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.无符号乘 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.无符号除 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.无符号余 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析位)
      ElseIf 类型 = 运算符类型.无符号整除 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析位)
      End If
   Wend
End Sub

Function 是否位运算符(ByRef 类型 As 运算符类型) As BOOL
   Dim 下一个单元 As 词法器.单元信息 Ptr
   Dim 再下一个单元 As 词法器.单元信息 Ptr
   词法器.连续窥视(下一个单元, 再下一个单元)
   If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "&" Then
      Function = True:类型=运算符类型.与
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "|" Then
      Function = True:类型=运算符类型.或
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "<" AndAlso 再下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = "<" Then
      Function = True:类型=运算符类型.左移
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = ">" AndAlso 再下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 再下一个单元)->值 = ">"  Then
      Function = True:类型=运算符类型.右移
   ElseIf 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "~" Then
      Function = True:类型=运算符类型.异或
   Else
      Function = False
   End If
End Function


Sub 分析位(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr,ByRef 项_返回 As 项 Ptr)
   Left_Value(分析因子)
   While 是否位运算符(类型)
      If 类型 = 运算符类型.与 Then
         词法器.取下一个单元()
         Right_Value(分析因子)
      ElseIf 类型 = 运算符类型.或 Then
         词法器.取下一个单元()
         Right_Value(分析因子)
      ElseIf 类型 = 运算符类型.左移 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析因子)
      ElseIf 类型 = 运算符类型.右移 Then
         词法器.取下一个单元() : 词法器.取下一个单元()
         Right_Value(分析因子)
      ElseIf 类型 = 运算符类型.异或 Then
         词法器.取下一个单元()
         Right_Value(分析因子)
      End If
   Wend
End Sub


Sub 分析因子(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr, ByRef 项_返回 As 项 Ptr)
    Dim 指针 As 词法器.单元信息 Ptr, 下一个单元 As 词法器.单元信息 Ptr
    词法器.连续窥视(指针, 下一个单元)
    
    If 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 AndAlso 下一个单元->类型 = 词法器.单元类型.标识符 AndAlso Cast(词法器.单元信息_标识符 Ptr, 下一个单元)->名称 = "F" Then 'FLOAT
        词法器.取下一个单元() : 词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_单精度浮点型(Val(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 AndAlso 下一个单元->类型 = 词法器.单元类型.标识符 AndAlso Cast(词法器.单元信息_标识符 Ptr, 下一个单元)->名称 = "D" Then 'Double
        词法器.取下一个单元() : 词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_双精度浮点型(Val(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 Then 'default Double
        词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_双精度浮点型(Val(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 = False AndAlso 下一个单元->类型 = 词法器.单元类型.标识符 AndAlso Cast(词法器.单元信息_标识符 Ptr, 下一个单元)->名称 = "B" Then 'Byte
        词法器.取下一个单元() : 词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_字节型(ValInt(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 = False AndAlso 下一个单元->类型 = 词法器.单元类型.标识符 AndAlso Cast(词法器.单元信息_标识符 Ptr, 下一个单元)->名称 = "S" Then 'Short
        词法器.取下一个单元() : 词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_短整数型(ValInt(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 = False AndAlso 下一个单元->类型 = 词法器.单元类型.标识符 AndAlso Cast(词法器.单元信息_标识符 Ptr, 下一个单元)->名称 = "I" Then 'Int
        词法器.取下一个单元() : 词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_整数型(ValInt(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 = False AndAlso 下一个单元->类型 = 词法器.单元类型.标识符 AndAlso Cast(词法器.单元信息_标识符 Ptr, 下一个单元)->名称 = "L" Then 'Long
        词法器.取下一个单元() : 词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_长整数型(ValLng(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.数字 AndAlso Cast(词法器.单元信息_数字 Ptr, 指针)->是否小数 = False Then 'default Int
        词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_整数型(ValInt(Cast(词法器.单元信息_数字 Ptr, 指针)->值)))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_true Then
        词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_逻辑型(True))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_false Then
        词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_逻辑型(False))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.字符串 Then
        词法器.取下一个单元()
        表达式结果 = 对象管理.加入(New 表达式格式_字符串(Cast(词法器.单元信息_字符串 Ptr, 指针)->值))
        项_返回    = New 项_字面量
        Cast(项_字面量 Ptr, 项_返回)->值 = Cast(表达式格式_字面量 Ptr, 表达式结果)
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "(" Then
        词法器.取下一个单元()
        表达式(模式, 编译信息指针, 项_返回)
        指针 = 词法器.取下一个单元()
        If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> ")" Then
            错误信息("期待符号')'") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
        End If
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "-" Then
        词法器.取下一个单元()
        表达式(模式, 编译信息指针, 项_返回)
        If *表达式结果 Is 表达式格式_单精度浮点型 Then
            Cast(表达式格式_单精度浮点型 Ptr, 表达式结果)->值 = - Cast(表达式格式_单精度浮点型 Ptr, 表达式结果)->值
        ElseIf *表达式结果 Is 表达式格式_双精度浮点型 Then
            Cast(表达式格式_双精度浮点型 Ptr, 表达式结果)->值 = - Cast(表达式格式_双精度浮点型 Ptr, 表达式结果)->值
        ElseIf *表达式结果 Is 表达式格式_字节型 Then
            Cast(表达式格式_字节型 Ptr, 表达式结果)->值 = - Cast(表达式格式_字节型 Ptr, 表达式结果)->值
        ElseIf *表达式结果 Is 表达式格式_短整数型 Then
            Cast(表达式格式_短整数型 Ptr, 表达式结果)->值 = - Cast(表达式格式_短整数型 Ptr, 表达式结果)->值
        ElseIf *表达式结果 Is 表达式格式_整数型 Then
            Cast(表达式格式_整数型 Ptr, 表达式结果)->值 = - Cast(表达式格式_整数型 Ptr, 表达式结果)->值
        ElseIf *表达式结果 Is 表达式格式_长整数型 Then
            Cast(表达式格式_长整数型 Ptr, 表达式结果)->值 = - Cast(表达式格式_长整数型 Ptr, 表达式结果)->值
        Else
            错误信息("不支持对该值取负") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
        End If
        
        链接式(模式, 编译信息指针, 项_返回)
    ElseIf 指针->类型 = 词法器.单元类型.标识符 Then
        词法器.取下一个单元()
        
        Dim temp   As 词法器.单元信息_标识符 Ptr = Cast(词法器.单元信息_标识符 Ptr, 指针)
        Dim FindOK As BOOL
        
        FindOK = False
        
        If FindOK = False Then
            If 编译信息指针->所属类节点 <> 0 Then
                For i As Integer = LBound(编译信息指针->所属类节点->泛型表) To UBound(编译信息指针->所属类节点->泛型表)
                    If 编译信息指针->所属类节点->泛型表(i)->名称 = temp->名称 Then
                        '表达式结果 = 对象管理.加入(New 表达式格式_类型(对象管理.加入(New 类型格式_类(编译信息指针->所属类节点->泛型表(i)->继承))))
                        表达式结果 = 对象管理.加入(New 表达式格式_类型(对象管理.加入(New 类型格式_泛型类(编译信息指针->所属类节点->泛型表(i)->继承, i))))
                        FindOK     = True : Exit For
                    End If
                Next
            End If
        End If
        
        Dim 范围域 As 节点_范围 Ptr = IIf( *编译信息指针->现行节点 Is 节点_范围, Cast(节点_范围 Ptr, 编译信息指针->现行节点), 0)
        While 范围域 <> 0
            For i As Integer = LBound(范围域->变量表) To UBound(范围域->变量表)
                If 范围域->变量表(i)->名称 = temp->名称 Then
                    表达式结果 = 对象管理.加入(New 表达式格式_变量(范围域->变量表(i)))
                    
                    Dim 容器序列 As Integer
                    Dim 节点索引 As 节点_范围 Ptr = Cast(节点_范围 Ptr, 编译信息指针->现行节点)->上一级
                    While 节点索引 <> 0
                        容器序列 = 容器序列 + ARRAYSIZE(节点索引->变量表)
                        节点索引 = 节点索引->上一级
                    Wend
                    容器序列 = 容器序列 + i
                    
                    项_返回 = New 项_局部变量
                    Cast(项_局部变量 Ptr, 项_返回)->容器序列 = 容器序列
                    FindOK = True : Exit While
                End If
            Next
            范围域 = 范围域->上一级
        Wend
        
        If FindOK = False Then
            If 编译信息指针->所属方法节点 <> 0 Then
                For i As Integer = LBound(编译信息指针->所属方法节点->参数表) To UBound(编译信息指针->所属方法节点->参数表)
                    If 编译信息指针->所属方法节点->参数表(i).名称 = temp->名称 Then
                        表达式结果 = 对象管理.加入(New 表达式格式_参数)
                        Cast(表达式格式_值 Ptr, 表达式结果)->值类型 = 编译信息指针->所属方法节点->参数表(i).类型
                        
                        项_返回 = New 项_参数
                        Cast(项_参数 Ptr, 项_返回)->参数序列 = i
                        FindOK = True : Exit For
                    End If
                Next
            End If
        End If
        
        If FindOK = False Then
            For i As Integer = LBound(编译信息指针->所属命名空间节点->类表) To UBound(编译信息指针->所属命名空间节点->类表)
                If 编译信息指针->所属命名空间节点->类表(i)->名称 = temp->名称 Then
                    表达式结果 = 对象管理.加入(New 表达式格式_类型(对象管理.加入(New 类型格式_类(编译信息指针->所属命名空间节点->类表(i)))))
                    FindOK     = True : Exit For
                End If
            Next
        End If
        
        If FindOK = False Then
            For i As Integer = LBound(编译信息指针->所属命名空间节点->命名空间表) To UBound(编译信息指针->所属命名空间节点->命名空间表)
                If 编译信息指针->所属命名空间节点->命名空间表(i)->名称 = temp->名称 Then
                    表达式结果 = 对象管理.加入(New 表达式格式_命名空间(编译信息指针->所属命名空间节点->命名空间表(i)))
                    FindOK     = True : Exit For
                End If
            Next
        End If
        
        If FindOK = False Then
            For i As Integer = LBound(编译信息指针->根节点->根命名空间->类表) To UBound(编译信息指针->根节点->根命名空间->类表)
                If 编译信息指针->根节点->根命名空间->类表(i)->名称 = temp->名称 Then
                    表达式结果 = 对象管理.加入(New 表达式格式_类型(对象管理.加入(New 类型格式_类(编译信息指针->根节点->根命名空间->类表(i)))))
                    FindOK     = True : Exit For
                End If
            Next
        End If
        
        If FindOK = False Then
            For i As Integer = LBound(编译信息指针->根节点->根命名空间->命名空间表) To UBound(编译信息指针->根节点->根命名空间->命名空间表)
                If 编译信息指针->根节点->根命名空间->命名空间表(i)->名称 = temp->名称 Then
                    表达式结果 = 对象管理.加入(New 表达式格式_命名空间(编译信息指针->根节点->根命名空间->命名空间表(i)))
                    FindOK     = True : Exit For
                End If
            Next
        End If
        
        If FindOK = False Then
            Print temp->名称
            错误信息("未知标识符") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
        End If
        
        链接式(模式, 编译信息指针, 项_返回)
        
    ElseIf 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_new Then
        词法器.取下一个单元()
        
        表达式(模式, 编译信息指针, 0)
        If *表达式结果 Is 表达式格式_类型 = False OrElse *Cast(表达式格式_类型 Ptr, 表达式结果)->类型 Is 类型格式_类=False Then 错误信息("期待类") : 代码块(模式, 编译信息指针) : Return
        
        Dim 类型   As 类型格式 Ptr = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
        Dim 类引用 As 节点_类 Ptr  = Cast(类型格式_类 Ptr, Cast(表达式格式_类型 Ptr, 表达式结果)->类型)->引用
        
        Dim 方法引用 As 节点_方法 Ptr
        
        方法引用 = 0
        For i As Integer = LBound(类引用->方法表) To UBound(类引用->方法表)
            If 类引用->方法表(i)->类型 = 方法类型.构造方法 Then
                方法引用 = 类引用->方法表(i) : Exit For
            End If
        Next
        
        If 方法引用 = 0 Then 错误信息("没有构造方法") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
        
        指针 = 词法器.取下一个单元()
        If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> "(" Then
            错误信息("期待符号'('") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
        End If
        
        Dim 结果项 As 项_调用 Ptr
        Dim 临时项 As 项 Ptr
        结果项 = New 项_调用
        Dim 索引         As Integer
        Dim 实参类型表() As 类型格式 Ptr
        项_返回 = 结果项
        
        下一个单元 = 词法器.窥视()
        While True
            If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = ")" Then
                词法器.取下一个单元()
                Exit While
            End If
            
            表达式(模式, 编译信息指针, 临时项)
            If *表达式结果 Is 表达式格式_值 = False Then 错误信息("期待值") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
            
            索引 = UBound(结果项->参数) + 1
            ReDim Preserve 结果项->参数(索引)
            结果项->参数(索引) = 临时项
            
            索引 = UBound(实参类型表) + 1
            ReDim Preserve 实参类型表(索引)
            实参类型表(索引) = Cast(表达式格式_值 Ptr, 表达式结果)->值类型
            
            下一个单元 = 词法器.窥视()
            If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "," Then
                词法器.取下一个单元()
                下一个单元 = 词法器.窥视()
            End If
        Wend
        
        方法引用 = 重载决议(类引用->方法表(), 对象管理.加入(New 决议线索_构造方法), 实参类型表())
        If 方法引用 = -1 Then 错误信息("找不到合适的方法") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
        If 方法引用 = 0  Then 错误信息("找到了多个能够接纳实参的方法") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
        结果项->方法 = 方法引用
        
        表达式结果 = 对象管理.加入(New 表达式格式_返回值(类型))
        链接式(模式, 编译信息指针, 项_返回)
    Else
        Print "未知"
        Dump_单元信息(指针)
        
        词法器.取下一个单元()
    End If
End Sub

Function 是否等同(类1 As 类型格式 Ptr, 类2 As 类型格式 Ptr) As BOOL
    If *类1 Is 类型格式_类 AndAlso *类2 Is 类型格式_类 Then
        
        Dim temp1 As 类型格式_类 Ptr = Cast(类型格式_类 Ptr, 类1), temp2 As 类型格式_类 Ptr = Cast(类型格式_类 Ptr, 类2)
        If LBound(temp1->泛参) = LBound(temp2->泛参) AndAlso UBound(temp1->泛参) = UBound(temp2->泛参) Then
            For i As Integer = LBound(temp1->泛参) To UBound(temp1->泛参)
                If 是否等同(temp1->泛参(i), temp2->泛参(i)) = False Then
                    Return False
                End If
            Next
        Else
            Return False
        End If
        
        Function = (temp1->引用 = temp2->引用)
    ElseIf *类1 Is 类型格式_方法 AndAlso *类2 Is 类型格式_方法 Then
        Dim temp1 As 类型格式_方法 Ptr = Cast(类型格式_方法 Ptr, 类1), temp2 As 类型格式_方法 Ptr = Cast(类型格式_方法 Ptr, 类2)
        
        If LBound(temp1->参数表) = LBound(temp2->参数表) AndAlso UBound(temp1->参数表) = UBound(temp2->参数表) Then
            For i As Integer = LBound(temp1->参数表) To UBound(temp1->参数表)
                If 是否等同(temp1->参数表(i), temp2->参数表(i)) = False Then
                    Return False
                End If
            Next
            
            If temp1->是否函数 = True AndAlso temp2->是否函数 = True Then
                Function = 是否等同(temp1->返回值, temp2->返回值)
            ElseIf temp1->是否函数 = False AndAlso temp2->是否函数 = False Then
                Function = True
            Else
                Function = False
            End If
        Else
            Function = False
        End If
        
    Else
        Function = False
    End If
End Function

Function 是否协变(起始类 As 类型格式 Ptr, 目标类 As 类型格式 Ptr, ByRef 矛盾值 As Integer) As BOOL
    矛盾值 = 0
    If *起始类 Is 类型格式_类 AndAlso *目标类 Is 类型格式_类 Then
        Dim 节点索引 As 节点_类 Ptr = Cast(类型格式_类 Ptr, 起始类)->引用
        While True
            If 节点索引 = 0 Then
                Exit While
            End If
            
            If 节点索引 = Cast(类型格式_类 Ptr, 目标类)->引用 Then
                Return True
            End If
            矛盾值 = 矛盾值 + 1
            
            节点索引 = 节点索引->父类
        Wend
        
        Function = False
    Else
        Function = 是否等同(起始类, 目标类)
    End If
End Function


Type 决议线索 Extends Object : End Type

Type 决议线索_方法名 Extends 决议线索
   是否静态 As BOOL
   名称 As String
   Declare Constructor(是否静态 As BOOL, 名称 As String)
End Type
Constructor 决议线索_方法名(是否静态 As BOOL, 名称 As String)
   This.是否静态 = 是否静态
   This.名称 = 名称
End Constructor
Type 决议线索_运算符 Extends 决议线索
   类型 As 运算符类型
   Declare Constructor(类型 As 运算符类型)
End Type
Constructor 决议线索_运算符(类型 As 运算符类型)
   This.类型 = 类型
End Constructor

Type 决议线索_构造方法 Extends 决议线索
    
End Type



'返回值问题
'-1      找不到合适的方法
'0       找到了多个能够接纳实参的方法
Function 重载决议(方法表() As 节点_方法 Ptr, 线索 As 决议线索 Ptr, 实参类型表() As 类型格式 Ptr) As 节点_方法 Ptr
    Dim 矛盾值_对应表 As 表.映射表
    For i As Integer = LBound(方法表) To UBound(方法表)
        Dim temp   As 节点_方法 Ptr = 方法表(i)
        Dim 矛盾值 As Integer       = 0
        
        If LBound(temp->参数表) <> LBound(实参类型表) OrElse UBound(temp->参数表) <> UBound(实参类型表) Then
            Continue For
        End If
        
        If *线索 Is 决议线索_方法名 Then
            If Cast(决议线索_方法名 Ptr, 线索)->是否静态 Then
                If temp->类型 <> 方法类型.静态方法 Then
                    Continue For
                End If
            Else
                If temp->类型 <> 方法类型.实例方法 Then
                    Continue For
                End If
            End If
            If temp->名称 <> Cast(决议线索_方法名 Ptr, 线索)->名称 Then
                Continue For
            End If
        ElseIf *线索 Is 决议线索_运算符 Then
            If temp->是否运算符 = False OrElse Cast(决议线索_运算符 Ptr, 线索)->类型 <> temp->运算符 Then
                Continue For
            End If
        ElseIf *线索 Is 决议线索_构造方法 Then
            If temp->类型 <> 方法类型.构造方法 Then
                Continue For
            End If
        End If
        
        Dim fail As BOOL
        
        fail = False
        For j As Integer = LBound(temp->参数表) To UBound(temp->参数表)
            Dim 临时矛盾值 As Integer = 0
            If 是否协变(实参类型表(j), temp->参数表(j).类型, 临时矛盾值) Then
                矛盾值 = 矛盾值 + 临时矛盾值
            Else
                fail = True
                Exit For
            End If
        Next
        If fail = True Then
            Continue For
        End If
        矛盾值_对应表.加入(temp, 矛盾值)
    Next
    
    If ARRAYSIZE(矛盾值_对应表.表) = 0 Then
        Function = -1
    ElseIf ARRAYSIZE(矛盾值_对应表.表) = 1 Then
        Function = 矛盾值_对应表.表(0).起始
    Else
        Dim 最小值索引 As Integer
        Dim 最小值     As Integer
        
        For i As Integer = LBound(矛盾值_对应表.表) To UBound(矛盾值_对应表.表)
            最小值索引 = 0
            For j As Integer = LBound(矛盾值_对应表.表) To UBound(矛盾值_对应表.表)
                If 矛盾值_对应表.表(j).目标 < 最小值 OrElse 最小值索引 = 0 Then
                    最小值     = 矛盾值_对应表.表(j).目标
                    最小值索引 = j
                End If
            Next
            Dim temp As 表.映射项
            temp = 矛盾值_对应表.表(i)
            矛盾值_对应表.表(i)          = 矛盾值_对应表.表(最小值索引)
            矛盾值_对应表.表(最小值索引) = temp
        Next
        If 矛盾值_对应表.表(0).目标 = 矛盾值_对应表.表(1).目标 Then
            Function = 0
        Else
            Function = 矛盾值_对应表.表(0).起始
        End If
    End If
End Function


Sub 链接式(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr, ByRef 项_返回 As 项 Ptr)
    Dim 第一个单元 As 词法器.单元信息 Ptr
    Dim 第二个单元 As 词法器.单元信息 Ptr
    Dim 第三个单元 As 词法器.单元信息 Ptr
    词法器.三连窥视(第一个单元, 第二个单元, 第三个单元)
    
    If *表达式结果 Is 表达式格式_命名空间 Then
        If 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = ":" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = ":" AndAlso 第三个单元->类型 = 词法器.单元类型.标识符 Then
            词法器.取下一个单元() : 词法器.取下一个单元()
            Dim 指针         As 词法器.单元信息_标识符 Ptr = Cast(词法器.单元信息_标识符 Ptr, 词法器.取下一个单元())
            Dim FindOK       As BOOL
            Dim 命名空间引用 As 节点_命名空间 Ptr = Cast(表达式格式_命名空间 Ptr, 表达式结果)->引用
            
            FindOK = False
            For i As Integer = LBound(命名空间引用->类表) To UBound(命名空间引用->类表)
                If 命名空间引用->类表(i)->名称 = 指针->名称 Then
                    表达式结果 = 对象管理.加入(New 表达式格式_类型(对象管理.加入(New 类型格式_类(命名空间引用->类表(i)))))
                    FindOK     = True : Exit For
                End If
            Next
            If FindOK = False Then
                For i As Integer = LBound(命名空间引用->命名空间表) To UBound(命名空间引用->命名空间表)
                    If 命名空间引用->命名空间表(i)->名称 = 指针->名称 Then
                        表达式结果 = 对象管理.加入(New 表达式格式_命名空间(命名空间引用->命名空间表(i)))
                        FindOK     = True : Exit For
                    End If
                Next
                If FindOK = False Then
                    错误信息("未知标识符") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
                End If
            End If
            链接式(模式, 编译信息指针, 项_返回)
        End If
    ElseIf *表达式结果 Is 表达式格式_类型 AndAlso *Cast(表达式格式_类型 Ptr, 表达式结果)->类型 Is 类型格式_类 Then
        If 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = ":" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = ":" AndAlso 第三个单元->类型 = 词法器.单元类型.标识符 Then
            词法器.取下一个单元() : 词法器.取下一个单元()
            Dim 指针     As 词法器.单元信息_标识符 Ptr = Cast(词法器.单元信息_标识符 Ptr, 词法器.取下一个单元())
            Dim 类引用   As 节点_类 Ptr = Cast(类型格式_类 Ptr, Cast(表达式格式_类型 Ptr, 表达式结果)->类型)->引用
            Dim 方法引用 As 节点_方法 Ptr
            
            方法引用 = 0
            For i As Integer = LBound(类引用->方法表) To UBound(类引用->方法表)
                If 类引用->方法表(i)->名称 = 指针->名称 Then
                    方法引用 = 类引用->方法表(i) : Exit For
                End If
            Next
            
            If 方法引用 = 0 Then 错误信息("未知标识符") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
            
            Dim 下一个单元 As 词法器.单元信息 Ptr = 词法器.窥视()
            If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "(" Then
                词法器.取下一个单元()
                
                Dim 结果项 As 项_调用 Ptr
                Dim 临时项 As 项 Ptr
                结果项 = New 项_调用
                Dim 索引         As Integer
                Dim 实参类型表() As 类型格式 Ptr
                项_返回 = 结果项
                
                下一个单元 = 词法器.窥视()
                While True
                    If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = ")" Then
                        词法器.取下一个单元()
                        Exit While
                    End If
                    
                    表达式(模式, 编译信息指针, 临时项)
                    If *表达式结果 Is 表达式格式_值 = False Then 错误信息("期待值") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
                    
                    索引 = UBound(结果项->参数) + 1
                    ReDim Preserve 结果项->参数(索引)
                    结果项->参数(索引) = 临时项
                    
                    索引 = UBound(实参类型表) + 1
                    ReDim Preserve 实参类型表(索引)
                    实参类型表(索引) = Cast(表达式格式_值 Ptr, 表达式结果)->值类型
                    
                    下一个单元 = 词法器.窥视()
                    If 下一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 下一个单元)->值 = "," Then
                        词法器.取下一个单元()
                        下一个单元 = 词法器.窥视()
                    End If
                Wend
                方法引用 = 重载决议(类引用->方法表(), 对象管理.加入(New 决议线索_方法名(True, 指针->名称)), 实参类型表())
                If 方法引用 = -1 Then 错误信息("找不到合适的方法") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
                If 方法引用 = 0  Then 错误信息("找到了多个能够接纳实参的方法") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
                结果项->方法 = 方法引用
                
                If 方法引用->是否函数 Then
                    表达式结果 = 对象管理.加入(New 表达式格式_返回值(方法引用->返回值))
                Else
                    表达式结果 = 对象管理.加入(New 表达式格式_空白)
                End If
            Else
                表达式结果 = 对象管理.加入(New 表达式格式_方法(方法引用))
            End If
            
            链接式(模式, 编译信息指针, 项_返回)
            
        ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "." AndAlso 第二个单元->类型 = 词法器.单元类型.标识符 Then
            词法器.取下一个单元()
            Dim 指针         As 词法器.单元信息_标识符 Ptr = Cast(词法器.单元信息_标识符 Ptr, 词法器.取下一个单元())
            Dim 类引用       As 节点_类 Ptr = Cast(类型格式_类 Ptr, Cast(表达式格式_类型 Ptr, 表达式结果)->类型)->引用
            Dim 静态变量引用 As 静态变量 Ptr
            
            静态变量引用 = 0
            For i As Integer = LBound(类引用->静态变量表) To UBound(类引用->静态变量表)
                If 类引用->静态变量表(i)->名称 = 指针->名称 Then
                    静态变量引用 = 类引用->静态变量表(i) : Exit For
                End If
            Next
            
            If 静态变量引用 = 0 Then 错误信息("未知标识符") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
            
            项_返回 = New 项_静态变量
            Cast(项_静态变量 Ptr, 项_返回)->引用 = 静态变量引用
            
            表达式结果 = 对象管理.加入(New 表达式格式_变量(静态变量引用))
            
            链接式(模式, 编译信息指针, 项_返回)
            
        ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "<" Then
            词法器.取下一个单元()
            Dim 类型引用 As 类型格式_类 Ptr = Cast(类型格式_类 Ptr, Cast(表达式格式_类型 Ptr, 表达式结果)->类型)
            Dim 类引用 As 节点_类 Ptr         = Cast(类型格式_类 Ptr, Cast(表达式格式_类型 Ptr, 表达式结果)->类型)->引用
            Dim 索引   As Integer
            Dim 指针   As 词法器.单元信息 Ptr = 词法器.窥视()
            While True
                If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = ">" Then
                    词法器.取下一个单元()
                    Exit While
                End If
                
                表达式(模式, 编译信息指针, 0)
                If *表达式结果 Is 表达式格式_类型 = False Then 错误信息("期待类型") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
                
                索引 = UBound(类型引用->泛参) + 1
                ReDim Preserve 类型引用->泛参(索引)
                类型引用->泛参(索引) = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
                
                指针 = 词法器.窥视()
                If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "," Then
                    词法器.取下一个单元()
                    指针 = 词法器.窥视()
                End If
            Wend
            
            表达式结果 = 对象管理.加入(New 表达式格式_类型(类型引用))
            链接式(模式, 编译信息指针, 项_返回)
        End If
        
    ElseIf *表达式结果 Is 表达式格式_值 Then
        
        If *Cast(表达式格式_值 Ptr, 表达式结果)->值类型 Is 类型格式_类 AndAlso 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "." AndAlso 第二个单元->类型 = 词法器.单元类型.标识符 Then
            词法器.取下一个单元()
            Dim 指针 As 词法器.单元信息_标识符 Ptr = Cast(词法器.单元信息_标识符 Ptr, 词法器.取下一个单元())
            
            Dim 类型引用 As 类型格式_类 Ptr = Cast(类型格式_类 Ptr, Cast(表达式格式_值 Ptr, 表达式结果)->值类型)
            Dim 类引用 As 节点_类 Ptr = Cast(类型格式_类 Ptr, Cast(表达式格式_值 Ptr, 表达式结果)->值类型)->引用
            
            Dim 结果项 As 项_成员变量 Ptr
            Dim 临时项 As 项_变量 Ptr = Cast(项_变量 Ptr, 项_返回)
            结果项  = New 项_成员变量
            项_返回 = 结果项
            
            结果项->变量 = 临时项
            
            Dim 实例定位下标 As Integer
            Dim 节点索引 As 节点_类 Ptr = 类引用->父类
            While 节点索引 <> 0
                实例定位下标 = 实例定位下标 + ARRAYSIZE(节点索引->成员表)
                节点索引 = 节点索引->父类
            Wend
            
            Dim 成员变量引用 As 成员变量 Ptr
            
            成员变量引用 = 0
            For i As Integer = LBound(类引用->成员表) To UBound(类引用->成员表)
                If 类引用->成员表(i)->名称 = 指针->名称 Then
                    成员变量引用 = 类引用->成员表(i)
                    结果项->实例定位序列 = 实例定位下标 + i
                    Exit For
                End If
            Next
            
            If 成员变量引用 = 0 Then 错误信息("未知标识符") : 表达式结果 = 对象管理.加入(New 表达式格式_空白) : Return
            表达式结果 = 对象管理.加入(New 表达式格式_变量(成员变量引用))
            
            If *成员变量引用->类型 Is 类型格式_泛型类 Then
                Cast(表达式格式_值 Ptr,表达式结果)->值类型 = 类型引用->泛参(Cast(类型格式_泛型类 Ptr, 成员变量引用->类型)->索引)
            End If
            
            链接式(模式, 编译信息指针, 项_返回)
        ElseIf 第一个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第一个单元)->值 = "=" AndAlso 第二个单元->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 第二个单元)->值 = ">" Then
            '强制转换
            词法器.取下一个单元() : 词法器.取下一个单元()
            
            Dim 临时 As 表达式格式 Ptr = 表达式结果
            
            表达式(模式, 编译信息指针, 0)
            If *表达式结果 Is 表达式格式_类型 = False Then 错误信息("期待类型") : 代码块(模式, 编译信息指针) : Return
            
            Cast(表达式格式_值 Ptr, 临时)->值类型 = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
            
            表达式结果 = 临时
            链接式(模式, 编译信息指针, 项_返回)
        End If
        
    End If
    
End Sub

Sub 功能_匿名参数表(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr, 参数表() As 类型格式 Ptr, ByRef 是否错误_返回 As BOOL)
    Dim 指针 As 词法器.单元信息 Ptr
    
    指针 = 词法器.取下一个单元()
    If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> "(" Then
        错误信息("期待符号'('") : 代码块(模式, 编译信息指针) : 是否错误_返回 = True : Return
    End If
    
    指针 = 词法器.取下一个单元()
    
    Dim 索引 As Integer
    
    While True
        If (指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = ")") Then
            Exit While
        End If
        
        If 指针->类型 <> 词法器.单元类型.关键字 OrElse Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 <> 词法器.关键字类型.key_as Then
            错误信息("期待'as'") : 代码块(模式, 编译信息指针) : 是否错误_返回 = True : Return
        End If
        
        索引 = UBound(参数表) + 1
        ReDim Preserve 参数表(索引)
        
        表达式(模式, 编译信息指针, 0)
        If *表达式结果 Is 表达式格式_类型 = False Then 错误信息("期待类型") : 代码块(模式, 编译信息指针) : 是否错误_返回 = True : Return
        参数表(索引) = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
        
        指针 = 词法器.取下一个单元()
        If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "," Then
            指针 = 词法器.取下一个单元()
        End If
    Wend
End Sub

Sub 功能_参数表(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr,参数表() As 参数信息,ByRef 是否错误_返回 As BOOL)
   Dim 指针 As 词法器.单元信息 Ptr
   
   指针 = 词法器.取下一个单元()
   If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr,指针)->值<>"(" Then
      错误信息("期待符号'('"):代码块(模式,编译信息指针):是否错误_返回=True:Return
   End If
   
   指针 = 词法器.取下一个单元()
   
   Dim 索引 As Integer
   
   While True
      If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = ")" Then
         Exit While
      End If
      
      索引 = UBound(参数表) + 1
      ReDim Preserve 参数表(索引)
      
      参数表(索引).名称 = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
      
      指针 = 词法器.取下一个单元()
      If 指针->类型 <> 词法器.单元类型.关键字 OrElse Cast(词法器.单元信息_关键字 Ptr,指针)->类型<>词法器.关键字类型.key_as Then
         错误信息("期待'as'"):代码块(模式,编译信息指针):是否错误_返回=True:Return
      End If
      
      表达式(模式, 编译信息指针,0)
      If *表达式结果 Is 表达式格式_类型 = False Then 错误信息("期待类型"):代码块(模式,编译信息指针):是否错误_返回=True:Return
      参数表(索引).类型 = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
      
      指针 = 词法器.取下一个单元()
      
      If 指针->类型 = 词法器.单元类型.符号 AndAlso Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "," Then
         指针 = 词法器.取下一个单元()
      End If
   Wend
End Sub




Sub 语法_方法(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr, 是否函数 As BOOL)
    Dim 指针 As 词法器.单元信息 Ptr
    
    Dim 是否本地     As BOOL
    Dim 类型         As 方法类型
    Dim 方法签名     As String
    Dim 方法签名摘要 As ULongInt
    Dim 方法名       As String
    
    Dim 是否运算符     As BOOL = False
    Dim 重载运算符类型 As 运算符类型
    
    If *编译信息指针->现行节点 Is 节点_类 = False Then
        错误信息("在非'类域'创建'方法'") : 代码块(模式, 编译信息指针) : Return
    End If
    
    Dim super As 节点_类 Ptr = Cast(节点_类 Ptr, 编译信息指针->现行节点)
    
    指针 = 词法器.取下一个单元()
    While 指针->类型 = 词法器.单元类型.关键字
        If Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_native Then
            是否本地 = True
        ElseIf Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_static Then
            类型 = 方法类型.静态方法
        Else
            Exit While
        End If
        指针 = 词法器.取下一个单元()
    Wend
    
    If 类型 = 方法类型.静态方法 Then
        If 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_operator Then
            If 是否关系运算符(重载运算符类型) OrElse 是否加减运算符(重载运算符类型) OrElse 是否项运算符(重载运算符类型) OrElse 是否位运算符(重载运算符类型) Then
            Else
                词法器.取下一个单元()
                错误信息("不能重载此符号") : 代码块(模式, 编译信息指针) : Return
            End If
            
            是否运算符 = True
            If 重载运算符类型 = 运算符类型.等于 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.不等于 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.大于等于 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.小于等于 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.大于 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.小于 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号大于等于 Then
                词法器.取下一个单元() : 词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号小于等于 Then
                词法器.取下一个单元() : 词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号大于 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号小于 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.加 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.减 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.乘 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.除 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.余 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.幂 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.整除 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号乘 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号除 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号余 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.无符号整除 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.与 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.或 Then
                词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.左移 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.右移 Then
                词法器.取下一个单元() : 词法器.取下一个单元()
            ElseIf 重载运算符类型 = 运算符类型.异或 Then
                词法器.取下一个单元()
            End If
            
            Print "运算符", 类型
            方法签名 = IIf(是否函数, "T", "F") + IIf(是否本地, "T", "F") + Str(类型) + "%" + Str(重载运算符类型)
            
        ElseIf 指针->类型 = 词法器.单元类型.标识符 Then
            方法签名 = IIf(是否函数, "T", "F") + IIf(是否本地, "T", "F") + Str(类型) + Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
            方法名   = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
        Else
            错误信息("期待标识符或者关键字'operator'") : 代码块(模式, 编译信息指针) : Return
        End If
    Else
        If 指针->类型 = 词法器.单元类型.符号 Then
            类型 = 方法类型.析构方法
            指针 = 词法器.取下一个单元()
            If 指针->类型 <> 词法器.单元类型.关键字 OrElse Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 <> 词法器.关键字类型.key_self Then
                错误信息("期待符号关键字'self'") : 代码块(模式, 编译信息指针) : Return
            End If
            方法签名 = IIf(是否函数, "T", "F") + IIf(是否本地, "T", "F") + Str(类型) + "!"
        Else
            If 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_self Then
                类型     = 方法类型.构造方法
                方法签名 = IIf(是否函数, "T", "F") + IIf(是否本地, "T", "F") + Str(类型) + "$"
            ElseIf 指针->类型 = 词法器.单元类型.标识符 Then
                类型     = 方法类型.实例方法
                方法签名 = IIf(是否函数, "T", "F") + IIf(是否本地, "T", "F") + Str(类型) + Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
                方法名   = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
            Else
                错误信息("期待标识符或者关键字'self'") : 代码块(模式, 编译信息指针) : Return
            End If
        End If
    End If
    
    
    Dim temp As 节点_方法 Ptr
    
    If 模式 = 解析模式.声明模式 Then
        temp             = 对象管理.加入(New 节点_方法())
        temp->用户序号   = 计数_用户方法
        计数_用户方法    = 计数_用户方法 + 1
        temp->绑定类     = super
        temp->名称       = 方法名
        temp->主范围     = 对象管理.加入(New 节点_范围())
        temp->是否函数   = 是否函数
        temp->类型       = 类型
        temp->是否运算符 = 是否运算符
        temp->运算符     = 重载运算符类型
        temp->是否本地   = 是否本地
        
        If 是否运算符 Then
            Dim 索引 As Integer
            索引 = UBound(编译信息指针->运算符方法表) + 1
            ReDim Preserve 编译信息指针->运算符方法表(索引)
            编译信息指针->运算符方法表(索引) = temp
        End If
        If 是否本地 = False Then
            方法映射.加入(temp, 对象管理.加入(New 方法信息))
        End If
        
        检查_跳过()
        If 是否本地 Then
            指针 = 词法器.取下一个单元()
            If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> ";" Then
                错误信息("期待符号';'") : 代码块(模式, 编译信息指针) : Return
            End If
        Else
            检查_跳过()
        End If
        Dim 索引 As Integer
        索引 = UBound(super->方法表) + 1
        ReDim Preserve super->方法表(索引)
        super->方法表(索引) = temp
        Print "创建方法", temp->名称
    ElseIf 模式 = 解析模式.定义模式 OrElse 模式 = 解析模式.流程模式 Then
        For i As Integer = LBound(super->方法表) To UBound(super->方法表)
            If super->方法表(i)->用户序号 = 计数_用户方法 Then
                temp = super->方法表(i)
                Exit For
            End If
        Next
        计数_用户方法 = 计数_用户方法 + 1
        Print "找到方法", temp->名称
        
        If 模式 = 解析模式.定义模式 Then
            Dim 是否错误 As BOOL
            功能_参数表(模式, 编译信息指针, temp->参数表(), 是否错误)
            If 是否错误 Then Return
            
            Dim 临时文本 As String
            
            方法签名 = 方法签名 + "("
            生成文式_参数表(temp->参数表(), 方法签名)
            方法签名 = 方法签名 + ")"
            
            If 是否函数 Then
                指针 = 词法器.取下一个单元()
                If 指针->类型 <> 词法器.单元类型.关键字 OrElse Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 <> 词法器.关键字类型.key_as Then
                    错误信息("期待'as'") : 代码块(模式, 编译信息指针) : Return
                End If
                
                表达式(模式, 编译信息指针, 0)
                If *表达式结果 Is 表达式格式_类型 = False Then 错误信息("期待类型") : 代码块(模式, 编译信息指针) : Return
                temp->返回值 = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
                
                方法签名 = 方法签名 + "->"
                生成文式_类型(temp->返回值, 方法签名)
            End If
            
            方法签名摘要 = 哈希算法.MD5u64(方法签名)
            
            For i As Integer = LBound(super->方法表) To UBound(super->方法表)
                If super->方法表(i)->方法签名摘要 = 方法签名摘要 then
                    错误信息("重复定义")
                    Exit For
                End If
            Next
            
            temp->方法签名摘要 = 方法签名摘要
            
            Print "方法签名", 方法签名摘要, 方法签名
            
            指针 = 词法器.取下一个单元()
            If 指针->类型 <> 词法器.单元类型.跃点 Then
                错误信息("期待符号'@'") : 代码块(模式, 编译信息指针) : Return
            End If
            If 是否本地 Then
                指针 = 词法器.取下一个单元()
                If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> ";" Then
                    错误信息("期待符号';'") : 代码块(模式, 编译信息指针) : Return
                End If
            Else
                检查_跳过()
            End If
            
        ElseIf 模式 = 解析模式.流程模式 Then
            检查_跳过()
            If 是否本地 Then
                指针 = 词法器.取下一个单元()
                If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> ";" Then
                    错误信息("期待符号';'") : 代码块(模式, 编译信息指针) : Return
                End If
            Else
                指针 = 词法器.取下一个单元()
                If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> "{" Then
                    错误信息("期待符号'{'") : 代码块(模式, 编译信息指针) : Return
                End If
                
                Dim 上一级方法节点 As 节点_方法 Ptr = 编译信息指针->所属方法节点
                编译信息指针->所属方法节点 = temp
                编译信息指针->现行节点     = temp->主范围
                
                代码块(模式, 编译信息指针)
                
                编译信息指针->所属方法节点 = 上一级方法节点
                编译信息指针->现行节点     = super
                
                指针 = 词法器.取下一个单元()
                If 指针->类型 <> 词法器.单元类型.跃点 Then
                    错误信息("期待符号'@'") : 代码块(模式, 编译信息指针) : Return
                End If
            End If
        End If
    End If
    
    代码块(模式, 编译信息指针)
End Sub


Type 类型格式 Extends Object
    
End Type

Type 节点 Extends Object
    
End Type

Type 参数信息
    名称 As String
    类型 As 类型格式 Ptr
End Type

Type 变量信息 Extends Object
    名称 As String
    类型 As 类型格式 Ptr
End Type
Type 局部变量 Extends 变量信息
    
End Type




Type 节点_类_别名   As 节点_类
Type 节点_方法_别名 As 节点_方法



Type 别名_项 As 项

Type 节点_范围 Extends 节点
    上一级 As 节点_范围 Ptr
    
    变量表(Any) As 局部变量 Ptr
    项表(Any)   As 别名_项 Ptr
End Type


Enum 方法类型
    实例方法
    静态方法
    构造方法
    析构方法
End Enum

Type 节点_方法 Extends 节点
    绑定类 As 节点_类_别名 Ptr
    
    是否本地 As BOOL
    
    类型 As 方法类型
    
    序号     As Integer
    用户序号 As Integer
    
    是否函数 As BOOL
    返回值   As 类型格式 Ptr
    
    是否运算符 As BOOL
    运算符     As 运算符类型
    
    名称        As String
    参数表(Any) As 参数信息
    主范围      As 节点_范围 Ptr
    
    方法签名摘要 As ULongInt
    
    完成 As BOOL
End Type

Type 项 Extends Object
    丢弃值 As BOOL
End Type

Type 项_声明局部变量 Extends 项
    容器序列 As Integer
End Type



Type 项_范围 Extends 项
    容器序列下标 As Integer
    引用         As 节点_范围 Ptr
End Type

Type 项_参数 Extends 项
    参数序列 As Integer
End Type

Type 项_变量 Extends 项
    引用 As 变量信息 Ptr
End Type

Type 项_局部变量 Extends 项_变量
    容器序列 As Integer
End Type
Type 项_成员变量 Extends 项_变量
    变量         As 项_变量 Ptr
    实例定位序列 As Integer
End Type

Type 项_静态变量 Extends 项_变量
    引用 As 别名_静态变量 Ptr
End Type

Type 项_赋值 Extends 项
    变量 As 项_变量 Ptr
    值   As 项 Ptr
End Type

Type 别名_表达式格式_字面量 As 表达式格式_字面量

Type 项_字面量 Extends 项
    值 As 别名_表达式格式_字面量 Ptr
End Type

Type 项_调用 Extends 项
    方法      As 节点_方法 Ptr
    参数(Any) As 项 Ptr
End Type


Type 项_返回值 Extends 项
    值 As 项 Ptr
End Type

Type 项_如果 Extends 项
    条件 As 项 Ptr
    真域 As 项_范围 Ptr
    假域 As 项_范围 Ptr
End Type

Type 成员变量 Extends 变量信息
    绑定类 As 节点_类_别名 Ptr
End Type
Type 静态变量 Extends 变量信息
    绑定类 As 节点_类_别名 Ptr
End Type

Type 别名_节点_命名空间 As 节点_命名空间


Type 泛型信息
    名称 As String
    
    继承 As 节点_类_别名 Ptr
    
    引用类型表 As 表.线性表 Ptr
End Type


Type 节点_类 Extends 节点
    绑定命名空间 As 别名_节点_命名空间 Ptr
    
    用户类 As BOOL '如果是导入的类则不是用户类，不会进行模块生成
    
    父类 As 节点_类 Ptr
    
    名称            As String
    方法表(Any)     As 节点_方法 Ptr
    成员表(Any)     As 成员变量 Ptr
    静态变量表(Any) As 静态变量 Ptr
    
    泛型表(Any) As 泛型信息 Ptr
    
    完成 As BOOL
End Type





Type 节点_命名空间 Extends 节点
    上一级 As 节点_命名空间 Ptr
    
    名称            As String
    类表(Any)       As 节点_类 Ptr
    命名空间表(Any) As 节点_命名空间 Ptr
End Type

Type 节点_根 Extends 节点
    根命名空间 As 节点_命名空间 Ptr
End Type




Type 类型格式_类 Extends 类型格式
    引用 As 节点_类 Ptr
    
    泛参(Any) As 类型格式 Ptr
    
    Declare Constructor(引用 As 节点_类 Ptr=0)
End Type


Constructor 类型格式_类(引用 As 节点_类 Ptr=0)
    This.引用 = 引用
End Constructor

Type 类型格式_泛型类 Extends 类型格式_类
    索引 As Integer
    Declare Constructor(引用 As 节点_类 Ptr,索引 As Integer)
End Type

Constructor 类型格式_泛型类(引用 As 节点_类 Ptr,索引 As Integer)
    Base.引用 = 引用
    This.索引 = 索引
End Constructor

Type 类型格式_方法 Extends 类型格式
    是否函数    As BOOL
    返回值      As 类型格式 Ptr
    参数表(Any) As 类型格式 Ptr
End Type


Type 表达式格式 Extends Object
    
End Type

Type 表达式格式_空白 Extends 表达式格式
    
End Type
Type 表达式格式_命名空间 Extends 表达式格式
    引用 As 节点_命名空间 Ptr
    Declare Constructor(引用 As 节点_命名空间 Ptr)
End Type
Constructor 表达式格式_命名空间(引用 As 节点_命名空间 Ptr)
    This.引用 = 引用
End Constructor

Type 表达式格式_类型 Extends 表达式格式
    类型 As 类型格式 Ptr
    Declare Constructor(类型 As 类型格式 Ptr = 0)
End Type
Constructor 表达式格式_类型(类型 As 类型格式 Ptr = 0)
    This.类型 = 类型
End Constructor



Type 表达式格式_值 Extends 表达式格式
    值类型 As 类型格式 Ptr
End Type

Type 表达式格式_返回值 Extends 表达式格式_值
    Declare Constructor(值类型 As 类型格式 Ptr)
End Type
Constructor 表达式格式_返回值(值类型 As 类型格式 Ptr)
    Base.值类型 = 值类型
End Constructor

Type 表达式格式_字面量 Extends 表达式格式_值
    
End Type

Type 表达式格式_方法 Extends 表达式格式_字面量
    引用 As 节点_方法 Ptr
    Declare Constructor(引用 As 节点_方法 Ptr)
End Type
Constructor 表达式格式_方法(引用 As 节点_方法 Ptr)
    This.引用 = 引用
End Constructor

Type 表达式格式_单精度浮点型 Extends 表达式格式_字面量
    值 As Single
    Declare Constructor(值 As Single)
End Type
Constructor 表达式格式_单精度浮点型(值 As Single)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.单精度浮点型
    This.值 = 值
End Constructor

Type 表达式格式_双精度浮点型 Extends 表达式格式_字面量
    值 As Double
    Declare Constructor(值 As Double)
End Type
Constructor 表达式格式_双精度浮点型(值 As Double)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.双精度浮点型
    This.值 = 值
End Constructor
Type 表达式格式_字节型 Extends 表达式格式_字面量
    值 As Byte
    Declare Constructor(值 As Byte)
End Type
Constructor 表达式格式_字节型(值 As Byte)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.字节型
    This.值 = 值
End Constructor

Type 表达式格式_短整数型 Extends 表达式格式_字面量
    值 As Short
    Declare Constructor(值 As Short)
End Type
Constructor 表达式格式_短整数型(值 As Short)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.短整数型
    This.值 = 值
End Constructor
Type 表达式格式_整数型 Extends 表达式格式_字面量
    值 As Long
    Declare Constructor(值 As Long)
End Type
Constructor 表达式格式_整数型(值 As Long)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.整数型
    This.值 = 值
End Constructor
Type 表达式格式_长整数型 Extends 表达式格式_字面量
    值 As LongInt
    Declare Constructor(值 As LongInt)
End Type
Constructor 表达式格式_长整数型(值 As LongInt)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.长整数型
    This.值 = 值
End Constructor

Type 表达式格式_逻辑型 Extends 表达式格式_字面量
    值 As BOOL
    Declare Constructor(值 As BOOL)
End Type
Constructor 表达式格式_逻辑型(值 As BOOL)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.逻辑型
    This.值 = 值
End Constructor

Type 表达式格式_字符串 Extends 表达式格式_字面量
    值 As String
    Declare Constructor(值 As String)
End Type
Constructor 表达式格式_字符串(值 As String)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 基本类型.文本型
    This.值 = 值
End Constructor

Type 表达式格式_变量 Extends 表达式格式_值
    变量 As 变量信息 Ptr
    Declare Constructor(变量 As 变量信息 Ptr)
End Type
Constructor 表达式格式_变量(变量 As 变量信息 Ptr)
    Cast(表达式格式_值 Ptr, @This)->值类型 = 变量->类型
    This.变量 = 变量
End Constructor

Type 表达式格式_参数 Extends 表达式格式_值
    
End Type

Type 编译信息
    现行节点 As 节点 Ptr
    根节点   As 节点_根 Ptr
    
    所属命名空间节点 As 节点_命名空间 Ptr
    所属方法节点     As 节点_方法 Ptr
    所属类节点       As 节点_类 Ptr
    
    运算符方法表(Any) As 节点_方法 Ptr
End Type

Sub Dump_单元信息(指针 As 词法器.单元信息 Ptr)
    Select Case 指针->类型
        Case 词法器.单元类型.标识符
            Print "标识符", Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
        Case 词法器.单元类型.数字
            Print "数字", Cast(词法器.单元信息_数字 Ptr, 指针)->值
        Case 词法器.单元类型.字符串
            Print "字符串", Cast(词法器.单元信息_字符串 Ptr, 指针)->值
        Case 词法器.单元类型.符号
            Print "符号", Cast(词法器.单元信息_符号 Ptr, 指针)->值
        Case 词法器.单元类型.关键字
            Print "关键字", Cast(词法器.单元信息_关键字 Ptr, 指针)->类型
    End Select
End Sub

Sub 语法_表达式(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
    Dim 结果项 As 项 Ptr
    Dim 指针   As 词法器.单元信息 Ptr
    表达式(模式, 编译信息指针, 结果项)
    If *编译信息指针->现行节点 Is 节点_范围 AndAlso 结果项 <> 0 Then
        Dim temp As 节点_范围 Ptr = Cast(节点_范围 Ptr, 编译信息指针->现行节点)
        Dim 索引 As Integer
        索引 = UBound(temp->项表) + 1
        ReDim Preserve temp->项表(索引)
        temp->项表(索引) = 结果项
        If *表达式结果 Is 表达式格式_值 Then
            结果项->丢弃值 = True
        End If
    End If
    
    指针 = 词法器.取下一个单元()
    If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> ";" Then
        错误信息("期待符号';'") : 代码块(模式, 编译信息指针) : Return
    End If
    
    代码块(模式, 编译信息指针)
End Sub

Sub 语法_范围(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
   Dim 指针 As 词法器.单元信息 Ptr
   
   If *编译信息指针->现行节点 Is 节点_范围=False Then
      错误信息("在非'范围域'创建'范围'"):代码块(模式,编译信息指针):Return
   End If
   
   指针 = 词法器.取下一个单元()
   If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr,指针)->值<>"{" Then
      错误信息("期待符号'{'"):代码块(模式,编译信息指针):Return
   End If
   
   
   Dim super As 节点_范围 Ptr = Cast(节点_范围 Ptr, 编译信息指针->现行节点)
   Dim temp As 节点_范围 Ptr = 对象管理.加入(New 节点_范围())
   temp->上一级 = super
   
   
   Dim 容器序列下标 As Integer
   Dim 节点索引 As 节点_范围 Ptr = super
   While 节点索引 <> 0
      容器序列下标 = 容器序列下标 + ARRAYSIZE(节点索引->变量表)
      节点索引 = 节点索引->上一级
   Wend

   
   Dim 结果项 As 项_范围 Ptr
   结果项 = New 项_范围
   结果项->引用 = temp
   结果项->容器序列下标 = 容器序列下标
   Dim 索引 As Integer
   索引 = UBound(super->项表) + 1
   ReDim Preserve super->项表(索引)
   super->项表(索引) = 结果项
   
   编译信息指针->现行节点 = temp
   代码块(模式, 编译信息指针)
   编译信息指针->现行节点 = super
   
   代码块(模式, 编译信息指针)
End Sub

Sub 语法_声明(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
    Dim 指针 As 词法器.单元信息 Ptr
    
    Dim 名称 As String
    Dim 类型 As 类型格式 Ptr
    
    If *编译信息指针->现行节点 Is 节点_范围 = False AndAlso *编译信息指针->现行节点 Is 节点_类 = False Then
        错误信息("在非'范围域'和'类域'声明") : 代码块(模式, 编译信息指针) : Return
    End If
    
    Dim 是否静态 As BOOL
    指针 = 词法器.窥视()
    If *编译信息指针->现行节点 Is 节点_类 AndAlso 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_static Then
        词法器.取下一个单元()
        是否静态 = True
    End If
    
    指针 = 词法器.取下一个单元()
    If 指针->类型 <> 词法器.单元类型.标识符 Then
        错误信息("期待标识符") : 代码块(模式, 编译信息指针) : Return
    End If
    名称 = Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
    
    指针 = 词法器.取下一个单元()
    If 指针->类型 <> 词法器.单元类型.关键字 OrElse Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 <> 词法器.关键字类型.key_as Then
        错误信息("期待'as'") : 代码块(模式, 编译信息指针) : Return
    End If
    
    
    If *编译信息指针->现行节点 Is 节点_范围 Then
        表达式(模式, 编译信息指针, 0)
        If *表达式结果 Is 表达式格式_类型 = False Then 错误信息("期待类型") : 代码块(模式, 编译信息指针) : Return
        类型 = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
        
        Dim temp As 节点_范围 Ptr = Cast(节点_范围 Ptr, 编译信息指针->现行节点)
        Dim 索引 As Integer
        
        索引 = UBound(temp->变量表) + 1
        ReDim Preserve temp->变量表(索引)
        temp->变量表(索引) = New 局部变量
        
        temp->变量表(索引)->名称 = 名称
        temp->变量表(索引)->类型 = 类型
        
        Dim 容器序列 As Integer
        Dim 节点索引 As 节点_范围 Ptr = temp
        While 节点索引 <> 0
            容器序列 = 容器序列 + ARRAYSIZE(节点索引->变量表)
            节点索引 = 节点索引->上一级
        Wend
        容器序列 = 容器序列 -1
        
        Dim 结果项 As 项_声明局部变量 Ptr
        结果项           = New 项_声明局部变量
        结果项->容器序列 = 容器序列
        索引 = UBound(temp->项表) + 1
        ReDim Preserve temp->项表(索引)
        
        temp->项表(索引) = 结果项
    ElseIf *编译信息指针->现行节点 Is 节点_类 Then
        If 模式 = 解析模式.声明模式 Then
            检查_跳过()
            Dim temp As 节点_类 Ptr = Cast(节点_类 Ptr, 编译信息指针->现行节点)
            Dim 索引 As Integer
            
            
            If 是否静态 Then
                索引 = UBound(temp->静态变量表) + 1
                ReDim Preserve temp->静态变量表(索引)
                temp->静态变量表(索引)         = 对象管理.加入(New 静态变量)
                temp->静态变量表(索引)->绑定类 = temp
                temp->静态变量表(索引)->名称   = 名称
            Else
                索引 = UBound(temp->成员表) + 1
                ReDim Preserve temp->成员表(索引)
                temp->成员表(索引)         = 对象管理.加入(New 成员变量)
                temp->成员表(索引)->绑定类 = temp
                temp->成员表(索引)->名称   = 名称
            End If
            
        ElseIf 模式 = 解析模式.定义模式 OrElse 模式 = 解析模式.流程模式 Then
            If 模式 = 解析模式.定义模式 Then
                表达式(模式, 编译信息指针, 0)
                If *表达式结果 Is 表达式格式_类型 = False Then 错误信息("期待类型") : 代码块(模式, 编译信息指针) : Return
                类型 = Cast(表达式格式_类型 Ptr, 表达式结果)->类型
                Dim temp As 节点_类 Ptr = Cast(节点_类 Ptr, 编译信息指针->现行节点)
                
                If 是否静态 Then
                    For i As Integer = LBound(temp->静态变量表) To UBound(temp->静态变量表)
                        If temp->静态变量表(i)->名称 = 名称 Then
                            temp->静态变量表(i)->类型 = 类型
                            Exit For
                        End If
                    Next
                Else
                    For i As Integer = LBound(temp->成员表) To UBound(temp->成员表)
                        If temp->成员表(i)->名称 = 名称 Then
                            temp->成员表(i)->类型 = 类型
                            Exit For
                        End If
                    Next
                End If
                
                指针 = 词法器.取下一个单元()
                If 指针->类型 <> 词法器.单元类型.跃点 Then
                    错误信息("期待符号'@'") : 代码块(模式, 编译信息指针) : Return
                End If
            ElseIf 模式 = 解析模式.流程模式 Then
                检查_跳过()
            End If
        End If
    End If
    
    指针 = 词法器.取下一个单元()
    If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr, 指针)->值 <> ";" Then
        错误信息("期待符号';'") : 代码块(模式, 编译信息指针) : Return
    End If
    
    Print "声明变量", 名称
    
    代码块(模式, 编译信息指针)
End Sub

Sub 语法_返回(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
   Dim 指针 As 词法器.单元信息 Ptr
   
   If *编译信息指针->现行节点 Is 节点_范围=False Then
      错误信息("在非'范围域'中返回"):代码块(模式,编译信息指针):Return
   End If
   
   Dim 结果项 As 项 Ptr,temp As 项_返回值 Ptr= 对象管理.加入(New 项_返回值)
   表达式(模式, 编译信息指针, 结果项)
   If *表达式结果 Is 表达式格式_值 = False Then 错误信息("期待值") : 代码块(模式, 编译信息指针) : Return
   temp->值 = 结果项
   
   Dim self As 节点_范围 Ptr = Cast(节点_范围 Ptr, 编译信息指针->现行节点), 索引 As Integer
   索引 = UBound(self->项表) + 1
   ReDim Preserve self->项表(索引)
   self->项表(索引) = temp
   
   指针 = 词法器.取下一个单元()
   If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr,指针)->值<>";" Then
      错误信息("期待符号';'"):代码块(模式,编译信息指针):Return
   End If
   
   代码块(模式, 编译信息指针)
End Sub

Sub 语法_如果(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
   Dim 指针 As 词法器.单元信息 Ptr
   
   If *编译信息指针->现行节点 Is 节点_范围=False Then
      错误信息("在非'范围域'中如果"):代码块(模式,编译信息指针):Return
   End If
   Dim self As 节点_范围 Ptr = Cast(节点_范围 Ptr, 编译信息指针->现行节点)
   
   Dim 返回项 As 项 Ptr
   表达式(模式, 编译信息指针, 返回项)
   
   If *表达式结果 Is 表达式格式_值 = False Then 错误信息("期待值") : 代码块(模式, 编译信息指针) : Return
   If 是否协变(Cast(表达式格式_值 Ptr, 表达式结果)->值类型, 基本类型.逻辑型, NULL) = False Then 错误信息("期待逻辑型值") : 代码块(模式, 编译信息指针) : Return
   
   Dim 结果项 As 项_如果 Ptr = 对象管理.加入(New 项_如果)
   结果项->条件 = 返回项
   
   Dim 索引 As Integer
   索引 = UBound(self->项表) + 1
   ReDim Preserve self->项表(索引)
   self->项表(索引) = 结果项
   
   
   Dim temp_true As 节点_范围 Ptr = 对象管理.加入(New 节点_范围()), temp_false As 节点_范围 Ptr = 对象管理.加入(New 节点_范围())
   temp_true->上一级 = self
   temp_false->上一级 = self
   
   Dim 容器序列下标 As Integer
   Dim 节点索引 As 节点_范围 Ptr = self
   While 节点索引 <> 0
      容器序列下标 = 容器序列下标 + ARRAYSIZE(节点索引->变量表)
      节点索引 = 节点索引->上一级
   Wend
   
   Dim 结果项_true As 项_范围 Ptr=对象管理.加入(New 项_范围)
   结果项_true->引用 = temp_true
   结果项_true->容器序列下标 = 容器序列下标
   结果项->真域 = 结果项_true
   
   
   指针 = 词法器.取下一个单元()
   If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr,指针)->值<>"{" Then
      错误信息("期待符号'{'"):代码块(模式,编译信息指针):Return
   End If
   编译信息指针->现行节点 = temp_true
   代码块(模式, 编译信息指针)
   编译信息指针->现行节点 = self
   
   指针 = 词法器.窥视()
   If 指针->类型 = 词法器.单元类型.关键字 AndAlso Cast(词法器.单元信息_关键字 Ptr, 指针)->类型 = 词法器.关键字类型.key_else Then
      词法器.取下一个单元()
      
      指针 = 词法器.取下一个单元()
      If 指针->类型 <> 词法器.单元类型.符号 OrElse Cast(词法器.单元信息_符号 Ptr,指针)->值<>"{" Then
         错误信息("期待符号'{'"):代码块(模式,编译信息指针):Return
      End If
      
      Dim 结果项_false As 项_范围 Ptr=对象管理.加入(New 项_范围)
      结果项_false->引用 = temp_false
      结果项_false->容器序列下标 = 容器序列下标
      结果项->假域 = 结果项_false
      
      编译信息指针->现行节点 = temp_false
      代码块(模式, 编译信息指针)
      编译信息指针->现行节点 = self
   End If
   
   代码块(模式, 编译信息指针)
End Sub



Sub 代码块(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
    If 词法器.是否代码尾() Then
        Print "代码解析完成"
        Return
    End If
    
    Dim 指针 As 词法器.单元信息 Ptr = 词法器.窥视()
    
    Select Case 指针->类型
        Case 词法器.单元类型.标识符
            Print "标识符", Cast(词法器.单元信息_标识符 Ptr, 指针)->名称
        Case 词法器.单元类型.数字
            Print "数字", Cast(词法器.单元信息_数字 Ptr, 指针)->值
        Case 词法器.单元类型.字符串
            Print "字符串", Cast(词法器.单元信息_字符串 Ptr, 指针)->值
        Case 词法器.单元类型.符号
            Print "符号", Cast(词法器.单元信息_符号 Ptr, 指针)->值
        Case 词法器.单元类型.关键字
            Print "关键字", Cast(词法器.单元信息_关键字 Ptr, 指针)->类型
    End Select
    
    Select Case 指针->类型
        Case 词法器.单元类型.关键字
            Select Case Cast(词法器.单元信息_关键字 Ptr, 指针)->类型
                Case 词法器.关键字类型.key_namespace
                    词法器.取下一个单元() : 语法_命名空间(模式, 编译信息指针)
                Case 词法器.关键字类型.key_class
                    词法器.取下一个单元() : 语法_类(模式, 编译信息指针)
                Case 词法器.关键字类型.key_sub
                    词法器.取下一个单元() : 语法_方法(模式, 编译信息指针, False)
                Case 词法器.关键字类型.key_function
                    词法器.取下一个单元() : 语法_方法(模式, 编译信息指针, True)
                Case 词法器.关键字类型.key_scope
                    词法器.取下一个单元() : 语法_范围(模式, 编译信息指针)
                Case 词法器.关键字类型.key_dim
                    词法器.取下一个单元() : 语法_声明(模式, 编译信息指针)
                Case 词法器.关键字类型.key_return
                    词法器.取下一个单元() : 语法_返回(模式, 编译信息指针)
                Case 词法器.关键字类型.key_if
                    词法器.取下一个单元() : 语法_如果(模式, 编译信息指针)
            End Select
        Case 词法器.单元类型.符号
            If Cast(词法器.单元信息_符号 Ptr, 指针)->值 = "}" Then '语法结束符
                词法器.取下一个单元()
                Print "语法结束"
                Return
            Else
                语法_表达式(模式, 编译信息指针)
            End If
        Case Else
            语法_表达式(模式, 编译信息指针)
    End Select
End Sub


Sub 解析器(模式 As 解析模式, 编译信息指针 As 编译信息 Ptr)
   计数_用户方法 = 0
   
   词法器.位置 = 1
   
   代码块(模式,编译信息指针)
End Sub


Sub 生成文式_命名空间(临时节点命名空间 As 节点_命名空间 Ptr, ByRef 临时文本 As String)
    Dim 节点索引 As 节点_命名空间 Ptr = 临时节点命名空间
    Dim temp     As String
    While True
        If 节点索引->上一级 = 0 Then
            Exit While
        End If
        temp     = 节点索引->名称 + temp
        节点索引 = 节点索引->上一级
        If 节点索引->上一级 <> 0 Then
            temp = "::" + temp
        End If
    Wend
    临时文本 = 临时文本 + temp
End Sub

Sub 生成文式_类(临时节点类 As 节点_类 Ptr, ByRef 临时文本 As String)
   生成文式_命名空间(临时节点类->绑定命名空间, 临时文本)
   临时文本 = 临时文本 + "::"
   临时文本 = 临时文本 + 临时节点类->名称
End Sub

Sub 生成文式_类型(临时类型格式 As 类型格式 Ptr, ByRef 临时文本 As String)
    If *临时类型格式 Is 类型格式_类 Then
        临时文本 = 临时文本 + "CLASS->"
        生成文式_类(Cast(类型格式_类 Ptr, 临时类型格式)->引用, 临时文本)
        生成文式_泛型(Cast(类型格式_类 Ptr, 临时类型格式), 临时文本)
    ElseIf *临时类型格式 Is 类型格式_方法 Then
        临时文本 = 临时文本 + "METHOD->"
        生成文式_方法(Cast(类型格式_方法 Ptr, 临时类型格式), 临时文本)
    End If
End Sub

Sub 生成文式_泛型(临时类型格式 As 类型格式_类 Ptr, ByRef 临时文本 As String)
    临时文本 = 临时文本 + "<"
    For i As Integer = LBound(临时类型格式->泛参) To UBound(临时类型格式->泛参)
        生成文式_类型(临时类型格式->泛参(i), 临时文本)
        If i <> UBound(临时类型格式->泛参) Then
            临时文本 = 临时文本 + ","
        End If
    Next
    临时文本 = 临时文本 + ">"
End Sub

Sub 生成文式_方法(临时类型格式方法 As 类型格式_方法 Ptr, ByRef 临时文本 As String)
    临时文本 = 临时文本 + IIf(临时类型格式方法->是否函数, "T", "F")
    If 临时类型格式方法->返回值 = 0 Then
        临时文本 = 临时文本 + "|VOID|"
    Else
        临时文本 = 临时文本 + "|"
        生成文式_类型(临时类型格式方法->返回值, 临时文本)
        临时文本 = 临时文本 + "|"
    End If
    生成文式_匿名参数表(临时类型格式方法->参数表(), 临时文本)
End Sub

Sub 生成文式_匿名参数表(参数表() As 类型格式 Ptr, ByRef 临时文本 As String)
    For i As Integer = LBound(参数表) To UBound(参数表)
        生成文式_类型(参数表(i), 临时文本)
        If i <> UBound(参数表) Then
            临时文本 = 临时文本 + ","
        End If
    Next
End Sub

Sub 生成文式_参数表(参数表() As 参数信息, ByRef 临时文本 As String) '会丢失参数名称
    For i As Integer = LBound(参数表) To UBound(参数表)
        生成文式_类型(参数表(i).类型, 临时文本)
        If i <> UBound(参数表) Then
            临时文本 = 临时文本 + ","
        End If
    Next
End Sub





Sub 生成项(临时项 As 项 Ptr, 结果 As 方法信息 Ptr)
    If *临时项 Is 项_范围 Then
        生成中间语言(Cast(项_范围 Ptr, 临时项)->引用, 结果, Cast(项_范围 Ptr, 临时项)->容器序列下标)
    ElseIf *临时项 Is 项_声明局部变量 Then
        结果->指令_创建变量(Cast(项_声明局部变量 Ptr, 临时项)->容器序列)
    ElseIf *临时项 Is 项_赋值 Then
        生成项(Cast(项_赋值 Ptr, 临时项)->值, 结果)
        If *Cast(项_赋值 Ptr, 临时项)->变量 Is 项_局部变量 Then
            结果->指令_保存(Cast(项_局部变量 Ptr, Cast(项_赋值 Ptr, 临时项)->变量)->容器序列)
        ElseIf *Cast(项_赋值 Ptr, 临时项)->变量 Is 项_静态变量 Then
            结果->指令_静态保存(Cast(项_静态变量 Ptr, Cast(项_赋值 Ptr, 临时项)->变量)->引用)
        ElseIf *Cast(项_赋值 Ptr, 临时项)->变量 Is 项_成员变量 Then
            生成项(Cast(项_成员变量 Ptr, Cast(项_赋值 Ptr, 临时项)->变量)->变量, 结果)
            结果->指令_成员保存(Cast(项_成员变量 Ptr, Cast(项_赋值 Ptr, 临时项)->变量)->实例定位序列)
        End If
        
    ElseIf *临时项 Is 项_字面量 Then
        If *Cast(项_字面量 Ptr, 临时项)->值 Is 表达式格式_整数型 Then
            结果->指令_加载整数(Cast(表达式格式_整数型 Ptr, Cast(项_字面量 Ptr, 临时项)->值)->值)
        ElseIf *Cast(项_字面量 Ptr, 临时项)->值 Is 表达式格式_字符串 Then
            结果->指令_加载文本(Cast(表达式格式_字符串 Ptr, Cast(项_字面量 Ptr, 临时项)->值)->值)
        ElseIf *Cast(项_字面量 Ptr, 临时项)->值 Is 表达式格式_逻辑型 Then
            结果->指令_加载逻辑(Cast(表达式格式_逻辑型 Ptr, Cast(项_字面量 Ptr, 临时项)->值)->值)
        End If
    ElseIf *临时项 Is 项_局部变量 Then
        结果->指令_读取(Cast(项_局部变量 Ptr, 临时项)->容器序列)
    Elseif *临时项 Is 项_静态变量 Then
        结果->指令_静态读取(Cast(项_静态变量 Ptr, 临时项)->引用)
    ElseIf *临时项 Is 项_成员变量 Then
        生成项(Cast(项_成员变量 Ptr, 临时项)->变量, 结果)
        结果->指令_成员读取(Cast(项_成员变量 Ptr, 临时项)->实例定位序列)
    ElseIf *临时项 Is 项_参数 Then
        结果->指令_加载参数(Cast(项_参数 Ptr, 临时项)->参数序列)
    ElseIf *临时项 Is 项_调用 Then
        Dim temp As 项_调用 Ptr = Cast(项_调用 Ptr, 临时项)
        
        For i As Integer = LBound(temp->参数) To UBound(temp->参数)
            生成项(temp->参数(i), 结果)
        Next
        
        结果->指令_调用(temp->方法)
    ElseIf *临时项 Is 项_返回值 Then
        生成项(Cast(项_返回值 Ptr, 临时项)->值, 结果)
        结果->指令_返回()
    ElseIf *临时项 Is 项_如果 Then
        生成项(Cast(项_如果 Ptr, 临时项)->条件, 结果)
        
        Dim 真域_标识 As Integer
        Dim 假域_标识 As Integer
        
        真域_标识 = 结果->取占位符标识()
        假域_标识 = 结果->取占位符标识()
        
        结果->指令_如果(真域_标识, 假域_标识)
        
        结果->占位符(真域_标识)
        生成中间语言(Cast(项_如果 Ptr, 临时项)->真域->引用, 结果, Cast(项_如果 Ptr, 临时项)->真域->容器序列下标)
        
        结果->占位符(假域_标识)
        If Cast(项_如果 Ptr, 临时项)->假域 <> 0 Then
            生成中间语言(Cast(项_如果 Ptr, 临时项)->假域->引用, 结果, Cast(项_如果 Ptr, 临时项)->假域->容器序列下标)
        End If
        
    End If
End Sub

Sub 生成中间语言(临时范围 As 节点_范围 Ptr, 结果 As 方法信息 Ptr, 容器序列下标 As Integer)
    For i As Integer = LBound(临时范围->项表) To UBound(临时范围->项表)
        'Print 临时范围->项表(i)
        生成项(临时范围->项表(i), 结果)
        If 临时范围->项表(i)->丢弃值 Then
            结果->指令_丢弃()
        End If
    Next
    
    For i As Integer = 容器序列下标 To 容器序列下标 + ARRAYSIZE(临时范围->变量表) -1
        结果->指令_销毁变量(i)
    Next
End Sub


Sub 编译(代码路径 As String, 结果路径 As String)
    If Right(结果路径, 1) = "\" OrElse Right(结果路径, 1) = "/" Then '目录标准化
        结果路径 = Left(结果路径, Len(结果路径) -1)
    End If
    
    文件.KillDir(结果路径)
    
    Dim 编译信息指针 As 编译信息 Ptr = New 编译信息
    编译信息指针->根节点 = 对象管理.加入(New 节点_根())
    编译信息指针->根节点->根命名空间       = 对象管理.加入(New 节点_命名空间())
    编译信息指针->根节点->根命名空间->名称 = "Root"
    编译信息指针->现行节点         = 编译信息指针->根节点
    编译信息指针->所属命名空间节点 = 编译信息指针->根节点->根命名空间
    
    词法器.源 = 文件.LoadFile(代码路径)
    
    Print "声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式声明模式"
    解析器(解析模式.声明模式, 编译信息指针)
    
    If 基本类型.单精度浮点型 = 0 Then Print("单精度浮点型未被定义") : Sleep : Return
    If 基本类型.双精度浮点型 = 0 Then Print("双精度浮点型未被定义") : Sleep : Return
    If 基本类型.字节型 = 0       Then Print("字节型未被定义")       : Sleep : Return
    If 基本类型.短整数型 = 0 Then Print("短整数型未被定义") : Sleep : Return
    If 基本类型.整数型 = 0 Then Print("整数型未被定义") : Sleep : Return
    If 基本类型.长整数型 = 0 Then Print("长整数型未被定义") : Sleep : Return
    If 基本类型.文本型 = 0 Then Print("文本型未被定义") : Sleep : Return
    If 基本类型.逻辑型 = 0 Then Print("逻辑型未被定义") : Sleep : Return
    If 基本类型.对象型 = 0 Then Print("对象型未被定义") : Sleep : Return
    
    Print "定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式定义模式"
    解析器(解析模式.定义模式, 编译信息指针)
    Print "流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式流程模式"
    解析器(解析模式.流程模式, 编译信息指针)
    
    If 是否出错 Then
        对象管理.释放()
        Print "致命错误"
        Delete 编译信息指针
        Sleep
    End If
    
    For i As Integer = LBound(方法映射.表) To UBound(方法映射.表)
        Print Str(Cast(节点_方法 Ptr, 方法映射.表(i).起始)->方法签名摘要) + IIf(Cast(节点_方法 Ptr, 方法映射.表(i).起始)->名称 <> "", "-" + Cast(节点_方法 Ptr, 方法映射.表(i).起始)->名称, "") ;":"
        生成中间语言(Cast(节点_方法 Ptr, 方法映射.表(i).起始)->主范围, 方法映射.表(i).目标, 0)
        Cast(方法信息 Ptr, 方法映射.表(i).目标)->编码数据()
        
        Dim temp As 方法信息 Ptr = Cast(方法信息 Ptr, 方法映射.表(i).目标)
        For j As Integer = LBound(temp->流空间) To UBound(temp->流空间)
            Print !"\t";
            If *temp->流空间(j) Is 单元_占位符 Then
                Print "$" + Str(Cast(单元_占位符 Ptr, temp->流空间(j))->标识) ;":"
            ElseIf *temp->流空间(j) Is 单元_返回 Then
                Print "ret"
            ElseIf *temp->流空间(j) Is 单元_加载参数 Then
                Print "load.arg." + Str(Cast(单元_加载参数 Ptr, temp->流空间(j))->序列)
            ElseIf *temp->流空间(j) Is 单元_丢弃 Then
                Print "popn"
            ElseIf *temp->流空间(j) Is 单元_读取 Then
                Print "read." + Str(Cast(单元_读取 Ptr, temp->流空间(j))->序列)
            ElseIf *temp->流空间(j) Is 单元_静态读取 Then
                Print "read.static"
            ElseIf *temp->流空间(j) Is 单元_成员读取 Then
                Print "read.member."+Str(Cast(单元_成员读取 Ptr, temp->流空间(j))->实例定位序列)
            ElseIf *temp->流空间(j) Is 单元_调用 Then
                Print "call " + Cast(单元_调用 Ptr, temp->流空间(j))->方法->名称
            ElseIf *temp->流空间(j) Is 单元_保存 Then
                Print "save." + Str(Cast(单元_保存 Ptr, temp->流空间(j))->序列)
            ElseIf *temp->流空间(j) Is 单元_静态保存 Then
                Print "save.static"
            ElseIf *temp->流空间(j) Is 单元_成员保存 Then
                Print "save.member."+ Str(Cast(单元_成员保存 Ptr, temp->流空间(j))->实例定位序列)
            ElseIf *temp->流空间(j) Is 单元_加载整数 Then
                Print "load.int." + Str(Cast(单元_加载整数 Ptr, temp->流空间(j))->值)
            ElseIf *temp->流空间(j) Is 单元_加载文本 Then
                Print "load.str"
            ElseIf *temp->流空间(j) Is 单元_加载逻辑 Then
                Print "load.bool"
            ElseIf *temp->流空间(j) Is 单元_创建变量 Then
                Print "create"
            ElseIf *temp->流空间(j) Is 单元_销毁变量 Then
                Print "close"
            ElseIf *temp->流空间(j) Is 单元_如果 Then
                Print "if $";Str(Cast(单元_如果 Ptr, temp->流空间(j))->真目标) ;" $";Str(Cast(单元_如果 Ptr, temp->流空间(j))->假目标)
            End If
        Next
    Next
    
    架构目录(结果路径, 编译信息指针->根节点->根命名空间)
    
    对象管理.释放()
    Print "代码生成完毕"
    Delete 编译信息指针
    Sleep
End Sub

Sub 架构目录(路径 As String,命名空间 As 节点_命名空间 Ptr)
   If Right(路径, 1) = "\" OrElse Right(路径, 1) = "/" Then'目录标准化
      路径 = Left(路径, Len(路径) -1)
   End If
   
   MkDir(路径)
   
   For i As Integer = LBound(命名空间->命名空间表) To UBound(命名空间->命名空间表)
      架构目录(路径 + "\" + 命名空间->命名空间表(i)-> 名称, 命名空间->命名空间表(i))
   Next
   
   For i As Integer = LBound(命名空间->类表) To UBound(命名空间->类表)
      If 命名空间->类表(i)->用户类 Then
         写出类(路径 + "\" + 命名空间->类表(i)->名称, 命名空间->类表(i))
      End If
   Next
End Sub

Sub 写出类(路径 As String, 类 As 节点_类 Ptr)
    Dim fhandle As Integer
    
    fhandle = FreeFile
    
    Open 路径 For Binary As #fhandle
    
    Dim temp As UByte
    
    '写出类本身
    Scope
        Dim 节点索引 As 节点_命名空间 Ptr = 类->绑定命名空间
        Dim 组()     As String
        While True
            If 节点索引->上一级 = 0 Then
                Exit While
            End If
            Dim 索引 As Integer
            索引 = UBound(组) + 1
            ReDim Preserve 组(索引)
            组(索引) = 节点索引->名称
            节点索引 = 节点索引->上一级
        Wend
        
        Put #fhandle,, Cast(Long, ARRAYSIZE(组))
        For i As Integer = UBound(组) To LBound(组) Step -1
            Put #fhandle,, Cast(Long,Len(组(i)))
            Put #fhandle,, 组(i)
            temp=0:Put #fhandle,, temp
        Next
        Put #fhandle,, Cast(Long,Len(类->名称))
        Put #fhandle,, 类->名称
        temp=0:Put #fhandle,,temp
    End Scope
    
    '写出父类
    If 类->父类 = 0 Then
        temp=0:Put #fhandle,, temp
    Else
        temp=1:Put #fhandle, ,temp
        Scope
            Dim 节点索引 As 节点_命名空间 Ptr = 类->父类->绑定命名空间
            Dim 组()     As String
            While True
                If 节点索引->上一级 = 0 Then
                    Exit While
                End If
                Dim 索引 As Integer
                索引 = UBound(组) + 1
                ReDim Preserve 组(索引)
                组(索引) = 节点索引->名称
                节点索引 = 节点索引->上一级
            Wend
            Put #fhandle,, Cast(Long, ARRAYSIZE(组))
            For i As Integer = UBound(组) To LBound(组) Step -1
                Put #fhandle,, Cast(Long,Len(组(i)))
                Put #fhandle,, 组(i)
                temp=0:Put #fhandle,, temp
            Next
            Put #fhandle,, Cast(Long,Len(类->父类->名称))
            Put #fhandle,, 类->父类->名称
            temp=0:Put #fhandle,, temp
        End Scope
    End If
    
    
    '写出方法
    Put #fhandle,, Cast(Long, ARRAYSIZE(类->方法表))
    
    For i As Integer = LBound(类->方法表) To UBound(类->方法表)
        Dim 方法引用 As 节点_方法 Ptr = 类->方法表(i)
        
        temp = IIf(方法引用->是否本地, Cast(UByte, 1), Cast(UByte, 0))
        Put #fhandle,, temp
        Put #fhandle,, Cast(Long, 方法引用->类型)
        
        temp = IIf(方法引用->是否函数, Cast(UByte, 1), Cast(UByte, 0))
        Put #fhandle,, temp
        
        If 方法引用->是否函数 Then
            Dim 返回值_文式 As String
            生成文式_类型(方法引用->返回值, 返回值_文式)
            
            Put #fhandle,, Cast(Long,Len(返回值_文式))
            Put #fhandle,, 返回值_文式
            temp=0:Put #fhandle,, temp
        End If
        
        temp = IIf(方法引用->是否运算符, Cast(UByte, 1), Cast(UByte, 0))
        Put #fhandle,, temp
        
        If 方法引用->是否运算符 Then
            Put #fhandle,, Cast(Long, 方法引用->运算符)
        End If
        
        Put #fhandle,, Cast(Long,Len(方法引用->名称))
        Put #fhandle,, 方法引用->名称
        temp=0:Put #fhandle,, temp
        
        Put #fhandle,, Cast(Long, ARRAYSIZE(类->方法表))
        
        If 方法引用->是否本地 = False Then
            Dim temp As 方法信息 Ptr = Cast(方法信息 Ptr, 方法映射.寻找(方法引用))
            Put #fhandle,, Cast(Long, ARRAYSIZE(temp->字节码表)) '此时计数器便是长度
            Put #fhandle,, temp->字节码表()
        End If
    Next
    
    '写出成员
    Put #fhandle,, Cast(Long, ARRAYSIZE(类->成员表))
    For i As Integer = LBound(类->成员表) To UBound(类->成员表)
        
    Next
    
    '写出静态变量
    Put #fhandle,, Cast(Long, ARRAYSIZE(类->静态变量表))
    For i As Integer = LBound(类->静态变量表) To UBound(类->静态变量表)
        
    Next
    
    Close fhandle
End Sub



End Namespace


