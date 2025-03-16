Function 库_寻找类成员(临时成员路径 As 成员路径) As 成员信息 Ptr
   Function = Find_ClassMember(临时成员路径)
  
End Function


Function 库_新建类(类 As 类指向, 方法 As 方法指向, ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr) As 实例信息 Ptr
   GC_Check()
   
   Dim 临时实例信息 As 实例信息 Ptr
   临时实例信息=实例化(类, 方法, 栈指针,互斥锁)
   Function = 临时实例信息
End Function

Function 库_寻找类(临时类路径 As 类路径) As 类信息 Ptr
   Function = Find_Class(临时类路径)
End Function

Function 库_寻找类方法(临时方法路径 As 方法路径) As 函数信息 Ptr
   Function = Find_ClassMethod(临时方法路径)
End Function

Function 库_取实例类(临时实例信息 As 实例信息 Ptr) As 类指向
   Function = ( * (临时实例信息->对象表 + (临时实例信息->对象计数 -1)))->类位置
End Function

Function 库_取类路径文本(类 As 类指向) As String
   Function = "ROOT::" + Dump_命名空间路径(类->命名空间) + 类->名称
End Function
Function 库_取类方法文本(方法 As 方法指向) As String
   Function = 方法->方法名
End Function
Function 库_寻找类方法By类(类 As 类指向, 方法签名 As ULongInt) As 函数信息 Ptr
   For i As Integer = 0 To UBound(类->函数表)
      If 类->函数表(i)->方法签名 = 方法签名 Then
         Return 类->函数表(i)
         Exit For
      End If
   Next
   Return 0
End Function

Sub 库_启动静态方法线程(临时方法指向 As 方法指向)
   启动线程(临时方法指向)
End Sub
Sub 库_垃圾回收()
   'GC()
   GC_Feedback()
End Sub

Sub 库_启动实例方法线程(临时方法指向 As 方法指向, 对象表() As 实例信息 Ptr)
   启动线程_初始化(临时方法指向, 对象表())
End Sub

Sub 库_调用方法(方法 As 方法指向, ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr)
   If 方法->本地方法 Then
      方法->本地方法实体(栈指针,互斥锁)
   Else
      环境(方法, 栈指针,互斥锁)
   End If
End Sub

Sub 库_运行静态方法(临时方法指向 As 方法指向)
   工作线程(临时方法指向)
End Sub

Function 库_寻找方法(临时实例 As 实例信息 Ptr, 方法签名 As ULongInt) As 方法指向
   Dim 临时方法指向 As 方法指向
   临时方法指向 = 临时实例->虚方法表->Find(方法签名)
   Function = 临时方法指向
End Function

Function 库_申请栈() As 栈标记类型 Ptr
   Function = 申请栈()
End Function

Sub 库_回收栈(临时栈标记 As 栈标记类型 Ptr)
   回收栈(临时栈标记)
End Sub

Sub 库_记录(临时实例信息 As 实例信息 Ptr)
   'Print "Join",临时实例信息
   对象池.join(临时实例信息)
End Sub

Function 库_新建对象() As 实例信息 Ptr
   Function = New 实例信息
End Function

Sub 库_初始化表(临时表信息 As 表信息 Ptr, 维度表() As Integer, 维度 As Integer, 最大维度 As Integer,ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr)
   初始化表(临时表信息, 维度表(), 维度, 最大维度, 栈指针,互斥锁)
End Sub

Function 库_新建表( ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr) As 实例信息 Ptr
   Function=实例化表(栈指针,互斥锁)
End Function

Function 库_新建回调( ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr) As 实例信息 Ptr
   Function = 实例化回调(栈指针, 互斥锁)
End Function

Sub 初始化库接口表()
   库接口.寻找类成员 = @库_寻找类成员
   库接口.新建类 = @库_新建类
   库接口.寻找类 = @库_寻找类
   库接口.寻找类方法 = @库_寻找类方法
   库接口.取实例类=@库_取实例类
   库接口.取类路径文本 = @库_取类路径文本
   库接口.取类方法文本 = @库_取类方法文本
   库接口.寻找类方法By类 = @库_寻找类方法By类
   库接口.启动静态方法线程 = @库_启动静态方法线程
   库接口.垃圾回收 = @库_垃圾回收
   库接口.调用方法 = @库_调用方法
   库接口.启动实例方法线程 = @库_启动实例方法线程
   库接口.运行静态方法 = @库_运行静态方法
   库接口.寻找方法 = @库_寻找方法
   库接口.申请栈 = @库_申请栈
   库接口.回收栈 = @库_回收栈
   库接口.记录 = @库_记录
   库接口.新建对象 = @库_新建对象
   库接口.初始化表 = @库_初始化表
   库接口.新建表 = @库_新建表
   库接口.新建回调 = @库_新建回调
End Sub

Type 库接口信息
   Dim 寻找类成员 As Function(临时成员路径 As 成员路径) As 成员信息 Ptr
   Dim 新建类 As Function(类 As 类指向, 方法 As 方法指向, ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr) As 实例信息 Ptr
   Dim 寻找类 As Function(临时类路径 As 类路径) As 类信息 Ptr
   Dim 寻找类方法 As Function(临时方法路径 As 方法路径) As 函数信息 Ptr
   Dim 取实例类 As Function(临时实例信息 As 实例信息 Ptr) As 类指向
   Dim 取类路径文本 As Function(类 As 类指向) As String
   Dim 取类方法文本 As Function(方法 As 方法指向) As String
   Dim 寻找类方法By类 As Function(类 As 类指向, 方法签名 As ULongInt) As 函数信息 Ptr
   Dim 启动静态方法线程 As Sub(临时方法指向 As 方法指向)
   Dim 垃圾回收 As Sub()
   Dim 调用方法 As Sub(方法 As 方法指向, ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr)
   Dim 启动实例方法线程 As Sub(临时方法指向 As 方法指向, 对象表() As 实例信息 Ptr)
   Dim 运行静态方法 As Sub(临时方法指向 As 方法指向)
   Dim 寻找方法 As Function(临时实例 As 实例信息 Ptr, 虚方法实例索引 As Integer, 方法签名 As ULongInt) As 方法指向
   Dim 申请栈 As Function() As 栈标记类型 Ptr
   Dim 回收栈 As Sub(临时栈标记 As 栈标记类型 Ptr)
   Dim 记录 As Sub(临时实例信息 As 实例信息 Ptr)
   Dim 新建对象 As Function() As 实例信息 Ptr
   Dim 初始化表 As Sub(临时表信息 As 表信息 Ptr, 维度表() As Integer, 维度 As Integer, 最大维度 As Integer, ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr)
   Dim 新建表 As Function(ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr) As 实例信息 Ptr
   Dim 新建回调 As Function(ByRef 栈指针 As 栈信息 Ptr,互斥锁 As Any Ptr) As 实例信息 Ptr
End Type

