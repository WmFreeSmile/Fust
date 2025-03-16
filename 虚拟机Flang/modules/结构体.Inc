
Type 命名空间指向 As 命名空间信息 Ptr
Type 类指向 As 类信息 Ptr
Type 方法指向 As 函数信息 Ptr


Enum box_type
box_null
box_ref
box_refpass
End Enum

Enum stack_type
stack_box
stack_object
End Enum

Enum is_code
is_code_nop
is_code_push_box
is_code_push_box_ref
is_code_push_method
is_code_push_byte
is_code_push_short
is_code_push_int
is_code_push_long
is_code_push_float
is_code_push_double
is_code_push_bool
is_code_push_string
is_code_push_global
is_code_popn
is_code_new
is_code_next
is_code_change
is_code_create
is_code_close
is_code_return
is_code_call_path
is_code_call_virtual

is_code_jmp
is_code_jz

is_code_new_array
is_code_set


is_code_assign_box_box_ref
is_code_assign_box_obj


is_code_pop_box_box_ref
is_code_pop_box_obj


is_code_locate

is_code_ref

is_code_call_callback_set
is_code_call_callback

is_code_print
is_code_load

is_code_size
is_code_clear
is_code_state
is_code_type
is_code_lock
is_code_unlock

End Enum

Enum control_code
unknown   
label_is_nop
label_is_push_box
label_is_push_box_ref
label_is_push_method_static
label_is_push_method_instance
label_is_push_byte
label_is_push_short
label_is_push_int
label_is_push_long
label_is_push_float
label_is_push_double
label_is_push_bool
label_is_push_string
label_is_push_global
label_is_popn
label_is_new
label_is_next
label_is_change
label_is_create
label_is_close
label_is_return
label_is_call_path
label_is_call_path_native
label_is_call_virtual
label_is_jmp
label_is_jz
label_is_new_array
label_is_set
label_is_assign_box_box_ref
label_is_assign_box_obj
label_is_pop_box_box_ref
label_is_pop_box_obj
label_is_locate
label_is_ref
label_is_call_callback_set
label_is_call_callback
label_is_print
label_is_load
label_is_size
label_is_clear
label_is_state
label_is_type
label_is_lock
label_is_unlock
End Enum




Type Trigger
   Dim m_mutexID As Any Ptr
   Dim m_condID As Any Ptr
   
   Declare Constructor()
   Declare Destructor()
   
   Declare Sub Wait()
   Declare Sub Run()
End Type
Constructor Trigger()
   This.m_mutexID = MutexCreate()
   This.m_condID = CondCreate()
   
#ifdef __FB_WIN32__
   CondWait(This.m_condID, This.m_mutexID)
#endif
End Constructor

Destructor Trigger()
   MutexDestroy(This.m_mutexID)
   CondDestroy(This.m_condID)
End Destructor

Sub Trigger.Wait()
   CondWait(This.m_condID, This.m_mutexID)
End Sub

Sub Trigger.Run()
   MutexLock This.m_mutexID
   CondBroadcast(This.m_condID)
   MutexUnlock This.m_mutexID
End Sub



'Type Trigger
'   Dim m_mutexID As Any Ptr
'   
'   Declare Constructor()
'   Declare Destructor()
'   
'   Declare Sub Wait()
'   Declare Sub Run()
'End Type
'Constructor Trigger()
'   This.m_mutexID = MutexCreate()
'End Constructor
'
'Destructor Trigger()
'   MutexDestroy(This.m_mutexID)
'End Destructor
'
'Sub Trigger.Wait()
'   MutexLock(This.m_mutexID)
'   MutexLock(This.m_mutexID)
'End Sub
'
'Sub Trigger.Run()
'   MutexUnlock(This.m_mutexID)
'End Sub



Type StackMutex
   Dim m_mutexID As Any Ptr
   Dim m_level As Integer
   Declare Constructor()
   Declare Destructor()
   
   Declare Sub Lock()
   Declare Sub Unlock()
End Type
Constructor StackMutex()
   This.m_mutexID = MutexCreate()
   This.m_level = 0
End Constructor

Destructor StackMutex()
   MutexDestroy(This.m_mutexID)
End Destructor

Sub StackMutex.Lock()
   If This.m_level = 0 Then
      MutexLock(This.m_mutexID)
   End If
   This.m_level = This.m_level + 1
End Sub

Sub StackMutex.Unlock()
   This.m_level = This.m_level - 1
   If This.m_level = 0 Then
      MutexUnlock(This.m_mutexID)
   End If
End Sub

Type HashPool
   Declare Constructor()
   Declare Destructor()
   ReDim m_pool(Any) As Any Ptr
   Dim m_mutex As Any Ptr
   Declare Sub Join(l_data As Any Ptr)
   Declare Sub Del(l_data As Any Ptr)
   Declare Sub BatchDel(index() As Integer)
   Declare Sub SetMutex(l_mutex As Any Ptr)
   Declare Function Conut() As Integer
End Type
Constructor HashPool
   This.m_mutex = MutexCreate
End Constructor
Destructor HashPool
   MutexDestroy This.m_mutex
End Destructor

Sub HashPool.SetMutex(l_mutex As Any Ptr)
   This.m_mutex = l_mutex
End Sub
Function HashPool.Conut() As Integer
   Dim result As Integer
   MutexLock(This.m_mutex)
   result = ARRAYSIZE(This.m_pool)
   MutexUnlock(This.m_mutex)
   Function = result
End Function
Sub HashPool.Join(l_data As Any Ptr)
   Dim index As Integer
   MutexLock(This.m_mutex)
   index = UBound(This.m_pool) + 1
   ReDim Preserve This.m_pool(index)
   This.m_pool(index) = l_data
   MutexUnlock(This.m_mutex)
   
End Sub
Sub HashPool.Del(l_data As Any Ptr)
   MutexLock(This.m_mutex)
   For i As Integer = 0 To UBound(This.m_pool)
      If This.m_pool(i) = l_data Then
         Dim temp As Any Ptr
         temp = This.m_pool(UBound(This.m_pool))
         This.m_pool(i) = temp
         If UBound(This.m_pool) -1 = -1 Then
            Erase This.m_pool
         Else
            ReDim Preserve This.m_pool(UBound(This.m_pool) -1)
         End If
         'Return
         Exit For
      End If
   Next
   MutexUnlock(This.m_mutex)
End Sub
Sub HashPool.BatchDel(index() As Integer)
   For i As Integer = 0 To UBound(index)
      This.m_pool(index(i)) = -1
   Next
   
   ReDim backups() As Any Ptr
   ReDim backups(UBound(This.m_pool))
   For i As Integer = 0 To UBound(This.m_pool)
      backups(i) = This.m_pool(i)
   Next
   Erase This.m_pool
   Dim 索引 As Integer
   For i As Integer = 0 To UBound(backups)
      If backups(i) <> -1 Then
         索引 = UBound(This.m_pool) + 1
         ReDim Preserve This.m_pool(索引)
         This.m_pool(索引) = backups(i)
      End If
   Next
End Sub

'Type HashItem
'   m_back As HashItem Ptr:m_next As HashItem Ptr:m_data As Any Ptr
'End Type
'
'Type HashPool
'   m_size As Integer:m_first As HashItem Ptr:m_node As HashItem Ptr:m_map As TMAPINTEGERINTEGER
'   
'   Declare Sub Join(l_data As Any Ptr)
'   Declare Function Del(l_data As Any Ptr) As HashItem Ptr
'   
'   Declare Destructor()
'End Type
'Destructor HashPool
'   
'   This.m_map.DeleteTree()
'   Dim temp_HashItem As HashItem Ptr
'   temp_HashItem = This.m_first
'   If This.m_size <> 0 Then
'      Delete This.m_node
'   End If
'   For i As Integer = 1 To This.m_size
'      temp_HashItem = This.Del(temp_HashItem->m_data)
'   Next
'End Destructor
'
'Sub HashPool.Join(l_data As Any Ptr)
'   If This.m_size = 0 Then
'      This.m_size = 1
'      This.m_node = New HashItem
'      This.m_first = This.m_node
'      This.m_node->m_data = l_data
'      This.m_map.Insert(l_data, This.m_node)
'      This.m_node->m_next = New HashItem
'      This.m_node->m_next->m_back = This.m_node
'      This.m_node = This.m_node->m_next
'   Else
'      This.m_size = This.m_size + 1
'      This.m_node->m_data = l_data
'      This.m_map.Insert(l_data, This.m_node)
'      This.m_node->m_next = New HashItem
'      This.m_node->m_next->m_back = This.m_node
'      This.m_node = This.m_node->m_next
'   End If
'End Sub
'Function HashPool.Del(l_data As Any Ptr) As HashItem Ptr
'   Dim retnext As HashItem Ptr
'   Dim temp As HashItem Ptr
'   temp = This.m_map.Find(l_data)
'   This.m_map.DeleteNode(l_data)
'   retnext = temp->m_next
'   If temp->m_back = 0 Then
'      This.m_first = temp->m_next
'      temp->m_next->m_back = 0
'   Else
'      temp->m_back->m_next = temp->m_next
'   End If
'   
'   If This.m_size = 1 Then
'      Delete This.m_node
'   End If
'   
'   Delete temp
'   
'   This.m_size = This.m_size -1
'   Function = retnext
'End Function



'Type 实例信息
'   Dim 父类 As 实例信息 Ptr
'   Dim 超类 As BOOL
'   
'   Dim 类型 As UByte
'   
'   ReDim 表(Any) As Any Ptr 'As 容器信息 ptr
'   
'   ReDim 虚方法实例表(Any) As 虚方法信息
'   ReDim 成员表(Any) As Any Ptr 'as 容器信息 ptr
'   
'   Dim 类位置 As 类指向
'   
'   ReDim 库实例指针(Any) As Any Ptr
'   
'   Dim 标记 As BOOL
'End Type


Type 对象信息
   Dim 类位置 As 类指向
   Dim 库数据 As Any Ptr
End Type

Type 表信息
   Dim 容器表 As Any Ptr 'As 容器信息 ptr ptr
   Dim 元素数 As Integer
   Dim 支表 As BOOL
End Type

Type 实例信息
   Dim 对象计数 As Integer
   Dim 成员计数 As Integer
   
   Dim 对象表 As 对象信息 Ptr Ptr
   Dim 虚方法表 As TMAPULONGINTINTEGER Ptr
   
   Dim 成员表 As Any Ptr 'as 容器信息 ptr ptr
   
   Dim 标记 As BOOL
   
   Dim 类 As Any Ptr'as 类信息 ptr
End Type


Type 回调信息
   Dim 类型 As UByte 'callback_*
   
   Dim 实例引用 As 实例信息 Ptr
   Dim 方法 As 方法指向
End Type


Union 容器值联合
Dim 实例引用 As 实例信息 Ptr
Dim 容器引用 As Any Ptr
End Union

Type 容器信息
   Dim 类型 As box_type
   Dim 值 As 容器值联合
   Dim 关键容器 As BOOL'局部容器和全局容器都是关键容器，会纳入GCRoots（成员容器，表容器不是）
End Type

Type 成员信息
   Dim 名称 As String '
   Dim 序号 As Integer '
   Dim 类型表达式 As String '
   Dim 偏移 As Integer
End Type

Type 静态成员信息
   Dim 名称 As String
   Dim 类型表达式 As String '
   
   Dim 容器 As 容器信息 Ptr
End Type

Union 栈联合值
Dim 容器值 As 容器信息 Ptr
Dim 对象值 As 实例信息 Ptr
End Union

Type 栈信息
   Dim 类型 As stack_type
   
   Dim 值 As 栈联合值
End Type

Type 函数信息
   Dim 方法签名 As ULongInt
   Dim 静态方法 As BOOL '
   Dim 方法名 As String '
   Dim 地址 As LongInt
   Dim 实例化方式 As UByte 'method_*
   Dim 返回值类型表达式 As String'
   Dim 形参类型表达式 As String '
   Dim 重载运算符 As BOOL '
   Dim 运算符 As UByte '
   Dim 静态构造方法 As BOOL
   Dim 本地方法 As BOOL
   
   'Dim 本地方法实体 As Sub(ByRef 栈指针 As 栈信息 Ptr) = -1
   Dim 本地方法实体 As Sub(ByRef 栈指针 As 栈信息 Ptr, 互斥锁 As Any Ptr) = -1
   
   Dim 类 As 类指向
   
   Dim 构造方法 As BOOL
   Dim 析构方法 As BOOL
   Dim 抽象方法 As BOOL
   
   Dim 最大容器数量 As Long
   
   Dim 指令数 As Long
End Type

Type 解释传递信息
   Dim 局部容器 As 容器信息 Ptr Ptr
   Dim 待跳转回调 As 回调信息 Ptr
   Dim 指令指针指针 As Any Ptr Ptr
End Type

Type 指令信息
   Dim 参数 As Any Ptr
   Dim 释放函数 As Sub(参数 As Any Ptr)
   
   Dim 操作码 As UByte
   Dim 控制码 As UByte
End Type



Type 类信息
   Dim 名称 As String
   
   ReDim 函数表(Any) As 函数信息 Ptr
   ReDim 成员表(Any) As 成员信息
   ReDim 静态成员表(Any) As 静态成员信息
   ReDim 指令表(Any) As 指令信息
   
   Dim 基类 As 类指向
   Dim 超类 As BOOL
   
   Dim 命名空间 As 命名空间指向
   
   Dim 成员偏移计数器 As Integer
   
   Dim 有析构方法 As BOOL
   
   Dim 析构方法 As 方法指向
   
   Dim 标志 As Integer
   
   Dim 深度 As Integer
   Dim 成员深度 As Integer
   
   Dim 是否抽象 As BOOL
   
   Declare Destructor()
End Type

Destructor 类信息
   For i As Integer = 0 To UBound(This.静态成员表)
      Delete(This.静态成员表(i).容器)
   Next
   
   For i As Integer = 0 To UBound(This.指令表)
      This.指令表(i).释放函数(This.指令表(i).参数)
   Next
End Destructor


Type 命名空间信息
   Dim 名称 As String
   
   ReDim 类表(Any) As 类信息 Ptr
   
   ReDim 子空间(Any) As 命名空间信息 Ptr
   
   Dim 父空间 As 命名空间信息 Ptr
   Dim 超空间 As BOOL
End Type


Type 类路径
   ReDim 命名空间路径(Any) As String
   Dim 类名 As String
End Type

Type 成员路径
   Dim 类 As 类路径
   Dim 成员名 As String
End Type

Type 方法路径
   Dim 类 As 类路径
   Dim 方法签名 As ULongInt
End Type

Type 方法实体覆写信息
   Dim 路径 As 方法路径
   Dim 实体 As Any Ptr
End Type

Type 库信息类型
   Dim 释放函数 As Sub()
   ReDim 覆写表(Any) As 方法实体覆写信息
   Dim 句柄 As Any Ptr
   Dim 库依赖目录 As String
End Type

Type 栈标记类型
   Dim 栈底 As 栈信息 Ptr
   Dim 栈指针指针 As 栈信息 Ptr Ptr
   
   Dim 互斥锁 As Any Ptr
End Type

Type 调用虚方法信息
   Dim 虚方法实例索引 As Integer
   Dim 方法签名 As ULongInt
End Type

Type type_byte
   Dim 值 As Byte
End Type

Type type_short
   Dim 值 As Short
End Type

Type type_int
   Dim 值 As Long
End Type

Type type_long
   Dim 值 As LongInt
End Type

Type type_float
   Dim 值 As Single
End Type

Type type_double
   Dim 值 As Double
End Type

Type type_bool
   Dim 值 As BOOL
End Type

Type type_string
   Dim 值 As String
End Type
