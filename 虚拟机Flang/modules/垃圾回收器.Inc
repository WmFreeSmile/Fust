

Dim Shared thread_handle As Any Ptr

'Dim Shared cond As Any Ptr
'Dim Shared mutex As Any Ptr

Dim Shared GC_Trigger As Trigger Ptr

Dim Shared exit_cond As Any Ptr
Dim Shared exit_mutex As Any Ptr
Dim Shared skip As Integer


Dim Shared GC_Flag As Integer
Dim Shared GC_Flag_Mutex As Any Ptr


Sub Free_Obj(临时实例信息 As 实例信息 Ptr)
   'Print "Free",临时实例信息
   
   Dim 临时类位置 As 类信息 Ptr

   '临时类位置 = ( * (临时实例信息->对象表 + (临时实例信息->对象计数 -1)))->类位置
   '
   'If 临时类位置->有析构方法 Then
   '   Cast(Sub(TheThis As Any Ptr), 临时类位置->析构方法->本地方法实体)(临时实例信息)
   'End If
   
   
   For i As Integer = 临时实例信息->对象计数 -1 To 0 Step -1
      临时类位置 = ( * (临时实例信息->对象表 + i))->类位置
      If 临时类位置->有析构方法 Then
         Cast(Sub(TheThis As Any Ptr), 临时类位置->析构方法->本地方法实体)(临时实例信息)
      End If
   Next
   
   For i As Integer = 0 To 临时实例信息->成员计数 -1 
      Delete * (Cast(容器信息 Ptr Ptr,临时实例信息->成员表) + i)
   Next
   
   For i As Integer = 0 To 临时实例信息->对象计数 -1
      Delete * (临时实例信息->对象表 + i)
   Next
   
   临时实例信息->虚方法表->DeleteTree()
   
   Deallocate 临时实例信息->成员表
   Deallocate 临时实例信息->对象表
   Delete 临时实例信息->虚方法表
   Delete 临时实例信息
End Sub

Sub TagObject(Link As 实例信息 Ptr)
   Link->标记 = True
   
   For i As Integer = 0 To Link->成员计数 -1 
      TagBox( * (Cast(容器信息 Ptr Ptr, Link->成员表) + i))
   Next
End Sub

Sub TagBox(Link As 容器信息 Ptr)
   If Link->类型 <> box_ref Then Return
   TagObject(Link->值.实例引用)
End Sub


Sub GC()
   MutexLock(对象池.m_mutex)
   For i As Integer = 0 To UBound(对象池.m_pool)
      Cast(实例信息 Ptr, 对象池.m_pool(i))->标记 = False
   Next
   MutexUnlock(对象池.m_mutex)
   
   ReDim GCRoots() As 容器信息 Ptr
   Dim index As Integer
   
   MutexLock(容器池.m_mutex)
   For i As Integer = 0 To UBound(容器池.m_pool)
      If (Cast(容器信息 Ptr, 容器池.m_pool(i))->关键容器) Then
         index = UBound(GCRoots) + 1
         ReDim Preserve GCRoots(index)
         GCRoots(index) = 容器池.m_pool(i)
      End If
   Next
   MutexUnlock(容器池.m_mutex)
   
   
   MutexLock(栈池.m_mutex)
   For i As Integer = 0 To UBound(栈池.m_pool)
      MutexLock Cast(StackMutex Ptr, Cast(栈标记类型 Ptr, 栈池.m_pool(i))->互斥锁)->m_mutexID
      For j As 栈信息 Ptr = Cast(栈标记类型 Ptr, 栈池.m_pool(i))->栈底 To *Cast(栈标记类型 Ptr, 栈池.m_pool(i))->栈指针指针-1 Step + 1
         'Print "STACK",j
         If j->类型 = stack_object Then
            TagObject(j->值.对象值)
         ElseIf j->类型 = stack_box Then
            TagBox(j->值.容器值)
         End If
      Next
      MutexUnlock Cast(StackMutex Ptr, Cast(栈标记类型 Ptr, 栈池.m_pool(i))->互斥锁)->m_mutexID 
   Next
   MutexUnlock(栈池.m_mutex)
   
   For i As Integer = 0 To UBound(GCRoots)
      TagBox(GCRoots(i))
   Next
   
   '
   'MutexLock(对象池.m_mutex)
   'For i As Integer = 0 To UBound(对象池.m_pool)
   '   Cast(实例信息 Ptr, 对象池.m_pool(i))->标记 = False
   'Next
   '
   'MutexLock(容器池.m_mutex)
   'For i As Integer = 0 To UBound(容器池.m_pool)
   '   If Cast(容器信息 Ptr, 容器池.m_pool(i))->类型 = box_ref Then
   '      Cast(容器信息 Ptr, 容器池.m_pool(i))->值.实例引用->标记 = True
   '   End If
   'Next
   'MutexUnlock(容器池.m_mutex)
   '
   'MutexLock(栈池.m_mutex)
   'For i As Integer = 0 To UBound(栈池.m_pool)
   '   For j As 栈信息 Ptr = Cast(栈标记类型 Ptr, 栈池.m_pool(i))->栈底 To *Cast(栈标记类型 Ptr, 栈池.m_pool(i))->栈指针指针-1 Step + 1
   '      If j->类型 = stack_object Then
   '         j->值.对象值->标记 = True
   '      End If
   '   Next
   'Next
   'MutexUnlock(栈池.m_mutex)
   
   
   MutexLock(对象池.m_mutex)
   ReDim free_index(Any) As Integer
   For i As Integer = 0 To UBound(对象池.m_pool)
      If Cast(实例信息 Ptr, 对象池.m_pool(i))->标记 = False Then
         Free_Obj(对象池.m_pool(i))'Native的析构方法不允许进行对象\容器分配
         index = UBound(free_index) + 1
         ReDim Preserve free_index(index)
         free_index(index) = i
      End If
   Next
   
   对象池.BatchDel(free_index())
   MutexUnlock(对象池.m_mutex)
   
   MutexLock(GC_Flag_Mutex)
   GC_Flag = 0
   MutexUnlock(GC_Flag_Mutex)
End Sub

Sub GC_Feedback()
   'MutexLock mutex
   'CondBroadcast(cond)
   'MutexUnlock mutex
   GC_Trigger->Run()
End Sub

Sub GC_Boot()
   MutexLock(GC_Flag_Mutex)
   If GC_Flag = 0 Then
      GC_Flag = 1
      ThreadCreate(@GC)
   End If
   MutexUnlock(GC_Flag_Mutex)
End Sub

'垃圾监视线程
Sub GMT()
   While True
      'CondWait(cond, mutex)
      GC_Trigger->Wait()
      If skip Then
         Exit While
      End If
      
      'GC()
      GC_Boot()
   Wend
   
   MutexLock exit_mutex
   CondBroadcast(exit_cond)
   MutexUnlock exit_mutex
End Sub


Sub GC_Check()
   'If Fre() < 1024 * 1024 * 1024 * 4 Then'可用内存小于4GB
      GC_Feedback()
   'End If
   'GC()
End Sub

