
Namespace 哈希算法


Function MD5u64(sInput As String) As ULongInt '字符MD5加密转换为 64位整数
 Dim ctx As MD5Context 
 Dim md5sum(0 To 15) As UByte 
 MYMD5Init( @ctx )
 MYMD5Update( @ctx,Cast(UByte Ptr, StrPtr(sInput) ), Len(sInput) )
 MYMD5Final( @md5sum(0), @ctx)
 Function = *CPtr(ULongInt Ptr, @md5sum(8))
End Function



End Namespace
