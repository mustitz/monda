; ===============================================
; libc externals
; ===============================================

; Needed internally
declare void @free(i8*)

; function write
;   fd: Int
;   buf: Pointer
;   count: Size
; returns ForeignCall SSize
; linkage "libc", "write"
declare i64 @write(i32, i8*, i64)



; ===============================================
; monda internal implementations
; ===============================================

; addref, internal, assume that reference counter is first field
define void @monda-addref(i64* %ref) alwaysinline {
entry:
  %current_ref = load i64, i64* %ref
  %is_static = icmp eq i64 %current_ref, -1
  br i1 %is_static, label %exit, label %increment

increment:
  %new_ref = add i64 %current_ref, 1
  store i64 %new_ref, i64* %ref
  br label %exit

exit:
  ret void
}

; decref, internal, assume that reference counter is first field
define void @monda-decref(i64* %ref) alwaysinline {
entry:
  %current_ref = load i64, i64* %ref
  %is_static = icmp eq i64 %current_ref, -1
  br i1 %is_static, label %exit, label %increment

increment:
  %new_ref = sub i64 %current_ref, 1
  store i64 %new_ref, i64* %ref

  %should_free = icmp eq i64 %new_ref, 0
  br i1 %should_free, label %release, label %exit

release:
  call void @free(i8* %ref)
  br label %exit

exit:
  ret void
}



; builtin
; record Buffer
;   ptr: Pointer
;   size: Size

%Monda-Buffer = type { i8*, i64 }



; builtin
; record String
;   ref_count: Int64
;   buffer: Buffer

%Monda-String = type { i64, %Monda-Buffer }



; impelentation Monad Identity
;   function pure Value: Exit_Code
;   returns Identity Exit_Code
define i8 @monda-pure-Identity-i8(i8 %value) alwaysinline {
entry:
  ret i8 %value
}

; impelentation Monad Identity
;   function bind
;     Value: Exit_Code
;   returns Identity Exit_Code
define i8 @monda-bind-Identity-i64-i8(i64 %arg, i8 (i64)* %fn) alwaysinline {
entry:
  %result = call i8 %fn(i64 %arg)
  ret i8 %result
}



; function id arg: T
; returns T
; requires T
; monadic
;   arg
;
; function id: String
; returns String
define %Monda-String* @monda-id-String(%Monda-String* %str) alwaysinline {
entry:
  %refcount_ptr = getelementptr inbounds %Monda-String, %Monda-String* %str, i32 0, i32 0
  call void @monda-addref(i64* %refcount_ptr)
  ret %Monda-String* %str
}



; implementation Showable String
;   show = id

define %Monda-String* @monda-show-String(%Monda-String* %str) alwaysinline {
entry:
  %result = call %Monda-String* @monda-id-String(%Monda-String* %str)
  ret %Monda-String* %result
}



; implementation Buffered String
;   buf = builtin

define %Monda-Buffer* @monda-buf-String(%Monda-String* %str) alwaysinline {
entry:
  %buffer_ptr = getelementptr inbounds %Monda-String, %Monda-String* %str, i32 0, i32 1
  ret %Monda-Buffer* %buffer_ptr
}



; function write obj: T
; returns Stdout SSize
; requires Buffered T
; monadic ForeignCall
;   buf: Buffer := obj.buf
;   write 1 buf.ptr buf.size

define i64 @monda-write-String(%Monda-String* %str) {
entry:
  %buffer_ptr = call %Monda-Buffer* @monda-buf-String(%Monda-String* %str)
  %buffer = load %Monda-Buffer, %Monda-Buffer* %buffer_ptr

  %ptr = extractvalue %Monda-Buffer %buffer, 0
  %size = extractvalue %Monda-Buffer %buffer, 1
  %result = call i64 @write(i32 1, i8* %ptr, i64 %size)

  ret i64 %result
}



; function print obj: String
; requires Showable T
; returns Stdout SSize
; monadic Stdout
;   write obj.show

define i64 @monda-print-String(%Monda-String* %str) {
entry:
  %shown = call %Monda-String* @monda-show-String(%Monda-String* %str)
  %result = call i64 @monda-write-String(%Monda-String* %shown)

  %shown_refcount_ptr = getelementptr inbounds %Monda-String, %Monda-String* %shown, i32 0, i32 0
  call void @monda-decref(i64* %shown_refcount_ptr)
  ret i64 %result
}



; function main
; returns ret_code: Stdout Exit_Code
; guarantee ret_code = 0
; monadic Stdout
;   msg: String := "Hello, Monda!\n"
;   <- print msg
;   0

@hello-main-msg-bytes = private unnamed_addr constant [15 x i8] c"Hello, Monda!\0a\00"

@hello-main-msg = private unnamed_addr constant %Monda-String {
  i64 -1,
  %Monda-Buffer {
    i8* getelementptr inbounds ([15 x i8], [15 x i8]* @hello-main-msg-bytes, i32 0, i32 0),
    i64 14
  }
}

define i8 @hello-main-lambda-01(i64 %unused) alwaysinline {
entry:
  %result = call i8 @monda-pure-Identity-i8(i8 0)
  ret i8 %result
}

define i8 @hello-main() {
entry:
  %write_result = call i64 @monda-print-String(%Monda-String* @hello-main-msg)
  %bind_result = call i8 @monda-bind-Identity-i64-i8(i64 %write_result, i8 (i64)* @hello-main-lambda-01)
  ret i8 %bind_result
}



; Entry point

define i32 @main() {
entry:
  %exit_code = call i8 @hello-main()
  ret i32 0
}
