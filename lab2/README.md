## LAB2: 操作系统调用
---
### 1. 实验目的
> + 建立对系统调用接口的深入认识;  
> + 掌握系统调用的基本过程;  
> + 能完成系统调用的全面控制；  
> + 为后续实验做准备;

### 2. 实验内容
> 1) **`iam()`**
>> 第一个系统调用是 `iam()`，其原型为：  
>> ```c
>> int iam(const char * name);
>>```
>>完成的功能是将字符串参数 name 的内容拷贝到内核中保存下来。要求 name 的长度不能超过 23 个字符。返回值是拷贝的字符数。如果 name 的字符个数超过了 23，则返回 “-1”，并置 errno 为 EINVAL。在 kernal/who.c 中实现此系统调用。
> 2) **`whoami()`**
>> 第二个系统调用是 `whoami()`，其原型为：  
>> ```c
>> int whoami(char* name, unsigned int size);
>>```
>> 它将内核中由 iam() 保存的名字拷贝到 name 指向的用户地址空间中，同时确保不会对 name 越界访存（name 的大小由 size 说明）。返回值是拷贝的字符数。如果 size 小于需要的空间，则返回“-1”，并置 errno 为 EINVAL。也是在 kernal/who.c 中实现。

### 3. 实验的基本过程
> + 应用程序调用库函数（API）；
> + API 将系统调用号存入 EAX，然后通过中断调用使系统进入内核态；
> + 内核中的中断处理函数根据系统调用号，调用对应的内核函数（系统调用）；
> + 系统调用完成相应功能，将返回值存入 EAX，返回到中断处理函数；
> + 中断处理函数返回到 API 中；
> + API 将 EAX 返回给应用程序;

### 4.实验具体实现
> 4.1 修改`unistd.h` Unix 标准函数， 增加宏定义
>> 在文件`~/oslab/linux-0.11/include/unistd.h`中：  
>> 第`-131` 行后添加宏定义： 
>>```c
>>#define __NR_iam 72
>>#define __NR_whoami 73
>>```  
>
> 4.2 修改`system_call.s`, 扩大系统调用的数量
>> 在文件`~/oslab/linux-0.11/kernel/system_call.s`中：  
>> 第`-61` 行的值改为
>>```c
>>nr_system_calls = 74
>>```
>
> 4.3 修改`sys.h`, 当0x80中断时，调用系统函数
>> 在文件`~/oslab/linux-0.11/include/liunx/sys.h`第`72 - 73`行添加：
>>```c
>> extern int sys_iam();
>> extern int sys_whoami();
>>```
>> 第`-61` 行添加：`fn_ptr sys_call_table[]` 数组中
>>```c
>> sys_iam, sys_whoami
>>```
>
> 4.4 修改`Makefile`
>> 在文件`~/oslab/linux-0.11/kernel/Makefile`第`- 29`行，`OBJS = ` 的最后添加：
>>```c
>> who.o
>>```
>> 第`-83` 行之后添加：
>>```c
>> who.s who.o:who.c ../include/asm/segment.h ../include/string.h ../include/errno.h
>>```
> 4.5 实现 `sys_iam()` 和 `sys_whoami()`
> `*.c`文件放在`~/oslab/linux-0.11/kernel/`文档里， `*.h`文件放在`~/oslab/linux-0.11/include/`  
> 在`~/oslab/linux-0.11/kernel/`文件中创建`who.c`文件  
> 注意`get_fs_byte()`， `put_fs_byte()` 函数在`~/oslab/linux-0.11/include/asm/segment.h`
>```cpp
> #include<errno.h>
> #include<asm/segment.h>
> #include<unistd.h>
>
> char sys_name[24];  //23个字符 + '\0' = 24
>
> int sys_iam(const char * name){
>    char tmp[26];
>    int i;
>    for(i = 0; i < 26; i++){
>        tmp[i] = get_fs_byte(name + i);
>        if(tmp[i] == '\0') break;
>    }
>    //超出限制，输出-(EINVAL)(/* Invalid argument */)
>    if(i > 23) return -(EINVAL);
>    for(i = 0; i < 24; i++){
>        sys_name[i] = tmp[i];
>        if(tmp[i] == '\0') break;
>    }
>    return i;
>}
>
>int sys_whoami(char* name, unsigned int size){
>    int num = 0;
>    //计算长度
>    while(sys_name[num] != '\0'){
>        num++;
>    }
>
>    if(num > size) return -(EINVAL);
>
>    int i;
>    for(i = 0; i < size; i++){
>        put_fs_byte(sys_name[i], name + i);
>        if(sys_name[i] == '\0') break;
>    }
>    return i;
>}
>```
>
> 4.6 创建`iam.c`和`whoami.c`
> `sudo umount hdc` 挂载Bochs虚拟机硬盘
>> 4.6.1 在目录`~/oslab/hdc/usr/include`中创建`name.h`头文件
>>>```cpp
>>>#define __LIBRARY__
>>>#include<unistd.h>
>>>#include<errno.h>
>>>
>>>_syscall1(int,iam,const char*,name)
>>>_syscall2(int,whoami,char*,name,unsigned int,size)
>>>```
>>4.6.2 拷贝`sys.h`和`unistd.h` **这步很重要！**
>>> `cp ~/oslab/linux-0.11/include/linux/sys.h ~/oslab/hdc/usr/include`  
>>> `cp ~/oslab/linux-0.11/include/unistd.h ~/oslab/hdc/usr/include`  
>>
>>4.6.3 写`iam.c`和`whoami.c`两个函数
>>
>>
>>```cpp
>>//iam.c
>>#include<stdio.h>
>>#include<name.h>
>>
>>int main(int argc, char *argv[]){
>>    int r;
>>    if(argc != 2){
>>        puts("Argument Error!");
>>        r = -1;
>>    }else{
>>        r = iam(argv[1]);
>>        if(r != -1) r = 0;
>>    }
>>    return r;
>>}
>>```
>>
>>```cpp
>>//whoami.c
>>#include<stdio.h>
>>#include<name.h>
>>
>>int main(){
>>    char s[26];
>>	int res = whoami(s,26);
>>	if(res == -1){
>>		return res;
>>	}
>>	printf("%s\n",s);
>>
>>	return 0;
>>}
>>```
### 5.测试
>>