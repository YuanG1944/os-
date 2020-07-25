## LAB1: 操作系统的引导s
### 1. 实验目的
> + 熟悉 hit-oslab 实验环境；
> + 建立对操作系统引导过程的深入认识；
> + 掌握操作系统的基本开发过程；
> + 能对操作系统代码进行简单的控制，揭开操作系统的神秘面纱；

### 2. 实验内容
> 1) **改写 `bootsect.s` 主要完成如下功能：**
>> bootsect.s 能在屏幕上打印一段提示信息“XXX is booting...”，其中 XXX 是你给自己的操作系统起的名字.
> 2) **改写 `setup.s` 主要完成如下功能：**
>> + bootsect.s 能完成 setup.s 的载入，并跳转到 setup.s 开始地址执行。而 setup.s 向屏幕输出一行"Now we are in SETUP"。
>> + setup.s 能获取至少一个基本的硬件参数（如内存参数、显卡参数、硬盘参数等），将其存放在内存的特定地址，并输出到屏幕上。
>> + setup.s 不再加载 Linux 内核，保持上述信息显示在屏幕上即可。

### 3. 修改`bootsect.s` 和 `setup.s`
> 菜鸡对汇编知之甚少，完全按照参考copy的代码：
> 代码实现了打印个性化操作系统名字，和各个已知硬件信息，并未停掉加载linux内核
```s
! bootsect.s
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

.globl begtext, begdata, begbss, endtext, enddata, endbss !六个全局标识符
.text !文本段
begtext:
.data  !数据段
begdata:
.bss  !未初始化数据段(BLock Started by Symbol)
begbss:
.text !文本段

SETUPLEN = 4				! nr of setup-sectors setup的扇区数
BOOTSEG  = 0x07c0			! original address of boot-sector 
INITSEG  = 0x9000			! we move boot here - out of the way
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

entry _start
_start:
	mov	ax,#BOOTSEG
	mov	ds,ax  !ds段寄存器置为0x7c0
	mov	ax,#INITSEG
	mov	es,ax  !es段寄存器置为0x9000;
	mov	cx,#256
	sub	si,si
	sub	di,di
	rep
	movw
	jmpi	go,INITSEG
go:	mov	ax,cs
	mov	ds,ax
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
	int	0x13			! read it
	jnc	ok_load_setup		! ok - continue
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs
	mov	sectors,cx
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#21
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

!/*
! * This procedure turns off the floppy drive motor, so
! * that we enter the kernel in a known state, and
! * don't have to worry about it later.
! */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Welcome to YOS~"
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:

```

```s
!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

!---增加的代码--打印‘setup’正在运行
! Print some inane message

	mov	    ax, #SETUPSEG	
	mov     es, ax
    mov     ah, #0x03
    mov     bh, #0
    int     0x10
	
	mov	cx,#26
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg2
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

!---增加的代码--打印‘setup’正在运行

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.
! Get memory size (extended mem, kB)

	mov	ah,#0x88
	int	0x15
	mov	[2],ax
	mov     cx, #23
    mov     bx, #0x0007
    mov     bp, #msg3
    mov     ax, #0x1301
    int     0x10
    mov     dx, [2]
    add     dx, #0x400  !Print the total memory.
    mov     cx, #4
    call    printargs

! Get video-card data:

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

	mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #13
    mov     bx, #0x0007
    mov     bp, #msg4
    mov     ax, #0x1301
    int     0x10
    mov     dx, [4]
    mov     cx, #2      !Just print display page.
    call    printargs

    mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #11
    mov     bx, #0x0007
    mov     bp, #msg5
    mov     ax, #0x1301
    int     0x10
    mov     dx, [6]
    mov     dh, dl      !Get video mode and print it.
    mov     cx, #2
    call    printargs

    mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #25
    mov     bx, #0x0007
    mov     bp, #msg6
    mov     ax, #0x1301
    int     0x10
    mov     dx, [6]
    mov     cx, #2      !Just print window width.
    call    printargs

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

	mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #12
    mov     bx, #0x0007
    mov     bp, #msg7
    mov     ax, #0x1301
    int     0x10
    mov     dx, [10]
    mov     cx, #2      !Just print video state.
    call    printargs

    mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #18
    mov     bx, #0x0007
    mov     bp, #msg8
    mov     ax, #0x1301
    int     0x10
    mov     dx, [10]
    mov     dh, #0      !Calculate the video memory and print it.
    mov     cx, dx
    add     cx, #1
    mov     dx, #0
    calvm:
        add     dx, #64
        loop    calvm
    mov     cx, #4
    call    printargs


! Get hd0 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb


mov     ax,#INITSEG
    mov     ds,ax
    mov     ax, #SETUPSEG
    mov     es, ax

    mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #10
    mov     bx, #0x0007
    mov     bp, #msg9
    mov     ax, #0x1301
    int     0x10
    mov     dx, [128]
    mov     cx, #4
    call    printargs

    mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #6
    mov     bx, #0x0007
    mov     bp, #msg10
    mov     ax, #0x1301
    int     0x10
    mov     dx, [130]
    mov     cx, #4
    call    printargs

    mov     ah, #0x03
    mov     bh, #0
    int     0x10
    mov     cx, #8
    mov     bx, #0x0007
    mov     bp, #msg11
    mov     ax, #0x1301
    int     0x10
    mov     dx, [142]
    mov     cx, #4
    call    printargs
! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...

	cli			! no interrupts allowed !

! first we move the system to it's rightful place

	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		! source segment
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000
	rep
	movsw
	jmp	do_move

! then we load the segment descriptors

end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0
	lgdt	gdt_48		! load gdt with whatever appropriate

! that was painless, now we enable A20

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.

	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it!
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

gdt:
	.word	0,0,0,0		! dummy

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

printargs:
    mov ax, #0x0E30
    int 0x10
    mov ax, #0x0E58
    int 0x10
    print_digit:
        mov ax, #0x0E0F
        rol dx, #4
        and al, dl
        cmp al, #0x0A
        jb ltA
        jnb nltA
        ltA:
            add al, #0x30
            jmp ctnp
        nltA:
            add al, #0x37
            jmp ctnp
        ctnp:
            int 0x10
    loop print_digit
    mov ax, #0x0E0D
    int 0x10
    mov ax, #0x0E0A
    int 0x10
    ret

msg2:
    .byte 13,10
    .ascii "Now we are in SETUP."
    .byte 13,10,13,10
	
msg3:
    .ascii "Total Memory Size(KiB):"

msg4:
    .ascii "Display Page:"

msg5:
    .ascii "Video Mode:"

msg6:
    .ascii "Window Width(Characters):"

msg7:
    .ascii "Video State:"

msg8:
    .ascii "Video Memory(KiB):"

msg9:
    .ascii "Cylinders:"

msg10:
    .ascii "Heads:"

msg11:
    .ascii "Sectors:"



idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries
	.word	512+gdt,0x9	! gdt base = 0X9xxxx
	
.text
endtext:
.data
enddata:
.bss
endbss:
```