.class public Programa
.super java/lang/Object

.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static maior(DD)D
	.limit stack 50
	.limit locals 5

	dload 0
	dload 2
	dcmpg
	ifgt L0
	goto L1
L0:
	dload 0
	d2i
	istore 4
	goto L2
L1:
	dload 2
	d2i
	istore 4
L2:
	iload 4
	i2d
	dreturn
.end method

.method public static fat(I)I
	.limit stack 50
	.limit locals 2

	ldc 1
	istore 1
L3:
	iload 0
	ldc 0
	if_icmpgt L6
	goto L5
L6:
	iload 0
	ldc 10
	if_icmplt L4
	goto L5
L4:
	iload 1
	iload 0
	imul
	istore 1
	iload 0
	ldc 1
	isub
	istore 0
	goto L3
L5:
	iload 1
	ireturn
.end method

.method public static somatorio(I)I
	.limit stack 50
	.limit locals 4

	ldc 0
	i2d
	dstore 2
	dload 2
	d2i
	ireturn
.end method

.method public static imprimir(Ljava/lang/String;D)V
	.limit stack 50
	.limit locals 3

	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload 0
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	dload 1
	invokevirtual java/io/PrintStream/println(D)V
	return
	return
.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 50
	.limit locals 6

	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Numero:"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	new java/util/Scanner
	dup
	getstatic java/lang/System/in Ljava/io/InputStream;
	invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
	invokevirtual java/util/Scanner/nextInt()I
	istore 1
	ldc2_w 6.5
	d2i
	invokestatic Programa/fat(I)I
	istore 0
	ldc2_w 2.5
	ldc 10
	i2d
	invokestatic Programa/maior(DD)D
	dstore 2
	ldc "teste:"
	ldc 9
	i2d
	invokestatic Programa/imprimir(Ljava/lang/String;D)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 0
	invokevirtual java/io/PrintStream/println(I)V
	return
	return
.end method
