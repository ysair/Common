1、 MD5String、MD5File、MD5Print、MD5Match这四个函数是供调用的。其他是用来辅助这几个函数的子函数。
2、MD5String为加密字符串。
3、MD5File为加密这个文件。
4、MD5Print是将加密后的密文转换成字符串。
5、MD5Match是用来比较密文是否一致。

加密字符串aaa             MD5String('aaa')
将加密后的aaa显示出来     MD5Print(MD5String('aaa'))
比较两次密文是否一致：    MD5Match(MD5String('第一次明文'),MD5String('第二次输入的明文'))

如果要用这个单元，只需要在form上添加uses md5，就可以了

将md5.pas放到Delphi6\Lib目录下