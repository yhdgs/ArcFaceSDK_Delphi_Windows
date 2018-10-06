# ArcFaceSDK_Delphi
基于虹软免费人脸识别库（Windows版）的Delphi(10.2.3)封装，支持检测、跟踪、验证、年龄、性别功能

## 文件列表

Source	源码目录

Source\ArcFaceSDK.pas	基于 TBitmap 封装

Source\ArcFaceSDKIEVersion.pas	基于 ImageEN 封装

Source\amcomDef.pas	虹软公共C++头文件翻译

Source\ammemDef.pas	虹软公共C++头文件翻译

Source\arcsoft_fsdk_age_estimation.pas	虹软年龄识别API C++头文件翻译

Source\arcsoft_fsdk_face_detection.pas	虹软人脸检测API C++头文件翻译

Source\arcsoft_fsdk_face_recognition.pas	虹软人脸特征提取API C++头文件翻译

Source\arcsoft_fsdk_face_tracking.pas	虹软人脸追踪API C++头文件翻译

Source\arcsoft_fsdk_gender_estimation.pas	虹软性别识别API C++头文件翻译

Source\arcsoft_fsdk_fic.pas	虹软人证SDK  API C++头文件翻译

Source\asvloffscreenDef.pas	虹软公共C++头文件翻译

Source\merrorDef.pas	虹软错误代码定义

Demo	演示项目（Delphi10.2.3编译通过）

Demo\BmpDemo	基于TArcFaceSDK的DEMO，无需其他第三方控年

Demo\BmpRZDemo	基于TArcFaceSDK的人证SDK DEMO，无需其他第三方控

Demo\IEDemo	基于TArcFaceSDKIEVersion的DEMO，需ImageEN支持

Demo\IERZDemo	基于TArcFaceSDKIEVersion的人证SDK DEMO，需ImageEN支持

README.md	说明文档

## 虹软人证SDK使用
1、先到虹软主页下载人证SDK（需要认证后才有下载权限），共四个DLL：

	libarcsoft_fsdk_face_tracking.dll

	libarcsoft_fsdk_face_recognition.dll

	libarcsoft_fsdk_face_detection.dll

	libarcsoft_fsdk_fic.dll

2、更名SDK，由于人证SDK简化了函数，不能直接调用tracking、recognition、detection链接库函数，所以如果要和普通SDK共存使用，需将原普通SDK以下三个文件进行更名：

	libarcsoft_fsdk_face_tracking.dll    -->  libarcsoft_fsdk_face_n_tracking.dll

	libarcsoft_fsdk_face_recognition.dll    -->  libarcsoft_fsdk_face_n_recognition.dll

	libarcsoft_fsdk_face_detection.dll    -->  libarcsoft_fsdk_face_n_detection.dll 

当然你可更改成其他名称，只要将相关头文件中的文件名常量更改即可

3、启用人证SDK配置，修改 ArcFace.inc ，将 {$DEFINE ARC_RZ_SDK} 激活

SDK下载：http://www.arcsoft.com.cn/ai/arcface.html


## 更新历史

2018.10.07

1、移动源码目录到Source下

2018.9.24

1、移除 ArcFaceSDK 单元对 ImageEN 的依赖

2、移动所有DEMO到demo目录下

3、新增基于 ImageEN 的人证SDK DEMO （IERzDemo）

2018.9.23

1、新增对人证SDK的支持，可以提高二代证芯片中照片的1:1比对相似度20%左右，官方推荐相似度 0.82 以上即可认定同一人

2、修正 TArcFaceSDK DrawFaceRect、DrawFaceRectAgeGender函数字体问题，根据图像缩放率自动调整字体大小，保证摄像头缩放后字体仍清晰，并增加人脸框区域透明叠加支持

3、新增人证SDK DEMO，在BmpRZDemo下