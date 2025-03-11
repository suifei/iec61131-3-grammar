# IEC61131-3 Grammar for ANTLR4

[![GitHub](https://img.shields.io/github/stars/suifei/iec61131-3-grammar?style=social)](https://github.com/suifei/iec61131-3-grammar)
[![GPL-3.0 License](https://img.shields.io/github/license/suifei/iec61131-3-grammar)](https://github.com/suifei/iec61131-3-grammar/blob/main/LICENSE)
[![ANTLR Version](https://img.shields.io/badge/ANTLR-4.9+-blue)](https://www.antlr.org/)
[![IEC Standard](https://img.shields.io/badge/IEC-61131--3%202023-orange)](https://webstore.iec.ch/publication/67349)

## 简介

这个项目提供了一个完整的IEC61131-3第四版(2023)编程语言的ANTLR4语法文件。IEC61131-3是可编程逻辑控制器(PLC)编程语言的国际标准，定义了结构化文本(ST)、梯形图(LD)、功能块图(FBD)、顺序功能图(SFC)和指令表(IL)等编程语言。

本语法文件主要聚焦于结构化文本(ST)语法，但也包含了对SFC的支持。

## 仓库地址

https://github.com/suifei/iec61131-3-grammar

## 特性

- 支持IEC61131-3第四版(2023)标准
- 完整实现所有标准函数块和函数
- 支持面向对象扩展(接口、类、命名空间)
- 支持SFC(顺序功能图)语法元素
- 包含现代语言特性(异步/等待、Lambda表达式、泛型类型)
- 内置错误恢复和诊断机制
- 支持通信和分布式控制功能

## 使用方法

### 环境要求

- Java 8 或更高版本
- ANTLR 4.9 或更高版本

### 生成解析器

```bash
# 生成Java解析器代码
antlr4 -visitor IEC61131_3.g4

# 编译生成的Java代码
javac *.java
```

### 解析IEC61131-3代码

```bash
# 使用grun(ANTLR工具)测试解析
grun IEC61131_3 compilation_unit -gui your_program.st
```

## 语法覆盖范围

该语法支持以下IEC61131-3元素:

- 程序组织单元(POUs)：程序、函数块、函数
- 数据类型定义：基本类型、枚举、结构、数组
- 变量声明和初始化
- 流程控制：IF-THEN-ELSE、CASE、FOR、WHILE、REPEAT
- 表达式和运算符
- 接口和类(面向对象扩展)
- 顺序功能图(SFC)元素
- 任务和资源配置
- 命名空间和可见性控制

## 实际应用场景

- PLC编程语言的编译器和解释器
- 代码分析和验证工具
- 语法高亮和自动完成
- 代码重构工具
- 模型驱动的工程工具

## 许可证

本项目采用 [GNU通用公共许可证 3.0版本（GPL-3.0）](https://www.gnu.org/licenses/gpl-3.0.html) 开源。

### GPL-3.0 协议说明

GPL-3.0 是一个强著作权（Copyleft）许可证，它要求：

1. 任何修改或基于本项目创建的衍生作品必须以相同的许可证（GPL-3.0）发布
2. 任何使用本代码的软件也必须开源并采用GPL-3.0许可证
3. 必须保留原始作者的版权声明
4. 如果您分发本软件的修改版本，必须在显著位置明确说明您修改了源代码
5. 为用户提供相应的源代码获取方式

GPL-3.0 比早期版本有更强的专利保护条款和更明确的兼容性规定，能更好地保护您的权益。

## 贡献指南

欢迎提交问题报告和Pull请求。在提交Pull请求前，请确保您的代码:

1. 符合IEC61131-3标准规范
2. 通过所有语法测试用例
3. 不引入不必要的复杂性

欢迎在GitHub上提交Issue或Pull Request: https://github.com/suifei/iec61131-3-grammar

## 联系方式

如有问题或建议，请通过[GitHub Issues](https://github.com/suifei/iec61131-3-grammar/issues)提交，或联系[QQ 410000]。
