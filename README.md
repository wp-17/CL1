# Cl1 Core

Cl1 是一个使用 Chisel 开发的 32 位 RISC-V 处理器核。

## 处理器特性

| 特性 | 描述 |
|------|------|
| 架构 | RV32IMC_Zicsr |
| 指令集 | I (基础整数) + M (乘除法) + C (压缩指令) + Zicsr (CSR 操作) |
| 流水线 | 多级流水线，支持可选的 WB 流水级 |
| 缓存 | 可选的 ICache 和 DCache |
| 总线 | AXI4 接口 |
| 调试 | 支持 RISC-V Debug Module |
| 中断 | 支持外部中断、软件中断、定时器中断 |
| 特权模式 | Machine Mode |

## 目录结构

```
cl1_core/
├── cl1/
│   └── src/
│       ├── scala/          # 核心 Chisel 源码
│       │   ├── Cl1Config.scala    # 配置对象
│       │   ├── Cl1Core.scala      # 核心顶层
│       │   ├── Cl1Top.scala       # 顶层封装
│       │   ├── Cl1IFStage.scala   # 取指阶段
│       │   ├── Cl1IDEXStage.scala # 译码执行阶段
│       │   ├── Cl1WBStage.scala   # 写回阶段
│       │   ├── Cl1ALU.scala       # 算术逻辑单元
│       │   ├── Cl1MDU.scala       # 乘除单元
│       │   ├── Cl1LSU.scala       # 访存单元
│       │   ├── Cl1CSR.scala       # CSR 寄存器
│       │   ├── Cl1ICACHE.scala    # 指令缓存
│       │   ├── Cl1DCACHE.scala    # 数据缓存
│       │   ├── Cl1BPU.scala       # 分支预测单元
│       │   └── ...
│       └── utils/          # 工具模块
├── vsrc/                   # 生成的 Verilog 输出
├── wave/                   # 仿真波形文件
├── patch/                  # firtool 补丁
├── build.sc                # Mill 构建配置
├── Makefile                # 构建脚本
└── flake.nix               # Nix flake 配置
```

## 配置说明

### globalConfig

控制全局编译模式，三者必须有且仅有一个为 `true`：

| 配置项 | 描述 |
|--------|------|
| `syn` | 综合模式 |
| `simpleSocTest` | 简单 SoC 测试模式 |
| `fullSocTest` | 完整 SoC 测试模式 |

### Cl1Config

| 配置项 | 默认值 | 描述 |
|--------|--------|------|
| `BOOT_ADDR` | `0x80000000` | 处理器启动地址 |
| `TVEC_ADDR` | `0x20000000` | 异常向量基地址 |
| `BUS_WIDTH` | 32 | 总线数据宽度 |
| `CKG_EN` | false | WFI时钟门控使能 |
| `difftest` | false | 差分测试使能 |
| `DBG_ENTRYADDR` | `0x800` | 调试模式入口地址 |
| `DBG_EXCP_BASE` | `0x800` | 调试异常基地址 |
| `MDU_SHAERALU` | true | MDU 与 ALU 共享数据通路 |
| `WB_PIPESTAGE` | true | 启用 WB 流水级 |
| `HAS_ICACHE` | true | 启用指令缓存 |
| `HAS_DCACHE` | true | 启用数据缓存 |
| `RST_ACTIVELOW` | true | 低电平有效复位 |
| `RST_ASYNC` | true | 异步复位 |
| `SOC_DIFF` | false | SoC 差分测试接口 |
| `SramFoundary` | false | 使用 Foundry SRAM |
| `SOC_D64` | false | 64 位 AXI 数据总线 |

## 构建与生成 Verilog

### 环境要求

- Mill 构建工具
- Scala 2.13.15
- Chisel 6.6.0
- firtool 1.105.0 (会自动下载)

### 生成 Verilog

```bash
make verilog
```

生成的 Verilog 文件位于 `vsrc/Cl1Top.sv`。

