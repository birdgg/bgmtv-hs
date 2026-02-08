# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
cabal build          # 构建项目
cabal repl           # 启动 GHCi REPL
cabal haddock        # 生成文档
```

## Architecture

BGM.tv API 的 Haskell 客户端库，使用 servant-client 实现类型安全的 HTTP 请求。

### 模块结构

- **Web.Bgmtv** - 主入口模块，重导出所有公共 API
- **Web.Bgmtv.Types.Id** - 类型安全的 ID 包装器 (`SubjectId`, `EpisodeId`)
- **Web.Bgmtv.Types.Enums** - 枚举类型 (`SubjectType`, `EpisodeType`, `Platform`)
- **Web.Bgmtv.Types.Subject** - 条目相关类型 (`Subject`, `SubjectDetail`, `SubjectImages`)
- **Web.Bgmtv.Types.Episode** - 剧集相关类型 (`Episode`, `EpisodesResponse`)
- **Web.Bgmtv.Types.Search** - 搜索相关类型 (`SearchRequest`, `SearchFilter`, `SearchResponse`)
- **Web.Bgmtv.API** - Servant API 类型定义，使用 `NamedRoutes` 模式
- **Web.Bgmtv.Client** - 客户端实现，包含高级便利函数和低级 `ClientM` 函数

### API 层次

1. **高级函数** (`searchAnime`, `getSubject`, `getAllEpisodes`) - 返回 `IO (Either BgmtvError a)`，自动处理配置和 HTTP 管理器
2. **低级函数** (`searchSubjectsM`, `getSubjectM`, `getEpisodesM`) - 返回 `ClientM a`，可通过 `runBgmtv` 组合执行

### 客户端创建

```haskell
-- 简单用法：自动创建 HTTP Manager
client <- newBgmtvClient (defaultConfig "my-app/1.0")

-- 高级用法：传入自定义 Manager（共享连接池、自定义 TLS 设置等）
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

manager <- newManager tlsManagerSettings
let client = newBgmtvClientWith manager (defaultConfig "my-app/1.0")
```

### 类型安全

ID 类型使用 newtype 包装，防止编译时混淆不同类型的标识符：

```haskell
-- SubjectId 和 EpisodeId 是不同类型，编译器会阻止混用
getSubject :: BgmtvConfig -> SubjectId -> IO (Either BgmtvError SubjectDetail)
getAllEpisodes :: BgmtvConfig -> SubjectId -> IO (Either BgmtvError [Episode])

-- 创建 ID
let sid = SubjectId 12345
let eid = EpisodeId 67890
```

### 默认启用的 GHC 扩展

项目使用 GHC2021 语言标准，额外启用：`DataKinds`, `DeriveAnyClass`, `DuplicateRecordFields`, `OverloadedRecordDot`, `OverloadedStrings`, `TypeOperators`
