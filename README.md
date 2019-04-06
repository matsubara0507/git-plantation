# git-plantation

[![Build Status](https://travis-ci.org/matsubara0507/git-plantation.svg?branch=master)](https://travis-ci.org/matsubara0507/git-plantation)
[![](https://images.microbadger.com/badges/image/matsubara0507/git-plantation.svg)](https://microbadger.com/images/matsubara0507/git-plantation "Get your own image badge on microbadger.com")

![](./image/scoreboard.png)

## Requirement

- [Haskell Stack](https://docs.haskellstack.org/) : use to build application
- [Elm compiler](http://elm-lang.org/) : use to build scoreboard page
- Docker and docker-compose : optional, if run Drone on local
- [ngrok](https://ngrok.com/) : optional, if run Drone and app on local

## Usage

### 0. Drone の起動

もしローカルで起動する場合は:

1. ngrok などで外への通信を開ける(`ngrok http 8000`)
2. Drone の GitHub App を作成し `Authorization callback URL` に `https://{ngrok's url}/login` を追加
2. `drone/.env` を設定する(ref. `drone/.env.template`)
  - `DRONE_HOST` に ngrok の URL を設定する
  - `DRONE_GITHUB_CLIENT` と `DRONE_GITHUB_SECRET` に GitHub App のものを設定
  - `DRONE_SECRET` には適当な文字列を設定
4. `drone` ディレクトリで `docker-compose up`

これで ngrok の生成した URL にアクセスすると Drone CI にアクセスできる

### 1. git-plantation の設定を記述

ref. `config/.git-plantation.yaml`

### 2. 環境変数を設定する

ref. `.env.template`

- `PORT` は app のポート (app を docker で起動する場合)
- `WORK` は git コマンドを実行するワークディレクトリ (app を docker で起動する場合)
- `CONFIG` は git-plantation の設定のパス (app を docker で起動する場合)
- `DRONE_HOST` は Drone CI の URL
- `DRONE_PORT` は Drone CI のポート
- `DRONE_TOKEN` は Drone CI のトークン
- `GH_TOKEN` は GitHub のトークン
- `GH_SECRET` は GitHub Webhook のシークレットキー
- `APP_SERVER` は `git-plantation-app` が動作してる URL (例: `https://example.com`)

### 3. Create team's repository in team

using `git-plantation-tool`:

```
$ stack exec -- git-plantation-tool -c .git-plantation.yaml --work .temp new_repo sample
```

### 4. Run app

run app:

```
$ stack exec -- git-plantation-app --port 8080 --work ".temp" --verbose .git-plantation.yaml
```

## Build with Docker

Define environment to `.env` from `.env.template`.

```
$ stack test # ganerate elm code
$ stack docker pull
$ stack --docker --no-terminal build -j 1 Cabal # if `out of memory`
$ stack --docker image container
$ docker build -t git-plantation .
$ docker run --rm -it -v `pwd`/config.yaml:/work/config.yaml -p 8080:8080 --env-file .env git-plantation
```
