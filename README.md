# git-plantation

[![Build Status](https://travis-ci.org/matsubara0507/git-plantation.svg?branch=master)](https://travis-ci.org/matsubara0507/git-plantation)
[![](https://github.com/matsubara0507/git-plantation/workflows/.github/workflows/build.yml/badge.svg)](https://github.com/matsubara0507/git-plantation/actions?query=branch%3Amaster)
[![](https://images.microbadger.com/badges/image/matsubara0507/git-plantation.svg)](https://microbadger.com/images/matsubara0507/git-plantation "Get your own image badge on microbadger.com")

![](./image/scoreboard.png)

## Usage

config 以下で docker-compose を使う

### 0. Drone の起動

もしローカルで起動する場合は:

1. ngrok などで外への通信を開ける(`ngrok http 8000`)
2. Drone の GitHub App を作成し `Authorization callback URL` に `https://{ngrok's url}/login` を追加
3. `config/.env` を設定する(ref. `config/.env.template`)
  - `DRONE_HOST` に ngrok の URL を設定する
  - `DRONE_GITHUB_CLIENT` と `DRONE_GITHUB_SECRET` に GitHub App のものを設定
  - `DRONE_SECRET` には適当な文字列を設定
  - `HOSTNAME` は agent のラベルなので適当な識別用文字列を設定
4. `config` ディレクトリで `docker-compose up drone agent`

これで ngrok の生成した URL にアクセスすると Drone CI にアクセスできる

### 1. git-plantation の設定を記述

ref. `config/.git-plantation.yaml`

### 2. 環境変数を設定する


`config/docker-compose.yml` の `app` の `environment` を参照

- `PORT` は app のポート (app を docker で起動する場合)
- `WORK` は git コマンドを実行するワークディレクトリ (app を docker で起動する場合)
- `CONFIG` は git-plantation の設定のパス (app を docker で起動する場合)
- `DRONE_HOST` は Drone CI の URL
- `DRONE_PORT` は Drone CI のポート
- `DRONE_TOKEN` は Drone CI のトークン
- `DRONE_HTTP` は Drone CI と HTTP 通信するかどうかの設定 (`false` の場合は HTTPS)
- `GH_TOKEN` は GitHub のトークン
- `GH_SECRET` は GitHub Webhook のシークレットキー
- Slack で回答リポジトリをリセットするための Slack Bot の設定
    - `SLACK_API_TOKEN` は slack トークン
    - `SLACK_TEAM_ID` は slack の workspace の ID
    - `SLACK_CHANNEL_IDS` はリセットできる slack channel の IDs
    - `SLACK_RESET_REPO_CMD` はリセットボットのコマンド
- `SLACK_WEBHOOK` は回答者の通知を slack にするための Slack Webhook URL を設定
- `STORE_URL` は git-plantation-store の URL を設定

`config/docker-compose.yml` の `store` の `environment` を参照するとわかる通り，一部は git-plantation-store でも利用される．
ちなみに，docker-compose を使う場合は `config/.env.template` に書いてある分だけを設定すれば良い．

git-plantation-tool が利用する環境変数は下記の通り(用途は同じ):

- `DRONE_HOST`
- `DRONE_PORT`
- `DRONE_TOKEN`
- `GH_TOKEN`
- `GH_SECRET`
- `APP_SERVER` は git-plantation-app が動作してる URL (例: `https://example.com`) で GitHub Webhook に設定する

### 3. Create team's repository in team

using `git-plantation-tool`:

```
$ git-plantation-tool -c .git-plantation.yaml --work .temp repo new sample
```

### 4. Run app

```
$ docker-compose up app store
```

## Build with Docker

```
$ make image tag=matsubara0507/git-plantation:dev
```

## Reset repository By Slack

use `git-plantation-slack` with docker-compose:

```
$ docker-compose up slack
```
