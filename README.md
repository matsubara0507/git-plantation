# git-plantation

![](./image/scoreboard.png)

## Usage

config 以下で docker-compose を使う

### 1. git-plantation の設定を記述

ref. `config/.git-plantation.yaml`

### 2. 環境変数を設定する


`config/docker-compose.yml` の `app` の `environment` を参照

```
# スコアボードにログインするための GitHub OAuth のクライアントIDとシークレット
AUTHN_CLIENT_ID
AUTHN_CLIENT_SECRET

# スコアサーバーが GitHub Webhook を受けたときにリクエストを検証するためのシークレット
# 同様の値を回答リポジトリに設定している（リポジトリ作成は自作 CLI ツール）
GH_SECRET

# 回答リポジトリの push を受け取ったことを Slack へ通知するための Incomming Webhook
# 管理者側で誰がいつ push したのかを観測するために利用する（基本、参加者には見えないようにする）
SLACK_PUSH_NOTIFY_WEBHOOK

# ジョブサーバーから採点の結果をスニペットで通知するための Slack API トークン
SLACK_API_TOKEN

# 以下は Slack のスラッシュコマンドで回答リポジトリのリセットを受けるための秘匿情報
SLACK_SIGNING_SECRET # Slack からのリクエストを検証するのに使う
SLACK_VERIFY_TOKEN   # 同上
SLACK_SLASH_WEBHOOK  # スラッシュコマンドの受け付けメッセージを送るための Incomming Webhook

# 以下は回答リポジトリのリセットをする際に使う
GH_TOKEN # GitHub API を実行したり、リポジトリを操作するための API トークン
GH_USER  # トークンのアカウント（リポジトリの操作で必要）
```

git-plantation-tool が利用する環境変数は下記の通り(用途は同じ):

- `GH_TOKEN`
- `GH_USER`
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
