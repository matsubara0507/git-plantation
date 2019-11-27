# Changelog for git-plantation

- スコアボードにグラフを描写するページを追加(#49)
  - `end_time` と `zone` を設定に追加
  - `tesk9/palette` に対応させた `terezka/line-charts` のフォークをローカルで差し替え
- スコアボードのページの構造をリファクタリング(#50)
  - `Score` 型を作って両方のページで共用した
  - 一つの SPA にした
- チームごとのページを追加(#52)
  - 特定のチームだけのボード + グラフ
- スコアボードの見た目を改修(#52)
  - ヘッダーを追加した
  - `primer.css` の cdn を変更(13.2 にバージョンアップ)
  - ボード部分がでか過ぎるレイアウトを修正
- スコアボードに個人のページを追加(#56)
- GitHub Actions の設定を追加(#57)
- Elm のバージョンを 0.19.1 に変更(#57)

## Unreleased changes

## v0.4.0

* キャッシュ(store)の更新を遅延させた(#46)
* リセットするSlackボット用のAPIをAppから切り分けた (#47)
  * Slack からのIP制限をすることができないため
* リセットの結果はチャンネルにいる全員が見えるようにした (#47)
* Drone の Build を作成時刻でフィルタリングする機能を追加 (#48)
  * `start_time` 設定を追加(ただし unix time を直接書く)
  * `drone` が `Int64` に対応してなかったのでバージョンアップ

## v0.3.0

* mix パッケージを作成 (#26)
  * [tonatona](http://hackage.haskell.org/package/tonatona) にインスパイアされた [rio](http://hackage.haskell.org/package/rio) の薄いラッパーパッケージ
  * rio-logger, [github-client](https://github.com/matsubara0507/github/tree/collaborator-api), [drone-client](https://github.com/matsubara0507/drone-haskell), [shh](http://hackage.haskell.org/package/shh) プラグインも作成
  * ~~shelly を shh に移行~~ thred unsafe だったので shelly に戻した(#40)　
* リセットするSlackボット用のAPIを追加 (#27)
* shh-cmd パッケージを作成 (#28)
  * [shh](http://hackage.haskell.org/package/shh) の薄いラッパーパッケージ
  * よく使うコマンド関数を作成
* `tool` をサブサブコマンド化 (#29)
  * `git-plantation-tool (config|problem|member|repo) COMMAND` と指定する形式に変更
  * 合わせてディレクトリ構造も刷新
  * `problem` コマンドも追加
* `mix-json-logger` パッケージを作成(#30)
* GitHub Team に関するコマンドを追加(#31)
  * `github` パッケージを更新
  * `member invite/kick` に `--org` `--gh_team` オプションを追加して GitHub Team に招待/除外
  * `org create_team` コマンドで GitHub Team を作成
  * `repo add_gh_team` コマンドでリポジトリを GitHub Team に追加する
* 問題リポジトリの Drone CI をアクティベイトするコマンドを追加(#32)
* リポジトリのリセット処理を修正(#33)
  * ワークディレクトリをチームごとに区切ってたが、さらに問題リポジトリ用のサブディレクトリ `/problem` を追加
* 回答者を Slack に通知する機能を追加(#34)
* 回答リポジトのリデフォルトブランチを更新するコマンドを追加(#35)
  * `repo new` の時にも実行される
  * `github` パッケージが対応してなかったので独自で更新
* 独自キャッシュサーバー `git-plantation-store` を作成(#41)
  * DroneCI のビルドデータを必要な分だけキャッシュしている
  * スコアボードなどの負荷対策
  * App で GitHub Webohook 取得時・DroneCI での採点終了時に更新をする
* Drone とのやりとりに http を指定できるようにした(#42)
* `stack image container` を使わないで Docker Image を作れるように変更(#45)
  * stack v2 対応するため
* LTS を 14.6 に更新(#45)
  * `extensible` のバージョンを 0.6.1 に変更
  * `drone` のバージョンを 1.0.1 に更新
  * `github` を更新 (0.22 をマージしたもの)
  * `elm-export` から `elmap.hs` に変更
  * `mix.hs` を更新(`extensible` の 0.6 以上に対応したもの)
* stack docker integration の base image を `matsubara0507/stack-build` に変更(#45)
  * こっちの方が軽くて CI との相性が良い
* docker-compose での Drone のバージョンを 1.4.0 に変更

## v0.2.0

* org アカウント以外で config 設定 (#10)
* dotenv ファイルが使えるようになる (#10)
* `/score` エンドポイントで drone から取得できなくても空リストを返す (#10)
* `fail` の代わりに例外処理を追加 (#11)
* JSON 形式のログを追加(#11)
* 回答リポジトリの生成の各ステップコマンドを追加(#13)
  * GitHub に空リポジトリを作成
  * リポジトリを初期化(問題リポジトリを参照して)
  * 回答のためCIを問題リポジトリに設定
* メンバーを回答リポジトリに招待する(#13)
* 回答リポジトリのリセット(#13)
* コマンドの追加と変更(#14)
  * `verify` : 設定ファイルの検査
  * `delete_repo` : 回答リポジトリとCIの設定の削除
  * private リポジトリを生成できるように変更
  * org アカウント以外でもちゃんと動作するように修正
* 設定周りの更新(#14)
  * `Problem` と 回答リポジトリの対応関係を `id` にした
  * `provate` 設定の追加
  * `org` と `owner` を明示的に指定するように変更
* 解答リポジトリの生成時に GitHub Webhook の設定をする(#15)
* GitHub Webhook の設定をするコマンドを追加(#15)
* Webhook API の修正(#15)
  * 問題の検索周りの処理が間違っていた
* Docker イメージの修正(#15)
  * `static` ディレクトリの追加
  * git コマンドの設定を追加
* `.env` ファイルがなくても動作するように修正(#15)
* app の base image を変更(#16)
* `invite_member` コマンドの修正(#17)
  * 変数の指定間違い
  * 失敗時にエラーを返すように(`github` パッケージから修正)
* `setup_webhook` コマンドの GitHub Webhook の設定の仕方を修正(#17)
  * `APP_HOST` や `APP_PORT` を `APP_SERVER` 環境変数に変更して URL を修正
  * Secret を追加 (`GH_SECRET`)
* 各コマンドで `git checkout` の前に `git pull` をするように修正(#17)
* リポジトリ系のコマンドの `--repo` 引数を `problem.id` に変更(#17)
* work space をチームごとに区切るように修正(#17)
  * 別々のチームから同じ問題に対し同時にプッシュが来ても問題ないようになった
  * 同じチームから同じ問題で別々のブランチなどに対し同時にプッシュが来たらおそらくまずい
* webhook の時に `ci` ブランチにプッシュするのを `answer_branch` だけに限定(#17)
  * これで「同じチームから同じ問題で別々のブランチなどに対し同時にプッシュ」も平気
  * 一つのブランチでしか動作しないので
* スコアボードに「採点中」を追加(#19)
* `invite_member` コマンドを修正(#20)
  * 201 が返ってくる(`github` 側から修正)
* リポジトリ系コマンドの修正・変更(#20)
  * work directory の cd 先が間違っていたのを修正
  * `--repos` で複数問題を指定できるように変更
* スコアボードのスコアの総和の仕方が間違っていたのを修正(#20)
* スコアボードで回答リポジトリに飛べるようにした(#20)
* `new_repo` コマンドで任意の処理をスキップできるように変更(#21)
* スコアボードの更新間隔を設定ファイルから指定できるように変更(#21)
* 参加者をリポジトリからキックするコマンドを追加(#24)
* スコア取得失敗時に画面を真っ白にしないように修正(#39)
  * `api/scores` で取得できないときに前のモデルを見るようにした
* shell コマンド実行が thread unsafe だったのを修正(#40)
  * shh から shelly に戻した
* [mix パッケージ](https://github.com/matsubara0507/mix.hs)を外部のに変更(#38)
* LTS を 13.21 に更新(#38)
