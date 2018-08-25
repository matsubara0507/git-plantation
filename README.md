# git-plantation

```
$ stack exec -- git-plantation-app example/config.yaml
problems @= [problem_name @= "tutorial" <: repo_name @= "matsubara0507/git-challenge-tutorial" <: difficulty @= 1 <: challenge_branches @= ["readme","master","task-1","task-2"] <: ci_branch @= "ci" <: nil] <: nil
```

## Usage

### 1. Write config file

ref. `example/config.yaml`

### 2. Create problem repository in team

using `git-plantation-tool`:

```
$ GH_TOKEN=XXX stack exec -- git-plantation-tool -c example/config.yaml --work .temp new_repo sample
```

### 3. Run app and drone

run app:

```
$ GH_TOKEN=XXX GH_SECRET=YYY stack exec -- git-plantation-app --port 8080 --work ".temp" --verbose example/config.yaml
```

run drone ci:

```
$ cd drone
$ docker-compose up
```

run ngrok:

```
$ ngrok start --config ngrok/config.yml app drone
```

setting ngrok URL in GitHub Webhook.
