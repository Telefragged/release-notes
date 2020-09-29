open Octokit
open FSharp.Control

open NuGet.Versioning

let toVersion (versionString: string) =
    match NuGetVersion.TryParseStrict(versionString.Trim().TrimStart('v')) with
    | (true, version) -> version
    | (false, _) -> NuGetVersion(0, 0, 0)

type RepositoryTag with
    member this.Version = toVersion this.Name

let (|Insensitive|_|) (compare: string) (str: string) =
    if str.ToUpperInvariant() = compare.ToUpperInvariant()
    then Some str
    else None

[<RequireQualifiedAccess>]
type CommentMode =
    | Off
    | On

module CommentMode =
    let tryParse str =
        match str with
        | Insensitive "false" _
        | Insensitive "off" _ -> Some CommentMode.Off
        | Insensitive "true" _
        | Insensitive "on" _ -> Some CommentMode.On
        | _ -> None

let (|CommentMode|_|) str = CommentMode.tryParse str

let getTagPair (client: GitHubClient) (owner: string) (name: string) (tagName: string) =
    async {
        let! tags =
            client.Repository.GetAllTags(owner, name)
            |> Async.AwaitTask

        let sortedTags =
            tags
            |> Seq.sortByDescending (fun tag -> tag.Version)
            |> List.ofSeq

        let version = NuGetVersion(tagName.TrimStart('v'))

        let firstTag =
            sortedTags
            |> List.tryFind (fun tag -> toVersion tag.Name = version)

        let secondTag =
            sortedTags
            |> Seq.skipWhile (fun tag -> tag.Version > version)
            |> Seq.tryFind (fun tag ->
                not <| tag.Version.IsPrerelease
                && tag.Version <> version)

        return (firstTag, secondTag)
    }

let getCommits (client: GitHubClient)
               (owner: string)
               (name: string)
               (firstTag: RepositoryTag option)
               (secondTag: RepositoryTag option)
               =
    async {
        let! compareResult =
            match firstTag, secondTag with
            | Some current, Some previous ->
                client.Repository.Commit.Compare(owner, name, previous.Commit.Sha, current.Commit.Sha)
                |> Async.AwaitTask
            // | [current] ->
            //     client.Repository.Commit.

            | _ -> failwith "Couldn't get commits"

        return compareResult.Commits
    }

open System.Text.RegularExpressions

let parseCommits client owner repository firstTag secondTag =
    asyncSeq {
        let regex =
            Regex("""(?:(?<![/\w-.])(\w[\w-.]+)\/(\w[\w-.]+)|\B)#([1-9]\d*)\b""")

        let! commits = getCommits client owner repository firstTag secondTag

        for commit in commits do
            use lineReader =
                new System.IO.StringReader(commit.Commit.Message)

            let mutable line = lineReader.ReadLine()

            while not <| isNull line do
                for m in regex.Matches(line) do
                    match List.ofSeq m.Groups with
                    | _ :: owner :: repository :: [ issue ] when owner.Success && repository.Success ->
                        yield (owner.Value, repository.Value, int issue.Value)
                    | _ :: _ :: _ :: [ issue ] -> yield (owner, repository, int issue.Value)
                    | _ -> failwith "wtf"

                line <- lineReader.ReadLine()
    }

let getIssueLine (client: GitHubClient) owner repository issueNumber =
    async {
        let! issue =
            client.Issue.Get(owner, repository, issueNumber)
            |> Async.AwaitTask

        return sprintf "%s (%s/%s#%d)" issue.Title owner repository issueNumber
    }

let createIssueComment (client: GitHubClient)
                       (releaseName: string)
                       (releaseUrl: string)
                       (owner: string)
                       (repository: string)
                       (issueNumber: int)
                       =
    let body =
        sprintf "This issue has been referenced in release [%s](%s)" releaseName releaseUrl

    client.Issue.Comment.Create(owner, repository, issueNumber, body)
    |> Async.AwaitTask
    |> Async.Ignore

let setReleaseText (client: GitHubClient)
                   (owner: string)
                   (repository: string)
                   (tagName: string)
                   (commentMode: CommentMode)
                   =
    async {
        let! (firstTag, secondTag) = getTagPair client owner repository tagName

        let! issues =
            parseCommits client owner repository firstTag secondTag
            |> AsyncSeq.toListAsync

        let issues = List.distinct issues

        let! issueText =
            issues
            |> AsyncSeq.ofSeq
            |> AsyncSeq.mapAsync (fun tp -> tp |||> (getIssueLine client))
            |> AsyncSeq.fold (fun state cur -> state + "\n- " + cur) System.String.Empty

        let sinceText =
            match secondTag with
            | Some tag -> sprintf "previous release (%s)" tag.Name
            | None -> "start"

        let body =
            sprintf """Release %s

Issues referenced since %s: %s""" tagName sinceText issueText

        let versionInfo = toVersion tagName

        let newRelease =
            NewRelease(tagName, Name = tagName, Body = body, Prerelease = versionInfo.IsPrerelease, Draft = false)

        let! newRelease =
            client.Repository.Release.Create(owner, repository, newRelease)
            |> Async.AwaitTask

        match commentMode with
        | CommentMode.On ->
            do! issues
                |> AsyncSeq.ofSeq
                |> AsyncSeq.iterAsync (fun tp ->
                    tp
                    |||> createIssueComment client tagName newRelease.HtmlUrl)
        | CommentMode.Off -> ()
    }

open System

let environVarOrFail variableName =
    Environment.GetEnvironmentVariable(variableName)
    |> Option.ofObj
    |> Option.defaultWith (fun () -> failwithf "Environment variable %s not found" variableName)

open Argu

[<RequireQualifiedAccess>]
type Arguments =
    | [<CustomAppSettings("COMMENT_MODE")>] Comment_Mode of CommentMode
    | [<CustomAppSettings("TOKEN"); Mandatory; AltCommandLine("-t")>] Token of token: string
    | [<CustomAppSettings("GITHUB_REF"); Mandatory>] Github_Ref of ref: string
    | [<CustomAppSettings("GITHUB_REPOSITORY"); Mandatory>] Github_Repository of repository: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Comment_Mode _ -> "Enable or disable comments for the referenced issues. Default off"
            | Token _ -> "The Github API token"
            | Github_Ref _ -> "The git ref or tag name"
            | Github_Repository _ -> "The repository, in format '<owner>/<repository>'"

[<EntryPoint>]
let main argv =
    let envReader = EnvironmentVariableConfigurationReader()

    let errorHandler =
        ProcessExiter
            (colorizer = function
            | ErrorCode.HelpText -> None
            | _ -> Some ConsoleColor.Red)

    let parser =
        ArgumentParser.Create<Arguments>(programName = "release-notes", errorHandler = errorHandler)

    let results =
        parser.Parse(argv, configurationReader = envReader)

    let tagName =
        let tagName = results.GetResult(Arguments.Github_Ref)

        let tagName =
            if tagName.StartsWith("refs/tags/") then tagName.Substring(10) else tagName

        NuGetVersion.TryParseStrict(tagName.TrimStart('v'))
        |> function
        | (true, _) -> tagName
        | (false, _) ->
            printf "Ref %s is not a version, exiting" tagName
            exit 0

    let (owner, repository) =
        let repository =
            results.GetResult(Arguments.Github_Repository)

        match List.ofArray (repository.Split('/')) with
        | owner :: [ repository ] -> (owner, repository)
        | _ -> failwithf "Failed to get repository name from %s" repository

    let token = results.GetResult(Arguments.Token)

    let commentMode =
        results.TryGetResult(Arguments.Comment_Mode)
        |> Option.defaultValue CommentMode.Off

    let client =
        GitHubClient(ProductHeaderValue("release-notes"), Credentials = Credentials(token))

    try
        setReleaseText client owner repository tagName commentMode
        |> Async.RunSynchronously
    with ex -> printfn "%A" ex

    let apiInfo = client.GetLastApiInfo()
    printfn "Remaining calls: %d, Refresh: %s" (apiInfo.RateLimit.Remaining) (apiInfo.RateLimit.Reset.ToString())

    0 // return an integer exit code
