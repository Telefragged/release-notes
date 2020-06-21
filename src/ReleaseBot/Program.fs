open Octokit
open FSharp.Control

open NuGet.Versioning

let toVersion (versionString : string) =
    match NuGetVersion.TryParseStrict(versionString.Trim().TrimStart('v')) with
        | (true, version) -> version
        | (false, _) -> NuGetVersion(0, 0, 0)

type RepositoryTag with
    member this.Version =
        toVersion this.Name

let getTagPair (client : GitHubClient)
               (owner : string)
               (name : string)
               (tagName : string) =
    async {
        let! tags = client.Repository.GetAllTags(owner, name) |> Async.AwaitTask

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
            |> Seq.tryFind (fun tag -> not <| tag.Version.IsPrerelease && tag.Version <> version)

        return (firstTag, secondTag)
    }

let getCommits (client : GitHubClient)
               (owner : string)
               (name : string)
               (firstTag : RepositoryTag option)
               (secondTag : RepositoryTag option) =
    async {
        let! compareResult =
            match firstTag, secondTag with
            | Some current, Some previous ->
                client.Repository.Commit.Compare(owner, name, previous.Commit.Sha, current.Commit.Sha) |> Async.AwaitTask
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
            use lineReader = new System.IO.StringReader(commit.Commit.Message)
            let mutable line = lineReader.ReadLine()
            while not <| isNull line do
                for m in regex.Matches(line) do
                    match List.ofSeq m.Groups with
                    | _::owner::repository::[issue] when owner.Success && repository.Success ->
                        yield (owner.Value, repository.Value, int issue.Value)
                    | _::_::_::[issue] ->
                        yield (owner, repository, int issue.Value)
                    | _ -> failwith "wtf"
                line <- lineReader.ReadLine()
    }

let getIssueLine (client : GitHubClient) owner repository issueNumber =
    async {
        let! issue = client.Issue.Get(owner, repository, issueNumber) |> Async.AwaitTask

        return sprintf "%s (%s/%s#%d)" issue.Title owner repository issueNumber
    }

let setReleaseText (client : GitHubClient)
                   (owner : string)
                   (repository : string)
                   (tagName : string) =
    async {
        let! (firstTag, secondTag) = getTagPair client owner repository tagName

        let! issues =
            parseCommits client owner repository firstTag secondTag
            |> AsyncSeq.toListAsync

        let! issues =
            List.distinct issues
            |> AsyncSeq.ofSeq
            |> AsyncSeq.mapAsync (fun tp -> tp |||> (getIssueLine client))
            |> AsyncSeq.fold (fun state cur -> state + "\n- " + cur) System.String.Empty

        let sinceText =
            match secondTag with
            | Some tag -> sprintf "previous release (%s)" tag.Name
            | None -> "start"

        let body =
            sprintf
                """Release %s

Issues referenced since %s: %s"""
                tagName
                sinceText
                issues

        let versionInfo = toVersion tagName

        let newRelease =
            NewRelease(tagName,
                       Name = tagName,
                       Body = body,
                       Prerelease = versionInfo.IsPrerelease,
                       Draft = false)

        let! _ = client.Repository.Release.Create(owner, repository, newRelease) |> Async.AwaitTask
        return ()
    }

open System

[<EntryPoint>]
let main argv =

    let (owner, repository, tagName) =
        match List.ofArray argv with
        | x::y::z::_ -> (x, y, z)
        | _ -> failwithf "Expected at least 3 arguments but got %d" argv.Length

    let token =
        Environment.GetEnvironmentVariable("GITHUB_TOKEN")
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith "A github token is required")

    let client = GitHubClient(ProductHeaderValue("release-notes"), Credentials = Credentials(token))
    try
        setReleaseText client owner repository tagName
        |> Async.RunSynchronously
    with ex ->
        printfn "%A" ex

    let apiInfo = client.GetLastApiInfo()
    printfn "Remaining calls: %d, Refresh: %s" (apiInfo.RateLimit.Remaining) (apiInfo.RateLimit.Reset.ToString())

    0 // return an integer exit code
