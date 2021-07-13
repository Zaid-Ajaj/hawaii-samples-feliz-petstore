namespace PetStore.Http

open System
open System.Collections.Generic
open Fable.SimpleJson
open Fable.SimpleHttp
open Fable.Core
open Browser.Types

module Serializer =
    let inline serialize<'t> (value: 't) = Json.serialize value
    let inline deserialize<'t> (content: string) = Json.parseNativeAs<'t>(content)

[<RequireQualifiedAccess>]
type OpenApiValue =
    | Int of int
    | Int64 of int64
    | String of string
    | Bool of bool
    | Double of double
    | Float of float32
    | List of OpenApiValue list

type MultiPartFormData =
    | Primitive of OpenApiValue
    | File of File

type RequestPart =
    | Query of string * OpenApiValue
    | Path of string * OpenApiValue
    | Header of string * OpenApiValue
    | MultiPartFormData of string * MultiPartFormData
    | UrlEncodedFormData of string * OpenApiValue
    | JsonContent of string
    | BinaryContent of byte[]
    | Ignore

    static member query(key: string, value: int) = Query(key, OpenApiValue.Int value)
    static member query(key: string, values: int list) = Query(key, OpenApiValue.List [ for value in values -> OpenApiValue.Int value ])
    static member query(key: string, value: int64) = Query(key, OpenApiValue.Int64 value)
    static member query(key: string, value: string) = Query(key, OpenApiValue.String value)
    static member query(key: string, value: string option) =
        match value with
        | Some text -> Query(key, OpenApiValue.String text)
        | None -> Ignore
    static member query(key: string, value: int option) =
        match value with
        | Some number -> Query(key, OpenApiValue.Int number)
        | None -> Ignore
    static member query(key: string, value: bool option) =
        match value with
        | Some flag -> Query(key, OpenApiValue.Bool flag)
        | None -> Ignore
    static member query(key: string, value: double option) =
        match value with
        | Some number -> Query(key, OpenApiValue.Double number)
        | None -> Ignore
    static member inline query< ^a when ^a : (member Format: unit -> string)>(key: string, value: ^a) : RequestPart =
        let format = (^a: (member Format: unit -> string) (value))
        Query(key, OpenApiValue.String format)
    static member inline query< ^a when ^a : (member Format: unit -> string)>(key: string, value: ^a option) : RequestPart =
        match value with
        | None -> Ignore
        | Some instance ->
            let format = (^a: (member Format: unit -> string) (instance))
            Query(key, OpenApiValue.String format)
    static member query(key: string, value: int64 option) =
        match value with
        | Some number -> Query(key, OpenApiValue.Int64 number)
        | None -> Ignore

    static member query(key: string, values: string list) = Query(key, OpenApiValue.List [ for value in values -> OpenApiValue.String value ])
    static member query(key: string, values: Guid list) = Query(key, OpenApiValue.List [ for value in values -> OpenApiValue.String (value.ToString()) ])
    static member query(key: string, value: bool) = Query(key, OpenApiValue.Bool value)
    static member query(key: string, value: double) = Query(key, OpenApiValue.Double value)
    static member query(key: string, value: float32) = Query(key, OpenApiValue.Float value)
    static member query(key: string, value: Guid) = Query(key, OpenApiValue.String (value.ToString()))
    static member query(key: string, value: DateTimeOffset) = Query(key, OpenApiValue.String (value.ToString("O")))
    static member path(key: string, value: int) = Path(key, OpenApiValue.Int value)
    static member path(key: string, value: int64) = Path(key, OpenApiValue.Int64 value)
    static member path(key: string, value: string) = Path(key, OpenApiValue.String value)
    static member path(key: string, value: bool) = Path(key, OpenApiValue.Bool value)
    static member path(key: string, value: double) = Path(key, OpenApiValue.Double value)
    static member path(key: string, value: float32) = Path(key, OpenApiValue.Float value)
    static member path(key: string, value: Guid) = Path(key, OpenApiValue.String (value.ToString()))
    static member path(key: string, value: DateTimeOffset) = Path(key, OpenApiValue.String (value.ToString("O")))
    static member path(key: string, values: string list) = Path(key, OpenApiValue.List [ for value in values -> OpenApiValue.String value ])
    static member path(key: string, values: Guid list) = Path(key, OpenApiValue.List [ for value in values -> OpenApiValue.String (value.ToString()) ])
    static member path(key: string, values: int list) = Path(key, OpenApiValue.List [ for value in values -> OpenApiValue.Int value ])
    static member path(key: string, values: int64 list) = Path(key, OpenApiValue.List [ for value in values -> OpenApiValue.Int64 value ])
    static member inline path< ^a when ^a : (member Format: unit -> string)>(key: string, value: ^a) : RequestPart =
        let format = (^a: (member Format: unit -> string) (value))
        Path(key, OpenApiValue.String format)
    static member inline path< ^a when ^a : (member Format: unit -> string)>(key: string, value: ^a option) : RequestPart =
        match value with
        | None -> Ignore
        | Some instance ->
            let format = (^a: (member Format: unit -> string) (instance))
            Path(key, OpenApiValue.String format)
    static member multipartFormData(key: string, value: int) =
        MultiPartFormData(key, Primitive(OpenApiValue.Int value))
    static member multipartFormData(key: string, value: int64) =
        MultiPartFormData(key, Primitive(OpenApiValue.Int64 value))
    static member multipartFormData(key: string, value: string) =
        MultiPartFormData(key, Primitive(OpenApiValue.String value))
    static member multipartFormData(key: string, value: bool) =
        MultiPartFormData(key, Primitive(OpenApiValue.Bool value))
    static member multipartFormData(key: string, value: double) =
        MultiPartFormData(key, Primitive(OpenApiValue.Double value))
    static member multipartFormData(key: string, value: float32) =
        MultiPartFormData(key, Primitive(OpenApiValue.Float value))
    static member multipartFormData(key: string, value: Guid) =
        MultiPartFormData(key, Primitive(OpenApiValue.String (value.ToString())))
    static member multipartFormData(key: string, value: DateTimeOffset) =
        MultiPartFormData(key, Primitive(OpenApiValue.String (value.ToString("O"))))
    static member multipartFormData(key: string, value: File) =
        MultiPartFormData(key, File value)
    static member multipartFormData(key: string, values: string list) = MultiPartFormData(key, Primitive(OpenApiValue.List [ for value in values -> OpenApiValue.String value ]))
    static member multipartFormData(key: string, values: Guid list) = MultiPartFormData(key, Primitive(OpenApiValue.List [ for value in values -> OpenApiValue.String (value.ToString()) ]))
    static member multipartFormData(key: string, values: int list) = MultiPartFormData(key, Primitive(OpenApiValue.List [ for value in values -> OpenApiValue.Int value ]))
    static member multipartFormData(key: string, values: int64 list) = MultiPartFormData(key, Primitive(OpenApiValue.List [ for value in values -> OpenApiValue.Int64 value ]))
    static member urlEncodedFormData(key: string, value: string) = UrlEncodedFormData(key, OpenApiValue.String value)
    static member urlEncodedFormData(key: string, value: bool) = UrlEncodedFormData(key, OpenApiValue.Bool value)
    static member urlEncodedFormData(key: string, value: double) = UrlEncodedFormData(key, OpenApiValue.Double value)
    static member urlEncodedFormData(key: string, value: float32) = UrlEncodedFormData(key, OpenApiValue.Float value)
    static member urlEncodedFormData(key: string, value: Guid) = UrlEncodedFormData(key, OpenApiValue.String (value.ToString()))
    static member urlEncodedFormData(key: string, value: DateTimeOffset) = UrlEncodedFormData(key, OpenApiValue.String (value.ToString("O")))
    static member header(key: string, value: int) = Header(key, OpenApiValue.Int value)
    static member header(key: string, value: int64) = Header(key, OpenApiValue.Int64 value)
    static member header(key: string, value: string) = Header(key, OpenApiValue.String value)
    static member header(key: string, value: bool) = Header(key, OpenApiValue.Bool value)
    static member header(key: string, value: double) = Header(key, OpenApiValue.Double value)
    static member header(key: string, value: float32) = Header(key, OpenApiValue.Float value)
    static member header(key: string, value: Guid) = Header(key, OpenApiValue.String (value.ToString()))
    static member header(key: string, value: DateTimeOffset) = Header(key, OpenApiValue.String (value.ToString("O")))
    static member inline jsonContent<'t>(content: 't) = JsonContent(Serializer.serialize content)
    static member binaryContent(content: byte[]) = BinaryContent(content)

module OpenApiHttp =
    let rec serializeValue = function
        | OpenApiValue.String value -> value
        | OpenApiValue.Int value -> string value
        | OpenApiValue.Int64 value -> string value
        | OpenApiValue.Double value -> string value
        | OpenApiValue.Float value -> string value
        | OpenApiValue.Bool value -> string value
        | OpenApiValue.List values ->
            values
            |> List.map serializeValue
            |> String.concat ","

    let applyPathParts (path: string) (parts: RequestPart list) =
        let applyPart (currentPath: string) (part: RequestPart) : string =
            match part with
            | Path(key, value) -> currentPath.Replace("{" + key + "}", serializeValue value)
            | _ -> currentPath

        parts |> List.fold applyPart path

    [<Emit "encodeURIComponent($0)">]
    let encodeURIComponent(queryValue: string) = jsNative

    let applyQueryStringParameters (currentPath: string) (parts: RequestPart list) =
        let cleanedPath = currentPath.TrimEnd '/'
        let queryParams =
            parts
            |> List.choose (function
                | Query(key, value) -> Some(key, value)
                | _ -> None)

        if List.isEmpty queryParams then
            cleanedPath
        else
            let combinedParamters =
                queryParams
                |> List.map (fun (key, value) -> $"{key}={encodeURIComponent(serializeValue value)}")
                |> String.concat "&"

            cleanedPath + "?" + combinedParamters

    let combineBasePath (basePath: string) (path: string) : string =
        basePath.TrimEnd '/' + "/" + path.TrimStart '/'

    let applyJsonRequestBody (parts: RequestPart list) (httpRequest: HttpRequest) =
        let bodyJson =
            parts
            |> List.choose(function
                | JsonContent content -> Some content
                | _ -> None)
            |> List.tryHead

        match bodyJson with
        | None -> httpRequest
        | Some json ->
            httpRequest
            |> Http.header (Headers.contentType "application/json")
            |> Http.content (BodyContent.Text json)

    let applyMultipartFormData (parts: RequestPart list) (httpRequest: HttpRequest) =
        let formParts =
            parts
            |> List.choose(function
                | MultiPartFormData(key, value) -> Some(key, value)
                | _ -> None
            )

        if formParts.Length = 0 then
            httpRequest
        else
            let formData = FormData.create()
            for (key, value) in formParts do
                match value with
                | Primitive primitive -> formData.append(key, serializeValue primitive)
                | File file -> formData.append(key, file)

            httpRequest
            |> Http.content (BodyContent.Form formData)

    let applyUrlEncodedFormData (parts: RequestPart list) (httpRequest: HttpRequest) =
        let formParts =
            parts
            |> List.choose(function
                | UrlEncodedFormData(key, value) -> Some(key, value)
                | _ -> None
            )

        if formParts.Length = 0 then
            httpRequest
        else
            let encodedData =
                formParts
                |> List.map (fun (key, value) -> $"{key}={encodeURIComponent(serializeValue value)}")
                |> String.concat "&"

            httpRequest
            |> Http.header (Headers.contentType "application/x-www-form-urlencoded")
            |> Http.content (BodyContent.Text encodedData)

    let sendAsync (method: HttpMethod) (basePath: string) (path: string) (parts: RequestPart list) : Async<int * string> =
        async {
            let requestPath = applyPathParts path parts
            let requestPathWithQuery = applyQueryStringParameters requestPath parts
            let fullPath = combineBasePath basePath requestPathWithQuery
            let! response =
                Http.request fullPath
                |> Http.method method
                |> applyJsonRequestBody parts
                |> applyMultipartFormData parts
                |> applyUrlEncodedFormData parts
                |> Http.send

            let status = response.statusCode
            let content = response.responseText
            return status, content
        }

    let getAsync (basePath: string) (path: string) (parts: RequestPart list) =
        sendAsync GET basePath path parts

    let postAsync (basePath: string) (path: string) (parts: RequestPart list) =
        sendAsync POST basePath  path parts

    let deleteAsync (basePath: string) (path: string) (parts: RequestPart list) =
        sendAsync DELETE basePath  path parts

    let putAsync (basePath: string) (path: string) (parts: RequestPart list) =
        sendAsync PUT basePath path parts

    let patchAsync (basePath: string) (path: string) (parts: RequestPart list) =
        sendAsync PATCH basePath path parts

    let headAsync (basePath: string) (path: string) (parts: RequestPart list) =
        sendAsync HEAD basePath path parts
