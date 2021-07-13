namespace App

open Feliz
open Feliz.UseDeferred
open PetStore
open PetStore.Types

type Components =
    [<ReactComponent>]
    static member PetStore() =
        let petStore = PetStoreClient("https://petstore3.swagger.io/api/v3")
        let data = React.useDeferred(petStore.findPetsByStatus(status="available"), [| |])
        match data with
        | Deferred.HasNotStartedYet -> Html.none
        | Deferred.InProgress -> Html.h1 "Loading"
        | Deferred.Failed error -> Html.span error.Message
        | Deferred.Resolved response ->
            match response with
            | FindPetsByStatus.BadRequest ->
                Html.h1 "Bad request :/"
            | FindPetsByStatus.OK pets ->
                Html.ul [ for pet in pets -> Html.li $"{pet.name}" ]

    [<ReactComponent>]
    static member Index() =
        Html.div [
            Html.h1 "PetStore API in Fable & Feliz"
            Components.PetStore()
        ]
