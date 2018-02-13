module LaabApp

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.PowerPack

importAll "core-js/shim"
importAll "whatwg-fetch"

let JustLazy = importDefault<obj> "../node_modules/justlazy/src/justlazy.js"

let ready fn =
    if (document.readyState <> "loading") 
    then 
        fn()
    else
        document.addEventListener("DOMContentLoaded",
                                  U2.Case1 (unbox (fun _ -> fn())))

let fetchMy (url:string) (loadme:Element) post hidden =
    promise {
        let! response = Fetch.fetch (url.Substring(2)) [] 
        let! body = response.text()
        let mydiv = document.createElement_div()
        mydiv.id <- url
        mydiv.hidden <- hidden
        mydiv.innerHTML <- body
        loadme.appendChild(mydiv) |> ignore
        post()
        return ()
    }

let findVisible (l : NodeListOf<Element>) =
    seq { for i in 0.0 .. (l.length - 1.0) do
            let el = l.item(i) :?> HTMLElement
            if (not el.hidden) then yield el }


let makeVisible (l: NodeListOf<Element>) =
    for i in 0.0 .. (l.length - 1.0) do
        JustLazy?registerLazyLoad(l.item(i)) |> ignore

let spotted = document.URL.Split([|'#'|])
let firstUrl = match spotted.Length > 1 with
               | true -> "#" + spotted.[1]
               | false -> "firstContent"         

let rec fluff (el: HTMLElement) url target origin =
    let curactive = document.querySelector(origin + ".active")
    try
        match curactive with
        | null -> ()
        | _ -> curactive.classList.remove("active")
        let old = document.querySelectorAll("#" + target + "> div") |> findVisible
        match Seq.isEmpty old with
        | true -> ()
        | false -> (old |> Seq.head).hidden <- true
        let newVis = document.getElementById(url)
        match newVis with
        | null -> printfn "Got null with document.getElementById(%s)" url
        | _ -> newVis.hidden <- false        
        if System.String.Equals(el.id,"first") then
            fluff (document.getElementById("firstContentClick")) "firstContent" "LoadMe" "a.pageFetcher" |> ignore
        else
            makeVisible(newVis.querySelectorAll(".justlazy-spinner"))
        with
            | ex -> printfn "%s" ex.Message
    el.classList.add("active")
    box true

let selectHref href = 
    document.querySelector(sprintf @"a[href=""%s""" href) :?> HTMLElement

let rec toload target origin =
    let loadme = document.getElementById(target)
    let links = document.querySelectorAll(origin)
    let l = links.length
    for i in 0.0 .. (l-1.0) do
        let el = links.item(i) :?> HTMLElement
        let url = 
            match (el.getAttribute("href")).Substring(0,2) with
            | "#/" -> el.getAttribute("href")
            | _ -> "#/" + el.getAttribute("href")
        el.setAttribute("href",url)
        let hidden = not (System.String.Equals(url,"#/Content.html"))
        let reparseFun =
            match target with
            | "content" when url.Equals("#/Content.html") || (firstUrl.Equals("firstContent")) -> (fun _ -> toload "LoadMe" "a.pageFetcher")
            | _ -> ignore
        fetchMy url loadme reparseFun hidden |> Async.AwaitPromise |> Async.StartImmediate

open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser

type Route = Nav of string | Content of string

type Model =
    { route : Route
      content : string option }

let contentParse (state: string) = 
    if state.StartsWith "content-" then
        Ok state
    else
        Error "Does not start with content-"

let route : Parser<Route->_,_> =
    oneOf
        [ map (Content "firstContent") (top </> s "Content.html")
          map (Content "firstContent") (top)
          map Content (top </> custom "content" contentParse)
          map Nav (top </> str)]
  
open Elmish

let urlUpdate (result:Option<Route>) model =
  printfn "urlUpdate : %A" result
  match result with
  | Some (Content content) ->
      { model with route = Nav "Content.html"; content = Some content }, [] // Issue some search Cmd instead
  | Some page ->
      { model with route = page; content = None }, []
  | None ->
      ( model, Navigation.modifyUrl "#" ) // no matching route - go home

let init (result:Option<Route>) =
    ready (fun _ -> toload "content" "nav a")
    printfn "init : %A" result 
    match result with
    | Some (Content "Content.html") -> { route = Nav "Content.html"; content = Some "firstContent" }, Cmd.none
    | Some (Content s) -> { route = Nav "Content.html"; content = Some s }, Cmd.none
    | Some s -> { route = s; content = None }, Cmd.none
    | None -> { route = Nav "Content.html"; content = Some "firstContent" }, Cmd.none

let update (msg:Option<Route>) model =
    printfn "msg : %A" msg
    printfn "model : %A" model
    model, Cmd.none

Program.mkProgram init update (fun model _ -> printf "%A\n" model )
|> Program.toNavigable (parseHash route) urlUpdate
|> Program.run

// let update model msg =
//     model, Navigation.newUrl "some_other_location"
