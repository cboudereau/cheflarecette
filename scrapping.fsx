#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"

open FSharp.Data

let (</>) x y = System.IO.Path.Combine(x, y)

let doc = HtmlDocument.Load(__SOURCE_DIRECTORY__ </> "recettes.htm", System.Text.Encoding.UTF8)

type Ingredient = Ingredient of string
type TitreRecette = TitreRecette of string

type Page = Page of int

type Recette = 
    { Page : Page
      Ingredients : Ingredient list
      Titre : TitreRecette }

module Hex = 
    let toInt x = System.Convert.ToInt32(x,16)

let recettes = 
    doc.CssSelect("div[data-page-no]")
    |> List.choose (fun x -> 
        //Il existe des pages sans recette donc la recette est optionelle dans la page et on va filtrer + analyser la data en même temps avec un Option type
        let recetteO =  x.CssSelect("div.t.m0.h5") |> List.map (fun n -> n.InnerText().ToLowerInvariant()) |> List.tryHead |> Option.map TitreRecette
        recetteO 
        |> Option.map(fun recette -> 
            let page = x.Attribute("data-page-no").Value() |> Hex.toInt        
            let ingredients = x.CssSelect("div.t.m0.h6.ff3") |> List.map (fun n -> n.InnerText().ToLowerInvariant() |> Ingredient)
            { Recette.Page = Page page; Ingredients = ingredients; Titre = recette }))

let entries = 
    //Fonction qui aggrège un index (sous la forme Map<Ingredient, TitreRecette list>) avec une recette contenant des ingrédients
    let folder idx r = 
        //La même fonction mais au niveau ingrédients : on descend sur la recette, on passe l'index en paramètre (le state) puis on gère le cas de l'ajout dans l'index
        let folder r idx i = 
            let recettes = 
                match idx |> Map.tryFind i with
                | Some recettes -> r.Titre :: recettes
                | None -> [r.Titre]
            idx |> Map.add i recettes
        r.Ingredients |> List.fold (folder r) idx
    recettes 
    |> List.fold folder Map.empty
    |> Map.toList