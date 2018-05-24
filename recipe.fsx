type Ingredient = Ingredient of string
type Recipe = Recipe of string

let cucumberSalad = 
    Recipe "Cucumber salad", 
    [ Ingredient "Cucumber"
      Ingredient "Vinegar"
      Ingredient "Oil" ]

let cucumberSaladWithCream = 
    Recipe "Cucumber salad with Cream", 
    [ Ingredient "Cucumber"
      Ingredient "Cream" ]


let tomatoesSalad = 
    Recipe "Tomatoe salad", 
    [ Ingredient "Tomatoe"
      Ingredient "Vinegar"
      Ingredient "Oil" ]

let reversedIngredientIndex : Map<Ingredient, Recipe list> = 
    [ cucumberSalad
      tomatoesSalad
      cucumberSaladWithCream ]
    |> List.fold (fun s (recipe, ingredients) -> 
        ingredients 
        |> List.fold (fun s x -> 
            match s |> Map.tryFind x with
            | Some l -> s |> Map.add x (recipe :: l)
            | None -> s |> Map.add x [recipe]) s) Map.empty

reversedIngredientIndex |> Map.tryFind (Ingredient "Cucumber")