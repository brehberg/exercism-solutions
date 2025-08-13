module PizzaPricing

type Pizza =
    | Margherita
    | Caprese
    | Formaggio
    | ExtraSauce of Pizza
    | ExtraToppings of Pizza

let rec pizzaPrice (pizza: Pizza): int = 
    match pizza with
    | Margherita -> 7
    | Caprese -> 9
    | Formaggio -> 10
    | ExtraSauce pizza -> 1 + pizzaPrice pizza
    | ExtraToppings pizza -> 2 + pizzaPrice pizza

let orderPrice(pizzas: Pizza list): int = 
    let rec computeOrderPrice(pizzas: Pizza list, totalPrice: int): int =
        match pizzas with
        | [] -> totalPrice
        | pizza::rest -> computeOrderPrice(rest, totalPrice + pizzaPrice pizza)

    let basePrice = 
        match pizzas.Length with
        | 1 -> 3
        | 2 -> 2
        | _ -> 0
    computeOrderPrice(pizzas, basePrice)
