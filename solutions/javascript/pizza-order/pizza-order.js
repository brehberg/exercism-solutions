/// <reference path="./global.d.ts" />
//
// @ts-check

/**
 * Determine the prize of the pizza given the pizza and optional extras
 *
 * @param {Pizza} pizza name of the pizza to be made
 * @param {Extra[]} extras list of extras
 *
 * @returns {number} the price of the pizza
 */
export function pizzaPrice(pizza, ...extras) {
  /**
   * @type {Record<Pizza|Extra, number>}
   */
  const PRICE = {
    Margherita: 7,
    Caprese: 9,
    Formaggio: 10,
    ExtraSauce: 1,
    ExtraToppings: 2,
  };

  /**
   * @param {number} total price of the pizza
   * @param {Extra[]} remaining list of extras
   *
   * @returns {number} the price of the pizza
   */
  function determinePrice(total, ...[extra, ...rest]) {
    return !extra ? total : determinePrice(total + PRICE[extra], ...rest);
  }

  return determinePrice(PRICE[pizza], ...extras);
}

/**
 * Calculate the prize of the total order, given individual orders
 *
 * @param {PizzaOrder[]} pizzaOrders a list of pizza orders
 * @returns {number} the price of the total order
 */
export function orderPrice(pizzaOrders) {
  return pizzaOrders.reduce(
    (total, order) => total + pizzaPrice(order.pizza, ...order.extras),
    0
  );
}
