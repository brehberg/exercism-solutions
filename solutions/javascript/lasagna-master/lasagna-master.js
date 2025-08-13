/// <reference path="./global.d.ts" />
// @ts-check

/**
 * Determine whether the lasagna is done.
 *
 * @param {number} currentTimer
 * @returns {string} friendly status message
 */
export function cookingStatus(currentTimer) {
  if (currentTimer === 0) {
    return "Lasagna is done.";
  }
  if (!currentTimer) {
    return "You forgot to set the timer.";
  }
  return "Not done, please wait.";
}

/**
 * Estimate the preparation time.
 *
 * @param {array} layers to prepare
 * @param {number} averageTime per layer in minutes
 * @returns {number} estimate for total prep time
 */
export function preparationTime(layers, averageTime = 2) {
  return layers.length * averageTime;
}

/**
 * Compute the amounts of noodles and sauce needed
 *
 * @param {array} layers to prepare
 * @returns {object} quantity of noodles and sauce
 */
export function quantities(layers) {
  const NOODLES_PER_LAYER = 50; // grams
  const SAUCE_PER_LAYER = 0.2; // liters

  const layer_counts = {};
  for (const layer of layers) {
    layer_counts[layer] = 1 + (layer_counts[layer] || 0);
  }
  return {
    noodles: NOODLES_PER_LAYER * layer_counts["noodles"] || 0,
    sauce: SAUCE_PER_LAYER * layer_counts["sauce"] || 0,
  };
}

/**
 * Add the secret ingredient
 *
 * @param {array} friendsList of layers
 * @param {array} myList of layers
 */
export function addSecretIngredient(friendsList, myList) {
  myList.push(friendsList[friendsList.length - 1]);
}

/**
 * Scale the recipe
 *
 * @param {object} originalRecipe of layers for 2 portions
 * @param {number} portions that you want to cook
 * @return {object} recipe with amounts needd for desired portions
 */
export function scaleRecipe(originalRecipe, portions) {
  const ORIGINAL_PORTIONS = 2;
  const newRecipe = {};
  for (const ingredient in originalRecipe) {
    const quantity = originalRecipe[ingredient];
    newRecipe[ingredient] = (quantity * portions) / ORIGINAL_PORTIONS;
  }
  return newRecipe;
}
