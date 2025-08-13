// @ts-check

const HUMIDITY_WARNING_LEVEL = 70;
const OVERHEAT_WARNING_TEMP = 500;
const OVERHEAT_CRITICAL_TEMP = 600;

export class ArgumentError extends Error {}

export class OverheatingError extends Error {
  /**
   * @param {number} temperature
   */
  constructor(temperature) {
    super(`The temperature is ${temperature} ! Overheating !`);
    this.temperature = temperature;
  }
}

/**
 * Check if the humidity level is not too high.
 *
 * @param {number} humidityPercentage
 * @throws {Error}
 */
export function checkHumidityLevel(humidityPercentage) {
  if (humidityPercentage > HUMIDITY_WARNING_LEVEL) {
    throw new Error("humidity percentage is too high");
  }
}

/**
 * Check if the temperature is not too high.
 *
 * @param {number|null} temperature
 * @throws {ArgumentError|OverheatingError}
 */
export function reportOverheating(temperature) {
  if (temperature === null) {
    throw new ArgumentError("the sensor is broken");
  } else if (temperature > OVERHEAT_WARNING_TEMP) {
    throw new OverheatingError(temperature);
  }
}

/**
 *  Triggers the needed action depending on the result of the machine check.
 *
 * @param {{
 * check: function,
 * alertDeadSensor: function,
 * alertOverheating: function,
 * shutdown: function
 * }} actions
 * @throws {ArgumentError|OverheatingError|Error}
 */
export function monitorTheMachine(actions) {
  try {
    actions.check();
  } catch (error) {
    if (error instanceof ArgumentError) {
      actions.alertDeadSensor();
    } else if (error instanceof OverheatingError) {
      if (error.temperature > OVERHEAT_CRITICAL_TEMP) {
        actions.shutdown();
      } else {
        actions.alertOverheating();
      }
    } else {
      throw error;
    }
  }
}
