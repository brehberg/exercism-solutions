public class Lasagna {
    private int preparationTimePerLayer = 2;

    /**
     * Define the expected oven time in minutes
     * 
     * @return total minutes the lasagna is expected to bake.
     */
    public int expectedMinutesInOven() {
        return 40;
    }

    /**
     * Calculate the remaining oven time in minutes
     * 
     * @param elaspsedBakeTime baking time already elapsed.
     * @return remaining bake time derived from 'expectedMinutesInOven'.
     */
    public int remainingMinutesInOven(int elaspsedBakeTime) {
        return expectedMinutesInOven() - elaspsedBakeTime;
    }

    /**
     * Calculate the preparation time in minutes
     * 
     * @param numberOfLayers number of layers added to the lasagna.
     * @return total prep time derived from 'preparationTimePerLayer'.
     */
    public int preparationTimeInMinutes(int numberOfLayers) {
        return preparationTimePerLayer * numberOfLayers;
    }

    /**
     * Calculate the total working time in minutes
     * 
     * @param numberOfLayers   number of layers of the lasagna.
     * @param elaspsedBakeTime baking time already elapsed.
     * @return total number of minutes you've been cooking.
     */
    public int totalTimeInMinutes(int numberOfLayers, int elaspsedBakeTime) {
        return preparationTimeInMinutes(numberOfLayers) + elaspsedBakeTime;
    }
}
