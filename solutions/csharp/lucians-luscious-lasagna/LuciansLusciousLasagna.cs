class Lasagna
{
    private int PreparationTimePerLayer => 2;

    // Define the expected oven time in minutes
    public int ExpectedMinutesInOven() => 40;

    // Calculate the remaining oven time in minutes.
    public int RemainingMinutesInOven(int ElapsedBakeTime) => ExpectedMinutesInOven() - ElapsedBakeTime;

    // Calculate the preparation time in minutes.
    public int PreparationTimeInMinutes(int NumberOfLayers) => PreparationTimePerLayer * NumberOfLayers;

    // Calculate the elapsed time in minutes.
    public int ElapsedTimeInMinutes(int NumberOfLayers, int ElapsedBakeTime)
    {
        return PreparationTimeInMinutes(NumberOfLayers) + ElapsedBakeTime;
    }
}
