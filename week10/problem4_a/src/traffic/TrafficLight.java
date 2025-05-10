package traffic;

public enum TrafficLight {
    RED(2000),
    YELLOW(1000),
    GREEN(3000);

    private final long durationMs;

    TrafficLight(long duration) {
        durationMs = duration;
    }

    public long getDurationMs() {
        return durationMs;
    }
}
