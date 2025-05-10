package traffic;

public class TrafficLightTest {
    private static final int SECOND_MS = 1000;

    public static void main(String[] args) {
        long start = System.currentTimeMillis();

        while (true) {
            long current = System.currentTimeMillis();

            for (TrafficLight light : TrafficLight.values()) {
                long duration = System.currentTimeMillis() - start;

                if (current - start >= 90 * SECOND_MS)
                    System.exit(0);

                System.out.println(light);
                waitDurationInMs(duration);
            }
        }
    }

    private static void waitDurationInMs(long duration) {
        long start = System.currentTimeMillis();
        while (true) {
            long currentTime = System.currentTimeMillis();
            if (currentTime - start >= duration)
                return;
        }
    }
}
