package bg.sofia.uni.fmi.mjt.imagekit.algorithm.pixels;

public class PixelUtils {
    public static final short CHANNEL_MASK = 0x000000FF;
    public static final short MAX_CHANNEL_VALUE = 255;
    public static final short MIN_CHANNEL_VALUE = 0;

    public static final int RED_OFFSET = 16;    // 2B (2 bytes)
    public static final int GREEN_OFFSET = 8;   // 1B (1 byte)
    public static final int BLUE_OFFSET = 0;    // 0B (0 bytes)

    public static short getGrayscaleValue(int rgb) {
        return (short) (rgb & CHANNEL_MASK);
    }

    public static int getRGBValue(double red, double green, double blue) {
        int redNormed = Math.clamp(Math.round(red), MIN_CHANNEL_VALUE, MAX_CHANNEL_VALUE);
        int greenNormed = Math.clamp(Math.round(green), MIN_CHANNEL_VALUE, MAX_CHANNEL_VALUE);
        int blueNormed = Math.clamp(Math.round(blue), MIN_CHANNEL_VALUE, MAX_CHANNEL_VALUE);

        return (redNormed << RED_OFFSET) | (greenNormed << GREEN_OFFSET) | (blueNormed << BLUE_OFFSET);
    }

    public static short getRed(int rgb) {
        return (short) ((rgb >> RED_OFFSET) & CHANNEL_MASK);
    }

    public static short getGreen(int rgb) {
        return (short) ((rgb >> GREEN_OFFSET) & CHANNEL_MASK);
    }

    public static short getBlue(int rgb) {
        return (short) (rgb & CHANNEL_MASK);
    }
}
