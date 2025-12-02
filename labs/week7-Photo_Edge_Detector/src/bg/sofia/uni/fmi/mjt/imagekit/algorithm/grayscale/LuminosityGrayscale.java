package bg.sofia.uni.fmi.mjt.imagekit.algorithm.grayscale;

import java.awt.image.BufferedImage;

public class LuminosityGrayscale implements GrayscaleAlgorithm {
    private final static float RED_FACTOR = 0.21f;
    private final static float GREEN_FACTOR = 0.72f;
    private final static float BLUE_FACTOR = 0.07f;

    @Override
    public BufferedImage process(BufferedImage image) {
        if (image == null) {
            throw new IllegalArgumentException("Cannot process null image");
        }

        int width = image.getWidth();
        int height = image.getHeight();

        BufferedImage grayscaleImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int rgb = image.getRGB(x, y);

                int grayscale = processPixel(rgb);
                grayscaleImage.setRGB(x, y, grayscale);
            }
        }
        return grayscaleImage;
    }

    private int processPixel(int rgb) {
        int red = (rgb >> 16) & 0xFF;
        int green = (rgb >> 8) & 0xFF;
        int blue = rgb & 0xFF;

        float factoredValue = RED_FACTOR * red + GREEN_FACTOR * green + BLUE_FACTOR * blue;
        int value = Math.clamp(Math.round(factoredValue), 0, 255);

        return (value << 16) | (value << 8) | value;
    }
}
