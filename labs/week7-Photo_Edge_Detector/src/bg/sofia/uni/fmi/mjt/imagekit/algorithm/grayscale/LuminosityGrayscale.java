package bg.sofia.uni.fmi.mjt.imagekit.algorithm.grayscale;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.pixels.PixelUtils;

import java.awt.image.BufferedImage;

public class LuminosityGrayscale implements GrayscaleAlgorithm {
    private static final float RED_FACTOR = 0.21f;
    private static final float GREEN_FACTOR = 0.72f;
    private static final float BLUE_FACTOR = 0.07f;

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
        int red = PixelUtils.getRed(rgb);
        int green = PixelUtils.getGreen(rgb);
        int blue = PixelUtils.getBlue(rgb);

        float value = RED_FACTOR * red + GREEN_FACTOR * green + BLUE_FACTOR * blue;
        return PixelUtils.getRGBValue(value, value, value);
    }
}
