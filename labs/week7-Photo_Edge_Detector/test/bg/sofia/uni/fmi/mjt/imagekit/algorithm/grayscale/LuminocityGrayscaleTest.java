package bg.sofia.uni.fmi.mjt.imagekit.algorithm.grayscale;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.pixels.PixelUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.awt.image.BufferedImage;

import static org.junit.jupiter.api.Assertions.*;

class LuminocityGrayscaleTest {
    private GrayscaleAlgorithm algorithm;

    @BeforeEach
    void setUp() {
        algorithm = new LuminosityGrayscale();
    }

    @Test
    void testProcessNullImageThrows() {
        assertThrows(IllegalArgumentException.class, () -> algorithm.process(null),
                     "Should throw IllegalArgumentException when trying to process null image");
    }

    @Test
    void testProcessProducesNewImage() {
        var input = new BufferedImage(100, 100, BufferedImage.TYPE_INT_RGB);

        var output = algorithm.process(input);
        assertNotSame(input, output, "Should produce new BufferedImage instance");
    }


    @Test
    void testProcessReturnsImageWithSameDimensionsAndType() {
        int width = 3;
        int height = 2;
        BufferedImage original = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        BufferedImage result = algorithm.process(original);

        assertNotNull(result);
        assertEquals(width, result.getWidth(), "Width should be preserved");
        assertEquals(height, result.getHeight(), "Height should be preserved");
        assertEquals(BufferedImage.TYPE_INT_RGB, result.getType(),
                     "Result image type should be TYPE_INT_RGB");
    }

    @Test
    void testProcessAppliesLuminosityFormulaForSinglePixel() {
        int red = 100;
        int green = 150;
        int blue = 200;

        BufferedImage original = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
        int rgb = PixelUtils.getRGBValue(red, green, blue);
        original.setRGB(0, 0, rgb);

        BufferedImage result = algorithm.process(original);
        int resultRgb = result.getRGB(0, 0);

        float value = 0.21f * red + 0.72f * green + 0.07f * blue;
        int expectedRgb = PixelUtils.getRGBValue(value, value, value);

        assertEquals(expectedRgb, resultRgb, "Grayscale RGB should match luminosity formula");

        short r = PixelUtils.getRed(resultRgb);
        short g = PixelUtils.getGreen(resultRgb);
        short b = PixelUtils.getBlue(resultRgb);

        assertEquals(r, g, "Red and green channels should be equal for grayscale");
        assertEquals(g, b, "Green and blue channels should be equal for grayscale");
    }

    @Test
    void testProcessDoesNotMutateOriginalImage() {
        BufferedImage original = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
        int red = 10, green = 20, blue = 30;
        int rgb = PixelUtils.getRGBValue(red, green, blue);
        original.setRGB(0, 0, rgb);

        algorithm.process(original);

        assertEquals(rgb, original.getRGB(0, 0),
                     "Original image must not be modified by the algorithm");
    }

    @Test
    void testProcessAllPixelsAreProcessed() {
        int width = 2;
        int height = 2;
        BufferedImage original = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        original.setRGB(0, 0, PixelUtils.getRGBValue(255, 0, 0));   // red
        original.setRGB(1, 0, PixelUtils.getRGBValue(0, 255, 0));   // green
        original.setRGB(0, 1, PixelUtils.getRGBValue(0, 0, 255));   // blue
        original.setRGB(1, 1, PixelUtils.getRGBValue(50, 100, 150));// arbitrary

        BufferedImage result = algorithm.process(original);

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int resultRgb = result.getRGB(x, y);
                short r = PixelUtils.getRed(resultRgb);
                short g = PixelUtils.getGreen(resultRgb);
                short b = PixelUtils.getBlue(resultRgb);

                assertEquals(r, g, "Red and green should match at (" + x + "," + y + ")");
                assertEquals(g, b, "Green and blue should match at (" + x + "," + y + ")");
            }
        }
    }
}
