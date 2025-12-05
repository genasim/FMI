package bg.sofia.uni.fmi.mjt.imagekit.algorithm.detection;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.grayscale.LuminosityGrayscale;
import bg.sofia.uni.fmi.mjt.imagekit.algorithm.pixels.PixelUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.awt.image.BufferedImage;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SobelEdgeDetectionTest {

    private SobelEdgeDetection algorithm;

    @BeforeEach
    void setUp() {
        algorithm = new SobelEdgeDetection(new LuminosityGrayscale());
    }

    @Test
    void testProcessNullImageThrows() {
        assertThrows(IllegalArgumentException.class,
                     () -> algorithm.process(null),
                     "Processing a null image should throw IllegalArgumentException");
    }

    @Test
    void testProcessKeepsImageDimensions() {
        int width = 5;
        int height = 4;
        BufferedImage original = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        BufferedImage result = algorithm.process(original);

        assertNotNull(result, "Resulting image must not be null");
        assertEquals(width, result.getWidth(), "Width should be preserved after Sobel processing");
        assertEquals(height, result.getHeight(), "Height should be preserved after Sobel processing");
    }

    @Test
    void testProcessUniformImageProducesNoEdges() {
        int width = 3;
        int height = 3;
        BufferedImage original = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        int gray = PixelUtils.getRGBValue(120, 120, 120);
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                original.setRGB(x, y, gray);
            }
        }

        BufferedImage result = algorithm.process(original);

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int rgb = result.getRGB(x, y);
                short value = PixelUtils.getGrayscaleValue(rgb);
                assertEquals(0, value,
                             "Uniform image should have zero Sobel response (no edges) at (" + x + "," + y + ")");
            }
        }
    }

    @Test
    void testProcessSingleBrightPixelProducesEdges() {
        int width = 3;
        int height = 3;
        BufferedImage original = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        int black = PixelUtils.getRGBValue(0, 0, 0);
        int white = PixelUtils.getRGBValue(255, 255, 255);

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                original.setRGB(x, y, black);
            }
        }

        original.setRGB(1, 1, white);

        BufferedImage result = algorithm.process(original);

        boolean anyEdgeDetected = false;
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int rgb = result.getRGB(x, y);
                short value = PixelUtils.getGrayscaleValue(rgb);
                if (value > 0) {
                    anyEdgeDetected = true;
                    break;
                }
            }
            if (anyEdgeDetected) {
                break;
            }
        }

        assertTrue(anyEdgeDetected,
                   "At least one pixel should have a non-zero edge magnitude around a single bright pixel");
    }

    @Test
    void testProcessDoesNotMutateOriginalImage() {
        int width = 3;
        int height = 3;
        BufferedImage original = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        int black = PixelUtils.getRGBValue(0, 0, 0);
        int white = PixelUtils.getRGBValue(255, 255, 255);

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                original.setRGB(x, y, black);
            }
        }
        original.setRGB(1, 1, white);

        int[][] before = new int[height][width];
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                before[y][x] = original.getRGB(x, y);
            }
        }

        algorithm.process(original);

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                assertEquals(before[y][x], original.getRGB(x, y),
                             "Original image must not be modified at (" + x + "," + y + ")");
            }
        }
    }
}
