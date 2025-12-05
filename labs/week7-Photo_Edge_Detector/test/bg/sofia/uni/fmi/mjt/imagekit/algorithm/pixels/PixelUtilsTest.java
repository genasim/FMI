package bg.sofia.uni.fmi.mjt.imagekit.algorithm.pixels;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PixelUtilsTest {
    @Test
    void testGetGrayscaleValueReturnsLeastSignificantByte() {
        int rgb = 0x123456; // red=0x12, green=0x34, blue=0x56
        short grayscale = PixelUtils.getGrayscaleValue(rgb);

        assertEquals(0x56, grayscale, "Grayscale value should be the least significant byte of the rgb value");
    }

    @Test
    void testGetGrayscaleValueForMaxChannel() {
        int rgb = 0x000000FF; // blue channel at max
        short grayscale = PixelUtils.getGrayscaleValue(rgb);

        assertEquals(PixelUtils.MAX_CHANNEL_VALUE, grayscale,
                     "Grayscale value should be equal to MAX_CHANNEL_VALUE when low byte is 0xFF");
    }

    @Test
    void testGetRGBValueComposesColorFromChannels() {
        double red = 10.0;
        double green = 20.0;
        double blue = 30.0;

        int rgb = PixelUtils.getRGBValue(red, green, blue);

        int expected =
            (0xFF << PixelUtils.ALPHA_OFFSET) |
                (10 << PixelUtils.RED_OFFSET) |
                (20 << PixelUtils.GREEN_OFFSET) |
                (30 << PixelUtils.BLUE_OFFSET);

        assertEquals(expected, rgb, "RGB value should be composed from the three channels using the defined offsets");
    }

    @Test
    void testGetRGBValueRoundsAndClampsChannels() {
        double red = -10.4;
        double green = 255.6;
        double blue = 1000.0;

        int rgb = PixelUtils.getRGBValue(red, green, blue);

        short extractedRed = PixelUtils.getRed(rgb);
        short extractedGreen = PixelUtils.getGreen(rgb);
        short extractedBlue = PixelUtils.getBlue(rgb);

        assertAll(() -> assertEquals(PixelUtils.MIN_CHANNEL_VALUE, extractedRed,
                                     "Red channel should be clamped to MIN_CHANNEL_VALUE when below range"),
                  () -> assertEquals(PixelUtils.MAX_CHANNEL_VALUE, extractedGreen,
                                     "Green channel should be clamped to MAX_CHANNEL_VALUE when slightly above range"),
                  () -> assertEquals(PixelUtils.MAX_CHANNEL_VALUE, extractedBlue,
                                     "Blue channel should be clamped to MAX_CHANNEL_VALUE when far above range"));
    }

    @Test
    void testGetRedExtractsRedChannel() {
        int red = 200;
        int green = 150;
        int blue = 100;

        int rgb =
            (red << PixelUtils.RED_OFFSET) | (green << PixelUtils.GREEN_OFFSET) | (blue << PixelUtils.BLUE_OFFSET);

        short extractedRed = PixelUtils.getRed(rgb);

        assertEquals(red, extractedRed, "getRed should return the most significant color byte");
    }

    @Test
    void testGetGreenExtractsGreenChannel() {
        int red = 50;
        int green = 180;
        int blue = 70;

        int rgb =
            (red << PixelUtils.RED_OFFSET) | (green << PixelUtils.GREEN_OFFSET) | (blue << PixelUtils.BLUE_OFFSET);

        short extractedGreen = PixelUtils.getGreen(rgb);

        assertEquals(green, extractedGreen, "getGreen should return the middle color byte");
    }

    @Test
    void testGetBlueExtractsBlueChannel() {
        int red = 25;
        int green = 80;
        int blue = 200;

        int rgb =
            (red << PixelUtils.RED_OFFSET) | (green << PixelUtils.GREEN_OFFSET) | (blue << PixelUtils.BLUE_OFFSET);

        short extractedBlue = PixelUtils.getBlue(rgb);

        assertEquals(blue, extractedBlue, "getBlue should return the least significant color byte");
    }
}
