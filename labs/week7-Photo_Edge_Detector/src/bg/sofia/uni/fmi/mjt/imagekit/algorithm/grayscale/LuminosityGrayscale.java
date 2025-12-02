package bg.sofia.uni.fmi.mjt.imagekit.algorithm.grayscale;

import java.awt.image.BufferedImage;

public class LuminosityGrayscale implements GrayscaleAlgorithm {
    @Override
    public BufferedImage process(BufferedImage image) {
        if (image == null) {
            throw new IllegalArgumentException("Cannot process null image");
        }

        image.getRGB(0, 0, image.getWidth(), image.getHeight(), null, 0, image.getWidth());
        return null;
    }
}
